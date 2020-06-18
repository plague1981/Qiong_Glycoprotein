library(shiny)
library(shinythemes)
library(shinydashboard)
library(datasets)
library(readxl)
library(xlsx)
library(VennDiagram)
library(rapportools)

ui <-dashboardPage(
  dashboardHeader(title = 'Qiong\'s project'),

  dashboardSidebar(  
    sidebarMenu(
      menuItem("Venn diagram 2", tabName = "venndiagram2", icon = icon("dashboard")),
      menuItem("Venn diagram 3", tabName = "venndiagram3", icon = icon("dashboard"))
    )

  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "venndiagram2", h2("Venn diagram 2"),
        box(
          plotOutput(outputId = 'venndiagram2'),
          downloadLink("downloadDE2", "Download xlsx file")
        ),
        box(
          # name the venn plot
          textInput('DE2_name', 'Please name the plot', value = 'Venn diagram 2'),
          tags$hr(),
          # upload DE2 files
          fileInput(inputId = 'protein_DE2_file1',label = 'File1', multiple = FALSE),
          textInput(inputId = 'protein_DE2_file1_name', 'Name of File1 (Optional)', value = 'File1'),
          tags$hr(),
          fileInput(inputId = 'protein_DE2_file2',label = 'File2', multiple = FALSE),
          textInput(inputId = 'protein_DE2_file2_name', 'Name of File2 (Optional)', value = 'File2'),
          # submit button for uploading DE2 files
          actionButton(inputId = "submit_DE2_files", label = "Submit")
        )
      ),
      tabItem(tabName = "venndiagram3", h2("Venn diagram 3"),
        box(
          plotOutput(outputId = 'venndiagram3'),
          downloadLink("downloadDE3", "Download xlsx file")
        ),
        box(
          # name the venn plot
          textInput('DE3_name', 'Please name the plot', value = 'Venn diagram 3'),
          tags$hr(),
          # upload DE3 files
          fileInput(inputId = 'protein_DE3_file1',label = 'File1', multiple = FALSE),
          textInput(inputId = 'protein_DE3_file1_name', 'Name of File1 (Optional)', value = 'File1'),
          tags$hr(),
          fileInput(inputId = 'protein_DE3_file2',label = 'File2', multiple = FALSE),
          textInput(inputId = 'protein_DE3_file2_name', 'Name of File2 (Optional)', value = 'File2'),
          tags$hr(),
          fileInput(inputId = 'protein_DE3_file3',label = 'File3', multiple = FALSE),
          textInput(inputId = 'protein_DE3_file3_name', 'Name of File3 (Optional)', value = 'File3'),
          # submit button for uploading DE3 files
          actionButton(inputId = "submit_DE3_files", label = "Submit")
        )
      ), tableOutput('test')
    )
  )
) 

server <- function(input, output) {
  # =========================Venn2
  # Condition
  list1.DE2<- eventReactive(input$submit_DE2_files,{ 
    plist<-read_excel(input$protein_DE2_file1$datapath,sheet = 1, col_names = FALSE)[1] 
    colnames(plist)<-input$protein_DE2_file1_name
    return(plist)
  })
  list2.DE2<- eventReactive(input$submit_DE2_files,{ 
    plist<-read_excel(input$protein_DE2_file2$datapath,sheet = 1, col_names = FALSE)[1]
    colnames(plist)<-input$protein_DE2_file2_name
    return(plist)
  })
  list1.only.DE2<-eventReactive(input$submit_DE2_files,{
    list.only<-setdiff(unlist(list1()),unlist(cross.list()))
    return(data.frame(list.only))
  })
  list2.only.DE2<-eventReactive(input$submit_DE2_files,{
    list.only<-setdiff(unlist(list2()),unlist(cross.list()))
    return(data.frame(list.only))
  })
  cross.list.DE2<- eventReactive(input$submit_DE2_files,{
    plist<-data.frame(intersect(unlist(read_excel(input$protein_DE2_file1$datapath,sheet = 1, col_names = FALSE)[1]),
                                unlist(read_excel(input$protein_DE2_file2$datapath,sheet = 1, col_names = FALSE)[1])))
    colnames(plist)<-'Cross'
    return(plist)
  })
  # Draw Venn diagram
  VD2<- eventReactive(input$submit_DE2_files,{ 
    VDiagram  <- venn.diagram(
      x<-list(A=unlist(list1.DE2()),B=unlist(list2.DE2())),
      category.names = c(input$protein_DE2_file1_name,input$protein_DE2_file2_name),
      fill=rainbow(length(x)),
      filename = NULL, main = input$DE2_name)
    grid.newpage()
    return(grid.draw(VDiagram))
  })
  # Display
  output$venndiagram2 <- renderPlot({
    VD2()
  })
  output$downloadDE2 <- downloadHandler(
    filename = function() {
      paste0(input$protein_DE2_file1_name, input$protein_DE2_file2_name, ".DE2.xlsx")
    },
    content = function(file) {
      if (!is.empty(unlist(list1.only.DE2())[1])){ 
      write.xlsx(list1.only.DE2(), sheetName = paste0(input$protein_DE2_file1_name,'only'),file)
      }
      if (!is.empty(unlist(list2.only.DE2())[1])){ 
      write.xlsx(list2.only.DE2(), sheetName = paste0(input$protein_DE2_file2_name,'only'),file, append = TRUE)
      }
      if (!is.empty(unlist(cross.list.DE2())[1])){ 
      write.xlsx(cross.list.DE2(), sheetName = 'Cross',file, append = TRUE)
      }
  })
  # ======================Venn3
  # Condition
  list1.DE3<- eventReactive(input$submit_DE3_files,{ 
    plist<-read_excel(input$protein_DE3_file1$datapath,sheet = 1, col_names = FALSE)[1] 
    colnames(plist)<-input$protein_DE3_file1_name
    return(plist)
  })
  list2.DE3<- eventReactive(input$submit_DE3_files,{ 
    plist<-read_excel(input$protein_DE3_file2$datapath,sheet = 1, col_names = FALSE)[1]
   colnames(plist)<-input$protein_DE3_file2_name
    return(plist)
  })
  list3.DE3<- eventReactive(input$submit_DE3_files,{ 
    plist<-read_excel(input$protein_DE3_file3$datapath,sheet = 1, col_names = FALSE)[1]
    colnames(plist)<-input$protein_DE3_file3_name
    return(plist)
  })
  list1.only.DE3<- eventReactive(input$submit_DE3_files,{ 
    plist<- data.frame(setdiff(unlist(list1.DE3()),union(unlist(list2.DE3()),unlist(list3.DE3()))))
    colnames(plist)<-paste0(input$protein_DE3_file1_name,'only')
    return(plist)
  })
  list2.only.DE3<- eventReactive(input$submit_DE3_files,{ 
    plist<- data.frame(setdiff(unlist(list2.DE3()),union(unlist(list1.DE3()),unlist(list3.DE3()))))
    colnames(plist)<-paste0(input$protein_DE3_file2_name,'only')
    return(plist)
  })
  list3.only.DE3<- eventReactive(input$submit_DE3_files,{ 
    plist<- data.frame(setdiff(unlist(list3.DE3()),union(unlist(list2.DE3()),unlist(list1.DE3()))))
    colnames(plist)<-paste0(input$protein_DE3_file3_name,'only')
    return(plist)
  })  
  cross.list.DE3<- eventReactive(input$submit_DE3_files,{
    plist<-data.frame(intersect(unlist(list1.DE3()), intersect(unlist(list2.DE3()),unlist(list3.DE3()))))
    colnames(plist)<-'Cross' 
    return(plist)
  })
  list1.list2.only.DE3<-eventReactive(input$submit_DE3_files,{
    plist<- data.frame(setdiff(intersect(unlist(list1.DE3()),unlist(list2.DE3())),unlist(cross.list.DE3())))
    colnames(plist)<-paste0(input$protein_DE3_file1_name,input$protein_DE3_file2_name)
    return(plist)
  })
  list2.list3.only.DE3<-eventReactive(input$submit_DE3_files,{
    plist<- data.frame(setdiff(intersect(unlist(list2.DE3()),unlist(list3.DE3())),unlist(cross.list.DE3())))
    colnames(plist)<-paste0(input$protein_DE3_file2_name,input$protein_DE3_file3_name)
    return(plist)
  })
  list1.list3.only.DE3<-eventReactive(input$submit_DE3_files,{
    plist<- data.frame(setdiff(intersect(unlist(list1.DE3()),unlist(list3.DE3())),unlist(cross.list.DE3())))
    colnames(plist)<-paste0(input$protein_DE3_file1_name,input$protein_DE3_file3_name)
    return(plist)
  })

  # Draw Venn diagram
  VD3<- eventReactive(input$submit_DE3_files,{ 
    VDiagram  <- venn.diagram(
      x<-list(A=unlist(list1.DE3()),B=unlist(list2.DE3()),C=unlist(list3.DE3())),
     category.names = c(input$protein_DE3_file1_name,input$protein_DE3_file2_name, input$protein_DE3_file3_name),
      fill=rainbow(length(x)),
      filename = NULL, main = input$DE3_name)
    grid.newpage()
    return(grid.draw(VDiagram))
  })
  # Display
  output$venndiagram3 <- renderPlot({
    VD3()
  })
  output$downloadDE3 <- downloadHandler(
    filename = function() {
      paste0(input$protein_DE3_file1_name, input$protein_DE3_file2_name, input$protein_DE3_file3_name, ".DE3.xlsx")
    },
    content = function(file) {
      if (!is.empty(unlist(list1.only.DE3())[1])){  
        write.xlsx(list1.only.DE3(), sheetName = paste0(input$protein_DE3_file1_name,'only'),file)
      } 
      if (!is.empty(unlist(list2.only.DE3())[1])){
        write.xlsx(list2.only.DE3(), sheetName = paste0(input$protein_DE3_file2_name,'only'),file, append = TRUE)
      } 
      if (!is.empty(unlist(list3.only.DE3())[1])){
        write.xlsx(list3.only.DE3(), sheetName = paste0(input$protein_DE3_file3_name,'only'),file, append = TRUE)
      } 
      if (!is.empty(unlist(list1.list2.only.DE3())[1])){
        write.xlsx(list1.list2.only.DE3(), sheetName = paste0(input$protein_DE3_file1_name,input$protein_DE3_file2_name),file, append = TRUE)
      } 
      if (!is.empty(unlist(list2.list3.only.DE3())[1])){
        write.xlsx(list2.list3.only.DE3(), sheetName = paste0(input$protein_DE3_file2_name,input$protein_DE3_file3_name),file, append = TRUE)
      } 
      if (!is.empty(unlist(list1.list3.only.DE3())[1])){
        write.xlsx(list1.list3.only.DE3(), sheetName = paste0(input$protein_DE3_file1_name,input$protein_DE3_file3_name),file, append = TRUE)
      }
      if (!is.empty(unlist(cross.list.DE3())[1])){
        write.xlsx(cross.list.DE3(), sheetName = 'Cross',file, append = TRUE)
      } 
    })
    output$test<-renderTable({

    })
}

shinyApp(ui, server)
