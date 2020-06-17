library(shiny)
library(shinythemes)
library(shinydashboard)
library(datasets)
library(readxl)
library(xlsx)
library(VennDiagram)

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
          plotOutput(outputId = 'venndiagram2', width = 400, height = 400),
          downloadLink("downloadDE2", "Download xlsx file")
        ),
        box(width = 3,
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
      )
    )
  )
) 

server <- function(input, output) {
  # Venn2
  # Condition
  cross.list<- eventReactive(input$submit_DE2_files,{
    plist<-data.frame(intersect(unlist(read_excel(input$protein_DE2_file1$datapath,sheet = 1, col_names = FALSE)[1]),
                                unlist(read_excel(input$protein_DE2_file2$datapath,sheet = 1, col_names = FALSE)[1])))
    colnames(plist)<-'Cross'
    return(plist)
  })
  list1<- eventReactive(input$submit_DE2_files,{ 
    plist<-read_excel(input$protein_DE2_file1$datapath,sheet = 1, col_names = FALSE)[1] 
    colnames(plist)<-'File1'
    return(plist)
  })
  list2<- eventReactive(input$submit_DE2_files,{ 
    plist<-read_excel(input$protein_DE2_file2$datapath,sheet = 1, col_names = FALSE)[1]
    colnames(plist)<-'File2'
    return(plist)
  })
  list1.only<-eventReactive(input$submit_DE2_files,{
    plist<-read_excel(input$protein_DE2_file1$datapath,sheet = 1, col_names = FALSE)[1]
    cross<-data.frame(intersect(unlist(read_excel(input$protein_DE2_file1$datapath,sheet = 1, col_names = FALSE)[1]),
                                unlist(read_excel(input$protein_DE2_file2$datapath,sheet = 1, col_names = FALSE)[1])))
    list.only<-setdiff(plist,cross)
    return(list.only)
  })
  # Draw Venn diagram
  VD2<- eventReactive(input$submit_DE2_files,{ 
    VDiagram  <- venn.diagram(
      x<-list(A=unlist(list1()),B=unlist(list2())),
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
      write.xlsx(list1.only(), sheetName = input$protein_DE2_file1_name,file)
      write.xlsx(list2(), sheetName = input$protein_DE2_file2_name,file, append = TRUE)
      write.xlsx(cross.list(), sheetName = 'Cross',file, append = TRUE)
  })
}

shinyApp(ui, server)
