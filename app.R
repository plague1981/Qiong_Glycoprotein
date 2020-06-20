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
      menuItem('Data analyze', tabName = 'data_analyze', icon = icon('file')),
      menuItem("Venn diagram 2", tabName = "venndiagram2", icon = icon("pie-chart")),
      menuItem("Venn diagram 3", tabName = "venndiagram3", icon = icon("pie-chart"))
    )
    
  ),
  dashboardBody(
    # data_analyze
    tabItems(
      tabItem(tabName = 'data_analyze',
              navbarPage(title = 'Data analyze',
                         tabPanel('Input files', icon = icon('file'),
                                  selectInput(inputId = 'filetype1', label = 'Please select input filetype:', choices = c('csv','xlsx')),
                                  box(width = 3,
                                      textInput(inputId = 'group1','Please enter 1st group name:', 'Group1'),
                                      fileInput(inputId = 'group1files',label = 'Group1 name', multiple = TRUE),
                                      tags$hr(),
                                      textInput(inputId = 'group2','Please enter 2nd group name:', 'Group2'),
                                      fileInput(inputId = 'group2files',label = 'Group1 name', multiple = TRUE),
                                      tags$hr(),
                                      textInput(inputId = 'group3','Please enter 3rd group name:', 'Group3'),
                                      fileInput(inputId = 'group3files',label = 'Group3 name', multiple = TRUE)
                                  ),
                                  box(width = 3,
                                      textInput(inputId = 'group4','Please enter 4th group name:', 'Group4'),
                                      fileInput(inputId = 'group4files',label = 'Group4 name', multiple = TRUE),
                                      tags$hr(),
                                      textInput(inputId = 'group5','Please enter 5th group name:', 'Group5'),
                                      fileInput(inputId = 'group5files',label = 'Group5 name', multiple = TRUE),
                                      tags$hr(),
                                      textInput(inputId = 'group6','Please enter 6th group name:', 'Group6'),
                                      fileInput(inputId = 'group6files',label = 'Group6 name', multiple = TRUE)                           
                                  ),
                                  box(width = 3,
                                      textOutput('group1name'),
                                      tableOutput('group1filesinfo'),
                                      tags$hr(),
                                      textOutput('group2name'),
                                      tableOutput('group2filesinfo'),
                                      tags$hr(),
                                      textOutput('group3name'),
                                      tableOutput('group3filesinfo')
                                  ),
                                  box(width = 3,
                                      textOutput('group4name'),
                                      tableOutput('group4filesinfo'),
                                      tags$hr(),
                                      textOutput('group5name'),
                                      tableOutput('group5filesinfo'),
                                      tags$hr(),
                                      textOutput('group6name'),
                                      tableOutput('group6filesinfo')                           
                                  )
                                  
                         ),
                         tabPanel('Analysis', icon = icon('line-chart'),
                                  box(width = 12,
                                   tableOutput('group1') 
                                  )
                                  
                         )
              )
      ),
      # venndiagram2
      tabItem(tabName = "venndiagram2",
              navbarPage(title = "Venn diagram 2", 
                         tabPanel("Plot", icon = icon("bar-chart-o"),
                                  box(width = 6,
                                      plotOutput(outputId = 'venndiagram2'),
                                      downloadLink("downloadDE2", "Download xlsx file"),
                                  ),
                                  box(width = 6,
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
                         tabPanel("Summary", icon = icon("list-alt")),
                         tabPanel("Table", icon = icon("table"),
                                  box(width = 12,
                                      tableOutput(outputId = 'DE2.table')
                                  )
                                  
                         )
              ),
      ),
      # venndiagram3
      tabItem(tabName = "venndiagram3",
              navbarPage("Venn diagram 3",
                         tabPanel("Plot", icon = icon("bar-chart-o"),
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
                         ),
                         tabPanel("Summary", icon = icon("list-alt")),
                         tabPanel("Table", icon = icon("table"),
                                  box(width = 12,
                                      tableOutput('DE3.table')
                                  )
                         )
              )
      )
    )
  )
) 

server <- function(input, output) {
  # files grouping
  output$group1name<-renderText({
    input$group1
  })
  output$group1filesinfo<-renderTable({
    input$group1files$name
  })
  output$group2name<-renderText({
    input$group2
  })
  output$group2filesinfo<-renderTable({
    input$group2files[1]
  })
  output$group3name<-renderText({
    input$group3
  })
  output$group3filesinfo<-renderTable({
    input$group3files[1]
  })
  output$group4name<-renderText({
    input$group4
  })
  output$group4filesinfo<-renderTable({
    input$group4files[1]
  })
  output$group5name<-renderText({
    input$group5
  })
  output$group5filesinfo<-renderTable({
    input$group5files[1]
  })
  output$group6name<-renderText({
    input$group6
  })
  output$group6filesinfo<-renderTable({
    input$group6files[1]
  })
  # files merging based on grouping
  output$group1<- renderTable({
    df1<-data.frame(read.csv(file = input$group1files$datapath[1], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
    df2<-data.frame(read.csv(file = input$group1files$datapath[2], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
    df3<-data.frame(read.csv(file = input$group1files$datapath[3], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
    df<-rbind(df1,df2,df3)
    return(df)
  })
  #
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
    list.only<-setdiff(unlist(list1.DE2()),unlist(cross.list.DE2()))
    df<-data.frame(list.only)
    colnames(df) <-paste(input$protein_DE2_file1_name,'.only')
    return(df)
  })
  list2.only.DE2<-eventReactive(input$submit_DE2_files,{
    list.only<-setdiff(unlist(list2.DE2()),unlist(cross.list.DE2()))
    df<-data.frame(list.only)
    colnames(df) <-paste(input$protein_DE2_file2_name,'.only')
    return(df)
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
  # Output
  output$list1.only<-renderTable({
    list1.only.DE2()
  })
  output$list2.only<-renderTable({
    list2.only.DE2()
  })
  output$cross<-renderTable({
    cross.list.DE2()
  })
  output$DE2.table<-renderTable({
    df<-data.frame()
    for (n in 1:nrow(list1.only.DE2())){
      df[n,1]<-list1.only.DE2()[n,1]
    }
    for (n in 1:nrow(list2.only.DE2())){
      df[n,2]<-list2.only.DE2()[n,1]
    }
    for (n in 1:nrow(cross.list.DE2())){
      df[n,3]<-cross.list.DE2()[n,1]
    }
    colnames(df)<-c(input$protein_DE2_file1_name,input$protein_DE2_file2_name, 'Cross')
    return(df)
  })
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
  output$DE3.table<-renderTable({
    df<-data.frame()
    for (n in 1:nrow(list1.only.DE3())){
      df[n,1]<-list1.only.DE3()[n,1]
    }
    for (n in 1:nrow(list2.only.DE3())){
      df[n,2]<-list2.only.DE3()[n,1]
    }
    for (n in 1:nrow(list3.only.DE3())){
      df[n,3]<-list3.only.DE3()[n,1]
    }
    for (n in 1:nrow(list1.list2.only.DE3())){
      df[n,4]<-list1.list2.only.DE3()[n,1]
    }
    for (n in 1:nrow(list2.list3.only.DE3())){
      df[n,5]<-list2.list3.only.DE3()[n,1]
    }
    for (n in 1:nrow(list1.list3.only.DE3())){
      df[n,6]<-list1.list3.only.DE3()[n,1]
    }
    for (n in 1:nrow(cross.list.DE3())){
      df[n,7]<-cross.list.DE3()[n,1]
    }
    colnames(df)<-c(input$protein_DE3_file1_name,input$protein_DE3_file2_name,input$protein_DE3_file3_name,
                    paste0(input$protein_DE3_file1_name,input$protein_DE3_file2_name),
                    paste0(input$protein_DE3_file2_name,input$protein_DE3_file3_name),
                    paste0(input$protein_DE3_file1_name,input$protein_DE3_file3_name), 'Cross')
    return(df)
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
  
}

shinyApp(ui, server)
