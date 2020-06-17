library(shiny)
library(shinythemes)
library(shinydashboard)
library(datasets)
library(readxl)
library(VennDiagram)

ui <-dashboardPage(
  dashboardHeader(title = 'My dashboard'),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput(outputId = 'venndiagram'), width = 8),
    box(tableOutput(outputId = 'list1')),
    box(tableOutput(outputId = 'list2')),
    box(tableOutput(outputId = 'cross.list')),
    box(
      # name the venn plot
      textInput('DE2_name', 'Please name the plot', value = 'Venn diagram'),
      tags$hr(),
      # upload DE2 files
      fileInput(inputId = 'protein_DE2_file1',label = 'File1', multiple = FALSE),
      textInput(inputId = 'protein_DE2_file1_name', 'Name of File1', value = 'File1'),
      tags$hr(),
      fileInput(inputId = 'protein_DE2_file2',label = 'File2', multiple = FALSE),
      textInput(inputId = 'protein_DE2_file2_name', 'Name of File2', value = 'File2'),
      # submit button for uploading DE2 files
      actionButton(inputId = "submit_DE2_files", label = "Submit")
    )
  )
) 

server <- function(input, output) {
  # Condition
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
  cross.list<- eventReactive(input$submit_DE2_files,{
    plist<-data.frame(intersect(unlist(read_excel(input$protein_DE2_file1$datapath,sheet = 1, col_names = FALSE)[1]),
                                unlist(read_excel(input$protein_DE2_file2$datapath,sheet = 1, col_names = FALSE)[1])))
    colnames(plist)<-'Cross'
    return(plist)
  })
  # Draw Venn diagram
  VD<- eventReactive(input$submit_DE2_files,{ 
    VDiagram  <- venn.diagram(
      x<-list(A=unlist(list1()),B=unlist(list2())),
      category.names = c(input$protein_DE2_file1_name,input$protein_DE2_file2_name),
      fill=rainbow(length(x)),
      filename = NULL, main = input$DE2_name)
    grid.newpage()
    return(grid.draw(VDiagram))
  })
  # Display
  output$venndiagram <- renderPlot({
    VD()
  })
  output$list1<-renderTable({
    list1()
  })
  output$list2<-renderTable({
    list2()
  })
  output$cross.list<-renderTable({
    cross.list()
  })

}

shinyApp(ui, server)
