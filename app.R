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
      # upload DE2 files
      fileInput(inputId = 'protein_DE2_file1',label = 'File1', multiple = FALSE),
      fileInput(inputId = 'protein_DE2_file2',label = 'File2', multiple = FALSE),
      tags$hr(),
      # name the venn plot
      textInput('DE2_name', 'Please name the plot', value = 'Venn diagram'),
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
  
  # Display
  output$list1<-renderTable({
    list1()
  })
  output$list2<-renderTable({
    list2()
  })
  output$cross.list<-renderTable({
    cross.list()
  })
  output$venndiagram <- renderImage({
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    # Generate a png
    png(outfile, width=400, height=400)
    # Draw a venn diagram
    draw.pairwise.venn(area1 = nrow(list1()),
                       area2 = nrow(list2()), 
                       cross.area = nrow(cross.list()), 
                       category = c("Dog People", "Cat People"))
    dev.off()
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
}


shinyApp(ui, server)
