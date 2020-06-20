merge.data<-function(filetype,file1,file2,file3){
  if (input$filetype=='csv'){
    csv.file.table<-NULL
    for (f in c(file1,file2,file3)){
      csv.file.table$f<-data.frame(read.csv(file = f, stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
    }
    csv.file.merge.table<-rbind(csv.file.table$file1,csv.file.table$file2,csv.file.table$file3)
    return(csv.file.merge.table)
  }
}
