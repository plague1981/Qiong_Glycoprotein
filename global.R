# grouping files
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

# meger data
group1_raw<- reactive({
  df1<-data.frame(read.csv(file = input$group1files$datapath[1], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df2<-data.frame(read.csv(file = input$group1files$datapath[2], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df3<-data.frame(read.csv(file = input$group1files$datapath[3], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df<-rbind(df1,df2,df3)

})
group2_raw<- reactive({
  df1<-data.frame(read.csv(file = input$group2files$datapath[1], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df2<-data.frame(read.csv(file = input$group2files$datapath[2], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df3<-data.frame(read.csv(file = input$group2files$datapath[3], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df<-rbind(df1,df2,df3)

})
group3_raw<- reactive({
  df1<-data.frame(read.csv(file = input$group3files$datapath[1], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df2<-data.frame(read.csv(file = input$group3files$datapath[2], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df3<-data.frame(read.csv(file = input$group3files$datapath[3], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df<-rbind(df1,df2,df3)

})
group4_raw<- reactive({
  df1<-data.frame(read.csv(file = input$group4files$datapath[1], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df2<-data.frame(read.csv(file = input$group4files$datapath[2], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df3<-data.frame(read.csv(file = input$group4files$datapath[3], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df<-rbind(df1,df2,df3)

})
group5_raw<- reactive({
  df1<-data.frame(read.csv(file = input$group5files$datapath[1], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df2<-data.frame(read.csv(file = input$group5files$datapath[2], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df3<-data.frame(read.csv(file = input$group5files$datapath[3], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df<-rbind(df1,df2,df3)

})
group6_raw<- reactive({
  df1<-data.frame(read.csv(file = input$group6files$datapath[1], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df2<-data.frame(read.csv(file = input$group6files$datapath[2], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df3<-data.frame(read.csv(file = input$group6files$datapath[3], stringsAsFactors = FALSE)[,c(1,2,8,7,20)])
  df<-rbind(df1,df2,df3)

})
# functions

glycan.freq.abundance<-function(merge.sample){
  glycan.freq.list <- data.frame(table(merge.sample[,4]))
  colnames(glycan.freq.list)[1]<-'Glycans'
  glycan.sum.list<-sum(as.numeric(merge.sample[,5]))
  abundance.list<-NULL
  for (m in glycan.freq.list[,1]){
    abundance.num <- merge.sample[,5][merge.sample[,4]==m]
    abundance <-sum(as.numeric(abundance.num))/glycan.sum.list
    abundance.list <- c(abundance.list, abundance)
  }
  glycan.abundance<-cbind(glycan.freq.list, data.frame(abundance.list))
  colnames(glycan.abundance)[3]<-'Abundance'
  return(glycan.abundance)
}