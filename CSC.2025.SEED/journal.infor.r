library(data.table)
library(stringi)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
target.journals<-read.csv("../Data/CSC/Total_reference_20250726.csv", head=TRUE)
target.journals[is.na(target.journals)]<-0
target.journals$Publisher<-""
target.journals$Note<-""
target.journals<-data.table(target.journals)
target.journals[!Journal_name %in% journals$journal]
crossref.year<-2025
unique(target.journals$Note)
source("Download.PDF/getArticles.r")
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
journals<-readRDS("../Data/JCR/Target.Journals.rda")
journals[grepl("ACTA OECOLOGICA", journal)]
target.journals[,Note.Qiao:=""]
target.journals$Note<-"Auto"


for (i in c(569:nrow(target.journals))){
  item<-target.journals[i]
  conf.item<-journals[journal==item$Journal_name]
  if (nrow(conf.item)==0){
    target.journals[i, Note.Qiao:="[no exist in jcr categories]"]
    next()
  }
  print(sprintf("%d/%d: %s", i, nrow(target.journals), item$Journal_name))
  if (item$Note.Qiao!=""){
    next()
  }
  if (item$Note==""){
    #next()
  }
  if (item$Note=="Okay"){
    target.journals[i, Note.Qiao:="Okay-CSC"]
    next()
  }
  
  if (item$Note=="Manual"){
    
    articles<-getArticles(conf.item, all_journal_folders)
    if (nrow(articles)==0){
      next()
    }
    articles$pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s", item$Journal_name, articles$pdf)
    articles$pdf.exist<-file.exists(articles$pdf)
    articles<-articles[pdf.exist==F]
    saveRDS(articles, sprintf("../Data/CSC/Missing.arcitle/%s.rda", item$Journal_name))
    publishers<-unique(articles$publisher)
    if (publishers[1] %in% cannot.download.journal.list){
      label<-"[robot banned]"
    }else{
      label<-"[exported]"
    }
    target.journals[i, Note.Qiao:=label]
    target.journals[i, Publisher:=paste(publishers, collapse =", ")]
    next()
  }
  if (item$Note=="Auto"){
    
    articles<-getArticles(conf.item, all_journal_folders)
    if (nrow(articles)==0){
      next()
    }
    target.journals[i, Crossref:=nrow(articles)]
    articles$pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s", item$Journal_name, articles$pdf)
    publishers<-unique(articles$publisher)
    articles$pdf.exist<-file.exists(articles$pdf)
    articles<-articles[pdf.exist==F]
    target.journals[i, PDF:=target.journals[i]$Crossref-nrow(articles)]
    
    if (publishers[1] %in% cannot.download.journal.list){
      label<-"[robot banned]"
    }else{
      label<-"[waiting]"
    }
    target.journals[i, Note.Qiao:=label]
    target.journals[i, Publisher:=paste(publishers, collapse =", ")]
    print(target.journals[i])
    next()
  }
  
}
#17: "Wiley"                        "Botanical Society of America"
fwrite(target.journals, "../Data/CSC/target.journals_20250726.csv")
