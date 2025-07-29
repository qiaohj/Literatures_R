library(data.table)
library(stringi)
library(tidyr)

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
crossref.year<-2025

journals<-readRDS(sprintf("/media/huijieqiao/WD22T_11/literatures/Data/JCR/Target.Journals.rda", crossref.year))
issn<-"1420-3049"
journals[ISSN==issn | eISSN==issn]
journals[grepl("FRONTIERS", journal)]
journals[journal=="LEGUME RESEARCH - AN INTERNATIONAL JOURNAL"]
target.journals<-read.csv("../Data/CSC/Total_reference_20250726.csv", head=TRUE)
target.journals[is.na(target.journals)]<-0
target.journals$Publisher<-""
target.journals$Note<-""
target.journals<-data.table(target.journals)
target.journals[!Journal_name %in% journals$journal]
cross_ref_folders<-readRDS(sprintf("../Data/cross_ref_folders.%d.rda", crossref.year))
target.journals$Crossref<-0
for (i in c(1:nrow(target.journals))){
  
  j.name<-target.journals[i]$Journal_name
  
  if (target.journals[i]$Crossref>0){
    next()
  }
  print(sprintf("%d/%d: %s", i, nrow(target.journals), j.name))
  item<-journals[journal==j.name]
  
  if (nrow(item)==0){
    next()
  }
  
  articles<-readRDS(sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Journal.Article/%d/%s.rda",
                            crossref.year, j.name))
  
  articles$pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s", j.name,
                        articles$pdf)
  articles$pdf.exist<-file.exists(articles$pdf)
  target.journals[i, Crossref:=nrow(articles)]
  target.journals[i, PDF:=nrow(articles[pdf.exist==T])]
  
  csv.file<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSV/%d/%s.rda",
                    crossref.year, j.name)
  if (file.exists(csv.file)){
    csv<-readRDS(csv.file)
    target.journals[i, ES:=length(csv)]
  }else{
    
  }
}




fwrite(target.journals, "~/Downloads/target.journals.csv")
