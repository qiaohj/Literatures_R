library(data.table)
library(stringi)
library(tidyr)

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
crossref.year<-2025
if (F){
  jcr_journals<-list.files("../Data/JCR/Target.Journals", pattern="\\.csv")
  downloaded.journals<-list()
  for (f in jcr_journals){
    print(f)
    journal.conf<-fread(sprintf("../Data/JCR/Target.Journals/%s", f), header=T)
    journal.conf$journal<-toupper(journal.conf$`Journal name`)
    journal.conf$journal<-gsub("&", "AND", journal.conf$journal)
    downloaded.journals[[length(downloaded.journals)+1]]<-journal.conf
    
    
  }
  downloaded.journals<-rbindlist(downloaded.journals, fill=T)
  downloaded.journals$Category<-NULL
  downloaded.journals<-downloaded.journals[, c("ISSN", "eISSN", "journal")]
  
  downloaded.journals<-unique(downloaded.journals)
  
  if (F){
    downloaded.journals[grepl("CANADIAN JOURNAL", journal)]
    downloaded.journals[ISSN=="1932-6203"]
  }
  target.journals<-fread("../Data/target.journals_20250721.csv", fill=TRUE)
  View(target.journals[!Journal_name %in% downloaded.journals$journal])
  
  
  xxx<-readRDS("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_By_Journal/2025/0976/0976-0571|0250-5371/journals.rda")
  df<-readRDS("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_Full/2025/journals.rda")
  journals<-unique(df[, c("Title", "ISSN")])
  journals$journal.name<-toupper(gsub("&", "AND", journals$Title))
  journals$journal.name<-toupper(gsub("ANDAMP;", "AND", journals$journal.name))
  journals<-journals[ISSN!="NA"]
  journals<-journals[Title!="NA"]
  
  journals[, ISSN := strsplit(ISSN, "\\|")]
  #journals[Title=="Basiswissen Sozialwirtschaft und Sozialmanagement|Kostenmanagement"]
  
  journals <- data.table(unnest(journals, cols = ISSN))
  journals<-unique(journals)
  journals$ISSN<-toupper(journals$ISSN)
  saveRDS(journals, "/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_Full/2025/journals.issn.rda")
  
  target.journals<-fread("../Data/target.journals_20250721.csv", fill=TRUE)
  target.journals$origin.journal.name<-target.journals$Journal_name
  target.journals$ISSN<-toupper(target.journals$ISSN)
  target.journals$EISSN<-toupper(target.journals$EISSN)
  journals<-readRDS(sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_Full/%d/journals.issn.rda", crossref.year))
  for (i in c(1:nrow(target.journals))){
    if (target.journals[i]$Crossref>0){
      next()
    }
    
    j.name<-target.journals[i]$Journal_name
    if (j.name %in% c("JOURNAL OF FOOD AGRICULTURE ENVIRONMENT", "ROSTLINNA VYROBA",
                      "ZEITSCHRIFT FUR PFLANZENKRANKHEITEN UND PFLANZENSCHUTZ JOURNAL OF PLANT DISEASES AND PROTECTION",
                      "AUSTRALIAN JOURNAL OF PLANT PHYSIOLOGY")){
      next()
    }
    print(sprintf("%d/%d: %s", i, nrow(target.journals), j.name))
    item<-journals[journal.name==j.name]
    item<-item[ISSN!="NA"]
    if (nrow(item)!=0){
      next()
    }
    if (F){
      journals[startsWith(journal.name, "AUSTRALIAN JOURNAL OF P")]
      journals[grepl("PFLANZENKRANKHEITEN UND", journal.name)]
    }
    if (target.journals[i]$ISSN=="" & target.journals[i]$EISSN==""){
      next()
    }
    item<-journals[grepl(target.journals[i]$ISSN, ISSN) | 
                     grepl(target.journals[i]$EISSN, ISSN)]
    title<-unique(item$journal.name)
    if (length(title)>=1){
      target.journals[i, Journal_name:=title[1]]
    }else{
      asdf
    }
  }
  saveRDS(target.journals, "../Data/target.journals_20250721.rda")
  
  cross_ref_folders<-list.dirs(sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_By_Journal/%d/", crossref.year))
  saveRDS(cross_ref_folders, sprintf("../Data/cross_ref_folders.%d.rda", crossref.year))
}
journals<-readRDS(sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_Full/%d/journals.issn.rda", crossref.year))
journals[ISSN=="0250-5371"]
journals[journal.name=="ECOLOGY"]
journals[journal.name=="LEGUME RESEARCH - AN INTERNATIONAL JOURNAL"]
target.journals<-readRDS("../Data/target.journals_20250721.rda")

cross_ref_folders<-readRDS(sprintf("../Data/cross_ref_folders.%d.rda", crossref.year))

for (i in c(1:nrow(target.journals))){
  
  j.name<-target.journals[i]$Journal_name
  if (j.name %in% c("IOP CONFERENCE SERIES EARTH AND ENVIRONMENTAL SCIENCE",
                    "JOURNAL OF FOOD AGRICULTURE ENVIRONMENT", "ROSTLINNA VYROBA",
                    "ZEITSCHRIFT FUR PFLANZENKRANKHEITEN UND PFLANZENSCHUTZ JOURNAL OF PLANT DISEASES AND PROTECTION",
                    "AUSTRALIAN JOURNAL OF PLANT PHYSIOLOGY",
                    "ZEITSCHRIFT FUR PFLANZENPHYSIOLOGIE")){
    target.journals[i, Status:=3] #Can't find the journal from the crossref
    next()
  }
  if (target.journals[i]$Crossref>0){
    next()
  }
  print(sprintf("%d/%d: %s", i, nrow(target.journals), j.name))
  item<-journals[journal.name==j.name]
  if (F){
    journals[grepl("PROCEEDINGS OF THE ROYAL SOCIETY B", journal.name)]
  }
  item<-item[ISSN!="NA"]
  if (nrow(item)==0){
    asdf
  }
  issns<-journals[ISSN %in% item$ISSN]
  issns<-journals[journal.name %in% issns$journal.name]
  issns<-unique(issns$ISSN)
  folders<-c()
  for (j in c(1:length(issns))){
    folders<-c(folders, cross_ref_folders[grepl(issns[j], cross_ref_folders)])
  }
  folders<-unique(folders)
  if (length(folders)>0){
    articles<-list()
    for (j in c(1:length(folders))){
      article<-readRDS(sprintf("%s/articles.rda",
                               folders[j]))
      articles[[length(articles)+1]]<-article
    }
    articles<-rbindlist(articles)
    
    articles<-articles[type=="journal-article"]
    articles[, c("doi.prefix", "doi.suffix") := {
      parts <- stri_split_fixed(doi, "/", n = 2)
      list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
    }]
    articles$pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s.PDF",
                          j.name, toupper(URLencode(articles$doi.suffix, reserved = TRUE)))
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
  
  
}

fwrite(target.journals, "~/Downloads/target.journals.csv")
