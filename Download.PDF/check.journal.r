library(data.table)
library(pdftools)
library(stringr)
library(stringi)
library(RCurl)
library(rvest)
library(httr)
library(pdftools)
library(scihubr)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
source("Download.PDF/read_html.r")
source("../tokens.r")
source("Download.PDF/getArticles.r")
source("Download.PDF/check.journal.pdf.r")
source("Download.PDF/download.journal.pdf.r")

args = commandArgs(trailingOnly=TRUE)
api_index<-as.numeric(args[1])
if (is.na(api_index)){
  api_index<-3
}
wiley.api<-wiley.api[api_index]
elsevier.api<-elsevier.api[api_index]

t.journal.name<-args[2]
crossref.year<-2025
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))

i=1
journals<-readRDS("../Data/JCR/Target.Journals.rda")
journals[journal=="CROP SCIENCE"]
target.journals<-fread("../Data/CSC/target.journals_20250726.csv")
target.journals<-target.journals[Count>500]
#target.journals<-target.journals[Publisher=="FapUNIFESP (SciELO)" & Note=="Auto"]
journals<-journals[journal %in% target.journals$Journal_name]
journals<-journals[sample(nrow(journals), nrow(journals))]
#t.journal.name<-"JOURNAL OF THE AMERICAN SOCIETY FOR HORTICULTURAL SCIENCE"
if (is.na(t.journal.name)){
  journals<-journals[sample(nrow(journals), nrow(journals))]
  
}else{
  journals<-journals[journal==t.journal.name]
  
}
i=1
if (nrow(journals)==0){
  next()
}
for (i in c(1:nrow(journals))){
  conf.item<-journals[i]
  check.point<-error.pdf<-sprintf("../Data/Error.PDF/%s.%d.rda", conf.item$journal, crossref.year)
  if (!file.exists(check.point)){
    check.journal.pdf(conf.item, crossref.year)
  }
  download.journal.pdf(conf.item, crossref.year, wiley.api, elsevier.api)
}
