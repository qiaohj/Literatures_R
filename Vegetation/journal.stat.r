library(data.table)
library(stringr)
library(stringi)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
journals<-readRDS("../Data/Vegetation/vegetation.journal.rda")
source("Download.PDF/getArticles.r")
source("Download.PDF/check.journal.pdf.r")
source("Download.PDF/download.journal.pdf.r")
i=1
crossref.year<-2025
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
all.info<-list()
for (i in c(1:nrow(journals))){
  journal<-journals[i]
  print(journal)
  articles<-getArticles(journal, all_journal_folders)
  articles$pdf.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s",
                             journal$journal, articles$pdf)
  articles$pdf.exist<-file.exists(articles$pdf.path)
  articles$xml.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s/%s",
                             journal$journal, gsub("\\.PDF", "\\.XML", articles$pdf))
  articles$xml.exist<-file.exists(articles$xml.path)
  item<-data.table(journal=journal$journal, ISSN=journal$ISSN, eISSN=journal$eISSN,
                   n.article=nrow(articles),
                   with.pdf=nrow(articles[pdf.exist==T]),
                   with.xml=nrow(articles[xml.exist==T]))
  all.info[[i]]<-item
}

all.info<-rbindlist(all.info)
fwrite(all.info, "../Data/Vegetation/journal.stat.csv")
