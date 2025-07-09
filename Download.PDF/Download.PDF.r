library(data.table)
library(pdftools)
library(stringr)
library(stringi)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
if (F){
  crossref.year<-2025
  journals<-readRDS(sprintf("../Data/CrossRef_Full/%d/journals.rda", crossref.year))
  journals[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(article_DOI, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  journals[, c("doi.key", "doi.id") := {
    parts <- stri_split_fixed(doi.suffix, "\\.", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  
  pdfs<-readRDS("/media/huijieqiao/Almost_Broken/SCIHUB/Data/PDF/full_pdf_scihub.rda")
  
  target_journal<-"Methods in Ecology and Evolution"
  items<-journals[Title==target_journal]
  
  prefix<-unique(items$doi.prefix)
  issn<-unique(items$ISSN)
  items<-items[!is.na(ISSN)]
  issn<-unique(items$ISSN)
  issn
  head(pdfs)
  pdfs[grepl("ecy.2993", DOI2)]
}

categories<-list.files("../Data/JCR/Target.Journals/", pattern="\\.csv")
categories<-gsub("\\.csv", "", categories)
category<-"AGRONOMY.2025"
crossref.year<-2025
pdfs<-readRDS("../Data/full_pdf_scihub.rda")
pdfs$DOI2<-tolower(pdfs$DOI2)
getISSN.folder<-function(issn){
  issn.items<-strsplit(issn, "-")[[1]]
  return(sprintf("%s/%s-%s", issn.items[1], issn.items[1], issn.items[2]))
}

for (category in categories){
  
  journal.conf<-fread(sprintf("../Data/JCR/Target.Journals/%s.csv", category), header=T)
  journal.conf$journal<-toupper(journal.conf$`Journal name`)
  journal.conf$journal<-gsub("&", "AND", journal.conf$journal)
  
  #journals<-readRDS(sprintf("../Data/CrossRef_Full/%d/journals.rda", crossref.year))
  i=1
  for (i in c(1:nrow(journal.conf))){
    conf.item<-journal.conf[i]
    target_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", conf.item$journal)
    if (dir.exists(target_folder)){
      #next()
    }
    article_item1<-NULL
    article_item2<-NULL
    if (conf.item$ISSN!=""){
      issnitem<-getISSN.folder(conf.item$ISSN)
      folders<-sprintf("../Data/CrossRef_By_Journal/%d/%s", crossref.year, issnitem)
      if (dir.exists(folders)){
        article_item1<-readRDS(sprintf("%s/articles.rda", folders))
        journal_item1<-readRDS(sprintf("%s/journals.rda", folders))
      }
    }
    
    if (conf.item$eISSN!=""){
      issnitem<-getISSN.folder(conf.item$eISSN)
      folders<-sprintf("../Data/CrossRef_By_Journal/%d/%s", crossref.year, issnitem)
      if (dir.exists(folders)){
        article_item2<-readRDS(sprintf("%s/articles.rda", folders))
        journal_item2<-readRDS(sprintf("%s/journals.rda", folders))
      }
    }
    if (is.null(article_item1) & !is.null(article_item2)){
      article_item<-article_item2
      journal_item<-journal_item2
    }
    if (!is.null(article_item1) & is.null(article_item2)){
      article_item<-article_item1
      journal_item<-journal_item1
    }
    if (!is.null(article_item1) & !is.null(article_item2)){
      article_item<-rbindlist(list(article_item1, article_item2))
      journal_item<-rbindlist(list(journal_item1, journal_item2))
    }
    article_item$abstract<-NULL
    #article_item[doi=="10.1002/ecy.2993"]
    article_item[, c("doi.prefix", "doi.suffix") := {
      parts <- stri_split_fixed(doi, "/", n = 2)
      list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
    }]
    article_item<-unique(article_item)
    
    doi.suffix<-tolower(unique(article_item$doi.suffix))
    dir.create(target_folder)
    pdf_target<-pdfs[DOI2 %in% doi.suffix]
    #issns<-c(conf.item$ISSN, conf.item$eISSN)
    #issns<-issns[issns!=""]
    #pdf_target<-pdf_target[DOI1 %in% issns]
    #pdf_target[grepwil("3071767", pdf)]
    for (j in c(1:nrow(pdf_target))){
      print(sprintf("%d/%d, %s (%d/%d), %s", j, nrow(pdf_target), 
                    conf.item$journal, i, nrow(journal.conf), category))
      item<-pdf_target[j]
      file.copy(item$pdf, target_folder, overwrite=F)
    }
  }
}
