library(data.table)
library(pdftools)
library(stringr)
library(stringi)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")


categories<-list.files("../Data/JCR/Target.Journals/", pattern="\\.csv")
categories<-gsub("\\.csv", "", categories)
categories<-categories[categories!="Evolutionary.Biology.2025"]
category<-"Ecology.2025"
crossref.year<-2025
cross_ref_folders<-readRDS(sprintf("../Data/cross_ref_folders.%d.rda", crossref.year))


getISSN.folder<-function(issn){
  issn.items<-strsplit(issn, "-")[[1]]
  return(sprintf("%s/%s-%s", issn.items[1], issn.items[1], issn.items[2]))
}
category<-categories[1]
for (category in categories){
  
  journal.conf<-fread(sprintf("../Data/JCR/Target.Journals/%s.csv", category), header=T)
  journal.conf$journal<-toupper(journal.conf$`Journal name`)
  journal.conf$journal<-gsub("&", "AND", journal.conf$journal)
  journal.conf$journal<-toupper(gsub("ANDAMP;", "AND", journal.conf$journal))
  
  #journals<-readRDS(sprintf("../Data/CrossRef_Full/%d/journals.rda", crossref.year))
  i=1
  for (i in c(1:nrow(journal.conf))){
    conf.item<-journal.conf[i]
    target_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", conf.item$journal)
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
    article_item$file1<-sprintf("/media/huijieqiao/GNAP140T/Download_PDF/%s/%s.pdf", 
                                 article_item$doi.prefix, article_item$doi.suffix)
    
    article_item$file2<-sprintf("/media/huijieqiao/GNAP140T/Download_PDF/%s/%s.pdf", 
                                article_item$doi.prefix, URLencode(article_item$doi.suffix, reserved = TRUE))
    
    article_item$file3<-sprintf("/media/huijieqiao/GNAP140T/Download_PDF/%s/%s.pdf", 
                                article_item$doi.prefix, tolower(URLencode(article_item$doi.suffix, reserved = TRUE)))
    
    article_item$file4<-sprintf("/media/huijieqiao/GNAP140T/Download_PDF/%s/%s.pdf", 
                                article_item$doi.prefix, toupper(URLencode(article_item$doi.suffix, reserved = TRUE)))
    
    article_item$folder<-article_item$file1
    article_item$pdf.exist<-F
    
    article_item[file.exists(file2), `:=`(folder=file2, pdf.exist=T)]
    article_item[file.exists(file3), `:=`(folder=file3, pdf.exist=T)]
    article_item[file.exists(file4), `:=`(folder=file4, pdf.exist=T)]
    
    
    pdf_target<-article_item[pdf.exist==T]
    #issns<-c(conf.item$ISSN, conf.item$eISSN)
    #issns<-issns[issns!=""]
    #pdf_target<-pdf_target[DOI1 %in% issns]
    #pdf_target[grepwil("3071767", pdf)]
    if (nrow(pdf_target)>0){
      
      for (j in c(1:nrow(pdf_target))){
        
        print(sprintf("%d/%d, %s (%d/%d), %s", j, nrow(pdf_target), 
                      conf.item$journal, i, nrow(journal.conf), category))
        item<-pdf_target[j]
        file.copy(item$folder, target_folder, overwrite=F)
      }
    }
  }
}
