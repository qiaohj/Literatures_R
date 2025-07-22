if (F){
  all_journal_folders<-list.dirs(sprintf("../Data/CrossRef_By_Journal/%d/", crossref.year))
  saveRDS(all_journal_folders, sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
}
if (F){
  categories<-list.files("../Data/JCR/Target.Journals/", pattern="\\.csv")
  categories<-gsub("\\.csv", "", categories)
  category<-"MULTIDISCIPLINARY SCIENCES.2025"
  
  journals<-list()
  for (category in categories){
    journal.conf<-readJournal(category)
    journals[[length(journals)+1]]<-journal.conf
  }
  journals<-rbindlist(journals, fill=T)
  journals$Category<-NULL
  journals<-journals[, c("ISSN", "eISSN", "journal")]
  journals<-unique(journals)
  journals.N<-journals[,.(N=.N), by=list(journal)]
  journals[journal=="GLOBAL CHANGE BIOLOGY"]
  saveRDS(journals, "../Data/JCR/Target.Journals.rda")
}
getArticles<-function(conf.item, all_journal_folders){
  
  conf.item$ISSN_1<-toupper(ifelse(conf.item$ISSN=="", "XXXXXXXXXXXXXXXX", conf.item$ISSN))
  conf.item$ISSN_2<-toupper(ifelse(conf.item$eISSN=="", "XXXXXXXXXXXXXXXX", conf.item$eISSN))
  
  
  folders<-all_journal_folders[grepl(conf.item$ISSN_1, toupper(all_journal_folders)) | 
                                 grepl(conf.item$ISSN_2, toupper(all_journal_folders))]
  article_item<-list()
  for (f in folders){
    article_item[[length(article_item)+1]]<-readRDS(sprintf("%s/articles.rda", f))
  }
  article_item<-rbindlist(article_item)
  if (nrow(article_item)==0){
    return(article_item)
  }
  article_item$abstract<-NULL
  #article_item[doi=="10.1002/ecy.2993"]
  article_item[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(doi, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  article_item<-unique(article_item)
  article_item<-article_item[type=="journal-article"]
  
  article_item
}

readJournal<-function(category){
  journal.conf<-fread(sprintf("../Data/JCR/Target.Journals/%s.csv", category), header=T)
  journal.conf$journal<-toupper(journal.conf$`Journal name`)
  journal.conf$journal<-gsub("&", "AND", journal.conf$journal)
  journal.conf$journal<-toupper(gsub("ANDAMP;", "AND", journal.conf$journal))
  journal.conf$journal<-gsub("/", "-", journal.conf$journal)
  journal.conf
}
