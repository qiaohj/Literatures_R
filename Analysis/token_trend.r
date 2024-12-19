library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
source("functions.r")
category<-"Ecology"
article_raw<-readArticle(category)
article<-article_raw
token<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/token_title.rda", category))
journal<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/journals.rda", category))

article$abstract<-NULL
article$File<-NULL
article<-article[, c("doi", "container_title", "issn", "Year")]
article<-unique(article)
table(article$type)
N_doi<-article[, .(N=.N), by=doi]
N_doi[N>1]
article_raw[doi=="10.2307/3071823"]

N_per_journal<-article[, .(N=.N), by=c("container_title", "Year")]

journal_issn<-unique(journal[, c("Title", "ISSN")])
#fix the NA journals

for (i in c(1:nrow(journal_issn))){
  item<-journal_issn[i]
  if (is.na(item$Title)){
    xxx<-journal_issn[ISSN==item$ISSN & !is.na(Title)]
    if (nrow(xxx)!=1){
      asdf
    }
  }
}
fwrite(journal_issn, "../Data/ecology_journal.csv")
NNN<-journal_issn[, .(N=.N), by=c("Title")]
NNN[N>3]
journal_issn[Title=="Biology Letters"]
ll<-list()
for (i in c(1:nrow(journal_issn))){
  item<-journal_issn[i]
  if (issn %in% names(ll)){
    
  }else{
    ll[[item$ISSN]]<-item$Title
  }
}


