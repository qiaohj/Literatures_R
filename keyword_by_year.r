library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
source("functions.r")
category<-"Ecology"

tokens<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/token_title.rda", category))
journals<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/journals.rda", category))
article_subject<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/article_subject_splitted.rda",
                                          category))
authors<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/authors.rda", category))
authors_country<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/authors_country_iso.rda", category))

articles<-readArticle(category)


table(articles$type)
threshold<-50
n_journal_year[grepl("Methods", container_title)]
n_journal_year<-articles[, .(N=.N), by=list(Year, container_title)]
item<-n_journal_year[between(Year, 2000, 2022)]
quantile(item$N)
setorderv(item, "N")
picked_journals<-n_journal_year[between(Year, 2010, 2022) & N>=threshold]
picked_journals_n<-picked_journals[, .(N=.N), by=list(container_title)]
picked_journals_n<-picked_journals_n[N==13]

sampled<-articles[(between(Year, 2010, 2022) & container_title %in% picked_journals_n$container_title),
                  .SD[sample(.N, 50)],by = list(container_title, Year)]

tokens_sampled<-tokens[doi %in% sampled$doi]
