library(data.table)
library(tm)
library(readr)
library(stringi)
library(RWeka)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(gridExtra)
library(dplyr)
library(rvest)
library(splitstackshape)
rm(list=ls())
setwd("/media/huijieqiao/WD22T1/pubmed/Script")

if (F){
  x <- read_html("http://www.englishpage.com/irregularverbs/irregularverbs.html")
  dic<-html_table(x, header = TRUE)[[1]]
  dic<-dic %>% bind_rows %>%
    rename(Infinitive = `Base Form`, Past = `Simple Past`, PP = `Past Participle`) %>%
    filter(!Infinitive %in% LETTERS)
  
  dic<-cSplit(dic, splitCols = c("Past", "PP"),
              sep = " / ", direction = "long", type.convert = F) %>%
    filter(complete.cases(.))
  dic<-  mutate_each(dic, list(~gsub(pattern = "\\s\\(.*\\)$|\\s\\[\\?\\]",
                                     replacement = "",
                                     x = .)))
  saveRDS(dic, "../Data/word_wrapper/irregular.verbs.rda")
}
source("functions.r")
category<-"WeedScience"
article_df<-readArticle(category)
table(article_df$type)


mydic<-readRDS("../Data/word_wrapper/irregular.verbs.rda")


token_title_list<-list()
token_abstract_list<-list()
for (i in c(1:nrow(article_df))){
  print(paste(i, nrow(article_df), article_df[i]$doi, article_df[i]$container_title))
  title<-article_df[i]$title
  if (is.na(title)){
    next()
  }
  if (title==""){
    next()
  }
  tokens<-get_Tokens(mydic, title, "title")
  if (is.null(tokens)){
    next()
  }
  token_title_list[[length(token_title_list)+1]]<-tokens
  abstract<-article_df[i]$abstract
  if (is.na(abstract)){
    next()
  }
  if (abstract==""){
    next()
  }
  tokens<-get_Tokens(mydic, abstract, "abstract")
  
  token_abstract_list[[length(token_abstract_list)+1]]<-tokens
  if (F){
    wordcloud(unigram$Word,
              unigram$Frequency,
              max.words=100, min.freq = 1,
              random.order=FALSE,
              colors=brewer.pal(8,"Dark2"), font = 3)
    
    subBigram <- bigram[1:10,]
    plotBiGram <- ggplot(subBigram, aes(x= reorder(Word, Frequency),y= Frequency )) + 
      geom_bar(stat="identity", fill="red") + 
      geom_text(aes(y= Frequency, label=Frequency), vjust=1) +
      coord_flip() + labs(x="Word", y="Frequency", title="Bigrams frequency")
    subTrigram <- trigram[1:10,]
    plotTriGram <- ggplot(subTrigram, aes(x= reorder(Word, Frequency),y= Frequency )) +
      geom_bar(stat="identity", fill="green") +
      geom_text(aes(y= Frequency, label=Frequency), vjust=1) +
      coord_flip() + labs(x="Word", y="Frequency", title="Trigrams frequency")
    
    grid.arrange(plotBiGram, plotTriGram, ncol=2)
  }
}
token_title_df<-rbindlist(token_title_list)
token_abstract_df<-rbindlist(token_abstract_list)
saveRDS(token_title_df, sprintf("../Data/CrossRef_By_Category/%s/token_title.rda", category))
saveRDS(token_abstract_df, sprintf("../Data/CrossRef_By_Category/%s/token_abstract.rda", category))

token_title_df[Word=="genet"]$Word<-"gene"
dim(token_title_df)
token_title_df<-unique(token_title_df)

token_doi_year<-token_title_df[, .(N_DOI=length(unique(doi)),
                                   N_container=length(unique(container_title))), 
                               by=list(Year)]
token_doi_year[Year==2000]
token_doi<-token_title_df[Year>=2000, .(N=.N,
                              N_container=length(unique(container_title))), 
                          by=list(Year, doi)]
token_doi
token_doi_year_sampled<-token_doi[Year>=2000,.SD[sample(.N, min(5000,.N))],by = Year]



token_title_se<-token_title_df[doi %in% token_doi_year_sampled$doi,
                               .(Frequency=sum(Frequency), N=.N, N_DOI=length(unique(doi))), 
                               by=list(Word, N_token, Year)]

skipped_word<-c("use", "ecolog")
token_title_se<-token_title_se[!(Word %in% skipped_word)]
token_title_se$MEAN_N<-token_title_se$N/token_title_se$N_DOI
setorderv(token_title_se, c("N_token", "Year", "N"), c(1, 1, -1))

token_title_top10_1<-token_title_se[((N_token==1)&!is.na(Year)), head(.SD, 100), 
                                    by=list(N_token, Year)]
tail(token_title_top10_1, 20)

length(unique(token_title_top10_1[Year>=2000]$Word))
target_Word_1<-unique(token_title_top10_1[Year>=2000]$Word)
token_title_se_top10_1<-token_title_se[Word %in% target_Word_1]
token_title_se_top10_1<-token_title_se_top10_1[Year>=2000]
token<-target_Word_1[1]
token_title_se_top10_1$Word<-as.character(token_title_se_top10_1$Word)
token_title_se_top10_1$Year<-as.numeric(token_title_se_top10_1$Year)
lm_token<-list()
for (token in target_Word_1){
  item<-token_title_se_top10_1[Word==token]
  model<-lm(N~Year, data=item)
  p_value<-summary(model)$coefficients[2,4]  
  estimate<-summary(model)$coefficients[2,1]  
  r2<-summary(model)$r.squared
  item<-data.table(Word=token, p_value, estimate, r2)
  lm_token[[length(lm_token)+1]]<-item
}
lm_token<-rbindlist(lm_token)
lm_token$p_label<-""
lm_token[p_value<=0.05]$p_label<-"*"
lm_token[p_value<=0.01]$p_label<-"**"
lm_token[p_value<=0.001]$p_label<-"***"
hist(lm_token[p_label!=""]$estimate)
quantiles<-quantile(lm_token[p_label!=""]$estimate, c(0.25, 0.75))
lm_token$trend<-"unknown"
lm_token[(p_label!="")&estimate<=-0.5]$trend<-"decrease"
lm_token[(p_label!="")&estimate>=0.5]$trend<-"increase"
lm_token[(p_label!="")&between(estimate, -0.5, 0.5)]$trend<-"stable"
table(lm_token$trend)
token_title_se_top10_1_with_trend<-merge(token_title_se_top10_1, lm_token, by="Word")
ggplot(token_title_se_top10_1_with_trend, 
       aes(x=Year, y=N, group=Word, color=Word))+geom_line()+
  geom_smooth(method="lm")+
  facet_wrap(~trend, scale="free_y")+
  theme(legend.position = "none")

setorderv(lm_token, "estimate", 1)
lm_token[grepl("geno", Word)]
token_abstract_se<-token_abstract_df[, .(Frequency=sum(Frequency)), by=list(Word, N_token, Year)]
token_title_all<-token_title_df[, .(Frequency=sum(Frequency)), by=list(Word, N_token)]
token_title_all_1<-token_title_all[N_token==1]
quantile(token_title_all_1$Frequency, c(.999))
nrow(token_title_all_1[Frequency>1000])
wordcloud(token_title_all_1[Frequency>1000]$Word,
          token_title_all_1[Frequency>1000]$Frequency,
          max.words=100, min.freq = 1,
          random.order=FALSE,
          colors=brewer.pal(8,"Dark2"), font = 3)
