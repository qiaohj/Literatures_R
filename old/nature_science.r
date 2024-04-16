library("RMySQL")
library("tidytext")
library("dplyr")
library("tidyr")
library("ggplot2")

data(stop_words)

setwd("~/Experiments/PubMed/Script/PubMed_R")
con<-dbConnect(MySQL(), user="", password="", 
               dbname="PubMed", host="172.16.120.50")

journals<-c("Nature", "Science")
journal<-journals[1]
for (journal in journals){
  sql<-sprintf("SELECT * FROM Article WHERE Journal_Title='%s'", journal)
  rs<-dbSendQuery(con, sql)
  df<-fetch(rs, n=-1)
  dbClearResult(rs)
  
  df[df==-9999]<-9999
  df_bak<-df
  
  d<-as_tibble(df)
  dim(d)
  d<-d %>% filter((ArticleYear!=9999) | (PubMedPubYear!=9999))
  dim(d)
  
  d<-d %>% mutate(year=pmin(ArticleYear, PubMedPubYear))
  d$full_text<-paste(d$Abstract, d$ArticleTitle)
  
  tidy_token<-d[, c("PMID", "full_text", "year")] %>%
    unnest_tokens(word, full_text)
  
  tidy_token <- tidy_token %>%
    anti_join(stop_words)
  
  artile_num<-d %>% count(year)
  colnames(artile_num)[2]<-"article_num"
  
  plot(artile_num$year, artile_num$article_num, type="l")
  
  word_group <- tidy_token %>% group_by(year) %>% count(word) 
  
  word_group<- word_group %>% inner_join(artile_num, by="year")
  
  word_group$frequency<-word_group$n/word_group$article_num
  
  all_sentiment <- tidy_token %>%
    inner_join(get_sentiments("bing")) %>%
    count(year, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  all_sentiment<-all_sentiment%>% inner_join(artile_num, by="year")
  all_sentiment$frequency_p<-all_sentiment$positive/all_sentiment$article_num
  all_sentiment$frequency_n<-all_sentiment$negative/all_sentiment$article_num
  
  key<-tidy_token %>% filter(PMID %in% (tidy_token %>% filter(word=="china"))[["PMID"]])
  
  key_group <- key %>% group_by(year) %>% count(word) 
  
  key_group<- key_group %>% inner_join(artile_num, by="year")
  
  key_group$frequency<-key_group$n/key_group$article_num
  

  key_sentiment <- key %>%
    inner_join(get_sentiments("bing")) %>%
    count(year, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  key_artile_num<-key %>% count(year)
  colnames(key_artile_num)[2]<-"article_num"
  plot(key_artile_num$year, key_artile_num$article_num, type="l")
  
  key_sentiment<-key_sentiment%>% inner_join(key_artile_num, by="year")
  key_sentiment$frequency_p<-key_sentiment$positive/key_sentiment$article_num
  key_sentiment$frequency_n<-key_sentiment$negative/key_sentiment$article_num
  
  ggplot(all_sentiment) + geom_line(aes(year, frequency_p), color="red")+
    geom_line(aes(year, frequency_n), color="blue") 
  ggplot()+
    geom_line(data=key_sentiment, aes(year, frequency_p), color="red")+
    geom_line(data=key_sentiment, aes(year, frequency_n), color="blue")
  
  
}