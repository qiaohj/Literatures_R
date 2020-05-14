#N of articles per year
library("RMySQL")
library("purrr")
library("dplyr")
con<-dbConnect(MySQL(), user="root", password="mikania", 
               dbname="PubMed", host="172.16.120.50")
sql<-"SELECT * FROM Article"
rs<-dbSendQuery(con, sql)
Article_bak<-fetch(rs, n=-1)
dbClearResult(rs)
Article<-Article_bak

sql<-"SELECT * FROM Article_Date"
rs<-dbSendQuery(con, sql)
Article_Date<-fetch(rs, n=-1)
dbClearResult(rs)


Article_Date[which(Article_Date$ArticleYear==-9999), "ArticleYear"]<-9999
Article_Date[which(Article_Date$DateCompletedYear==-9999), "DateCompletedYear"]<-9999
Article_Date[which(Article_Date$PubMedPubDateYear==-9999), "PubMedPubDateYear"]<-9999
Article_Date[which(Article_Date$JournalPubDateYear==-9999), "JournalPubDateYear"]<-9999
Article_Date[which(Article_Date$DateRevisedYear==-9999), "DateRevisedYear"]<-9999
Article_Date$year<-Article_Date$ArticleYear
Article_Date[which(Article_Date$year==9999), "year"]<-Article_Date[which(Article_Date$year==9999), "PubMedPubDateYear"]
Article_Date[which(Article_Date$year==9999), "year"]<-Article_Date[which(Article_Date$year==9999), "DateRevisedYear"]
Article_Date[which(Article_Date$year==9999), "year"]<-Article_Date[which(Article_Date$year==9999), "JournalPubDateYear"]
Article_Date[which(Article_Date$year==9999), "year"]<-Article_Date[which(Article_Date$year==9999), "DateCompletedYear"]

#Article_Date<-Article_Date %>% 
#  rowwise() %>% 
#  mutate(year= min(ArticleYear,DateCompletedYear,PubMedPubDateYear,JournalPubDateYear))

#set year to article
Article_full<-inner_join(Article, Article_Date[, c("PMID", "year", 
                                                  "ArticleYear",
                                                  "DateCompletedYear",
                                                  "JournalPubDateYear",
                                                  "DateRevisedYear",
                                                  "PubMedPubDateYear")], by="PMID")


saveRDS(Article_full, "../Objects/Article.rda")

N_article<-Article_full %>% count(year)
N_article<-N_article %>% filter(between(N_article$year, 1950, 2019))
plot(N_article$year, N_article$n, type="l")

sql<-"SELECT * FROM Author"
rs<-dbSendQuery(con, sql)
Author<-fetch(rs, n=-1)
dbClearResult(rs)

sql<-"SELECT * FROM Article_Author"
rs<-dbSendQuery(con, sql)
Article_Author<-fetch(rs, n=-1)
dbClearResult(rs)

N_Article_Author<-Article_Author %>%  count(PMID)
colnames(N_Article_Author)<-c("PMID", "N_Authors")

Article_full<-inner_join(Article_full, N_Article_Author, by="PMID")

source("functions.r")

N_Authors_se<-summarySE(Article_full, "N_Authors", "year", na.rm=T)
N_Authors_se<-N_Authors_se %>% filter(between(N_Authors_se$year, 1950, 2019))

library(ggplot2)
ggplot(N_Authors_se, aes(x=year, y=mean)) + geom_point() + geom_line() +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2,
                position=position_dodge(.9)) 
