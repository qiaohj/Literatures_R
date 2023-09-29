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

#add author name and gender to list
head(Author)
genders<-readRDS("../Objects/genders.rda")
nameFun<-function(name){
  items<-unlist(strsplit(trimws(name), " "))
  items<-items[nchar(items)==max(nchar(items))]
  return(items[1])
}

names<-map_chr(Author$ForeName, nameFun)
getGender<-function(t_name){
  item<-genders %>% filter(name==t_name)
  if (nrow(item)==0){
    item<-data.frame(name=t_name, proportion_male=NA,
                     proportion_female=NA,
                     gender=NA,
                     year_min=NA,
                     year_max=NA, stringsAsFactors = F) 
  }
  return(item)
}

t_names<-names[sample(length(names), 100)]
map_dfr(t_names, getGender)
length(names)
dim(Author)


name_gender<-map_dfr(unique(names), getGender)

head(name_gender)

saveRDS(name_gender, "../Objects/name_gender.rda")

head(Author)
Author<-Author[, colnames(Author)[1:9]]
colnames(name_gender)[1]<-"PotentialForeName"
Author_Full<-left_join(Author, name_gender, by="PotentialForeName")
saveRDS(Author_Full, "../Objects/Author_Full.rda")

head(Article_Author)
Article_Author_Full<-inner_join(Article_Author, Author_Full, by=c("AuthorID"="ID"))
Article_Author_Full<-inner_join(Article_Author_Full, Article_full, by=c("PMID"))
head(Article_Author_Full)

colnames(Article_Author_Full)

Article_Author_Full<-Article_Author_Full[, c("ID","PMID","AuthorID","Sort",
                                             "LastName","ForeName","Initials",
                                             "AffiliationInfo","PotentialForeName",
                                             "proportion_male","proportion_female",
                                             "gender","Journal_ISSN","N_Authors","year")]

saveRDS(Article_Author_Full, "../Objects/Article_Author_Full.rda")

head(Article_Author_Full)

IF<-read.csv("../Tables/IF_1997_2018.csv", head=T, sep=",", stringsAsFactors = F)
IF<-IF[,c("JCR.Abbreviated.Title", "Journal.Impact.Factor", "ISSN", "year", "Category")]
colnames(IF)<-c("ABBR", "IF", "Journal_ISSN", "year", "Category")
Article_Author_Full_With_IF<-inner_join(Article_Author_Full, IF, by=c("Journal_ISSN", "year"))
dim(Article_Author_Full_With_IF)
Article_Author_Full_With_IF<-Article_Author_Full_With_IF[which(!is.na(Article_Author_Full_With_IF$IF)),]
dim(Article_Author_Full_With_IF)

saveRDS(Article_Author_Full_With_IF, "../Objects/Article_Author_Full_With_IF.rda")


