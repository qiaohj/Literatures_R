library("RMySQL")
setwd("~/Experiments/PubMed/Script/PubMed_R")
con<-dbConnect(MySQL(), user="root", password="mikania", 
               dbname="PubMed", host="172.16.120.50")

journals<-c("Nature", "Science")
journal<-journals[1]
for (journal in journals){
  sql<-sprintf("SELECT * FROM Article WHERE Journal_Title='%s'", journal)
  rs<-dbSendQuery(con, sql)
  df<-fetch(rs, n=-1)
  dbClearResult(rs)
  dim(df)
  head(df[which(df$PubMedPubYear==-9999),])
  table(df$PubMedPubYear)
}