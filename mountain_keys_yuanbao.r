library("RMySQL")
con<-dbConnect(MySQL(), user="root", password="mikania", 
               dbname="PubMed", host="172.16.120.50")
keywords1<-c("mountain system", "mountainous region", "elevational gradient", "altitude gradient")
keywords2<-c("biodiversity", "species richness", "species diversity", "taxonomic diversity",
              "phylogenetic diversity", "phylogenetic structure", "functional diversity",
              "functional structure", "trait diversity")
sql_t<-"SELECT * FROM Article WHERE MATCH (`Abstract`, `ArticleTitle`) AGAINST ('\"%s\"')"
keyword<-keywords1[2]
result_1<-data.frame()
for (keyword in keywords1){
  print(keyword)
  sql<-sprintf(sql_t, keyword)
  rs<-dbSendQuery(con, sql)
  df<-fetch(rs, n=-1)
  print(sprintf("matched %d records", nrow(df)))
  dbClearResult(rs)
  if (nrow(result_1)==0){
    result_1<-df
  }else{
    result_1<-rbind(result_1, df)
  }
}

result_2<-data.frame()
for (keyword in keywords2){
  print(keyword)
  sql<-sprintf(sql_t, keyword)
  rs<-dbSendQuery(con, sql)
  df<-fetch(rs, n=-1)
  print(sprintf("matched %d records", nrow(df)))
  dbClearResult(rs)
  if (nrow(result_2)==0){
    result_2<-df
  }else{
    result_2<-rbind(result_2, df)
  }
}


id1<-unique(result_1$PMID)
id2<-unique(result_2$PMID)

length(id1)
length(id2)


id_all<-intersect(id1, id2)
length(id_all)

result_all<-result_1[which(result_1$PMID %in% id_all),]
table(result_all$ArticleYear)
table(result_all$Journal_Title)
head(result_all)
write.csv(result_all, "../mountain_keys_yuanbao.csv", row.names=F)
