#N of articles per year
library("RMySQL")
library("purrr")
library("dplyr")
con<-dbConnect(MySQL(), user="root", password="mikania", 
               dbname="PubMed", host="172.16.120.50")
sql<-"SELECT * FROM Article"
rs<-dbSendQuery(con, sql)
Article<-fetch(rs, n=-1)
dbClearResult(rs)
Article_bak<-Article

#remove all the artiles without an year information
Article<-Article_bak
Article$year<-Article$ArticleYear
local_temp<-which(Article$year==-9999)
Article[local_temp, "year"]<-Article[local_temp, "DateCompletedYear"]

Article_temp<-Article %>% filter(year==-9999)
dim(Article_temp)
