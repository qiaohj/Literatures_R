library("XML")
library("RMySQL")
library("R.utils")


#rs<-dbSendQuery(con, "select count(1) c, time from sensors group by time")

#df<-fetch(rs, n=-1)

setwd("~/Experiments/PubMed/Script/PubMed_R")


source("functions.r")
con<-dbConnect(MySQL(), user="", password="", 
               dbname="PubMed", host="localhost")

fs<-list.files("/media/huijieqiao/QNAS/PubMed/baseline", pattern="\\.gz$")
#fs<-c("pubmed19n0807.xml.gz")
from = 100
args = commandArgs(trailingOnly=TRUE)
from<-as.numeric(args[1])
to<-as.numeric(args[2])
#from=608
#to=608
#for (xxx in c(from:length(fs))){
for (xxx in c(from:to)){
  f<-fs[xxx]
  print(paste(xxx, to, f, sep="/"))
  xml<-sprintf("/media/huijieqiao/QNAS/PubMed/baseline/%s", gsub("\\.gz", "", f))
  
  print("loading xml")
  doc<-xmlParse(xml)
  df<-getNodeSet(doc, "//PubmedArticle")
  
  for (i in c(1:length(df))){
    if ((i %% 1000)==1){
      print(paste(Sys.time(), i, length(df), sep="/"))
    }
    item<-xmlDoc(df[[i]])
    PMID<-getXmlValue(item, "//PMID", "numeric")
    sql<-sprintf("SELECT * FROM Article_Date WHERE PMID=%d", PMID)
    if (is_exist_db(sql)){
      next()
    }
    
    ArticleYear<-getXmlValue(item, "//Article/ArticleDate/Year", "numeric")
    ArticleMonth<-getXmlValue(item, "//Article/ArticleDate/Month", "numeric") 
    ArticleDay<-getXmlValue(item, "//Article/ArticleDate/Day", "numeric") 
    
    DateCompletedYear<-getXmlValue(item, "//DateCompleted/Year", "numeric")
    DateCompletedMonth<-getXmlValue(item, "//DateCompleted/Month", "numeric")
    DateCompletedDay<-getXmlValue(item, "//DateCompleted/Day", "numeric")
    
    DateRevisedYear<-getXmlValue(item, "//DateRevised/Year", "numeric")
    DateRevisedMonth<-getXmlValue(item, "//DateRevised/Month", "numeric")
    DateRevisedDay<-getXmlValue(item, "//DateRevised/Day", "numeric")
    
    PubMedPubDateYear<-getXmlValue(item, "//PubmedData/History/PubMedPubDate/Year", "numeric")
    PubMedPubDateMonth<-getXmlValue(item, "//PubmedData/History/PubMedPubDate/Month", "numeric")
    PubMedPubDateDay<-getXmlValue(item, "//PubmedData/History/PubMedPubDate/Day", "numeric")
    
    JournalPubDateYear<-getXmlValue(item, "//MedlineCitation/Article/Journal/JournalIssue/PubDate/Year", "numeric")
    JournalPubDateMonth<-getXmlValue(item, "//MedlineCitation/Article/Journal/JournalIssue/PubDate/Month", "character")
    JournalPubDateDay<-getXmlValue(item, "//MedlineCitation/Article/Journal/JournalIssue/PubDate/Day", "character")
    
    sql<-sprintf("INSERT INTO `PubMed`.`Article_Date` (`PMID`, `ArticleYear`, `DateCompletedYear`,
    `PubMedPubDateYear`, `JournalPubDateYear`, `ArticleMonth`, `DateCompletedMonth`, `PubMedPubDateMonth`,
    `JournalPubDateMonth`, `JournalPubDateMonthGuess`, `ArticleDay`, `DateCompletedDay`, `PubMedPubDateDay`,
    `JournalPubDateDay`, `JournalPubDateDayGuess`, `DateRevisedYear`, `DateRevisedMonth`, `DateRevisedDay`)
    VALUES (%d, %d, %d, %d, %d, %d, %d, %d, '%s', %d, %d, %d, %d, '%s', %d, %d, %d, %d);",
                 PMID, ArticleYear, DateCompletedYear,
                 PubMedPubDateYear, JournalPubDateYear, ArticleMonth, DateCompletedMonth, PubMedPubDateMonth,
                 JournalPubDateMonth, -9999, ArticleDay, DateCompletedDay, PubMedPubDateDay,
                 JournalPubDateDay, -9999, DateRevisedYear, DateRevisedMonth, DateRevisedDay)
    dbExecute(con, sql)
  }
}

