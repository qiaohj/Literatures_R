library("XML")
library("RMySQL")
library("R.utils")


#rs<-dbSendQuery(con, "select count(1) c, time from sensors group by time")

#df<-fetch(rs, n=-1)

setwd("~/Experiments/PubMed/Script/PubMed_R")


getXmlValue<-function(xml, path, type){
  v<-xpathApply(xml, path, xmlValue)
  if (type!="list"){
    if (length(v)==0){
      v<-NA
    }else{
      v<-v[[1]]
    }
    if (type=="character"){
      if (is.na(v)){
        v<-""
      }
    }
    if (type=="numeric"){
      if (is.na(v)){
        v<- -9999
      }else{
        if (v==""){
          v<- -9999
        }
        v<-as.numeric(v)
      }
    }
    if (type=="boolean"){
      if (is.na(v)){
        v<-0
      }else{
        if (v=="Y"){
          v<-1
        }else{
          v<-0
        }
      }
    }
  }
  if (type=="character"){
    v<-fix_sql(v)
  }
  return (v)
}

fs<-list.files("../../baseline", pattern="\\.gz$")
#fs<-c("pubmed19n0807.xml.gz")
from = 100
args = commandArgs(trailingOnly=TRUE)
from<-as.numeric(args[1])
to<-as.numeric(args[2])
skip<-as.numeric(args[3])
#from=608
#to=608
#for (xxx in c(from:length(fs))){
for (xxx in c(from:to)){
  f<-fs[xxx]
  print(paste(xxx, to, f, sep="/"))
  xml<-sprintf("../../baseline/%s", gsub("\\.gz", "", f))
  
  print("loading xml")
  doc<-xmlParse(xml)
  df<-getNodeSet(doc, "//PubmedArticle")
  
  for (i in c(1:length(df))){
    if ((i %% 1000)==1){
      print(paste(Sys.time(), i, length(df), sep="/"))
    }
    item<-xmlDoc(df[[i]])
    PMID<-getXmlValue(item, "//PMID", "numeric")
    sql<-sprintf("UPDATE Article SET XMLID=%d WHERE PMID=%d", i, PMID)
    dbExecute(con, sql)
  }
}

