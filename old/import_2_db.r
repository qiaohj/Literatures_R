library("XML")
library("RMySQL")
library("R.utils")



#rs<-dbSendQuery(con, "select count(1) c, time from sensors group by time")

#df<-fetch(rs, n=-1)

setwd("~/Experiments/PubMed/Script/PubMed_R")
source("functions.r")
killDbConnections()
con<-dbConnect(MySQL(), user="", password="", 
               dbname="PubMed", host="localhost")
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
  if (!file.exists(xml)){
    print("unzipping")
    gunzip(sprintf("../../baseline/%s", f), remove=F)
  }else{
    if (skip==1){
      print("someone is running on this file, skip it")
      
      next()
    }
  }
  print("loading xml")
  doc<-xmlParse(xml)
  df<-getNodeSet(doc, "//PubmedArticle")
  if (F){
    for (i in c(1:length(df))){
      item<-xmlDoc(df[[i]])
      PMID<-getXmlValue(item, "//PMID", "numeric")
      if (PMID==18474828){
        print(i)
        print(XXX)
      }
      
    }
  }
  for (i in c(1:length(df))){
    if ((i %% 1000)==1){
      print(paste(Sys.time(), i, length(df), sep="/"))
    }
    item<-xmlDoc(df[[i]])
    PMID<-getXmlValue(item, "//PMID", "numeric")
    sql<-sprintf("SELECT * FROM Article WHERE PMID=%d", PMID)
    if (is_exist_db(sql)){
      next()
    }
    Journal_ISSN<-strtrim(getXmlValue(item, "//Article/Journal/ISSN", "character"), 100)
    Volume<-strtrim(getXmlValue(item, "//Article/Journal/JournalIssue/Volume", "character"), 100)
    
    Issue<-strtrim(getXmlValue(item, "//Article/Journal/JournalIssue/Issue", "character"), 100)
    Abstract<-getXmlValue(item, "//Article/Abstract", "character") 
    Pagination<-getXmlValue(item, "//Article/Pagination", "character")
    ArticleTitle<-getXmlValue(item, "//Article/ArticleTitle", "character") 
    Journal_Title<-getXmlValue(item, "//Article/Journal/Title", "character")
    ISOAbbreviation<-getXmlValue(item, "//Article/Journal/ISOAbbreviation", "character")
    DateCompletedYear<-getXmlValue(item, "//DateCompleted/Year", "numeric")
    DateCompletedMonth<-getXmlValue(item, "//DateCompleted/Month", "numeric")
    DateCompletedDay<-getXmlValue(item, "//DateCompleted/Day", "numeric")
    Language<-getXmlValue(item, "//Article/Language", "character") 
    ArticleYear<-getXmlValue(item, "//Article/ArticleDate/Year", "numeric")
    ArticleMonth<-getXmlValue(item, "//Article/ArticleDate/Month", "numeric") 
    ArticleDay<-getXmlValue(item, "//Article/ArticleDate/Day", "numeric") 
    Journal_ISSN_Type<-""
    if (length(getNodeSet(item, "//Article/Journal/ISSN"))>=1){
      Journal_ISSN_Type<-xmlAttrs(getNodeSet(item, "//Article/Journal/ISSN")[[1]])
      if ("IssnType" %in% names(Journal_ISSN_Type)){
        Journal_ISSN_Type<-Journal_ISSN_Type[which(names(Journal_ISSN_Type)=="IssnType")]
      }else{
        Journal_ISSN_Type<-""
      }
    }
    
    sql<-sprintf("INSERT INTO `PubMed`.`Article` (`PMID`, `Journal_ISSN`, `Volume`,
    `Issue`,`Abstract`,`Pagination`,`Journal_Title`,`ISOAbbreviation`,`ArticleTitle`,
    `DateCompleteYear`,`DateCompleteMonth`,`DateCompleteDay`,`Language`,`ArticleYear`,`ArticleMonth`,
    `ArticleDay`,`Journal_ISSN_Type`) VALUES ('%d',  '%s', '%s', '%s', '%s', '%s', '%s', '%s',
    '%s','%s','%s','%s','%s','%s','%s','%s','%s');", PMID, Journal_ISSN, Volume,
                 Issue,Abstract,Pagination,Journal_Title,ISOAbbreviation,ArticleTitle,
                 DateCompleteYear,DateCompleteMonth,DateCompleteDay,Language,ArticleYear,ArticleMonth,
                 ArticleDay,Journal_ISSN_Type)
    
    dbExecute(con, sql)
    AuthorList<-getNodeSet(item, "//Article/AuthorList/Author")
    j=2
    if (length(AuthorList)>0){
      for (j in c(1:length(AuthorList))){
        author_item<-xmlDoc(AuthorList[[j]])
        LastName<-getXmlValue(author_item, "//LastName", "character") 
        if (LastName==""){
          next()
        }
        ForeName<-getXmlValue(author_item, "//ForeName", "character") 
        Initials<-getXmlValue(author_item, "//Initials", "character") 
        AffiliationInfo<-strtrim(getXmlValue(author_item, "//AffiliationInfo", "character"), 2500)
        sql<-sprintf("SELECT * FROM Author WHERE LastName='%s' and ForeName='%s' and Initials='%s'
          and AffiliationInfo='%s'", LastName, ForeName, Initials, AffiliationInfo)
        rs<-dbSendQuery(con, sql)
        df_auther<-fetch(rs, n=-1)
        dbClearResult(rs)
        if (nrow(df_auther)==0){
          sql<-sprintf("INSERT INTO `PubMed`.`Author` (`LastName`, `ForeName`, `Initials`, 
            `AffiliationInfo`) VALUES ('%s', '%s', '%s', '%s');", LastName, ForeName, Initials, 
                       AffiliationInfo)
          dbExecute(con, sql)
          AuthorID<-dbGetQuery(con, "SELECT LAST_INSERT_ID();")[1, 1]
        }else{
          AuthorID<-df_auther[1, "ID"]
        }
        sql<-sprintf("INSERT INTO `PubMed`.`Article_Author`(`PMID`,`AuthorID`, Sort) VALUES (%d, %d, %d);", 
                     PMID, AuthorID, j)
        dbExecute(con, sql)
        #free(author_item)
      }
    }
    
    MeshHeadingList<-getNodeSet(item, "//MeshHeadingList/MeshHeading")
    j=2
    if (length(MeshHeadingList)>0){
      for (j in c(1:length(MeshHeadingList))){
        MeshHeading_item<-xmlDoc(MeshHeadingList[[j]])
        DescriptorName<-getNodeSet(MeshHeading_item, "//DescriptorName")
        Sort<-1
        if (length(DescriptorName)>0){
          for (k in c(1:length(DescriptorName))){
            DescriptorName_item<-DescriptorName[[k]]
            ids<-insertKeyword(DescriptorName_item, "DescriptorName")
            sql<-sprintf("INSERT INTO `PubMed`.`Article_Keyword` (`PMID`, `KeywordID`, `MajorTopicYN`,
              `Type`, `Sort`, `Group`) VALUES (%d, '%s', %d, '%s', %d, %d);", 
                         PMID, ids$UI, ids$MajorTopicYN, "DescriptorName", Sort, j)
            dbExecute(con, sql)
            Sort<-Sort+1
          }
        }
        QualifierName<-getNodeSet(MeshHeading_item, "//QualifierName")
        if (length(QualifierName)>0){
          for (k in c(1:length(QualifierName))){
            QualifierName_item<-QualifierName[[k]]
            ids<-insertKeyword(QualifierName_item, "QualifierName")
            sql<-sprintf("INSERT INTO `PubMed`.`Article_Keyword` (`PMID`, `KeywordID`, `MajorTopicYN`,
              `Type`, `Sort`, `Group`) VALUES (%d, '%s', %d, '%s', %d, %d);", 
                         PMID, ids$UI, ids$MajorTopicYN, "QualifierName", Sort, j)
            dbExecute(con, sql)
            Sort<-Sort+1
          }
        }
        #free(MeshHeading_item)
      }
    }
    
    ChemicalList<-getNodeSet(item, "//ChemicalList/Chemical")
    if (length(ChemicalList)>0){
      for (j in c(1:length(ChemicalList))){
        Chemical_item<-xmlDoc(ChemicalList[[j]])
        NameOfSubstance<-getNodeSet(Chemical_item, "//NameOfSubstance")
        if (length(NameOfSubstance)>0){
          for (k in c(1:length(NameOfSubstance))){
            NameOfSubstance_item<-NameOfSubstance[[k]]
            ids<-insertKeyword(NameOfSubstance_item, "NameOfSubstance")
            sql<-sprintf("INSERT INTO `PubMed`.`Article_Keyword` (`PMID`, `KeywordID`, `MajorTopicYN`,
              `Type`, `Sort`, `Group`) VALUES (%d, '%s', %d, '%s', %d, %d);", 
                         PMID, ids$UI, ids$MajorTopicYN, "NameOfSubstance", k, j)
            dbExecute(con, sql)
          }
        }
        #free(Chemical_item)
      }
    }
    
    GrantList<-getNodeSet(item, "//GrantList/Grant")
    if (length(GrantList)>0){
      for (j in c(1:length(GrantList))){
        Grant_item<-xmlDoc(GrantList[[j]])
        GrantID<-strtrim(getXmlValue(Grant_item, "//GrantID", "character"), 450)
        Acronym<-getXmlValue(Grant_item, "//Acronym", "character") 
        Agency<-getXmlValue(Grant_item, "//Agency", "character") 
        Country<-getXmlValue(Grant_item, "//Country", "character")
        
        sql<-sprintf("SELECT * FROM `Grant` WHERE GrantID='%s'", GrantID)
        if (!is_exist_db(sql)){
          sql<-sprintf("INSERT INTO `PubMed`.`Grant` (`GrantID`, `Acronym`, `Agency`,
            `Country`) VALUES ('%s', '%s', '%s', '%s');", 
                       GrantID, Acronym, Agency, Country)
          dbExecute(con, sql)
        }
        sql<-sprintf("INSERT INTO `PubMed`.`Article_Grant` (`PMID`, `GrantID`, `Sort`) VALUES
          (%d, '%s', %d);", PMID, GrantID, j)
        dbExecute(con, sql)
        #free(Grant_item)
      }
    }
    
    PublicationTypeList<-getNodeSet(item, "//PubmedArticle/MedlineCitation/Article/PublicationTypeList/PublicationType")
    if (length(PublicationTypeList)>0){
      j=1
      for (j in c(1:length(PublicationTypeList))){
        PublicationType_item<-PublicationTypeList[[j]]
        ids<-insertKeyword(PublicationType_item, "PublicationType")
        sql<-sprintf("INSERT INTO `PubMed`.`Article_Keyword` (`PMID`, `KeywordID`, `MajorTopicYN`,
              `Type`, `Sort`, `Group`) VALUES (%d, '%s', %d, '%s', %d, %d);", 
                     PMID, ids$UI, ids$MajorTopicYN, "PublicationType", 0, j)
        dbExecute(con, sql)
        
      }
    }
    
    ArticleIdList<-getNodeSet(item, "//PubmedArticle/PubmedData/ArticleIdList/ArticleId")
    if (length(ArticleIdList)>0){
      j=1
      for (j in c(1:length(ArticleIdList))){
        ArticleId_item<-ArticleIdList[[j]]
        attrs<-xmlAttrs(ArticleId_item)
        IdType<-attrs[which(names(attrs)=="IdType")]
        text<-gsub("'", "’", xmlValue(ArticleId_item))
        sql<-sprintf("INSERT INTO `PubMed`.`ArticleIDType` (`IdType`, `PMID`, `Text`) VALUES
          ('%s', %d, '%s');", IdType, PMID, text)
        dbExecute(con, sql)
        #free(ArticleId_item)
      }
    }
    
    ReferenceList<-getNodeSet(item, "//PubmedArticle/PubmedData/ReferenceList/Reference")
    if (length(ReferenceList)>0){
      j=1
      for (j in c(1:length(ReferenceList))){
        Reference_item<-xmlDoc(ReferenceList[[j]])
        Citation<-getXmlValue(Reference_item, "//Citation", "character")
        ArticleIdList<-getNodeSet(Reference_item, "//Reference/ArticleIdList/ArticleId")
        if (length(ArticleIdList)>0){
          j=1
          for (j in c(1:length(ArticleIdList))){
            ArticleId_item<-ArticleIdList[[j]]
            attrs<-xmlAttrs(ArticleId_item)
            IdType<-attrs[which(names(attrs)=="IdType")]
            CitationID<-gsub("'", "’", xmlValue(ArticleId_item))
            #free(ArticleId_item)
            sql<-sprintf("INSERT INTO `PubMed`.`Reference` (`PMID`, `Citation`,
              `CitationID`, `CitationType`) VALUES (%d, '%s', '%s', '%s');", 
                         PMID, Citation, CitationID, IdType)
            dbExecute(con, sql)
          }
        }
        #free(Reference_item)
      }
    }
    #free(item)
  }
}

