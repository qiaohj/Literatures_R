
## extract a data frame from the XML downloaded via https://pubmed.ncbi.nlm.nih.gov/download/
source("functions.r")
extract_pubmed<-function(xml){
  doc<-xmlParse(xml)
  df<-getNodeSet(doc, "//PubmedArticle")
  i=14
  Article_List<-list()
  Author_List<-list()
  Keyword_List<-list()
  PublicationType_List<-list()
  Grant_List<-list()
  ArticleId_List<-list()
  Reference_List<-list()
  for (i in c(1:length(df))){
    if ((i %% 1000)==1){
      print(paste(Sys.time(), i, length(df), sep="/"))
    }
    item<-xmlDoc(df[[i]])
    PMID<-getXmlValue(item, "//PMID", "numeric")
    
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
    
    Article_List[[length(Article_List)+1]]<-data.table(PMID, Journal_ISSN, Volume,
                                                       Issue,Abstract,Pagination,Journal_Title,
                                                       ISOAbbreviation,ArticleTitle,
                                                       DateCompletedYear,DateCompletedMonth,DateCompletedDay,
                                                       Language,ArticleYear,ArticleMonth,
                                                       ArticleDay,Journal_ISSN_Type)
    
    
    AuthorList<-getNodeSet(item, "//Article/AuthorList/Author")
    
    j=1
    if (length(AuthorList)>0){
      
      for (j in c(1:length(AuthorList))){
        author_item<-xmlDoc(AuthorList[[j]])
        LastName<-getXmlValue(author_item, "//LastName", "character") 
        if (LastName==""){
          next()
        }
        ForeName<-getXmlValue(author_item, "//ForeName", "character") 
        Initials<-getXmlValue(author_item, "//Initials", "character") 
        AffiliationInfo<-xpathApply(author_item, "//AffiliationInfo/Affiliation", xmlValue)
        
        if (length(AffiliationInfo)>0){
          for (k in c(1:length(AffiliationInfo))){
            Affiliation<-AffiliationInfo[[k]]
            author<-data.table(PMID, LastName, ForeName, Initials, Affiliation, Sort=k, Group=j)
            Author_List[[length(Author_List)+1]]<-author
          }
        }else{
          author<-data.table(PMID, LastName, ForeName, Initials, Affiliation="", Sort=1, Group=j)
          Author_List[[length(Author_List)+1]]<-author
        }
        
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
            ids<-getKeyword(DescriptorName_item, "DescriptorName")
            ids$PMID<-PMID
            ids$Sort<-k
            ids$Group<-j
            Keyword_List[[length(Keyword_List)+1]]<-ids
          }
        }
        QualifierName<-getNodeSet(MeshHeading_item, "//QualifierName")
        if (length(QualifierName)>0){
          for (k in c(1:length(QualifierName))){
            QualifierName_item<-QualifierName[[k]]
            ids<-getKeyword(QualifierName_item, "QualifierName")
            ids$PMID<-PMID
            ids$Sort<-k
            ids$Group<-j
            Keyword_List[[length(Keyword_List)+1]]<-ids
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
            ids<-getKeyword(NameOfSubstance_item, "NameOfSubstance")
            ids$PMID<-PMID
            ids$Sort<-k
            ids$Group<-j
            Keyword_List[[length(Keyword_List)+1]]<-ids
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
        Grant_List[[length(Grant_List)+1]]<-data.table(PMID, GrantID, Acronym, Agency, Country, Sort=j)
        
      }
    }
    
    PublicationTypeList<-getNodeSet(item, "//PubmedArticle/MedlineCitation/Article/PublicationTypeList/PublicationType")
    if (length(PublicationTypeList)>0){
      j=1
      for (j in c(1:length(PublicationTypeList))){
        PublicationType_item<-PublicationTypeList[[j]]
        ids<-getKeyword(PublicationType_item, "PublicationType")
        ids$PMID<-PMID
        ids$Sort<-1
        ids$Group<-j
        Keyword_List[[length(Keyword_List)+1]]<-ids
        
      }
    }
    
    ArticleIdList<-getNodeSet(item, "//PubmedArticle/PubmedData/ArticleIdList/ArticleId")
    if (length(ArticleIdList)>0){
      j=1
      for (j in c(1:length(ArticleIdList))){
        ArticleId_item<-ArticleIdList[[j]]
        attrs<-xmlAttrs(ArticleId_item)
        IdType<-attrs[which(names(attrs)=="IdType")]
        text<-xmlValue(ArticleId_item)
        ArticleId_List[[length(ArticleId_List)+1]]<-data.table(PMID, IdType, Text=text, Sort=j)
        
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
          k=1
          for (k in c(1:length(ArticleIdList))){
            ArticleId_item<-ArticleIdList[[k]]
            attrs<-xmlAttrs(ArticleId_item)
            IdType<-attrs[which(names(attrs)=="IdType")]
            CitationID<-xmlValue(ArticleId_item)
            Reference_List[[length(Reference_List)+1]]<-data.table(PMID, Citation, CitationID, IdType, Sort=k, Group=j)
            
          }
        }
        
      }
    }
    
  }
  
  Article_df<-rbindlist(Article_List)
  Author_df<-rbindlist(Author_List)
  Keyword_df<-rbindlist(Keyword_List)
  PublicationType_df<-rbindlist(PublicationType_List)
  Grant_df<-rbindlist(Grant_List)
  ArticleId_df<-rbindlist(ArticleId_List)
  Reference_df<-rbindlist(Reference_List)
  list("Article"=Article_df,
       "Author"=Author_df,
       "Keyword"=Keyword_df,
       "PublicationType"=PublicationType_df,
       "Grant"=Grant_df,
       "ArticleId"=ArticleId_df,
       "Reference"=Reference_df)
}



library(R.utils)
library(XML)
library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
#xx<-extract_pubmed("/media/huijieqiao/WD22T_old/pubmed23n1167.xml")
#source_folder<-"/media/huijieqiao/WD22T_11/literatures/RAW/baseline"
source_folder<-"/media/huijieqiao/WD22T_11/literatures/RAW/updatefiles"
zips<-list.files(source_folder, pattern = "\\.gz")
f<-zips[1]
for (i in c(1:length(zips))){
  f<-zips[i]
  if (grepl("md5", f)){
    next()
  }
  print(paste(i, length(zips), f))
  name<-gsub("\\.xml\\.gz", "", f)
  target<-sprintf("../Data/datatable/%s.rda", name)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  xml<-sprintf("../Data/xml/%s.xml", name)
  if (!file.exists(xml)){
    gunzip(sprintf("%s/%s", source_folder, f), remove=FALSE,
         destname = xml)
  }
  df<-extract_pubmed(xml)
  saveRDS(df, target)
}


if (F){
  library(data.table)
  files<-list.files("../Data/datatable", pattern="\\.rda")
  f<-files[1]
  Article_List<-list()
  Author_List<-list()
  Keyword_List<-list()
  Grant_List<-list()
  ArticleId_List<-list()
  Reference_List<-list()
  for (i in c(1:length(files))){
    f<-files[i]
    print(paste(i, length(files), f))
    dd<-readRDS(sprintf("../Data/datatable/%s", f))
    dd$Article$File<-f
    dd$Author$File<-f
    dd$Keyword$File<-f
    dd$PublicationType$File<-f
    dd$Grant$File<-f
    dd$ArticleId$File<-f
    dd$Reference$File<-f
    
    Article_List[[i]]<-dd$Article
    Author_List[[i]]<-dd$Author
    Keyword_List[[i]]<-dd$Keyword
    Grant_List[[i]]<-dd$Grant
    ArticleId_List[[i]]<-dd$ArticleId
    Reference_List[[i]]<-dd$Reference
    
  }
  Article_df<-rbindlist(Article_List)
  Author_df<-rbindlist(Author_List)
  Keyword_df<-rbindlist(Keyword_List)
  Grant_df<-rbindlist(Grant_List, fill=T)
  ArticleId_df<-rbindlist(ArticleId_List)
  Reference_df<-rbindlist(Reference_List)
  saveRDS(Article_df, "../Data/Article.rda")
  saveRDS(Author_df, "../Data/Author.rda")
  saveRDS(Keyword_df, "../Data/Keyword.rda")
  saveRDS(Grant_df, "../Data/Grant.rda")
  saveRDS(ArticleId_df, "../Data/ArticleId.rda")
  saveRDS(Reference_df, "../Data/Reference.rda")
  dim(Article_df)
  dim(Author_df)
  dim(Keyword_df)
  dim(Grant_df)
  dim(ArticleId_df)
  dim(Reference_df)
}
if (F){
  library(data.table)
  Article_df<-readRDS("../Data/Article.rda")
  Author_df<-readRDS("../Data/Author.rda")
  Keyword_df<-readRDS("../Data/Keyword.rda")
  Grant_df<-readRDS("../Data/Grant.rda")
  ArticleId_df<-readRDS("../Data/ArticleId.rda")
  Reference_df<-readRDS("../Data/Reference.rda")
  journal_df<-readRDS("../Data/JCR/journals.rda")
  
  issn<-unique(Article_df$Journal_ISSN)
  i=1
  for (i in c(1:length(issn))){
    x<-issn[i]
    print(paste(x, i, length(issn)))
    target<-sprintf("../Data/By_Journal/%s", x)
    if (dir.exists(target)){
      next()
    }
    dir.create(target, showWarnings = F)
    item<-Article_df[Journal_ISSN==x]
    item$File<-NULL
    item<-unique(item)
    pmid<-unique(item$PMID)
    author<-Author_df[PMID %in% pmid]
    author$File<-NULL
    author<-unique(author)
    keyword<-Keyword_df[PMID %in% pmid]
    keyword$File<-NULL
    keyword<-unique(keyword)
    grant<-Grant_df[PMID %in% pmid]
    grant$File<-NULL
    grant<-unique(grant)
    articleid<-ArticleId_df[PMID %in% pmid]
    articleid$File<-NULL
    articleid<-unique(articleid)
    reference<-Reference_df[PMID %in% pmid]
    reference$File<-NULL
    reference<-unique(reference)
    saveRDS(item, sprintf("%s/Article.rda", target))
    saveRDS(author, sprintf("%s/Author.rda", target))
    saveRDS(keyword, sprintf("%s/Keyword.rda", target))
    saveRDS(grant, sprintf("%s/Grant.rda", target))
    saveRDS(articleid, sprintf("%s/ArticleId.rda", target))
    saveRDS(reference, sprintf("%s/Reference.rda", target))
  }
  
  length(issn[issn %in% journal_df$ISSN])
  length(issn[issn %in% journal_df$eISSN])
  length(issn)
  
  dim(Article_df[Journal_ISSN==journal_df[Title=="AMERICAN NATURALIST"][1]$ISSN])
  dim(Article_df[Journal_ISSN==journal_df[Title=="AMERICAN NATURALIST"][1]$eISSN])
  dim(Article_df[Journal_ISSN=="1466-8238"])
  test_item<-Article_df[Journal_ISSN=="2041-210X"]
  
  
  test_item2<-Article_df[Journal_Title=="Methods in ecology and evolution"]
  
  Article_df$File<-NULL
  Article_df_unique<-unique(Article_df)
  dim(Article_df_unique)
  dim(Article_df)
  Article_se<-Article_df_unique[, .(N=.N), by=list(Journal_ISSN, Journal_Title, 
                                                   ISOAbbreviation, ArticleYear,
                                                   Journal_ISSN_Type)]
  saveRDS(Article_se, "../Data/N_Article_By_Journal_Year.rda")
  Article_se2<-Article_df_unique[, .(N=.N), by=list(Journal_ISSN, Journal_Title, 
                                                   ISOAbbreviation,
                                                   Journal_ISSN_Type)]
  saveRDS(Article_se2, "../Data/N_Article_By_Journal.rda")
  
  View(Article_se2[grepl("Ecolog", Journal_Title)])
}