library("XML")
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
  return (v)
}

doc<-xmlParse("../../baseline/pubmed19n0599.xml")
df<-getNodeSet(doc, "//PubmedArticle")
i=2
18474828
for (i in c(1:length(df))){
  item<-xmlDoc(df[[i]])
  PMID<-getXmlValue(item, "//PMID", "numeric")
  if (PMID==18474828){
    print(i)
    print(XXX)
  }
  
}
for (i in c(1:length(df))){
  item<-xmlDoc(df[[i]])
  PMID<-getXmlValue(item, "//PMID", "numeric")
  Journal_ISSN<-getXmlValue(item, "//Article/Journal/ISSN", "character")
  Volume<-getXmlValue(item, "//Article/Journal/JournalIssue/Volume", "numeric")
  Issue<-getXmlValue(item, "//Article/Journal/JournalIssue/Issue", "numeric")
  Abstract<-getXmlValue(item, "//Article/Abstract", "character") 
  Pagination<-getXmlValue(item, "//Article/Pagination", "character")
  ArticleTitle<-getXmlValue(item, "//Article/ArticleTitle", "character") 
  Journal_Title<-getXmlValue(item, "//Article/Journal/Title", "character")
  ISOAbbreviation<-getXmlValue(item, "//Article/Journal/ISOAbbreviation", "character")
  PubMedPubYear<-getXmlValue(item, "//DateCompleted/Year", "numeric")
  PubMedPubMonth<-getXmlValue(item, "//DateCompleted/Month", "numeric")
  PubMedPubDay<-getXmlValue(item, "//DateCompleted/Day", "numeric")
  Language<-getXmlValue(item, "//Article/Language", "character") 
  ArticleYear<-getXmlValue(item, "//Article/ArticleDate/Year", "numeric")
  ArticleMonth<-getXmlValue(item, "//Article/ArticleDate/Month", "numeric") 
  ArticleDay<-getXmlValue(item, "//Article/ArticleDate/Day", "numeric") 
  
  
  PublicationType<-getXmlValue(item, "//Article/PublicationTypeList/PublicationType", "list")
  
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
      AffiliationInfo<-getXmlValue(author_item, "//AffiliationInfo", "character") 
      free(author_item)
    }
  }
  
  MeshHeadingList<-getNodeSet(item, "//MeshHeadingList/MeshHeading")
  j=2
  if (length(MeshHeadingList)>0){
    for (j in c(1:length(MeshHeadingList))){
      MeshHeading_item<-xmlDoc(MeshHeadingList[[j]])
      DescriptorName<-getNodeSet(MeshHeading_item, "//DescriptorName")
      
      if (length(DescriptorName)>0){
        for (k in c(1:length(DescriptorName))){
          DescriptorName_item<-DescriptorName[[k]]
          ids<-insertKeyword(DescriptorName_item)
        }
      }
      QualifierName<-getNodeSet(MeshHeading_item, "//QualifierName")
      if (length(QualifierName)>0){
        for (k in c(1:length(QualifierName))){
          QualifierName_item<-QualifierName[[k]]
          ids<-insertKeyword(QualifierName_item)
        }
      }
      free(MeshHeading_item)
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
          ids<-insertKeyword(NameOfSubstance_item)
        }
      }
      free(Chemical_item)
    }
  }
  
  GrantList<-getNodeSet(item, "//GrantList/Grant")
  free(item)
}

insertKeyword<-function(node){
  attrs<-xmlAttrs(node)
  text<-xmlValue(node)
  if ("UI" %in% names(attrs)){
    UI<-attrs[which(names(attrs)=="UI")]
  }else{
    UI<-""
  }
  if ("MajorTopicYN" %in% names(attrs)){
    MajorTopicYN<-attrs[which(names(attrs)=="MajorTopicYN")]
  }else{
    MajorTopicYN<-""
  }
  if (MajorTopicYN=="Y"){
    MajorTopicYN<-1
  }else{
    MajorTopicYN<-0
  }
  return(data.frame(UI=UI, MajorTopicYN=MajorTopicYN))
}

