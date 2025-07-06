library(data.table)
library(pdftools)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
if (F){
  journals<-readRDS("../Data/CrossRef_Full/journals.rda")
  mee<-journals[Title=="Methods in Ecology and Evolution"]
}

if (F){
  pdfs<-readRDS("/media/huijieqiao/Almost_Broken/SCIHUB/Data/PDF/full_pdf_scihub.rda")
  pdf_mee<-pdfs[DOI1=="10.1111"]
  pdf_mee<-pdf_mee[grepl("2041-210x", DOI2)]
  for (i in c(1:nrow(pdf_mee))){
    print(i)
    item<-pdf_mee[i]
    file.copy(item$pdf, "/media/huijieqiao/WD22T_11/literatures/Data/MEE/")
  }
  issn1<-"2041"
  issn2<-"210X"
  articles<-readRDS(sprintf("../Data/CrossRef_By_Journal/%s/%s-%s/articles.rda", issn1, issn1, issn2))
  articles<-articles[type=="journal-article"]
  articles$Year<-format(articles$published, "%Y")
  articles[is.na(Year)]$Year<-format(articles[is.na(Year)]$published_online, "%Y")
  articles$Year<-as.numeric(articles$Year)
  dois<-strsplit(articles$doi, "/")
  
  doi_df<-data.table(do.call(rbind, dois))
  dois2<-strsplit(doi_df$V2, "\\.")
  doi2_df<-data.table(do.call(rbind, dois2))
  
  unique(doi_df$V1)
  unique(dois2[1,])
  
  files<-list.files("/media/huijieqiao/WD22T_11/literatures/Data/MEE/", pattern="\\.pdf")
  files_raw<-files
  files<-tolower(files)
  articles$doi<-tolower(articles$doi)
  articles$pdf<-""
  for (i in c(1:nrow(articles))){
    print(i)
    f<-files_raw[grep(gsub("10.1111/", "", articles[i]$doi), files)]
    if (length(f)>=1){
      if (length(f)>1){
        asdf
      }
      articles[i]$pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/MEE/%s", f[1])
    }
  }
  articles[pdf!=""]
  saveRDS(articles, "../Data/MEE/mee.rda")
}


if (F){
  articles$github<-""
  articles$double_check<-F
  articles$with_github<-F
  for (i in c(1:nrow(articles))){
    print(i)
    if (articles[i]$github!=""){
      next()
    }
    pdf<-articles[i]$pdf
    if (file.exists(pdf)){
      
      text<-pdf_text(pdf)
      text<-unlist(text)
      text<-paste(text, collapse = '')
      
      
      text<-gsub(" ", "", text)
      if (grepl("AcceptedArticle", text)){
        articles[i]$double_check<-T
      }
      text<-gsub("\n", "", text)
      articles[i]$with_github<-grepl("github", text)
      pattern <- "([\\w.-]*github[\\w.-]*|github[\\w.-]*)(/[\\w/?.&=]*)?"
      github_links <- str_extract_all(text, pattern)[[1]]
      
      if (length(github_links)>0){
        github_links<-github_links[github_links!="https://github.com/"]
        github_links<-unique(github_links)
        
        if (length(github_links)>1){
          articles[i]$github<-github_links[1]
          articles[i]$double_check<-T
          
        }else{
          articles[i]$github<-github_links
        }
        
      }
    }
  }
  saveRDS(articles, "../Data/MEE/mee.rda")
}

if (F){
  fff<-readRDS("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_Full/references.rda")
  aaa<-readRDS("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_Full/articles.rda")
  articles<-readRDS("../Data/MEE/mee.rda")
  ref<-list()
  for (i in c(1:length(fff))){
    print(paste(i, length(fff)))
    item<-fff[[i]]
    item<-item[ref_DOI %in% articles$doi]
    if (nrow(item)>0){
      ref[[length(ref)+1]]<-item
    }
  }
  ref_df<-rbindlist(ref)
  saveRDS(ref_df, "../Data/MEE/mee_references.rda")
  
  
  text <- "Visit https://github.com/user/repo or https://www.github.com/user/repo or http://github.io/user/repo?query=1."
  matches <- regmatches(text, gregexpr("https?://(?:[a-zA-Z0-9-]+\\.)*github\\.[a-z]{2,}(?:/[^\\s]*)?", text))
  print(matches)
  
}
