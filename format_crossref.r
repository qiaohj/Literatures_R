library(R.utils)
library(rjson)
library(data.table)
setwd("/media/huijieqiao/WD22T1/pubmed/Script")


getValue<-function(node, key, type="character"){
  if (is.null(node)){
    v<-NA
  }else{
    if (key %in% names(node)){
      v<-node[[key]]
      if (type=="datetime"){
        if (class(v)=="list"){
          v<-NA
        }else{
          v<-as.POSIXct(v, "UTC", "%Y-%m-%dT%H:%M:%S")
        }
      }
      if (type=="numeric"){
        v<-as.numeric(v)
      }
      if (type=="datetime_array"){
        v<-v[[1]] 
        year<-v[1]
        month<-v[2]
        if (length(v)==3){
          day<-v[3]
        }else{
          day<-1
        }
        v<-as.Date(sprintf("%d-%d-%d", year, month, day),
                   format="%Y-%m-%d")
      }
    }else{
      v<-NA
    }
  }
  if (is.null(v)){
    v<-NA
  }
  #print(v)
  if (length(v)==1){
    if (is.na(v)){
      if (type=="datetime_array"){
        v<-as.Date(NA)
      }
      if (type=="numeric"){
        v<-as.numeric(NA)
      }
      if (type=="datetime"){
        v<-as.POSIXct(NA)
      }
    }
  }
  v
}

source_folder<-"../RAW/April 2023 Public Data File from Crossref/"

zips<-list.files(source_folder, pattern = "\\.gz")
zips<-zips[sample(length(zips), length(zips))]
i=6363
for (i in c(1:length(zips))){
  f<-zips[i]
  
  print(paste(i, length(zips), f))
  name<-gsub("\\.json\\.gz", "", f)
  target<-sprintf("../Data/datatable_crossref/%s.rda", name)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  xml<-sprintf("../Data/json_crossref/%s.json", name)
  if (!file.exists(xml)){
    gunzip(sprintf("%s/%s", source_folder, f), remove=FALSE,
           destname = xml)
  }
  result <- fromJSON(file = xml)
  
  article_list<-list()
  journal_issn<-list()
  article_subject<-list()
  author_list<-list()
  reference_list<-list()
  for (j in c(1:length(result$items))){
    if (j %% 1000 ==0){
      print(paste(i, length(zips), f, j, length(result$items)))
    }
    item<-result$items[[j]]
    doi<-getValue(item, "DOI")
    url<-getValue(item, "URL")
    abstract<-getValue(item, "abstract")
    resource_primary_url<-getValue(item$resource$primary, "URL")
    created_date<-getValue(item$created, "date-time", type="datetime")
    issn<-paste(getValue(item, "ISSN"), collapse ="|")
    container_title<-paste(getValue(item, "container-title"), collapse ="|")
    journal_issn[[length(journal_issn)+1]]<-data.table(expand.grid(article_DOI=doi,
                                                                   Title=getValue(item, "container-title"), 
                                                                   ISSN=getValue(item, "ISSN")))
    if (class(item$issue)=="character"){
      issue<-getValue(item, "issue")
    }else{
      issue<-getValue(item$issue, "date-parts")
      issue<-as.character(issue)
    }
    reference_count<-getValue(item, "reference-count", type="numeric")
    indexed<-getValue(item$indexed, "date-time", type="datetime")
    deposited<-getValue(item$deposited, "date-time", type="datetime")
    language<-getValue(item, "language")
    page<-getValue(item, "page")
    
    published<-getValue(item$published, "date-parts", type="datetime_array")
    published_print<-getValue(item$`published-print`, "date-parts", type="datetime_array")
    published_online<-getValue(item$`published-online`, "date-parts", type="datetime_array")
    published_others<-getValue(item$`published-others`, "date-parts", type="datetime_array")
    publisher<-getValue(item, "publisher")
    subject<-paste(getValue(item, "subject"), collapse = "|")
    article_subject[[length(article_subject)+1]]<-data.table(article_DOI=doi, 
                                                             Subject=getValue(item, "subject"))
    title<-getValue(item, "title")
    type<-getValue(item, "type")
    
    volume<-getValue(item, "volume")
    
    article_list[[length(article_list)+1]]<-data.frame(doi=doi, issue=issue, volume=volume, 
                                                       page=page, language=language,
                                                       issn=issn, container_title=container_title, 
                                                       reference_count=reference_count,
                                                       type=type, subject=subject,
                                                       title=title, abstract=abstract,
                                                       url=url, resource_primary_url=resource_primary_url,
                                                       created_date=created_date, 
                                                       indexed=indexed,
                                                       deposited=deposited, published=published,
                                                       published_print=published_print, 
                                                       published_online=published_online,
                                                       published_others=published_others, 
                                                       publisher=publisher)
    references<-item$reference
    authors<-item$author
    k=1
    for (k in c(1:length(authors))){
      author_item<-authors[[k]]
      ORCID<-getValue(author_item, "ORCID")
      authenticated_orcid<-getValue(author_item, "authenticated-orcid")
      given<-getValue(author_item, "given")
      family<-getValue(author_item, "family")
      sequence<-getValue(author_item, "sequence")
      
      if (length(author_item$affiliation)>0){
        for (l in c(1:length(author_item$affiliation))){
          author_list[[length(author_list)+1]]<-data.frame(
            article_DOI=doi,
            ORCID=ORCID,
            authenticated_orcid=authenticated_orcid,
            Given=given, Family=family, sequence=sequence,
            Sort=k,
            affiliation=getValue(author_item$affiliation[[l]], "name"),
            affiliation_sort=l)
        }
      }else{
        author_list[[length(author_list)+1]]<-data.frame(article_DOI=doi,
                                                         ORCID=ORCID,
                                                         authenticated_orcid=authenticated_orcid,
                                                         Given=given, Family=family, sequence=sequence,
                                                         Sort=k,
                                                         affiliation="",
                                                         affiliation_sort=1)
      }
    }
    for (k in c(1:length(references))){
      reference_item<-references[[k]]
      key<-getValue(reference_item, "key")
      doi_asserted_by<-getValue(reference_item, "doi-asserted-by")
      DOI<-getValue(reference_item, "DOI")
      unstructured<-getValue(reference_item, "unstructured")
      volume_title<-getValue(reference_item, "volume-title")
      author<-getValue(reference_item, "author")
      
      year<-getValue(reference_item, "year")
      reference_list[[length(reference_list)+1]]<-data.frame(article_DOI=doi,
                                                             key=key, doi_asserted_by=doi_asserted_by, 
                                                             ref_DOI=DOI,
                                                             volume_title=volume_title, 
                                                             author=author, year=year,
                                                             unstructured=unstructured)
    }
    
  }
  article_df<-rbindlist(article_list)
  journal_df<-rbindlist(journal_issn)
  article_subject_df<-rbindlist(article_subject)
  author_df<-rbindlist(author_list)
  reference_df<-rbindlist(reference_list)
  df<-list(articles=article_df,
           journals=journal_df,
           article_subject=article_subject_df,
           authors=author_df,
           references=reference_df)
  saveRDS(df, target)
}
if (F){
  library(data.table)
  setwd("/media/huijieqiao/WD22T1/pubmed/Script")
  files<-list.files("../Data/datatable_crossref", pattern="\\.rda")
  article_list<-list()
  journal_issn<-list()
  article_subject<-list()
  author_list<-list()
  reference_list<-list()
  for (i in c(1:length(files))){
    f<-files[i]
    print(paste(i, length(files), f))
    df<-readRDS(sprintf("../Data/datatable_crossref/%s", f))
    df$articles$File<-f
    df$journals$File<-f
    df$article_subject$File<-f
    df$authors$File<-f
    df$references$File<-f
    
    
    article_list[[length(article_list)+1]]<-df$articles
    journal_issn[[length(journal_issn)+1]]<-df$journals
    article_subject[[length(article_subject)+1]]<-df$article_subject
    author_list[[length(author_list)+1]]<-df$authors
    reference_list[[length(reference_list)+1]]<-df$references
  }
  article_df<-rbindlist(article_list)
  saveRDS(article_df, "../Data/CrossRef_Full/articles.rda")
  journal_df<-rbindlist(journal_issn)
  saveRDS(journal_df, "../Data/CrossRef_Full/journals.rda")
  article_subject_df<-rbindlist(article_subject)
  saveRDS(article_subject_df, "../Data/CrossRef_Full/article_subject.rda")
  author_df<-rbindlist(author_list)
  saveRDS(author_df, "../Data/CrossRef_Full/authors.rda")
  #reference_df<-rbindlist(reference_list)
  saveRDS(reference_list, "../Data/CrossRef_Full/references.rda")
}
if (F){
  library(data.table)
  setwd("/media/huijieqiao/WD22T1/pubmed/Script")
  article_df<-readRDS("../Data/CrossRef_Full/articles.rda")
  journal_df<-readRDS("../Data/CrossRef_Full/journals.rda")
  article_subject_df<-readRDS("../Data/CrossRef_Full/article_subject.rda")
  author_df<-readRDS("../Data/CrossRef_Full/authors.rda")
  
  journal_df$ISSN<-as.character(journal_df$ISSN)
  journal_df$Title<-as.character(journal_df$Title)
  journal_df$ISSN<-as.character(journal_df$ISSN)
  journal_df$article_DOI<-as.character(journal_df$article_DOI)
  
  journals<-unique(journal_df[, c("Title", "ISSN")])
  journals<-journals[!is.na(ISSN)]
  journal$ISSN<-as.character(journal$ISSN)
  journals<-journals[sample(nrow(journals), nrow(journals))]
  for (i in c(1:nrow(journals))){
    
    journal<-journals[i]
    
    print(paste(i, nrow(journals), journal$Title))
    
    ISSNs<-strsplit(as.character(journal$ISSN), "-")[[1]]
    target_folder<-sprintf("../Data/CrossRef_By_Journal/%s", ISSNs[1])
    if (!dir.exists(target_folder)){
      dir.create(target_folder)
    }
    target<-sprintf("%s/%s", target_folder, journal$ISSN)
    if (dir.exists(target)){
      next()
    }
    dir.create(target)
    
    journal_item<-journal_df[ISSN==journal$ISSN]
    article_item<-article_df[doi %in% journal_item$article_DOI]
    article_subject_item<-article_subject_df[article_DOI %in% journal_item$article_DOI]
    author_item<-author_df[article_DOI %in% journal_item$article_DOI]
    saveRDS(journal_item, sprintf("%s/journals.rda", target))
    saveRDS(article_item, sprintf("%s/articles.rda", target))
    saveRDS(article_subject_item, sprintf("%s/article_subject.rda", target))
    saveRDS(author_item, sprintf("%s/authors.rda", target))
  }
}

if (F){
  library(R.utils)
  library(rjson)
  library(data.table)
  setwd("/media/huijieqiao/WD22T1/pubmed/Script")
  
  jcr_journals<-readRDS("../Data/JCR/journals.rda")
  Categories<-unique(jcr_journals$Category)
  for (i in c(1:length(Categories))){
    journals_category<-jcr_journals[Category==Categories[i]]
    target<-sprintf("../Data/CrossRef_By_Category/%s", Categories[i])
    if (dir.exists(target)){
      next()
    }
    dir.create(target)
    article_list<-list()
    journal_issn<-list()
    article_subject<-list()
    author_list<-list()
    for (j in c(1:nrow(journals_category))){
      jcr_item<-journals_category[j]
      print(paste(i, length(Categories), Categories[i], j, nrow(journals_category), jcr_item$ISSN, jcr_item$eISSN))
      ISSNs<-strsplit(as.character(jcr_item$ISSN), "-")[[1]]
      source_folder<-sprintf("../Data/CrossRef_By_Journal/%s", ISSNs[1])
      source<-sprintf("%s/%s", source_folder, jcr_item$ISSN)
      rda<-sprintf("%s/articles.rda", source)
      if (file.exists(rda)){
        article_df<-readRDS(rda)
        journal_df<-readRDS(sprintf("%s/journals.rda", source))
        article_subject_df<-readRDS(sprintf("%s/article_subject.rda", source))
        author_df<-readRDS(sprintf("%s/authors.rda", source))
        article_df$ISSN_Type<-"print"
        article_list[[length(article_list)+1]]<-article_df
        journal_issn[[length(journal_issn)+1]]<-journal_df
        article_subject[[length(article_subject)+1]]<-article_subject_df
        author_list[[length(author_list)+1]]<-author_df
      }
      if ((jcr_item$ISSN !=jcr_item$eISSN)&(jcr_item$eISSN!="")){
        eISSNs<-strsplit(as.character(jcr_item$eISSN), "-")[[1]]
        source_folder<-sprintf("../Data/CrossRef_By_Journal/%s", eISSNs[1])
        source<-sprintf("%s/%s", source_folder, jcr_item$eISSN)
        rda<-sprintf("%s/articles.rda", source)
        if (file.exists(rda)){
          article_df<-readRDS(rda)
          journal_df<-readRDS(sprintf("%s/journals.rda", source))
          article_subject_df<-readRDS(sprintf("%s/article_subject.rda", source))
          author_df<-readRDS(sprintf("%s/authors.rda", source))
          article_df$ISSN_Type<-"electronic"
          article_list[[length(article_list)+1]]<-article_df
          journal_issn[[length(journal_issn)+1]]<-journal_df
          article_subject[[length(article_subject)+1]]<-article_subject_df
          author_list[[length(author_list)+1]]<-author_df
        }
      }
    }
    article_df<-rbindlist(article_list)
    saveRDS(article_df, sprintf("%s/articles.rda", target))
    journal_df<-rbindlist(journal_issn)
    saveRDS(journal_df, sprintf("%s/journals.rda", target))
    article_subject_df<-rbindlist(article_subject)
    saveRDS(article_subject_df, sprintf("%s/article_subject.rda", target))
    author_df<-rbindlist(author_list)
    saveRDS(author_df, sprintf("%s/authors.rda", target))
    
    
  }
  
}

if (F){
  category<-"Ecology"
  article_df<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/articles.rda", category))
  article_df$Year<-format(article_df$published, "%Y")
  article_se<-article_df[, .(N=.N), by=list(container_title, type)]
  length(unique(article_se$container_title))
  library(ggplot2)
  ggplot(article_se[type=="journal-article"])+
    geom_histogram(aes(x=N), bins=20)+
    scale_x_log10()
  article_year<-article_df[, .(N=.N), by=list(Year)]
  ggplot(article_year)+geom_point(aes(x=Year, y=N))
}
