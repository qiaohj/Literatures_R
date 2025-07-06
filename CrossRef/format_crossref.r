library(R.utils)
library(rjson)
library(data.table)
library(stringi)


setwd("/media/huijieqiao/WD22T_11/literatures/Script")


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
        if (is.null(v)){
          v<-NA
        }
        year<-v[1]
        month<-ifelse(is.na(v[2]), 1, v[2])
        day<-ifelse(is.na(v[3]), 1, v[3])
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

if (F){
  #compare the cross refs
  source_folder_2023<-"../RAW/April 2023 Public Data File from Crossref/"
  
  zips_2023<-list.files(source_folder_2023, pattern = "\\.gz")
  
  source_folder_2024<-"../RAW/April 2024 Public Data File from Crossref/"
  
  zips_2024<-list.files(source_folder_2024, pattern = "\\.gz")
  
  filesize_2023<-file.size(sprintf("%s/%s", source_folder_2023, zips_2023))
  filesize_2024<-file.size(sprintf("%s/%s", source_folder_2024, zips_2024))
  
  file_2023<-data.table(zips=zips_2023, size=filesize_2023, year=2023)
  file_2024<-data.table(zips=zips_2024, size=filesize_2024, year=2024)
  file_match<-merge(file_2023, file_2024, by=c("zips", "size"), all=T)
  file_match[is.na(year.x)]
  file_2023[zips=="0.json.gz"]
  file_2024[zips=="0.json.gz"]
}

source_folder<-"../RAW/March 2025 Public Data File from Crossref/"

zips<-list.files(source_folder, pattern = "\\.gz")
#zips<-zips[sample(length(zips), length(zips))]
i=1

#match("9883.json.gz", zips)
year.crossref<-2025
zips<-zips[sample(length(zips), length(zips))]
for (i in c(1:length(zips))){
  f<-zips[i]
  
  print(paste(i, length(zips), f))
  name<-gsub("\\.jsonl\\.gz", "", f)
  target<-sprintf("../Data/datatable_crossref/%d/%s.rda", year.crossref, name)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  xml<-sprintf("../Data/json_crossref/%d/%s.json", year.crossref, name)
  if (!file.exists(xml)){
    gunzip(sprintf("%s/%s", source_folder, f), remove=FALSE,
           destname = xml)
  }
  lines <- readLines(xml)
  results <- lapply(lines, fromJSON)
  
  
  article_list<-list()
  journal_issn<-list()
  article_subject<-list()
  author_list<-list()
  reference_list<-list()
  for (j in c(1:length(results))){
    if (j %% 1000 ==0){
      print(paste(i, length(zips), f, j, length(results)))
    }
    item<-results[[j]]
    doi<-getValue(item, "DOI")
    url<-getValue(item, "URL")
    abstract<-getValue(item, "abstract")
    alternative.id<-paste(getValue(item, "alternative-id"), collapse ="|")
    resource_primary_url<-getValue(item$resource$primary, "URL")
    created_date<-getValue(item$created, "date-time", type="datetime")
    issn<-paste(getValue(item, "ISSN"), collapse ="|")
    issn_type<-paste(getValue(item, "issn-type"), collapse ="|")
    container_title<-paste(getValue(item, "container-title"), collapse ="|")
    short_container_title<-paste(getValue(item, "short-container-title"), collapse ="|")
    content_domain<-paste(getValue(item, "content-domain"), collapse ="|")
    prefix<-getValue(item, "prefix")
    result <- tryCatch({
      journal_issn[[length(journal_issn)+1]]<-data.table(article_DOI=doi,
                                                         Title=container_title,
                                                         short_container_title=short_container_title,
                                                         content_domain=content_domain,
                                                         ISSN=issn,
                                                         prefix=prefix,
                                                         stringsAsFactors = F)
    }, warning = function(w) {
      cat("A warning was caught by tryCatch and is being promoted to an error.\n")
      stop(w)
    })
    
    if (class(item$issue)=="character"){
      issue<-getValue(item, "issue")
    }else{
      issue<-paste(item$issue, collapse = "|")
    }
    issued<-paste(getValue(item, "issued"), collapse ="|")
    journal_issue<-paste(getValue(item, "journal-issue"), collapse ="|")
    reference_count<-getValue(item, "reference-count", type="numeric")
    is_referenced_by_count<-getValue(item, "is-referenced-by-count", type="numeric")
    indexed<-getValue(item$indexed, "date-time", type="datetime")
    deposited<-getValue(item$deposited, "date-time", type="datetime")
    language<-getValue(item, "language")
    license<-paste(getValue(item, "license"), collapse = "|")
    page<-getValue(item, "page")
    member<-getValue(item, "member")
    score<-getValue(item, "score")
    
    published<-getValue(item$published, "date-parts", type="datetime_array")
    published_print<-getValue(item$`published-print`, "date-parts", type="datetime_array")
    published_online<-getValue(item$`published-online`, "date-parts", type="datetime_array")
    published_others<-getValue(item$`published-others`, "date-parts", type="datetime_array")
    publisher<-getValue(item, "publisher")
    
    title<-getValue(item, "title")
    type<-getValue(item, "type")
    source<-getValue(item, "source")
    volume<-getValue(item, "volume")
    result <- tryCatch({
      
      article_list[[length(article_list)+1]]<-data.table(doi=doi, issue=issue, volume=volume, 
                                                         page=page, language=language, member=member,
                                                         score=score,
                                                         issn=issn, container_title=container_title, 
                                                         reference_count=reference_count,
                                                         is_referenced_by_count=is_referenced_by_count,
                                                         type=type, license=license, source=source,
                                                         alternative.id=alternative.id,
                                                         title=title, abstract=abstract,
                                                         url=url, resource_primary_url=resource_primary_url,
                                                         created_date=created_date, 
                                                         indexed=indexed,
                                                         deposited=deposited, published=published,
                                                         published_print=published_print, 
                                                         published_online=published_online,
                                                         published_others=published_others, 
                                                         publisher=publisher,
                                                         stringsAsFactors = F)
    }, warning = function(w) {
      cat("A warning was caught by tryCatch and is being promoted to an error.\n")
      stop(w)
    })
    
    authors<-item$author
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
    references<-item$reference
    for (k in c(1:length(references))){
      reference_item<-references[[k]]
      key<-getValue(reference_item, "key")
      doi_asserted_by<-getValue(reference_item, "doi-asserted-by")
      DOI<-getValue(reference_item, "DOI")
      unstructured<-getValue(reference_item, "unstructured")
      volume<-getValue(reference_item, "volume")
      author<-getValue(reference_item, "author")
      article_title<-getValue(reference_item, "article-title")
      first_page<-getValue(reference_item, "first-page")
      journal_title<-getValue(reference_item, "journal-title")
      year<-getValue(reference_item, "year")
      reference_list[[length(reference_list)+1]]<-data.frame(article_DOI=doi,
                                                             key=key, doi_asserted_by=doi_asserted_by, 
                                                             ref_DOI=DOI,
                                                             first_page=first_page,
                                                             volume=volume, 
                                                             author=author, year=year,
                                                             unstructured=unstructured,
                                                             article_title=article_title,
                                                             journal_title=journal_title)
    }
    
  }
  article_df<-rbindlist(article_list, use.names = T, fill=T)
  journal_df<-rbindlist(journal_issn, use.names = T, fill=T)
  author_df<-rbindlist(author_list, use.names = T, fill=T)
  reference_df<-rbindlist(reference_list, use.names = T, fill=T)
  
  journal_df[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(article_DOI, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  journal_df[, c("doi.key", "doi.id") := {
    parts <- stri_split_fixed(doi.suffix, ".", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  
  df<-list(articles=article_df,
           journals=journal_df,
           authors=author_df,
           references=reference_df)
  saveRDS(df, target)
}
if (F){
  yyy<-2025
  library(data.table)
  setwd("/media/huijieqiao/WD22T_11/literatures/Script")
  files<-list.files(sprintf("../Data/datatable_crossref/%d/", yyy), pattern="\\.rda")
  article_list<-list()
  journal_issn<-list()
  author_list<-list()
  reference_list<-list()
  for (i in c(1:length(files))){
    f<-files[i]
    print(paste(i, length(files), f))
    df<-readRDS(sprintf("../Data/datatable_crossref/%d/%s", yyy, f))
    df$articles$File<-f
    df$journals$File<-f
    df$authors$File<-f
    df$references$File<-f
    
    
    article_list[[length(article_list)+1]]<-df$articles
    journal_issn[[length(journal_issn)+1]]<-df$journals
    author_list[[length(author_list)+1]]<-df$authors
    reference_list[[length(reference_list)+1]]<-df$references
  }
  article_df<-rbindlist(article_list)
  saveRDS(article_df, sprintf("../Data/CrossRef_Full/%d/articles.rda", yyy))
  journal_df<-rbindlist(journal_issn)
  saveRDS(journal_df, sprintf("../Data/CrossRef_Full/%d/journals.rda", yyy))
  author_df<-rbindlist(author_list)
  saveRDS(author_df, sprintf("../Data/CrossRef_Full/%d/authors.rda", yyy))
  #reference_df<-rbindlist(reference_list)
  saveRDS(reference_list, sprintf("../Data/CrossRef_Full/%d/references.rda", yyy))
}
if (F){
  yyy<-2025
  library(data.table)
  setwd("/media/huijieqiao/WD22T_11/literatures/Script")
  print("reading articles")
  article_df<-readRDS(sprintf("../Data/CrossRef_Full/%d/articles.rda", yyy))
  print("reading journals.rda")
  journal_df<-readRDS(sprintf("../Data/CrossRef_Full/%d/journals.rda", yyy))
  #print("reading article_subject.rda")
  #article_subject_df<-readRDS(sprintf("../Data/CrossRef_Full/article_subject.rda", yyy))
  print("reading authors.rda")
  author_df<-readRDS(sprintf("../Data/CrossRef_Full/%d/authors.rda", yyy))
  
  journal_df$ISSN<-as.character(journal_df$ISSN)
  journal_df$Title<-as.character(journal_df$Title)
  journal_df$ISSN<-as.character(journal_df$ISSN)
  journal_df$article_DOI<-as.character(journal_df$article_DOI)
  
  journals<-unique(journal_df[, c("Title", "ISSN")])
  journals<-journals[!is.na(ISSN)]
  journals$ISSN<-as.character(journals$ISSN)
  journals<-journals[sample(nrow(journals), nrow(journals))]
  for (i in c(1:nrow(journals))){
    
    journal<-journals[i]
    
    print(paste(i, nrow(journals), journal$Title))
    
    ISSNs<-strsplit(as.character(journal$ISSN), "-")[[1]]
    target_folder<-sprintf("../Data/CrossRef_By_Journal/%d/%s", yyy, ISSNs[1])
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
    #article_subject_item<-article_subject_df[article_DOI %in% journal_item$article_DOI]
    author_item<-author_df[article_DOI %in% journal_item$article_DOI]
    saveRDS(journal_item, sprintf("%s/journals.rda", target))
    saveRDS(article_item, sprintf("%s/articles.rda", target))
    #saveRDS(article_subject_item, sprintf("%s/article_subject.rda", target))
    saveRDS(author_item, sprintf("%s/authors.rda", target))
  }
}

if (F){
  library(R.utils)
  library(rjson)
  library(data.table)
  setwd("/media/huijieqiao/WD22T_11/literatures/Script")
  
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
if (F){
  #28700.rda
  df_2023<-readRDS("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_2023/datatable_crossref/1.rda")
  df_2024<-readRDS("/media/huijieqiao/WD22T_11/literatures/Data/datatable_crossref/1.rda")
  
  tail(df_2023$articles$doi)
  tail(df_2024$articles$doi)
}