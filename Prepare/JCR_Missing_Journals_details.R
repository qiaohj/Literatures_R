library(R.utils)
library(rjson)
library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")

jcr_journals<-readRDS("../Data/missing_journals.rda")
Categories<-"missing_journals_ecology_biodiversity"
i=1
j=1
for (i in c(1:length(Categories))){
  journals_category<-jcr_journals
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
