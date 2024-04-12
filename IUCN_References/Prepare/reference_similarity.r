library(rcrossref)
library(data.table)
library(fedmatch)
library(stringdist)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(R.oo)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
source("functions.r")
group<-"Mammals"

if (F){
  
  groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")
  references_list<-list()
  start<-1
  for (group in groups){
    references_df<-readRDS(sprintf("../Data_IUCN_References/References/references_%s_cleaned.rda", group))
    references_df$ID<-c(start:(start+nrow(references_df)-1))
    start<-start+nrow(references_df)
    saveRDS(references_df, sprintf("../Data_IUCN_References/References/references_%s_cleaned.rda", group))
    references_list[[group]]<-references_df
  }
  references_list<-rbindlist(references_list)
  saveRDS(references_list, "../Data_IUCN_References/References/references_full_cleaned.rda")
  
  all_journals<-readRDS("../Data/CrossRef_Full/journals_articles.rda")
  all_journals<-unique(all_journals[, c("Title", "ISSN")])
  saveRDS(all_journals, "../Data/CrossRef_Full/journals.rda")
  
  references_list<-readRDS("../Data_IUCN_References/References/references_full_cleaned.rda")
  references<-unique(references_list[, c("reference", "reference_year", "reference_title", 
                                         "reference_container", "reference_rest",
                                         "reference_doi",
                                         "is_IUCN", "year_score", "reference_yearstr", "leng",
                                         "reference_clean", "reference_rest_clean", 
                                         "reference_title_clean", 
                                         "reference_container_clean",
                                         "reference_doi_clean")])
  
  saveRDS(references, "../Data_IUCN_References/References/references_only_cleaned.rda")
}
references<-readRDS("../Data_IUCN_References/References/references_only_cleaned.rda")
references[is_IUCN==T]
references_sub<-references[is_IUCN==F]
references_sub$LID<-paste("label", c(1:nrow(references_sub)), sep="_")
references_sub$reference_title_clean_length<-str_length(references_sub$reference_title_clean)
df<-references_sub
col<-"reference_doi_clean"
get_simi<-function(df, col, min_simil=0.8){
  labels<-df$LID
  items<-df[[col]]
  labels<-labels[!is.na(items)]
  items<-items[!is.na(items)]
  dfmat <- items%>%
    tokens(remove_punct = TRUE) %>%
    tokens_remove(stopwords("english")) %>%
    dfm()
  rownames(dfmat)<-labels
  tstat2 <- textstat_simil(dfmat, method = "cosine", margin = "documents", min_simil = min_simil)
  simil<-as.data.frame(tstat2, upper=F)
  simi_df<-data.table(simil)
  simi_df$document1<-as.character(simi_df$document1)
  simi_df$document2<-as.character(simi_df$document2)
  simi_df$type<-col
  simi_df$min_simil<-min_simil
  simi_df$label<-paste(simi_df$document1, simi_df$document2, sep="-")
  simi_df
}

simi_doi<-get_simi(references_sub, "reference_doi_clean", min_simil=0.9)
simi_title<-get_simi(references_sub, "reference_title_clean", min_simil=0.95)
simi_container<-get_simi(references_sub, "reference_container_clean", min_simil=0.95)
simi_rest<-get_simi(references_sub, "reference_rest_clean", min_simil=0.9)
simi_full<-get_simi(references_sub, "reference_clean", min_simil=0.90)
setorderv(simi_full, "cosine", 1)

all_labels<-unique(c(simi_full$document1, simi_full$document2))
result<-data.table(ID=all_labels, group=0)

group_label<-1
while(T){
  item<-result[group==0]
  if (nrow(item)==0){
    break()
  }
  item<-item[1]
  group_items<-simi_full[document1 %in% item$ID | document2 %in% item$ID]
  all_IDs<-unique(c(group_items$document1, group_items$document2))
  groups<-result[ID %in% all_IDs & group!=0]
  if (nrow(groups)==0){
    result[ID %in% all_IDs]$group<-group_label
    group_label<-group_label+1
  }else{
    group_id<-unique(groups$group)[1]
    result[ID %in% all_IDs]$group<-group_id
  }
  
}
xxx<-result[, .(N=.N), by=group]
simi_full_x<-merge(result, references_sub[, c("LID", "reference_clean")], 
                   by.x="ID", by.y="LID", all.x=T)


simi_full_x[is.na(reference_clean.y)]

simi_full_x<-unique(simi_full_x[, c("group", "reference_clean")])

saveRDS(simi_full_x, "../Data_IUCN_References/References/duplicated_reference.rda")

View(references_sub[LID %in% c("label_63207", "label_64018")])

if (F){
  simi_doi<-simi_doi[!(label %in% simi_full$label)]
  simi_title<-simi_title[!(label %in% simi_full$label)]
  
  simi_title_x<-merge(simi_title, references_sub[, c("ID", "reference_year", "reference_doi_clean", "reference_title_clean_length")], by.x="document1", by.y="ID", all.x=T)
  simi_title_x<-merge(simi_title_x, references_sub[, c("ID", "reference_year", "reference_doi_clean", "reference_title_clean_length")], by.x="document2", by.y="ID", all.x=T)
  setorderv(simi_title_x, "reference_title_clean_length.x")
  hist(simi_title_x$reference_title_clean_length.x)
  simi_title_x[reference_title_clean_length.x>50]
  
  get_simi(references_sub[ID %in% c("label_75941", "label_75867")], "reference_clean", min_simil=0.50)
  
  references_sub[ID %in% c("label_75941", "label_75867")]$reference_clean
  
  View(references_sub[ID %in% c("label_75941", "label_75867")])
  
  
  
  
  
  
  
  simi_title[(document1=="label_78831" | document2=="label_78831")]
  
  simi_title_x<-merge(simi_title, references_sub[, c("ID", "reference_year", "reference_doi_clean", "reference_title_clean_length")], by.x="document1", by.y="ID", all.x=T)
  simi_title_x<-merge(simi_title_x, references_sub[, c("ID", "reference_year", "reference_doi_clean", "reference_title_clean_length")], by.x="document2", by.y="ID", all.x=T)
  simi_title_x[]
  
  simi[(document1=="label_78831" | document2=="label_78831") & (type=="reference_title_clean")]
  View(references_sub[ID %in% c("label_92516", "label_92722")])
  
  simi_title[!(document1 %in% c(simi_full$document1, simi_title$document2))]
  simi<-rbindlist(list(simi_doi, simi_title, simi_full))
  
  simi$label<-paste(simi$document1, simi$document2, sep="-")
  
  simi<-merge(simi, references_sub[, c("ID", "reference_year", "reference_doi_clean", "reference_title_clean_length")], by.x="document1", by.y="ID", all.x=T)
  simi<-merge(simi, references_sub[, c("ID", "reference_year", "reference_doi_clean", "reference_title_clean_length")], by.x="document2", by.y="ID", all.x=T)
  
  
  sim_score<-simi[, .(score=mean(cosine),
                      N=.N), by=list(document1, document2,
                                     reference_year.x,
                                     reference_doi_clean.x,
                                     reference_year.y,
                                     reference_doi_clean.y)]
  
  xxx<-data.table(table(c(sim_score$document1, sim_score$document2)))
  xxx[N==max(xxx$N)]
  
  setorderv(sim_score, "score", -1)
  
  
  View(references_sub[reference_container %in% c("Herpetological Review")])
  
  container_removed<-c("The Atlas of European Mammals")
  title_removed<-c("geographic distribution")
  
  removed_labels<-references_sub[reference_container %in% container_removed | reference_title_clean %in% title_removed]$LID
  
  sim_score<-sim_score[!((document1 %in% removed_labels) | (document2 %in% removed_labels))]
  
  sim_score[reference_doi_clean.x==reference_doi_clean.y & reference_year.x!=reference_year.y]
  sim_score[is.na(reference_year.x)]
  sim_score[is.na(reference_year.y)]
  
  xxx<-simi[reference_year.x!=reference_year.y & type=="reference_clean"]
  xxx$year_differ<-abs(xxx$reference_year.y - xxx$reference_year.x)
  xxx[year_differ==max(xxx$year_differ)]
  xxx[year_differ==16]
  
  table(simi[reference_year.x!=reference_year.y]$type)
  
  
  
  trimws(xxx[1]$reference)==trimws(xxx[2]$reference)
  simi[document1=="label_71827" & document2=="label_71887"]
  simi[document1=="label_34514" | document2=="label_37421"]
  
}