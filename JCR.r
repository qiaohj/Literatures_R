library(data.table)
setwd("/media/huijieqiao/WD22T1/pubmed/Script")
journals<-fread("../Data/JCR/wos-core_SCIE 2023-August-21.csv")
year<-2023
unique(journals$Languages)
unique(journals$`Web of Science Categories`)
colnames(journals)<-c("Title", "ISSN", "eISSN", "Publisher", "Publisher_address", "Language", "Category")
journal_list<-list()
i=1
for (i in c(1:nrow(journals))){
  print(paste(i, nrow(journals)))
  item<-journals[i]
  Language<-trimws(strsplit(item$Language, ",")[[1]])
  if (length(Language)==0){
    Language<-""
  }
  Category<-trimws(strsplit(item$Category, "\\|")[[1]])
  items<-expand.grid(Language=Language, Category=Category)
  items$Title<-item$Title
  items$ISSN<-item$ISSN
  items$eISSN<-item$eISSN
  items$Publisher<-item$Publisher
  items$Publisher_address<-item$Publisher_address
  items$Year<-year
  journal_list[[length(journal_list)+1]]<-items
}
journal_df<-rbindlist(journal_list)
journal_df[Title=="METHODS IN ECOLOGY AND EVOLUTION"]
saveRDS(journal_df, "../Data/JCR/journals.rda")

