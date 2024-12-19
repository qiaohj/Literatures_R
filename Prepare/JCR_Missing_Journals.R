library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
if (F){
  #check the data
  for (y in c(1997:2022)){
    journals_jcr<-fread(sprintf("../Data/JCR/JCR1997-2022/JCR_JournalResults_%d.csv", y),
                        skip=2)
    if (!grepl(y, colnames(journals_jcr)[8])){
      print(paste("Error in year ", y))
    }
  } 
}
journals<-readRDS("../Data/JCR/journals.rda")
setorderv(journals, "ISSN")
journals[ISSN!=""]
journals[eISSN=="2639-5916"]
issns<-c(journals$ISSN, journals$eISSN)
issns<-unique(issns)
issns<-issns[issns!=""]
y=2022
missing_journals<-list()
for (y in c(1997:2022)){
  journals_jcr<-fread(sprintf("../Data/JCR/JCR1997-2022/JCR_JournalResults_%d.csv", y),
                      skip=3)
  colnames(journals_jcr)<-c("Journal", "Abbreviation",
                             "ISSN", "eISSN", "Category",
                             "Total_Citations", "JIF",
                             "JIF_Quartile", "JCI", "OA_Gold", "V11")
  
  missing_journal<-journals_jcr[!(ISSN %in% issns) & !(eISSN %in% issns)]
  missing_journal<-missing_journal[, c("Journal", "ISSN", "eISSN")]
  missing_journal<-unique(missing_journal)
  missing_journal$Missing_Year<-y
  missing_journals[[length(missing_journals)+1]]<-missing_journal
}
missing_journals<-rbindlist(missing_journals)

missing_journals<-missing_journals[, .(Missing_Year=max(Missing_Year)), 
                                   by=list(Journal, ISSN, eISSN)]
setorder(missing_journals, "Journal")

saveRDS(missing_journals, "../Data/missing_journals.rda")
fwrite(missing_journals, "../Data/missing_journals.csv")
