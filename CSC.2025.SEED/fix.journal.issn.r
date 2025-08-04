library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
journals<-fread("../Data/CSC/target.journals_20250730.csv")
journals[ISSN=="N/A", ISSN:=""]
journals[eISSN=="N/A", eISSN:=""]

full.journals<-fread("../Data/JCR/wos-core_masterlistJournals-October2024.csv")
colnames(full.journals)[1:3]<-c("Journal_name", "JCR.ISSN", "JCR.eISSN")
journals<-merge(journals, full.journals[, c("Journal_name", "JCR.ISSN", "JCR.eISSN")], 
                by="Journal_name", all.x=T)
setorderv(journals, "Count", -1)

journals[Journal_name=="AIP CONFERENCE PROCEEDINGS"]
journals[is.na(JCR.ISSN), JCR.ISSN:=""]
journals[is.na(JCR.eISSN), JCR.eISSN:=""]

error<-journals[ISSN!=JCR.ISSN | eISSN!=JCR.eISSN, 
         c("Journal_name","Count",  "ISSN", "JCR.ISSN", 
           "eISSN", "JCR.eISSN")]
error[Journal_name=="AIP CONFERENCE PROCEEDINGS"]

stat<-fread("../Data/CSC/target.20250802.csv")
journals.new<-merge(journals, stat[, c("journal", "wos")], by.x="Journal_name", by.y="journal", all.x=T)
journals.new$issn.match<-T
journals.new[ISSN!=JCR.ISSN | eISSN!=JCR.eISSN, issn.match:=F]
setorderv(journals.new, "Count", -1)
fwrite(journals.new, "../Data/CSC/error.issn.journals_20250803.csv")
