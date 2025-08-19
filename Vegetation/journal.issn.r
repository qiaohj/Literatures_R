library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
journals<-fread("../Data/Vegetation/journal.list.csv", sep=",")
#jcr<-readRDS("/media/huijieqiao/WD22T_11/literatures/Data/JCR/journals.rda")
jcr<-fread("/media/huijieqiao/WD22T_11/literatures/Data/JCR/wos-core_masterlistJournals-October2024.csv")
jcr$Title<-toupper(jcr$`Journal title`)
jcr$Title<-gsub("&", "AND", jcr$Title)
jcr$Title<-toupper(gsub("ANDAMP;", "AND", jcr$Title))

journals$ISSN<-""
journals$eISSN<-""
journals$Title<-toupper(journals$Journals)
i=1
for (i in c(1:nrow(journals))){
  item<-jcr[Title==journals[i]$Title]
  if (journals[i]$Title=="VEGETATIO"){
    item<-data.table(Title="VEGETATIO", ISSN="0042-3106", eISSN="")
  }
  if (journals[i]$Title=="生物多样性"){
    item<-data.table(Title="生物多样性", ISSN="1005-0094", eISSN="")
  }
  if (journals[i]$Title=="植物生态学报"){
    item<-data.table(Title="植物生态学报", ISSN="1005-264X", eISSN="")
  }
  if (journals[i]$Title=="中国科学·生命科学"){
    item<-data.table(Title="中国科学·生命科学", ISSN="1674-7232", eISSN="")
  }
  if (journals[i]$Title=="生态学报"){
    item<-data.table(Title="生态学报", ISSN="1000-0933", eISSN="")
  }
  if (journals[i]$Title=="草业科学"){
    item<-data.table(Title="草业科学", ISSN="1001-0639", eISSN="")
  }
  if (journals[i]$Title=="西北植物学报"){
    item<-data.table(Title="西北植物学报", ISSN="1000-4025", eISSN="")
  }
  if (journals[i]$Title=="山地学报"){
    item<-data.table(Title="山地学报", ISSN="1008-2186", eISSN="")
  }
  if (journals[i]$Title=="应用生态学报"){
    item<-data.table(Title="应用生态学报", ISSN="1001-9332", eISSN="")
  }
  
  if (nrow(item)==0){
    print(journals[i]$Title)
    asdf
  }
  item<-item[, c("Title", "ISSN", "eISSN")]
  item<-unique(item)
  journals[i, ISSN:=item$ISSN]
  journals[i, eISSN:=item$eISSN]
}
journals$journal<-toupper(journals$Title)
journals$`Journal name`<-journals$journal
saveRDS(journals, "../Data/Vegetation/vegetation.journal.rda")
fwrite(journals, "../Data/JCR/Target.Journals/Vegetation.csv")
jcr[grepl("NATURE ECOLOGY", toupper(Title))]

