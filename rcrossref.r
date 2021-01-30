setwd("D:/Experiments/PubMed_R/Script")
rm(list=ls())
library('rcrossref')

#science
issn<-"0036-8075"
journal_name<-"Science"
run_date<-"2020-11-12"
#nature
issn<-"0028-0836"
journal_name<-"Nature"
run_date<-"2020-11-12"

#filter_names()
offset<-1

limit<-20
start_date<-"2010-01-01"
dir.create(sprintf("../records/%s.%s", issn, journal_name), showWarnings = F)

pageleft<-999999999
while (pageleft>0){
  print(pageleft)
  target<-sprintf("../records/%s.%s/%d.%d.journal.rda", issn, journal_name, offset, offset+limit-1)
  target_paper<-sprintf("../records/%s.%s/%d.%d.paper.rda", issn, journal_name, offset, offset+limit-1)
  if (file.exists(target)){
    next()
  }
  df_journal<-cr_works(query=sprintf("/journals/%s/works", issn), offset=offset,
                     filter=c(from_created_date=start_date), limit=limit)
  if (pageleft==999999999){
    pageleft<-df_journal$meta$total_results
  }
  pageleft<-pageleft-limit
  offset<-offset+limit
  df_paper<-cr_works(dois=df_journal$data$doi)
  saveRDS(df_journal, target)
  saveRDS(df_paper, target_paper)
}

