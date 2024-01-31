library(ggplot2)
library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
category<-"Biodiversity Conservation"
source("functions.r")
articles<-readArticle(category)

length(unique(articles$doi))

articles$abstract_length<-nchar(articles$abstract)
articles[is.na(abstract)]$abstract_length<-0
hist(articles[abstract_length>0]$abstract_length)

nrow(articles[abstract_length==0])
nrow(articles[abstract_length<100])
nrow(articles[abstract_length>=100])

articles$with_abstract<-ifelse(articles$abstract_length>=100, T, F)

abstract_by_journal<-articles[, .(N=.N), by=list(container_title, Year, with_abstract)]

abstract_by_journal_full<-articles[, .(N.all=.N), by=list(container_title, Year)]

abstract_by_journal<-merge(abstract_by_journal[with_abstract==T], abstract_by_journal_full, 
                           by=c("container_title", "Year"))

abstract_by_journal$per<-abstract_by_journal$N/abstract_by_journal$N.all
abstract_by_journal$year_label<-"<=2000"
abstract_by_journal[between(Year, 2001, 2010)]$year_label<-"2001-2010"
abstract_by_journal[between(Year, 2011, 2020)]$year_label<-"2011-2020"
abstract_by_journal[Year>2020]$year_label<-">2020"
table(abstract_by_journal$year_label)
p<-ggplot(abstract_by_journal)+
  geom_boxplot(aes(x=container_title, y=per))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~year_label, nrow=4)

ggsave(p, filename="../Figures/abstract_proportion.png", width=8, height=12)
