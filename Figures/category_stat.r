library(data.table)
library(ggplot2)

setwd("/media/huijieqiao/WD22T_11/literatures/Script")
category<-"WeedScience"
authors_country<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/authors_country_iso.rda", category))
articles<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/articles.rda", category))
table(articles$type)
articles<-articles[type=="journal-article"]
articles$Year<-format(articles$published, "%Y")
articles[container_title=="Journal of Weed Science and Technology" & is.na(Year)]
articles[is.na(Year)]$Year<-format(articles[is.na(Year)]$published_print, "%Y")
articles[container_title=="Journal of Weed Science and Technology" & is.na(Year)]
articles[is.na(Year)]$Year<-format(articles[is.na(Year)]$created_date, "%Y")
articles[container_title=="Journal of Weed Science and Technology" & is.na(Year)]

articles_container_year<-articles[, .(N=.N), by=list(container_title, Year)]
articles_container<-articles[, .(N=.N), by=list(container_title)]
articles_container_2010<-articles[Year>=2010, .(N=.N), by=list(container_title)]
articles_year<-articles[, .(N=.N), by=list(Year)]
ggplot(articles_container)+geom_bar(aes(x=N, y=container_title), stat = "identity")
ggplot(articles_container_2010)+geom_bar(aes(x=N, y=container_title), stat = "identity")
ggplot(articles_container_year)+geom_line(aes(x=as.numeric(Year), y=N, color=container_title))
ggplot(articles_container_year[Year<=2022])+
  geom_line(aes(x=as.numeric(Year), y=N, color=container_title))+
  geom_text(data=articles_container_year[between(Year, 2022, 2022)],
            aes(x=2022, y=N, color=container_title, label=container_title),hjust = 0)+
  ylim(0, 600)+xlim(1960, 2050)+
  theme_bw()+
  theme(legend.position = "none")
articles_container_year[N>1000]
