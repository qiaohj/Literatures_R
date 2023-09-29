library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD22T1/pubmed/Script")
category<-"Ecology"
article_df<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/articles.rda", category))
article_df$Year<-format(article_df$published, "%Y")

article_se<-article_df[, .(N=.N), by=list(container_title, type)]
article_se<-article_se[type=="journal-article"]
setorderv(article_se, "N", 1)
article_se$container_title_f<-factor(article_se$container_title, 
                                      levels=article_se$container_title)
p<-ggplot(article_se[(nrow(article_se)-20):nrow(article_se)])+
  geom_bar(aes(y=container_title_f, x=N), stat='identity')
ggsave(p, filename=sprintf("../Figures/%s/N_papers_top20.png", category), width=10, height=10)
setorderv(article_se, "N", -1)
fwrite(article_se, sprintf("../Figures/%s/N_papers.csv", category))
length(unique(article_se$container_title))

article_se_year<-article_df[, .(N=.N), by=list(container_title, type, Year)]
article_se_year<-article_se_year[type=="journal-article"]

article_se_since_2010<-article_df[Year>=2010, .(N=.N), by=list(container_title, type)]
setorderv(article_se_since_2010, "N", -1)

article_se_year$journal_type<-""
article_se_year[container_title %in% article_se[1:10]$container_title]$journal_type<-"Top 10 over all"
article_se_year[container_title %in% article_se_since_2010[1:10]$container_title]$journal_type<-"Top 10 since 2010"
article_se_year[container_title %in% article_se_since_2010[1:10]$container_title &
                  container_title %in% article_se[1:10]$container_title]$journal_type<-"Top 10 both"

table(article_se_year$journal_type)
article_se_year$Year<-as.numeric(article_se_year$Year)
article_se_year<-article_se_year[!is.na(Year)]
p<-ggplot(article_se_year[journal_type!="" & Year!=2023])+
  geom_line(aes(x=Year, y=N, color=container_title, linetype=factor(journal_type)))
fwrite(article_se_year[journal_type!=""], sprintf("../Figures/%s/N_papers_by_year_top_10.csv", category))

ggsave(p, filename=sprintf("../Figures/%s/N_papers_top10_by_year.png", category), width=20, height=8)
p<-ggplot(article_se_year[journal_type!="" & between(Year, 2010, 2022)])+
  geom_line(aes(x=Year, y=N, color=container_title, linetype=factor(journal_type)))+
  geom_text(data=article_se_year[journal_type!="" & between(Year, 2022, 2022)],
            aes(x=2022, y=N, color=container_title, label=container_title),hjust = 0)+
  xlim(2010, 2032)+
  theme_bw()+
  theme(legend.position = "none")
p
ggsave(p, filename=sprintf("../Figures/%s/N_papers_top10_by_year_since2010.png", category), width=10, height=6)

article_subject<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/article_subject_splitted.rda", category))
nrow(article_df[is.na(abstract)])
nrow(article_df)
article_df[!(doi %in% article_subject$article_DOI)]

article_with_subject<-merge(unique(article_df[, c("doi", "Year")]), article_subject, 
                            by.x="doi", by.y="article_DOI")
article_with_subject<-article_with_subject[!is.na(Year)]

article_with_subject_se<-article_with_subject[, .(N=.N), by=list(Subject)]
setorderv(article_with_subject_se, "N", -1)

article_with_subject_se_year<-article_with_subject[, .(N=.N), by=list(Year, Subject)]

top_20_subject<-article_with_subject_se_year[Subject %in% article_with_subject_se[1:20]$Subject]
fwrite(article_with_subject_se, sprintf("../Figures/%s/subject.csv", category))
fwrite(top_20_subject, sprintf("../Figures/%s/subject_top20_by_year.csv", category))
p<-ggplot(top_20_subject[Year!=2023])+geom_line(aes(x=as.numeric(Year), y=N, color=Subject))+
  geom_text(data=top_20_subject[between(Year, 2022, 2022)],
            aes(x=2022, y=N, color=Subject, label=Subject),hjust = 0)+
  xlim(1867, 2060)+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_sqrt()
ggsave(p, filename=sprintf("../Figures/%s/top20_subject_by_year.png", category), width=10, height=6)
p<-ggplot(top_20_subject[between(Year, 2010, 2022)])+
  geom_line(aes(x=as.numeric(Year), y=N, color=Subject))+
  geom_text(data=top_20_subject[between(Year, 2022, 2022)],
            aes(x=2022, y=N, color=Subject, label=Subject),hjust = 0)+
  xlim(2010, 2032)+
  theme_bw()+
  theme(legend.position = "none")
p
ggsave(p, filename=sprintf("../Figures/%s/top20_subject_by_year_since2010.png", category), width=10, height=6)

authors<-article_subject<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/authors.rda", category))
