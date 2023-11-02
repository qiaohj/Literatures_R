library(data.table)
library(ggplot2)
setwd("~/git/literatures/Script")
source("functions.r")
categories<-c("Ecology", "Biodiversity Conservation")
articles<-list()
journals<-list()
tokens<-list()
for (cate in categories){
  print(cate)
  item<-readArticle(cate)
  journal<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/journals.rda", cate))
  if (cate!="Biodiversity Conservation"){
    token<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/token_title.rda", cate))  
  }
  
  #item$Categorie<-cate
  item$File<-NULL
  journal$File<-NULL
  articles[[length(articles)+1]]<-item
  journals[[length(journals)+1]]<-journal
}
articles<-rbindlist(articles)
journals<-rbindlist(journals)
tokens<-rbindlist(tokens)
articles<-unique(articles)

N_articles<-journals[, .(N=.N), by=list(article_DOI)]
journals[article_DOI %in% c("10.1590/1809-4392201703441")]
articles[doi %in% c("10.1590/1809-4392201703441")]
articles_sub<-unique(articles[, c("doi", "Year")])
x<-articles_sub[, .(N=.N), by=list(doi)]
x[N>1]

journals<-journals[article_DOI %in% articles$doi]
journals$Title<-toupper(journals$Title)
journals$Title<-gsub("&AMP;", "AND", journals$Title)
journals$Title<-gsub("THE ", "", journals$Title)
journal_issn<-journals[, .(N=.N), by=list(Title, ISSN)]
N_journal_issn<-journal_issn[, .(N=.N), by=list(ISSN)]
journal_issn[ISSN %in% N_journal_issn[N>=2]$ISSN]


journal_issn$Title_RAW<-journal_issn$Title
journal_issn[ISSN %in% c("1573-5125", "1386-2588")]$Title<-"AQUATIC ECOLOGY"
journal_issn[ISSN %in% c("1366-9516", "1472-4642")]$Title<-"DIVERSITY AND DISTRIBUTIONS"
journal_issn[ISSN %in% c("1439-0574")]$Title<-"EUROPEAN JOURNAL OF WILDLIFE RESEARCH"
journal_issn[ISSN %in% c("1385-0237", "1573-5052")]$Title<-"PLANT ECOLOGY"
journal_issn[ISSN %in% c("0962-8452", "1471-2954")]$Title<-"PROCEEDINGS OF THE ROYAL SOCIETY B: BIOLOGICAL SCIENCES"
journal_issn[ISSN %in% c("1464-5262")]$Title<-"JOURNAL OF NATURAL HISTORY"
journal_issn[ISSN %in% c("1195-6860")]$Title<-"ECOSCIENCE"
journal_issn[ISSN %in% c("0378-1909", "1573-5133")]$Title<-"ENVIRONMENTAL BIOLOGY OF FISHES"
journal_issn[ISSN %in% c("0367-2530")]$Title<-"FLORA"
journal_issn[ISSN %in% c("1578-665X", "2014-928X")]$Title<-"ANIMAL BIODIVERSITY AND CONSERVATION"
journal_issn[ISSN %in% c("0075-6458", "2071-0771")]$Title<-"KOEDOE"
journal_issn[ISSN %in% c("1936-0584", "1936-0592")]$Title<-"ECOHYDROLOGY"
journal_issn[ISSN %in% c("0370-047X")]$Title<-"PROCEEDINGS OF LINNEAN SOCIETY OF NEW SOUTH WALES"


N_journal_issn_new<-unique(journal_issn[, c("Title", "ISSN")])[, .(N=.N), by=list(ISSN)]
journal_issn[ISSN %in% N_journal_issn_new[N>1]$ISSN]

issu_journal<-journal_issn[, .(N=.N), by=list(Title)]
journal_titles<-unique(journal_issn[Title %in% issu_journal[N>1]$Title]$Title)
journal_issn$ISSN_RAW<-journal_issn$ISSN
for (t in journal_titles){
  journal_issn[Title==t]$ISSN<-journal_issn[Title==t][1]$ISSN
}
journal_issn$N<-NULL
journal_issn$Title_RAW<-NULL
journal_issn<-unique(journal_issn)

journals_new<-merge(unique(journal_issn[, c("Title", "ISSN", "ISSN_RAW")]),
                    unique(journals[, c("article_DOI", "ISSN")]), 
                           by.x="ISSN_RAW", by.y="ISSN")

articles_sub[!doi %in% journals_new$article_DOI]

article_new<-merge(articles_sub, journals_new, by.x="doi", by.y="article_DOI")

N_articles<-article_new[, .(N_articles=.N), by=list(Year, Title, ISSN)]
N_articles$Year<-as.numeric(N_articles$Year)
ggplot(N_articles)+geom_line(aes(x=Year, y=N_articles, color=Title))+
  theme(legend.position = "none")+
  xlim(2000, 2022)
  
quantile_20<-N_articles[, .(N=quantile(N_articles, .50)[1]), by=list(Year)]
ggplot(quantile_20[Year>=1950])+geom_line(aes(x=Year, y=N))
samples<-list()
for (i in c(1:100)){
  print(i)
  sample<-article_new[,.SD[sample(.N, min(100,.N))],by = list(Year, Title, ISSN)]
  sample$rep<-i
  samples[[i]]<-sample
}
samples<-rbindlist(samples)
samples$Year<-as.numeric(samples$Year)
saveRDS(samples, "../Data/Sample_Ecology_Biodiversity/samples.rda")
article_new$Year<-as.numeric(article_new$Year)
article_new$ISSN_RAW<-NULL
article_new<-article_new[!(Title=="PROCEEDINGS OF THE ROYAL SOCIETY B: BIOLOGICAL SCIENCES" &
                            Year==1948)]
article_new<-unique(article_new)
saveRDS(article_new, "../Data/Sample_Ecology_Biodiversity/articles.rda")

if (F){
  N_articles<-article_new[, .(N_article=length(unique(doi)),
                              N_journal=length(unique(Title))
                              ),
                          by=list(Year)]
  range(N_articles$N_article)
  range(N_articles$N_journal)
  coeff <- 150
  
  color_left<-"#0072B2"
  color_right<-"#CC79A7"
  
  p<-ggplot(N_articles[Year<=2022], aes(x=Year)) +
    geom_line( aes(y=N_journal), color=color_left) + 
    geom_line( aes(y=N_article / coeff), color=color_right) + 
    scale_y_continuous(
      # Features of the first axis
      name = "Number of journals",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="Number of papers")
    )+
    theme_bw()+
    theme( axis.line.y.left = element_line(color = color_left), 
           axis.ticks.y.left = element_line(color = color_left),
           axis.text.y.left = element_text(color = color_left),
           axis.title.y.left = element_text(color = color_left),
           axis.line.y.right = element_line(color = color_right), 
           axis.ticks.y.right = element_line(color = color_right),
           axis.text.y.right = element_text(color = color_right),
           axis.title.y.right = element_text(color = color_right))
  ggsave(p, filename="../Figures/Overview_Ecology_Biodiversity/overview.png", width=7, height=4)
  N_articles_journal<-article_new[, .(N_article=length(unique(doi))),
                                  by=list(Year, Title)]
  journal_start<-N_articles_journal[, .(Start_Year=min(Year)),
                                    by=list(Title)]
  N_articles_journal<-merge(N_articles_journal, journal_start, by=c("Title"))
  N_articles_journal$Age<-N_articles_journal$Year-N_articles_journal$Start_Year
  N_articles_journal$Current_Age<-2023-N_articles_journal$Start_Year
  
  N_articles_journal[N_article>1000]
  unique(N_articles_journal[Current_Age>100]$Title)
  range(N_articles_journal$Current_Age)
  N_articles_journal[]
  max(N_articles_journal$N_article)
  N_articles_journal_sub<-N_articles_journal[Year>=1950]
  N_articles_journal_sub<-N_articles_journal[Year<=2022]
  target_journales<-c(unique(N_articles_journal[Current_Age>100]$Title),
                      unique(N_articles_journal[N_article>500]$Title))
  ggplot(N_articles_journal_sub)+
    geom_line(aes(x=Age, y=N_article, group=Title), alpha=0.5, color="#999999")+
    geom_line(data=N_articles_journal_sub[Title %in% target_journales[2]], 
              aes(x=Age, y=N_article, group=Title, color=Title))
  
  N_articles_journal[Title=="MARINE ECOLOGY PROGRESS SERIES"]
  article_new[Title=="MARINE ECOLOGY PROGRESS SERIES" & Year==2007]
  articles[doi=="10.3354/meps001001"]
}