library(data.table)
library(ggplot2)
library(gganimate)
library(stringr)
setwd("~/git/literatures/Script")
source("functions.r")
categories<-c("Ecology", "Biodiversity Conservation")
cate<-categories[1]
articles<-list()
journals<-list()
tokens<-list()
tokens_abstract<-list()
authors_countries<-list()
authors<-list()
article_subject_splittedes<-list()
article_subjects<-list()
for (cate in categories){
  print(cate)
  item<-readArticle(cate)
  journal<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/journals.rda", cate))
  token<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/token_title.rda", cate)) 
  token_abstract<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/token_abstract.rda", cate)) 
  article_subject_splitted<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/article_subject_splitted.rda", cate)) 
  article_subject<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/article_subject.rda", cate)) 
  author<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/authors.rda", cate)) 
  authors_country<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/authors_country_iso.rda", cate)) 
  
  #item$Categorie<-cate
  #item$File<-NULL
  journal$File<-NULL
  authors_country$File<-NULL
  
  articles[[length(articles)+1]]<-item
  journals[[length(journals)+1]]<-journal
  authors[[length(authors)+1]]<-author
  article_subject_splittedes[[length(article_subject_splittedes)+1]]<-article_subject_splitted
  article_subjects[[length(article_subjects)+1]]<-article_subject
  authors_countries[[length(authors_countries)+1]]<-authors_country
  tokens[[length(tokens)+1]]<-token
  tokens_abstract[[length(tokens_abstract)+1]]<-token_abstract
  
}
articles<-rbindlist(articles)
journals<-rbindlist(journals)
tokens<-rbindlist(tokens)
tokens_abstract<-rbindlist(tokens_abstract)
authors_countries<-rbindlist(authors_countries)
article_subject_splittedes<-rbindlist(article_subject_splittedes)
article_subjects<-rbindlist(article_subjects)
authors<-rbindlist(authors)

tokens$Year<-NULL
tokens$container_title<-NULL
tokens$type<-NULL
tokens<-unique(tokens)


articles<-unique(articles)

N_articles<-journals[, .(N=.N), by=list(article_DOI)]
journals[article_DOI %in% c("10.1017/S037689290000196X")]
item[doi %in% c("10.1017/s037689290000196x")]
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
article_new[(Title=="PROCEEDINGS OF THE ROYAL SOCIETY B: BIOLOGICAL SCIENCES" &
                            Year==1948)]

article_new[(Title=="ENVIRONMENTAL CONSERVATION" &
                             Year==2009)]
article_new[(Title=="PALEOBIOLOGY" &
                             Year==2016)]

article_new<-unique(article_new)
article_new<-article_new[!is.na(Year)]
saveRDS(article_new, "../Data/Sample_Ecology_Biodiversity/articles.rda")

tokens_new<-merge(tokens, article_new, by="doi")
tokens_new$Word<-as.character(tokens_new$Word)
saveRDS(tokens_new, "../Data/Sample_Ecology_Biodiversity/tokens_title.rda")

#article_new[!(doi %in% tokens_new$doi)]
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
  p
  ggsave(p, filename="../Figures/Overview_Ecology_Biodiversity/overview.png", width=7, height=4)
  N_articles_journal<-article_new[, .(N_article=length(unique(doi))),
                                  by=list(Year, Title)]
  
  journal_start<-N_articles_journal[, .(Start_Year=min(Year)),
                                    by=list(Title)]
  N_articles_journal<-merge(N_articles_journal, journal_start, by=c("Title"))
  
  N_articles_journal$Age<-N_articles_journal$Year-N_articles_journal$Start_Year
  N_articles_journal$Current_Age<-2023-N_articles_journal$Start_Year
  
  unique(N_articles_journal[N_article>1000]$Title)
  N_articles_journal<-N_articles_journal[!is.na(Year)]
  fwrite(N_articles_journal, "../Data/N_articles_journal.csv")
  #N_articles_journal<-N_articles_journal[Title != "ENVIRONMENTAL CONSERVATION"]
  N_articles_journal[Title=="APPLIED ECOLOGY AND ENVIRONMENTAL RESEARCH"]
  N_articles_journal$Recent_Age<-abs(N_articles_journal$Year - 2023)
  N_articles_journal_all<-N_articles_journal[between(Recent_Age, 1, 3), 
                                             .(N_article=sum(N_article)),
                                             by=list(Title, Current_Age)]
  N_articles_journal_all$Average_N_article<-N_articles_journal_all$N_article/3
  setorderv(N_articles_journal_all, "Average_N_article", -1)
  
  N_articles_journal_all[grepl("NATURE", Title)]
  unique(N_articles_journal[Current_Age>100]$Title)
  range(N_articles_journal$Current_Age)
  
  max(N_articles_journal$N_article)
  N_articles_journal_sub<-N_articles_journal[Year>=1950]
  N_articles_journal_sub<-N_articles_journal[Year<=2022]
  target_journales<-c(unique(N_articles_journal[Current_Age>100]$Title),
                      unique(N_articles_journal[N_article>1000]$Title))
  ggplot(N_articles_journal_sub)+
    geom_line(aes(x=Age, y=N_article, group=Title), alpha=0.5, color="#999999")+
    geom_line(data=N_articles_journal_sub[Title %in% target_journales], 
              aes(x=Age, y=N_article, group=Title, color=Title))
  
  N_articles_journal[Title=="MARINE ECOLOGY PROGRESS SERIES"]
  N_articles_journal[grepl("FRONTIERS", Title)]
  item
  table(item[grepl("2296-701X", 
                   issn)]$container_title)
  
  article_new[Title=="MARINE ECOLOGY PROGRESS SERIES" & Year==2007]
  articles[doi=="10.3354/meps001001"]
  
  dois<-unique(article_new[Year>=2000]$doi)
  length(dois)
  length(dois[dois %in% unique(tokens_abstract$doi)])
  length(dois[dois %in% unique(authors_countries$article_DOI)])
  removed<-c("volum issu cover", "cover front matter", "issu cover front",
             "cover back matter", "issu cover back", "hard cover isbn",
             "illustr hard cover", "new south wale", "orx volum issu",
             "pab volum issu", "soft cover isbn", "pol volum issu", 
             "cover pictur issu", "pictur issu inform", "cambridg univers press",
             "two new speci", "illustr soft cover", "enc volum issu",
             "south eastern australia", "oxford univers press",
             "descript two new", "case studi use", "three new speci",
             "case studi", "volum issu", "new speci", "unit state",
             "new zealand", "south africa", "issu cover", "north american",
             "north america", "univers press", "book review",
             "new york", "cover isbn", "front matter", "issu inform",
             "editori board", "back matter", "cover front", "editori board page",
             "tro volum issu", "volum issu front", "issu front matter",
             "bci volum issu", "volum issu back", "issu back matter",
             "insid front cover")
  tokens_new<-tokens_new[!(Word %in% removed)]
  tokens_new<-tokens_new[!is.na(Year)]
  N_tokens<-tokens_new[, .(Frequency=sum(Frequency),
                           N=.N), by=list(Word, N_token)]
  
  items<-str_split(N_tokens$Word, " ")
  items_length<-lapply(items, str_length)
  items_length<-lapply(items_length, range)
  items_df<- data.frame(do.call(rbind, items_length))
  N_tokens$length_min<-items_df$X1
  N_tokens$length_max<-items_df$X2
  N_tokens<-N_tokens[length_min>2]
  N_tokens<-N_tokens[!(Word %in% removed)]
  setorderv(N_tokens, "N", -1)
  top100_tokens = N_tokens[,head(.SD, 100), by = c("N_token")]
  fwrite(top100_tokens, "../Data/top100_tokens.csv")
  top10_tokens = N_tokens[,head(.SD, 10), by = c("N_token")]
  top10_tokens$Rank<-rep(c(1:10), 3)
  N_tokens_yearly<-tokens_new[, .(Frequency_Year=sum(Frequency),
                           N_Year=.N), by=list(Word, N_token, Year)]
  N_tokens_yearly
  
  N_tokens_yearly_top10<-N_tokens_yearly[Word %in% top10_tokens$Word]
  N_tokens_yearly_top10<-merge(N_tokens_yearly_top10, top10_tokens, 
                               by=c("N_token", "Word"))
  
  N_tokens_yearly_all<-N_tokens_yearly_top10[, .(N_all=sum(Frequency_Year)),
                                             by=list(Year)]
  N_tokens_yearly_top10<-merge(N_tokens_yearly_top10, N_tokens_yearly_all, 
                               by="Year")
  N_tokens_yearly_top10$Per<-N_tokens_yearly_top10$Frequency_Year/N_tokens_yearly_top10$N_all
  range(N_tokens_yearly_top10$Per)
  ggplot(N_tokens_yearly_top10[between(Year, 2000, 2022)])+
    geom_line(aes(x=Year, y=Frequency_Year, color=Word))+
    facet_wrap(~N_token, nrow=3, scale="free")+
    theme_bw()+
    theme(legend.position = "none")
  
  p<-ggplot(N_tokens_yearly_top10[between(Year, 2000, 2022)])+
    geom_line(aes(x=Year, y=Per, color=Word))+
    geom_text(data=N_tokens_yearly_top10[Year==2022],
              aes(x=2022, y=Per, label=Word),hjust = 0, nudge_x = 0.05)+
    facet_wrap(~N_token, nrow=3, scale="free")+
    xlim(2000, 2030)+
    theme_bw()+
    theme(legend.position = "none")
  ggsave(p, filename="../Figures/tokens/tokens_years.png", width=10, height=12)  
  
  tokens_new$journal_type<-"<=10"
  tokens_new[Title %in% unique(N_articles_journal_all[Average_N_article>10]$Title)]$journal_type<-">10"
  tokens_new[Title %in% unique(N_articles_journal_all[Average_N_article>100]$Title)]$journal_type<-">100"
  tokens_new[Title %in% unique(N_articles_journal_all[Average_N_article>300]$Title)]$journal_type<-">300"
  tokens_new[Title %in% unique(N_articles_journal_all[Average_N_article>500]$Title)]$journal_type<-">500"
  tokens_new[Title %in% unique(N_articles_journal_all[Average_N_article>700]$Title)]$journal_type<-">700"
  
  tokens_N<-tokens_new[, .(N=.N), by=list(Word, N_token, journal_type)]
  tokens_N[Word=="speci distribut model"]
  tokens_N[Word=="climat chang"]
  
  if (F){
    ggplot(N_tokens_yearly_top10[N_token==2]) +
      geom_col(aes(x=Rank, y=Frequency_Year,
                   group=Word, fill=Word),
               width=0.4) +
      geom_text(aes(x=Rank, y=0,
                    label=Word, group=Word),
                hjust=1.25) +
      theme_minimal() + ylab('Value') +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(5,5,5,5),
                               'lines')) +
      scale_fill_brewer(palette="Dark2") +
      coord_flip(clip='off') + 
      ggtitle('{closest_state}') +             # title with the timestamp period
      transition_states(Year,
                        transition_length = 1,
                        state_length = 1) +
      exit_fly(x_loc = 0, y_loc = 0) +         # chart exit animation params
      enter_fly(x_loc = 0, y_loc = 0)     
  }
}
