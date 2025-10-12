library(data.table)
library(stringr)
library(stringi)
library(zoo)
library(readr)
library(pdftools)
library(jsonlite)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(colorspace)
library(ggpmisc)


setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

authors.df.full.gpd<-readRDS("../Data/BIOGEOGRAPHY/authors.rda")
authors.df.full.gpd<-authors.df.full.gpd[between(year, 2010, 2024)]
authors.df.full.gpd$country.group<-""

gdp<-fread("../Figures/BIOGEOGRAPHY/overview.pie.plot_data.csv")

for (i in c(1:nrow(gdp))){
  item<-gdp[i]
  if (item$country_name=="Others"){
    country_name_str<-sprintf("Others - %s", item$gdp.type)
  }else{
    country_name_str<-item$country_name
  }
  if (item$country_name=="Others"){
    authors.df.full.gpd[!(country_name %in% gdp$country_name) & 
                          (gdp.type==item$gdp.type), gdp.type:=item$gdp.type]
    authors.df.full.gpd[!(country_name %in% gdp$country_name) & 
                          (gdp.type==item$gdp.type), country.group:=country_name_str]
  }else{
    authors.df.full.gpd[country_name==country_name_str & 
                          (gdp.type==item$gdp.type), gdp.type:=item$gdp.type]
    authors.df.full.gpd[country_name==country_name_str & 
                          (gdp.type==item$gdp.type), country.group:=country_name_str]
  }
}


article.N<-authors.df.full.gpd[, .(N.article=length(unique(doi))),
                         by=list(year, journal)]

item<-authors.df.full.gpd[is_first_author==T]
article.N.1<-item[,.(N=length(unique(doi)), author="First author only"),
                               by=list(year, journal, gdp.type)]
article.N.1<-merge(article.N.1, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[(is_first_author==T | 
                             is_corresponding_author==T | 
                             is_co_first_author==T)]
article.N.2<-item[,.(N=length(unique(doi)), author="First, co-first and corresponding authors"),
                  by=list(year, journal, gdp.type)]
article.N.2<-merge(article.N.2, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[is_corresponding_author==T]
article.N.3<-item[,.(N=length(unique(doi)), author="Corresponding author only"),
                  by=list(year, journal, gdp.type)]
article.N.3<-merge(article.N.3, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[is_first_author==F & is_co_first_author==T]
article.N.4<-item[,.(N=length(unique(doi)), author="Co-first author only"),
                  by=list(year, journal, gdp.type)]
article.N.4<-merge(article.N.4, article.N, by=c("year", "journal"))
article.N.full<-rbindlist(list(article.N.1, article.N.2, article.N.3, article.N.4))
article.N.full$per<-article.N.full$N/article.N.full$N.article
article.N.full$gdp.type<-factor(article.N.full$gdp.type, levels=c("H", "UM", "L"))
article.N.full$journal.abbr<-factor(article.N.full$journal, 
                                    levels=c("DIVERSITY AND DISTRIBUTIONS",
                                             "ECOGRAPHY",
                                             "GLOBAL ECOLOGY AND BIOGEOGRAPHY",
                                             "JOURNAL OF BIOGEOGRAPHY"),
                                    labels=c("DDI", "ECOG", "GEB", "JBI"))



formula <- y ~ x
fig.df<-article.N.full[author=="First, co-first and corresponding authors"]
fig.df$x_pos<-ifelse(fig.df$year <= 2019, 0.05, 0.95)
fig.df$period <- ifelse(fig.df$year <= 2019, "Pre-2019", "Post-2019")
fig.df_positions <- unique(fig.df, by = c("gdp.type", "journal.abbr", "period"))
setorder(fig.df_positions, gdp.type, period, journal.abbr)

fig.df_positions[, ':=' (
  rank_id = seq_len(.N),
  y.position = 0.95-(seq_len(.N) - 1) * 0.05
), by = gdp.type]

fig.df_positions[, is_period_start := seq_len(.N) == 1, by = .(gdp.type, period)]

period_labels <- fig.df_positions[is_period_start == TRUE, 
                                  .(gdp.type, period, y.position)]
period_labels[, label := paste("Period:", period)]

p <- ggplot(fig.df, 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type,
                group=interaction(journal.abbr, period)))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  stat_poly_eq(
    formula = formula,
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = '~~~'),
        y = x_pos), 
    parse = TRUE,
    label.x = 0.95,
    #label.y = "middle",
    vjust = 1,
    size = 3.5
  )+geom_text(
    data = period_labels,
    # 将标签放在左侧，比 R2 标签高一点
    aes(x = 2025, y = y.position + 0.015, label = label), 
    inherit.aes = FALSE, 
    size = 3.5,
    fontface = "bold",
    hjust = 0 # 左对齐
  )+
  xlim(2010, 2029)+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.full.authors.pdf", width=9, height=8)


fig.df<-article.N.full[author=="Corresponding author only"]
fig.df$x_pos<-ifelse(fig.df$year <= 2019, 0.05, 0.95)
fig.df$period <- ifelse(fig.df$year <= 2019, "Pre-2019", "Post-2019")

fig.df[, c("year", "gdp.type", "per", "journal.abbr")]
p <- ggplot(fig.df, 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type,
            group=interaction(journal.abbr, period)))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.corresponding.authors.pdf", width=9, height=8)

fig.df<-article.N.full[author=="First author only"]
fig.df$x_pos<-ifelse(fig.df$year <= 2019, 0.05, 0.95)
fig.df$period <- ifelse(fig.df$year <= 2019, "Pre-2019", "Post-2019")

p <- ggplot(fig.df, 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type,
            group=interaction(journal.abbr, period)))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.first.authors.pdf", width=9, height=8)


###CHINA#########

item<-authors.df.full.gpd[is_first_author==T & country_iso3=="CHN"]
article.N.1<-item[,.(N=length(unique(doi)), author="First author only"),
                  by=list(year, journal, gdp.type)]
article.N.1<-merge(article.N.1, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[(is_first_author==T | 
                             is_corresponding_author==T | 
                             is_co_first_author==T) & country_iso3=="CHN"]
article.N.2<-item[,.(N=length(unique(doi)), author="First, co-first and corresponding authors"),
                  by=list(year, journal, gdp.type)]
article.N.2<-merge(article.N.2, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[is_corresponding_author==T & country_iso3=="CHN"]
article.N.3<-item[,.(N=length(unique(doi)), author="Corresponding author only"),
                  by=list(year, journal, gdp.type)]
article.N.3<-merge(article.N.3, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[is_first_author==F & is_co_first_author==T & country_iso3=="CHN"]
article.N.4<-item[,.(N=length(unique(doi)), author="Co-first author only"),
                  by=list(year, journal, gdp.type)]
article.N.4<-merge(article.N.4, article.N, by=c("year", "journal"))
article.N.full<-rbindlist(list(article.N.1, article.N.2, article.N.3, article.N.4))
article.N.full$per<-article.N.full$N/article.N.full$N.article
article.N.full$gdp.type<-factor(article.N.full$gdp.type, levels=c("H", "UM", "L"))
article.N.full$journal.abbr<-factor(article.N.full$journal, 
                                    levels=c("DIVERSITY AND DISTRIBUTIONS",
                                             "ECOGRAPHY",
                                             "GLOBAL ECOLOGY AND BIOGEOGRAPHY",
                                             "JOURNAL OF BIOGEOGRAPHY"),
                                    labels=c("DDI", "ECOG", "GEB", "JBI"))
formula <- y ~ x
fig.df<-article.N.full[author=="First, co-first and corresponding authors"]
fig.df$x_pos<-ifelse(fig.df$year <= 2019, 0.05, 0.95)
fig.df$period <- ifelse(fig.df$year <= 2019, "Pre-2019", "Post-2019")
fig.df[, c("year", "journal.abbr", "gdp.type", "per")]
p <- ggplot(fig.df, 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type,
                group=interaction(journal.abbr, period)))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  
  stat_poly_eq(
    formula = formula,
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = '~~~')), 
    parse = TRUE,
    label.x=0.98,
    label.y = "top",
    vjust = 1,
    size = 3.5
  )+
  xlim(2010, 2029)+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.full.authors.china.pdf", width=9, height=4)


fig.df<-article.N.full[author=="Corresponding author only"]
p <- ggplot(fig.df, 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  
  stat_poly_eq(
    formula = formula,
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = '~~~')), 
    parse = TRUE,
    label.x=0.98,
    label.y = "top",
    vjust = 1,
    size = 3.5
  )+
  xlim(2010, 2029)+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.corresponding.authors.china.pdf", width=9, height=4)

fig.df<-article.N.full[author=="First author only"]
p <- ggplot(fig.df, 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  
  stat_poly_eq(
    formula = formula,
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = '~~~')), 
    parse = TRUE,
    label.x=0.98,
    label.y = "top",
    vjust = 1,
    size = 3.5
  )+
  xlim(2010, 2029)+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.first.authors.china.pdf", width=9, height=4)

####Exclude CHN###########

item<-authors.df.full.gpd[is_first_author==T & country_iso3!="CHN"]
article.N.1<-item[,.(N=length(unique(doi)), author="First author only"),
                  by=list(year, journal, gdp.type)]
article.N.1<-merge(article.N.1, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[(is_first_author==T | 
                             is_corresponding_author==T | 
                             is_co_first_author==T) & country_iso3!="CHN"]
article.N.2<-item[,.(N=length(unique(doi)), author="First, co-first and corresponding authors"),
                  by=list(year, journal, gdp.type)]
article.N.2<-merge(article.N.2, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[is_corresponding_author==T & country_iso3!="CHN"]
article.N.3<-item[,.(N=length(unique(doi)), author="Corresponding author only"),
                  by=list(year, journal, gdp.type)]
article.N.3<-merge(article.N.3, article.N, by=c("year", "journal"))

item<-authors.df.full.gpd[is_first_author==F & is_co_first_author==T & country_iso3!="CHN"]
article.N.4<-item[,.(N=length(unique(doi)), author="Co-first author only"),
                  by=list(year, journal, gdp.type)]
article.N.4<-merge(article.N.4, article.N, by=c("year", "journal"))
article.N.full<-rbindlist(list(article.N.1, article.N.2, article.N.3, article.N.4))
article.N.full$per<-article.N.full$N/article.N.full$N.article
article.N.full$gdp.type<-factor(article.N.full$gdp.type, levels=c("H", "UM", "L"))
article.N.full$journal.abbr<-factor(article.N.full$journal, 
                                    levels=c("DIVERSITY AND DISTRIBUTIONS",
                                             "ECOGRAPHY",
                                             "GLOBAL ECOLOGY AND BIOGEOGRAPHY",
                                             "JOURNAL OF BIOGEOGRAPHY"),
                                    labels=c("DDI", "ECOG", "GEB", "JBI"))
formula <- y ~ x
fig.df<-article.N.full[author=="First, co-first and corresponding authors"]
p <- ggplot(fig.df[gdp.type=="UM"], 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  
  stat_poly_eq(
    formula = formula,
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = '~~~')), 
    parse = TRUE,
    label.x=0.98,
    label.y = "top",
    vjust = 1,
    size = 3.5
  )+
  xlim(2010, 2029)+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.full.authors.no.china.pdf", width=9, height=4)


fig.df<-article.N.full[author=="Corresponding author only"]
p <- ggplot(fig.df[gdp.type=="UM"], 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  
  stat_poly_eq(
    formula = formula,
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = '~~~')), 
    parse = TRUE,
    label.x=0.98,
    label.y = "top",
    vjust = 1,
    size = 3.5
  )+
  xlim(2010, 2029)+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.corresponding.authors.no.china.pdf", width=9, height=4)

fig.df<-article.N.full[author=="First author only"]
p <- ggplot(fig.df[gdp.type=="UM"], 
            aes(x=year, y=per, color=journal.abbr, linetype = gdp.type))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, formula="y ~ x")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Percentage of all papers per year", linetype="GDP level", color="Journal")+
  
  stat_poly_eq(
    formula = formula,
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = '~~~')), 
    parse = TRUE,
    label.x=0.98,
    label.y = "top",
    vjust = 1,
    size = 3.5
  )+
  xlim(2010, 2029)+
  theme_bw()+
  facet_wrap(~gdp.type, nrow=3, scales="free")
p

ggsave(p, filename="../Figures/BIOGEOGRAPHY/percentage.yearly.first.authors.no.china.pdf", width=9, height=4)

