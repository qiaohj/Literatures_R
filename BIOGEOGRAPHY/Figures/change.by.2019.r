library(data.table)
library(ggplot2)
library(forcats)
library(ggrepel)

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

authors.df.full.gpd<-readRDS("../Data/BIOGEOGRAPHY/authors.rda")
authors.df.full.gpd<-authors.df.full.gpd[between(year, 2010, 2025)]
authors.df.full.gpd$country.group<-""
authors.df.full.gpd[country_name %in% c("Hong Kong SAR, China", "Taiwan, China"), country_name:="China"]
authors.df.full.gpd[country_iso3 %in% unique(authors.df.full.gpd[country_name=="China"]$country_iso3), 
                    country_iso3:="CHN"]

article.N<-authors.df.full.gpd[, .(N.article=length(unique(doi))),
                               by=list(country_name)]
setorderv(article.N, "N.article", -1)
article.N.top10<-article.N[1:9]

authors.df.full.gpd[country_name %in% article.N.top10$country_name, country.group:=country_name]
authors.df.full.gpd[country.group=="", country.group:=sprintf("Others - %s", gdp.type)]
authors.df.full.gpd$journal.abbr<-factor(authors.df.full.gpd$journal, 
                                levels=c("DIVERSITY AND DISTRIBUTIONS",
                                         "ECOGRAPHY",
                                         "GLOBAL ECOLOGY AND BIOGEOGRAPHY",
                                         "JOURNAL OF BIOGEOGRAPHY"),
                                labels=c("DDI", "ECOG", "GEB", "JBI"))
full.text<-readRDS("../Data/BIOGEOGRAPHY/full.text.rda")
authors.df.full.gpd<-authors.df.full.gpd[doi %in% full.text$doi]
saveRDS(authors.df.full.gpd, "../Data/BIOGEOGRAPHY/authors.fixed.rda")


item<-authors.df.full.gpd[is_corresponding_author==T]
table(item$country.group)

author.N<-item[, .(N=length(unique(doi))), by=c("year", "country.group", "journal")]
article.N<-authors.df.full.gpd[, .(N.article=length(unique(doi))),
                               by=c("year", "journal")]
article.N.full<-merge(author.N, article.N, by=c("year", "journal"))

article.N.full$country.group<-factor(article.N.full$country.group, 
                                     levels=c("Australia", "Brazil","Canada","China","France","Germany",
                                              "Spain",
                                              "United Kingdom","United States",
                                              "Others - H","Others - UM","Others - L"))
article.N.full$journal.abbr<-factor(article.N.full$journal, 
                                    levels=c("DIVERSITY AND DISTRIBUTIONS",
                                             "ECOGRAPHY",
                                             "GLOBAL ECOLOGY AND BIOGEOGRAPHY",
                                             "JOURNAL OF BIOGEOGRAPHY"),
                                    labels=c("DDI", "ECOG", "GEB", "JBI"))

article.N.full$per<-article.N.full$N/article.N.full$N.article
article.N.full$period <- ifelse(article.N.full$year <= 2019, "Pre-2019", "Post-2019")

article.N.full[, period := factor(period, levels = c("Pre-2019", "Post-2019"))]
coms<-article.N.full[,.(N=.N), by=list(country.group, journal.abbr)]
i=4
item.list<-list()
for (i in c(1:nrow(coms))){
  com<-coms[i]
  item<-article.N.full[country.group==com$country.group & journal.abbr==com$journal.abbr]
  test_result <- wilcox.test(per ~ period, data = item, alternative = "two.sided", exact=T)
  p_val <- test_result$p.value
  item.N<-item[, .(mean.v=mean(per), N.record=.N, p_val=p_val),
               by=list(period, journal.abbr, country.group)]
  item.flat<-data.table(p_value=p_val, 
                        mean_pre=item.N[period=="Pre-2019"]$mean.v,
                        mean_post=item.N[period=="Post-2019"]$mean.v,
                        N_pre=item.N[period=="Pre-2019"]$N.record,
                        N_Post=item.N[period=="Post-2019"]$N.record,
                        country.group=com$country.group,
                        journal.abbr=com$journal.abbr)
  item.list[[i]]<-item.flat
}
analysis_dt<-rbindlist(item.list)
analysis_dt[, mean_diff := mean_post - mean_pre]
analysis_dt[, Significance_Star := cut(p_value, 
                                       breaks = c(0, 0.001, 0.01, 0.05, 2),
                                       labels = c("***", "**", "*", ""),
                                       right = FALSE)]


analysis_dt[, Result_Category := fcase(
  is.na(p_value), "N/A",
  p_value < 0.05 & mean_diff > 0, paste0("⬆", Significance_Star, ""),
  p_value < 0.05 & mean_diff < 0, paste0("⬇", Significance_Star, ""),
  default = ""
)]

plot_dt <- melt(analysis_dt, 
                id.vars = c("journal.abbr", "country.group", "Result_Category", "Significance_Star", "mean_diff"),
                measure.vars = c("mean_pre", "mean_post"),
                variable.name = "period_name",
                value.name = "per_mean")

plot_dt[, period := factor(gsub("mean_", "", period_name), levels = c("pre", "post"), 
                           labels = c("Pre-2019", "Post-2019"))]

plot_dt[, facet_label := paste0(journal.abbr, " - ", country.group)]

plot_dt[, facet_label := fct_reorder(facet_label, mean_diff)]


p <- ggplot(plot_dt, aes(x = period, y = per_mean, group = facet_label, color = Result_Category)) +
  geom_line(aes(), linewidth = 1.2) +
  geom_point(aes(shape = period), size = 3) +
  geom_text_repel(data=plot_dt[period=="Post-2019"],
    aes(x = 2, y = per_mean, 
                label = paste0("", sprintf("%.2f%%", mean_diff * 100), 
                               Result_Category, sprintf("(%s)", country.group))),
            hjust = -1, vjust=1, direction = "y", size = 3.5) +
  scale_color_manual(values = c(
    "⬆*" = "#e31a1c",
    "⬆**" = "#e31a1c",
    "⬆***" = "#e31a1c",
    "⬇*" = "#33a02c",
    "⬇**" = "#33a02c",
    "⬇***" = "#33a02c",
    "X" = "gray50",
    "N/A" = "#1f78b4"
  )) +
  scale_y_sqrt(labels = scales::percent) +
  scale_x_discrete(
    expand = expansion(mult = c(0.05, 0.7)) 
  )+
  facet_wrap(~ journal.abbr, scales = "free_y", ncol = 2) +
  labs(
    #title = "Percentage Change Comparison: Pre-2019 vs. Post-2019",
    #subtitle = "Wilcoxon Rank-Sum Test used for significance (p < 0.05)",
    y = "Mean Percentage (per)",
    x = "Period",
    color = "Statistical Outcome"
  ) +
  guides(color = guide_legend(
    override.aes = list(
      linetype = 0,
      shape = 19,
      size = 3 
    )
  ))+
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

p

cairo_pdf("../Figures/BIOGEOGRAPHY/change_per.pdf", width = 10, height = 10) 

print(p) 
dev.off()
