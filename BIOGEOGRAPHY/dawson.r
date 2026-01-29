library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

xxx<-readRDS("../Data/BIOGEOGRAPHY/doi_10_5061_dryad_p5hqbzkst__v20221226/wos_results_biog.rds")
authors<-data.table(xxx$author)
address<-data.table(xxx$address)
author_address<-data.table(xxx$author_address)
article_type<-data.table(xxx$doc_type)
final_author_address <- address[author_address[authors, on = .(ut, author_no)], on = .(ut, addr_no)]


publication<-xxx$publication
publication$abstract<-NULL
publication<-data.table(publication)

publication_type<-publication[article_type, on=.(ut)]

#check the papers with multi types
publication_type_N<-publication_type[,.(N=.N), by=(ut)]
publication_type_N[N>1]

arcitle<-publication_type[doc_type %in% c("Article")]

arcitle$year<-year(arcitle$date) 



arcitle$journal_group<-arcitle$journal
arcitle[!journal_group %in% 
            c("Journal of Biogeography", "Ecography", 
              "Diversity and Distributions", "Global Ecology and Biogeography"),
          journal_group:="Others"]

arcitle_N<-arcitle[,.(N=.N), by=c("journal_group", "year")]
arcitle_N<-arcitle_N[journal_group!="Others"]
range(arcitle_N$year)

arcitle_N <- arcitle_N[CJ(year = unique(arcitle_N$year), journal_group = unique(arcitle_N$journal_group)), 
         on = .(year, journal_group)]
arcitle_N[is.na(N), N := 0] 

p<-ggplot(arcitle_N, aes(x = year, y = N, fill = journal_group)) +
 geom_area(alpha = 0.85, color = "white", linewidth = 0.2) + 
  
  scale_fill_manual(values = c(
    "Journal of Biogeography" = "#CC79A7", 
    "Ecography"               = "#56B4E9", 
    "Global Ecology and Biogeography"          = "#009E73", 
    "Diversity and Distributions"     = "#E69F00"
  )) +
  
  scale_x_continuous(breaks = seq(1970, 2025, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  labs(x = "Year", 
       y = "Published manuscripts", 
       fill = "Journal") + # 图例标题
  
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )
ggsave(p, filename="../Figures/BIOGEOGRAPHY/Dawson.2023.pdf", width=8, height=6)
