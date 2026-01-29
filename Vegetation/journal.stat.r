library(data.table)
library(stringr)
library(stringi)
library(patchwork)

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
journals<-readRDS("../Data/Vegetation/vegetation.journal.rda")
source("Download.PDF/getArticles.r")
source("Download.PDF/check.journal.pdf.r")
source("Download.PDF/download.journal.pdf.r")
i=1
crossref.year<-2025
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
all.info<-list()
for (i in c(1:nrow(journals))){
  journal<-journals[i]
  print(journal)
  articles<-getArticles(journal, all_journal_folders)
  articles$pdf.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s",
                             journal$journal, articles$pdf)
  articles$pdf.exist<-file.exists(articles$pdf.path)
  articles$xml.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s/%s",
                             journal$journal, gsub("\\.PDF", "\\.XML", articles$pdf))
  articles$xml.exist<-file.exists(articles$xml.path)
  item<-data.table(journal=journal$journal, ISSN=journal$ISSN, eISSN=journal$eISSN,
                   n.article=nrow(articles),
                   with.pdf=nrow(articles[pdf.exist==T]),
                   with.xml=nrow(articles[xml.exist==T]))
  all.info[[i]]<-item
}

all.info<-rbindlist(all.info)
fwrite(all.info, "../Data/Vegetation/journal.stat.csv")

all.info<-fread("../Data/Vegetation/journal.stat.csv")
#all.info$group<-ifelse(all.info$n.article>5e4, "Multidisciplinary Journals", "Specialized Journals")

p1<-ggplot(all.info[group=="Multidisciplinary Journals"])+
  geom_bar(aes(x=abbr, y=n.article), stat = "identity", fill="#2166AC")+
  geom_bar(aes(x=abbr, y=with.pdf), stat = "identity", fill="#B2182B")+
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  )
p1

p2<-ggplot(all.info[group=="Specialized Journals"])+
  geom_bar(aes(x=abbr, y=n.article), stat = "identity", fill="#2166AC")+
  geom_bar(aes(x=abbr, y=with.pdf), stat = "identity", fill="#B2182B")+
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  )
p2

p<-p1 + p2 + plot_layout(widths = c(1, 9))
ggsave(p, filename="../Figures/Vegetation/journal.stat.pdf", width=10, height=6)

nrow(all.info)
table(all.info$group)
sum(all.info$with.pdf)/ sum(all.info$n.article)

all.info$per<-sprintf("%.2f%%", all.info$with.pdf/all.info$n.article * 100)
View(all.info[, c("abbr", "n.article", "per")])
