library(ggplot2)
library(data.table)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
source("functions.r")
group<-"Mammals"

groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")
for (group in groups){
  sp_list<-readRDS(sprintf("../Data/IUCN_PDF/Result/assessments_ref_%s.rda", group))
  references_df<-readRDS(sprintf("../Data/IUCN_PDF/Result/references_%s.rda", group))
  
  references_df[grep("IUCN", reference)]
  references_df$label<-"Before 1990"
  references_df[between(reference_year, 1991, 2000)]$label<-"1991 to 2000"
  references_df[between(reference_year, 2001, 2010)]$label<-"2001 to 2010"
  references_df[reference_year>=2011]$label<-"After 2010"
  references_df$label<-factor(references_df$label, 
                              levels=c("After 2010", "2001 to 2010", "1991 to 2000", "Before 1990"))
  references_se<-references_df[, .(N=.N), by=list(label, orderName, scientificName)]
  references_se_mean<-references_se[, .(mean_N=mean(N)), by=list(label, orderName)]
  references_se_all<-references_se_mean[, .(N.all=sum(mean_N)), by=list(orderName)]
  references_se_mean<-merge(references_se_mean, references_se_all, by=c("orderName"))
  setorderv(references_se_mean, "N.all")
  p<-ggplot(references_se_mean)+geom_bar(aes(x=reorder(str_to_title(orderName), N.all),
                                          y=mean_N, fill=label), stat = "identity")+
    labs(x="x", y="Mean publications per species", fill="fill")+
    scale_fill_manual(values=colors[c(4, 6, 7, 8)], 
                      breaks=c("After 2010", "2001 to 2010", "1991 to 2000", "Before 1990"))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank())
  ggsave(p, filename=sprintf("../Figures/IUCN/Bars/Fig.mean_by_order.%s.png", group),
         width=8, height=5, bg = "white")
  
  references_se<-references_df[, .(N=.N), by=list(label, familyName, scientificName)]
  references_se_mean<-references_se[, .(mean_N=mean(N)), by=list(label, familyName)]
  references_se_all<-references_se_mean[, .(N.all=sum(mean_N)), by=list(familyName)]
  references_se_mean<-merge(references_se_mean, references_se_all, by=c("familyName"))
  setorderv(references_se_mean, "N.all")
  p<-ggplot(references_se_mean)+geom_bar(aes(x=reorder(str_to_title(familyName), N.all),
                                             y=mean_N, fill=label), stat = "identity")+
    labs(x="x", y="Mean publications per species", fill="fill")+
    scale_fill_manual(values=colors[c(4, 6, 7, 8)], 
                      breaks=c("After 2010", "2001 to 2010", "1991 to 2000", "Before 1990"))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          
          legend.title = element_blank())
  ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.mean_by_family.%s.png", group),
         width=15, height=7, bg = "white")
  realms<-readRDS(sprintf("../Data_IUCN_References/REALMS/%s.rda", group))
  references_se<-references_df[, .(N=.N), by=list(orderName, scientificName)]
  
  references_se_with_realm<-merge(references_se, realms, by.x="scientificName", by.y="species.name")
  references_se_with_realm_mean<-references_se_with_realm[, .(mean_N=mean(N)), by=list(orderName, realm)]
  references_se_all<-references_se_with_realm_mean[, .(N.all=sum(mean_N)), by=list(orderName)]
  references_se_with_realm_mean<-merge(references_se_with_realm_mean, references_se_all, by=c("orderName"))
  setorderv(references_se_with_realm_mean, "N.all")
  
  p<-ggplot(references_se_with_realm_mean)+geom_bar(aes(x=reorder(str_to_title(orderName), N.all),
                                             y=mean_N, fill=realm), stat = "identity", 
                                             position = position_dodge2(width=1, preserve = "single"))+
                                             #position = "dodge")+
    labs(x="x", y="Mean publications per species", fill="fill")+
    scale_fill_manual(values=colors, 
                      breaks=unique(references_se_with_realm_mean$realm))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank())
  ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.mean_by_order_realm.%s.png", group),
         width=15, height=7, bg = "white")
  
  
  references_N<-references_df[,.(N_Speices=length(unique(scientificName)),
                                 N_References=length(unique(reference))),
                              by=list(orderName)]
  ggplot(references_N)+geom_point(aes(x=))
}
