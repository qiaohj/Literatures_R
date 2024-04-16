library(ggplot2)
library(data.table)
library(stringr)
library(ggrepel)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
source("functions.r")
group<-"Mammals"

groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")

duplicated_references<-readRDS("../Data_IUCN_References/References/duplicated_reference.rda")

configs<-list("Amphibians"=data.table(width=c(8, 12, 15, 15, 10, 10, 10), 
                                      height=c(5, 7, 7, 7, 8, 8, 8), 
                                      figure=c("mean_by_order",
                                               "mean_by_family",
                                               "mean_by_order_realm",
                                               "mean_by_order_realm_single",
                                               "species_vs_order",
                                               "species_vs_family",
                                               "mass_vs_order")),
              "Birds"=data.table(width=c(8, 15, 15, 15, 10, 10, 10), 
                                 height=c(5, 6, 7, 7, 8, 8, 8), 
                                 figure=c("mean_by_order",
                                          "mean_by_family",
                                          "mean_by_order_realm",
                                          "mean_by_order_realm_single",
                                          "species_vs_order",
                                          "species_vs_family",
                                          "mass_vs_order")),
              "Mammals"=data.table(width=c(8, 12, 15, 15, 10, 10, 10), 
                                   height=c(5, 7, 7, 7, 8, 8, 8), 
                                   figure=c("mean_by_order",
                                            "mean_by_family",
                                            "mean_by_order_realm",
                                            "mean_by_order_realm_single",
                                            "species_vs_order",
                                            "species_vs_family",
                                            "mass_vs_order")),
              "Odonata"=data.table(width=c(8, 12, 15, 15, 10, 10, 10), 
                                   height=c(5, 7, 7, 7, 8, 8, 8), 
                                   figure=c("mean_by_order",
                                            "mean_by_family",
                                            "mean_by_order_realm",
                                            "mean_by_order_realm_single",
                                            "species_vs_order",
                                            "species_vs_family",
                                            "mass_vs_order")),
              "Reptiles"=data.table(width=c(8, 12, 15, 15, 10, 10, 10), 
                                    height=c(5, 7, 7, 7, 8, 8, 8), 
                                    figure=c("mean_by_order",
                                             "mean_by_family",
                                             "mean_by_order_realm",
                                             "mean_by_order_realm_single",
                                             "species_vs_order",
                                             "species_vs_family",
                                             "mass_vs_order")))
for (group in groups){
  print(group)
  references_df<-readRDS(sprintf("../Data_IUCN_References/References/references_%s_cleaned.rda", group))
  fwrite(references_df, sprintf("../Data_IUCN_References/CSV/references_%s_cleaned.csv", group))
}
group<-"Birds"
for (group in groups){
  print(group)
  config<-configs[[group]]
  references_df<-readRDS(sprintf("../Data_IUCN_References/References/references_%s_cleaned.rda", group))
  references_df<-references_df[is_IUCN==FALSE]
  references_df[grep("IUCN", reference)]
  references_df$label<-"Before 1990"
  references_df[between(reference_year, 1991, 2000)]$label<-"1991 to 2000"
  references_df[between(reference_year, 2001, 2010)]$label<-"2001 to 2010"
  references_df[reference_year>=2011]$label<-"After 2010"
  references_df$label<-factor(references_df$label, 
                              levels=c("After 2010", "2001 to 2010", "1991 to 2000", "Before 1990"))
  references_df<-merge(references_df, 
                         duplicated_references[, c("group", "reference_clean")], 
                         by.x="reference_clean",
                         by.y="reference_clean",
                         all.x=T, all.y=F)
  max_group<-max(references_df$group, na.rm = T)
  references_df[is.na(group)]$group<-c((max_group+1):(max_group+nrow(references_df[is.na(group)])))
  
  references_se<-references_df[, .(N=.N), by=list(label, orderName, scientificName)]
  #references_se<-references_df[, .(N=length(unique(group))), by=list(label, orderName, scientificName)]
  
  references_se_mean<-references_se[, .(mean_N=mean(N)), by=list(label, orderName)]
  references_se_all<-references_se_mean[, .(N.all=sum(mean_N)), by=list(orderName)]
  references_se_mean<-merge(references_se_mean, references_se_all, by=c("orderName"))
  setorderv(references_se_mean, "N.all")
  p<-ggplot(references_se_mean)+geom_bar(aes(x=reorder(str_to_title(orderName), N.all),
                                          y=mean_N, fill=label), stat = "identity")+
    labs(x="x", y="Mean publications per species", fill="fill", title=group)+
    scale_fill_manual(values=colors[c(4, 6, 7, 8)], 
                      breaks=c("After 2010", "2001 to 2010", "1991 to 2000", "Before 1990"))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank())
  fwrite(references_se_mean, sprintf("../Figures_IUCN/IUCN/CSV/Fig.mean_by_order.%s.csv", group))
  ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.mean_by_order.%s.png", group),
         width=config[figure=="mean_by_order"]$width, 
         height=config[figure=="mean_by_order"]$height, 
         bg = "white")
  
  #references_se<-references_df[, .(N=length(unique(group))), by=list(label, familyName, scientificName)]
  references_se<-references_df[, .(N=.N), by=list(label, familyName, scientificName)]
  references_se_mean<-references_se[, .(mean_N=mean(N)), by=list(label, familyName)]
  references_se_all<-references_se_mean[, .(N.all=sum(mean_N)), by=list(familyName)]
  references_se_mean<-merge(references_se_mean, references_se_all, by=c("familyName"))
  setorderv(references_se_mean, "N.all")
  p<-ggplot(references_se_mean)+geom_bar(aes(x=reorder(str_to_title(familyName), N.all),
                                             y=mean_N, fill=label), stat = "identity")+
    labs(x="x", y="Mean publications per species", fill="fill", title=group)+
    scale_fill_manual(values=colors[c(4, 6, 7, 8)], 
                      breaks=c("After 2010", "2001 to 2010", "1991 to 2000", "Before 1990"))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          
          legend.title = element_blank())
  fwrite(references_se_mean, sprintf("../Figures_IUCN/IUCN/CSV/Fig.mean_by_family.%s.csv", group))
  ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.mean_by_family.%s.png", group),
         width=config[figure=="mean_by_family"]$width, 
         height=config[figure=="mean_by_family"]$height, 
         bg = "white")
  
  
  realms<-readRDS(sprintf("../Data_IUCN_References/REALMS/%s_IUCN.rda", group))
  realms$assessmentId<-NULL
  references_df$is_single_realm<-!grepl("\\|", references_df$realm)
  references_se<-references_df[, .(N=.N), by=list(internalTaxonId, assessmentId, scientificName, orderName, is_single_realm)]
  
  references_se_with_realm<-merge(references_se, realms, 
                                  by.x=c("internalTaxonId", "scientificName"), 
                                  by.y=c("internalTaxonId", "species.name"))
  references_se_with_realm_mean<-references_se_with_realm[, .(mean_N=mean(N)), by=list(orderName, realm)]
  references_se_all<-references_se_with_realm_mean[, .(N.all=sum(mean_N)), by=list(orderName)]
  references_se_with_realm_mean<-merge(references_se_with_realm_mean, references_se_all, by=c("orderName"))
  setorderv(references_se_with_realm_mean, "N.all")
  
  p<-ggplot(references_se_with_realm_mean)+geom_bar(aes(x=reorder(str_to_title(orderName), N.all),
                                             y=mean_N, fill=realm), stat = "identity", 
                                             position = position_dodge2(width=1, preserve = "single"))+
                                             #position = "dodge")+
    labs(x="x", y="Mean publications per species", fill="fill", title=sprintf("%s (all realms)", group))+
    scale_fill_manual(values=colors, 
                      breaks=unique(references_se_with_realm_mean$realm))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank())
  fwrite(references_se_with_realm_mean, sprintf("../Figures_IUCN/IUCN/CSV/Fig.mean_by_order_realm.%s.csv", group))
  ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.mean_by_order_realm.%s.png", group),
         width=config[figure=="mean_by_order_realm"]$width, 
         height=config[figure=="mean_by_order_realm"]$height, 
         bg = "white")
  
  references_se_with_realm<-merge(references_se[is_single_realm==T], realms, 
                                  by.x=c("internalTaxonId", "scientificName"), 
                                  by.y=c("internalTaxonId", "species.name"))
  references_se_with_realm_mean<-references_se_with_realm[, .(mean_N=mean(N)), by=list(orderName, realm)]
  references_se_all<-references_se_with_realm_mean[, .(N.all=sum(mean_N)), by=list(orderName)]
  references_se_with_realm_mean<-merge(references_se_with_realm_mean, references_se_all, by=c("orderName"))
  setorderv(references_se_with_realm_mean, "N.all")
  
  p<-ggplot(references_se_with_realm_mean)+geom_bar(aes(x=reorder(str_to_title(orderName), N.all),
                                             y=mean_N, fill=realm), stat = "identity", 
                                             position = position_dodge2(width=1, preserve = "single"))+
                                             #position = "dodge")+
    labs(x="x", y="Mean publications per species", fill="fill", title=sprintf("%s (single realm only)", group))+
    scale_fill_manual(values=colors, 
                      breaks=unique(references_se_with_realm_mean$realm))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank())
  fwrite(references_se_with_realm_mean, 
         sprintf("../Figures_IUCN/IUCN/CSV/Fig.mean_by_order_realm_single.%s.csv", group))
  ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.mean_by_order_realm_single.%s.png", group),
         width=config[figure=="mean_by_order_realm_single"]$width, 
         height=config[figure=="mean_by_order_realm_single"]$height, 
         bg = "white")
  
  references_N<-references_df[,.(N_Speices=length(unique(scientificName)),
                                 N_References=length(unique(group))),
                              by=list(orderName)]
  p<-ggplot(references_N, aes(x=N_Speices, y=N_References))+
    geom_point()+
    geom_text_repel(aes(label=str_to_title(orderName)))+
    scale_x_log10()+scale_y_log10()+
    geom_smooth(method="lm")+
    labs(x="Number of species", y="Total publications", title=group)+
    theme_bw()
  fwrite(references_N, 
         sprintf("../Figures_IUCN/IUCN/CSV/Fig.species_vs_order.%s.csv", group))
  ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.species_vs_order.%s.png", group),
         width=config[figure=="species_vs_order"]$width, 
         height=config[figure=="species_vs_order"]$height, 
         bg = "white")
  
  references_N<-references_df[,.(N_Speices=length(unique(scientificName)),
                                 N_References=length(unique(group))),
                              by=list(familyName)]
  references_N$mean<-references_N$N_References/references_N$N_Speices
  #hist(references_N$mean)
  threshold<-quantile(references_N$mean, c(0.1, 0.9))
  p<-ggplot(references_N, aes(x=N_Speices, y=N_References))+
    geom_point()+
    geom_text_repel(data=references_N[mean>threshold[2] | mean<threshold[1]], 
                    aes(label=str_to_title(familyName)))+
    scale_x_log10()+scale_y_log10()+
    geom_smooth(method="lm")+
    labs(x="Number of species", y="Total publications", title=group)+
    theme_bw()
  fwrite(references_N, 
         sprintf("../Figures_IUCN/IUCN/CSV/Fig.species_vs_family.%s.csv", group))
  ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.species_vs_family.%s.png", group),
         width=config[figure=="species_vs_family"]$width, 
         height=config[figure=="species_vs_family"]$height, 
         bg = "white")
  if (group %in% c("Birds", "Mammals")){
    biomass<-fread(sprintf("../Data_IUCN_References/biomass/%s.csv", group))
    biomass<-biomass[, .(BodyMass=mean(BodyMass)), by=list(scientificName)]
    biomass_ref<-merge(biomass, references_df, by=c("scientificName"))
    biomass_ref_se<-biomass_ref[, .(N=.N, BodyMass=mean(BodyMass)), 
                                by=list(scientificName, orderName)]
    biomass_ref_se<-biomass_ref_se[, .(N=mean(N), sd_N=sd(N),
                                       BodyMass=mean(BodyMass), 
                                       sd_BodyMass=sd(BodyMass)),
                                   by=list(orderName)]
    biomass_ref_se[orderName==toupper("carnivora")]
    p<-ggplot(biomass_ref_se, aes(x=N, y=BodyMass))+
      geom_point()+
      geom_text_repel(aes(label=str_to_title(orderName)))+
      scale_x_log10()+scale_y_log10()+
      geom_smooth(method="lm")+
      labs(x="Mean mass (g)", y="Mean publications", title=group)+
      theme_bw()
    fwrite(biomass_ref_se, 
           sprintf("../Figures_IUCN/IUCN/CSV/Fig.mass_vs_order.%s.csv", group))
    ggsave(p, filename=sprintf("../Figures_IUCN/IUCN/Bars/Fig.mass_vs_order.%s.png", group),
           width=config[figure=="mass_vs_order"]$width, 
           height=config[figure=="mass_vs_order"]$height, 
           bg = "white")
  }
}
