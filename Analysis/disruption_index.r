library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
setDTthreads(5)
category<-"Ecology"
#category<-"Biodiversity Conservation"
focus_ref<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/article_references.rda", category))
other_ref<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/citation_network.rda", category))

citations_100<-rbindlist(list(focus_ref, other_ref))
citations_100<-unique(citations_100)
if (F){
  full_article<-readRDS("../Data/CrossRef_Full/articles.rda")
  
  focus_N_citations<-N_citations[ref_DOI %in% focus_ref$article_DOI]
  focus_N_citations[N>1000]
  t_doi<-"10.1007/s12686-011-9548-7"
  results_df[doi==t_doi]
  
  cited_by<-citations_100[ref_DOI==t_doi]
  citations<-focus_ref[article_DOI ==t_doi]
  citations_citations<-citations_100[ref_DOI %in% citations$ref_DOI]
  cited_by_doi<-unique(cited_by$article_DOI)
  citations_doi<-unique(citations$ref_DOI)
  citations_doi<-citations_doi[citations_doi!=t_doi]
  citations_citations_doi<-unique(citations_citations$article_DOI)
  citations_citations_doi<-citations_citations_doi[citations_citations_doi!=t_doi]
  f<-0
  b<-0
  r<-0
  for (doi_item in cited_by_doi){
    item_citations<-unique(citations_100[article_DOI==doi_item]$ref_DOI)
    v2<-length(citations_doi[citations_doi %in% item_citations])
    if (v2>0){
      b<-b+1
    }else{
      f<-f+1
    }
  }
  
  f<-length(unique(cited_by[!(article_DOI %in% citations_citations$article_DOI)]$article_DOI))
  b<-length(unique(cited_by$article_DOI))-f
  
  articles<-full_article[doi %in% cited_by$article_DOI]
  
}


N_citations<-citations_100[,.(N=.N), by=ref_DOI]
quantile<-quantile(N_citations$N, c(0.90, 0.95, 0.99))

citations_90<-citations_100[ref_DOI %in% N_citations[N<=quantile[1]]$ref_DOI]
citations_95<-citations_100[ref_DOI %in% N_citations[N<=quantile[2]]$ref_DOI]
citations_99<-citations_100[ref_DOI %in% N_citations[N<=quantile[3]]$ref_DOI]

citation_list<-list("p90"=citations_90,
                    "p95"=citations_95,
                    "p99"=citations_99,
                    "p100"=citations_100)

focus_id<-unique(focus_ref$article_DOI)

focus_id<-focus_id[sample(length(focus_id), length(focus_id))]

target<-sprintf("../Data/CrossRef_By_Category/%s/di_items", category)
for (i in c(1:length(focus_id))){
  print(paste(i, length(focus_id)))
  
  doi<-focus_id[i]
  focus_item<-other_ref[ref_DOI==doi]
  citation_N<-length(unique(focus_item$article_DOI))
  if (citation_N<50){
    next()
  }
  target_f<-sprintf("%s/%s.rda", target, gsub("-", "", gsub("\\.", "", gsub("\\/", "", doi))))
  if (file.exists(target_f)){
    next()
  }
  saveRDS(NULL, target_f)
  results<-list()
  for (p in names(citation_list)){
    print(p)
    citations<-citation_list[[p]]
    if (!doi %in% citations$ref_DOI){
      print("Add focus paper's citation to the list")
      citations<-unique(rbindlist(list(citations, focus_item)))
    }
    citation<-citations[article_DOI==doi]
    citation_doi<-unique(citation$ref_DOI)
    cited_by_doi<-unique(citations[ref_DOI==doi]$article_DOI)
    f<-0
    b<-0
    for (doi2 in cited_by_doi){
      item_doi<-unique(citations[article_DOI==doi2]$ref_DOI)
      if (length(item_doi)==1){
        f<-f+1
      }else{
        b<-b+1
      }
      
    }
    
    citation_citation<-citations[ref_DOI %in% citation_doi]
    citation_citation_doi<-unique(citation_citation$article_DOI)
    
    r<-0
    for (doi2 in citation_citation_doi){
      
      if (doi2==doi){
        next()
      }
      
      item_doi<-unique(citations[article_DOI==doi2]$ref_DOI)
      item_doi2<-unique(citation[ref_DOI %in% item_doi]$article_DOI)
      
      
      if (doi %in% item_doi){
        
      }else{
        r<-r+1
      }
    }
    
    di<-(f-b)/(f+b+r)
    print(paste(p, ":", di))
    result_item<-data.table(doi=doi, f=f, b=b, r=r, di=di, N_cited=citation_N,
                            N_citation=length(citation_doi), 
                            N_citation_citation=length(citation_citation_doi),
                            threhold=p)
    results[[length(results)+1]]<-result_item
  }
  results_df<-rbindlist(results)
  saveRDS(results_df, target_f)
  
}
if (F){
  #category<-"Ecology"
  category<-"Biodiversity Conservation"
  target<-sprintf("../Data/CrossRef_By_Category/%s/di_items", category)
  
  results_df<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/disruption_index.rda", category))
  dois<-unique(results_df$doi)
  for (subdoi in dois){
    item<-results_df[doi==subdoi]
    target_f<-sprintf("%s/%s.rda", target, gsub("-", "", gsub("\\.", "", gsub("\\/", "", subdoi))))
    saveRDS(item, target_f)
  }
}
if (F){
  #category<-"Ecology"
  category<-"Biodiversity Conservation"
  target<-sprintf("../Data/CrossRef_By_Category/%s/di_items", category)
  files<-list.files(target, pattern="\\.rda")
  results<-list()
  for (f in files){
    item<-readRDS(sprintf("%s/%s", target, f))
    results[[length(results)+1]]<-item
  }
  results_df<-rbindlist(results)
  saveRDS(results_df, sprintf("../Data/CrossRef_By_Category/%s/disruption_index.rda", category))
  
  library(ggplot2)
  hist(results_df$di)
  range(results_df$di)
  ggplot(results_df)+geom_point(aes(x=di, y=N_cited))+
    facet_wrap(~threhold)
  
  
  dff<-results_df[threhold=="p95"]
  dff_1k<-dff[N_cited>=1000]
  median(dff_1k$di)
  dff$N_cited_bin<-floor(dff$N_cited/100)
  ddd<-dff[, .(mean_di=mean(di),
               median=median(di)),
           by=N_cited_bin]
  
  plot(ddd$N_cited_bin, ddd$mean_di)
  plot(ddd$N_cited_bin, ddd$median)
}