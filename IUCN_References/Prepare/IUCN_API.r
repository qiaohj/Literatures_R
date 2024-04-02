library(data.table)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")

group<-"Reptiles"
sp_list<-fread(sprintf("../Data_IUCN_References/IUCN_PDF/%s/Species_List/assessments.csv", group))
colnames(sp_list)
session<-"aHRjRzdiQ2ZId3M4Tjg2N281UC81TDhoN0ttSHlTM1I5bXlsR0NROWpCbmlESjNIekNxWFh2WGZaUEx4M1dWbnFCY2F2Q0FTRTZRaHlYSDI5NGxGY2F0ZXp3cTJTb25VUWxIRjFBK1g0a2k5WnpHYWNjVS9SUGVuNkVkSkhWeEtpVGFCaDRlTGNSbVJYTk51OHQ3eXE0dEJYdE9LV3l1aC9jTmVyMDcvOGJKVjFQTnB5UTNnS0dxMWNWMWw2eGd0K3BmQkRKNVhFYzVYVTBGMGhZNU9iZz09LS02MytIMU01UDAvSXdWY2liSkErRDdBPT0%3D--e592b40d7c65493381c8a3a04a783d24b3b7248b;"
template<-"curl 'https://www.iucnredlist.org/api/v4/species/%d' --compressed -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:124.0) Gecko/20100101 Firefox/124.0' -H 'Accept: application/json, text/plain, */*' -H 'Accept-Language: en-US,en;q=0.5' -H 'Accept-Encoding: gzip, deflate, br' -H 'Referer: https://www.iucnredlist.org/species/%d/%d' -H 'X-CSRF-Token: 3RUO46lEmpznIXuW40Z21uvPSRNoJQHJIFZ/CPJeWJcOJt2u35LvyTPmCVse6VX8pN+UDlL0EGqnezYSdFTugw==' -H 'Connection: keep-alive' -H 'Cookie: _application_devise_session=%s _ga_66YGJWTDLZ=GS1.1.1711810212.2.1.1711810392.0.0.0; _ga=GA1.2.1527067139.1711807059; _ga_T3K17G40FC=GS1.1.1711810212.2.1.1711810393.0.0.0; _ga_6HJED7KZLB=GS1.1.1711810212.2.1.1711810393.0.0.0; _gid=GA1.2.1831266670.1711807065; _gat_gtag_UA_11409245_4=1; _gat_gtag_UA_11409245_5=1; _gat_gtag_UA_11409245_6=1' -H 'Sec-Fetch-Dest: empty' -H 'Sec-Fetch-Mode: cors' -H 'Sec-Fetch-Site: same-origin' -H 'Pragma: no-cache' -H 'Cache-Control: no-cache' -H 'TE: trailers' > %s"
i=1
for (i in c(1:nrow(sp_list))){
  
  print(paste(i, nrow(sp_list), group))
  item<-sp_list[i]
  target<-sprintf("../Data_IUCN_References/IUCN_API/%s/APIs/%d.json", group, item$assessmentId)
  if (!file.exists(target)){
    url<-sprintf(template, item$assessmentId, item$internalTaxonId, item$assessmentId, session, target)
    system(url)
  }
}


df<-readRDS("/media/huijieqiao/WD22T_11/literatures/Data/CrossRef_By_Journal/0006/0006-128X/articles.rda")
View(df)
