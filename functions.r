readArticle<-function(category, all=F){
  articles<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/articles.rda", category))
  #table(articles$language)
  if (all==F){
    articles<-articles[type=="journal-article"]
    #articles<-articles[language=="en" | is.na(language)]
  }
  articles$Year<-format(articles$published, "%Y")
  #articles[is.na(Year)]$Year<-format(articles[is.na(Year)]$published_print, "%Y")
  articles[is.na(Year)]$Year<-format(articles[is.na(Year)]$published_online, "%Y")
  #articles[is.na(Year)]$Year<-format(articles[is.na(Year)]$published_others, "%Y")
  articles$Year<-as.numeric(articles$Year)
  
  #articles[container_title=="Journal of Weed Science and Technology" & is.na(Year)]
  #articles[is.na(Year)]$Year<-format(articles[is.na(Year)]$created_date, "%Y")
  #articles[container_title=="Journal of Weed Science and Technology" & is.na(Year)]
  articles
}

remove_chars <- function(x) {
  x <- gsub("<.*?>", "", x)
  x <- gsub("[^ ]{1,}@[^ ]{1,}", " ",x)
  x <- gsub("@[^ ]{1,}", " ",x)
  x <- gsub("#[^ ]{1,}", " ",x)
  x <- gsub("[^ ]{1,}://[^ ]{1,}", " ",x)
  x
}
remove_symbols <- function(x) {
  x <- gsub("&[^&]*;", " ", x)
  x <- gsub("<[^>]+>", " ",x)
  x <- gsub("[`??????]"," ",x)
  x <- gsub("[^A-Za-z']"," ",x)
  x <- gsub("'{2,}"," ",x)
  x <- gsub("' "," ",x)
  x <- gsub(" '"," ",x)
  x <- gsub("^'"," ",x)
  x <- gsub("'$"," ",x)
  x
}
format_words<-function(mydic, text){
  text<-remove_symbols(text)
  text<-remove_chars(text)
  
  corpusFeeds <- VCorpus(VectorSource(text))
  corpusFeeds <- tm_map(corpusFeeds, removePunctuation) # remove punctuation
  corpusFeeds <- tm_map(corpusFeeds, content_transformer(tolower))  # put in lower char
  corpusFeeds <- tm_map(corpusFeeds, removeWords, stopwords("english")) # remove English stop words
  corpusFeeds <- tm_map(corpusFeeds, stripWhitespace) # remove extra spaces
  corpusFeeds <- tm_map(corpusFeeds, PlainTextDocument)
  
  dfForNGrams <- data.frame(text = sapply(corpusFeeds, as.character), stringsAsFactors = FALSE)
  
  uniGramToken <- NGramTokenizer(dfForNGrams, Weka_control(min = 1, max = 1))
  temp<-stemDocument(uniGramToken)
  past <- mydic$Past[which(mydic$Past %in% temp)]
  inf1 <- mydic$Infinitive[which(mydic$Past %in% temp)]
  ind <- match(temp, past)
  ind <- ind[is.na(ind) == FALSE]
  
  ### Where are the past forms in temp?
  position <- which(temp %in% past)
  
  temp[position] <- inf1[ind]
  paste(temp, collapse = " ")
  
}
get_Tokens<-function(mydic, text, type="Title", doi=doi){
  formatted_text<-format_words(mydic, text)
  # unigram
  uniGramToken<-NGramTokenizer(formatted_text, Weka_control(min = 1, max = 1))
  if (length(uniGramToken)>=1){
    unigram <- data.frame(table(uniGramToken), 
                          N_token=1, doi=doi)
    colnames(unigram)[1:2] <- c("Word", "Frequency")
    unigram <- arrange(unigram, desc(Frequency))
  }else{
    return (NULL)
  }
  
  if (length(uniGramToken)>=2){
    # bigram
    biGramToken <- NGramTokenizer(formatted_text, Weka_control(min = 2, max = 2))
    bigram <- data.frame(table(biGramToken), 
                         N_token=2, doi=doi)
    colnames(bigram)[1:2] <- c("Word", "Frequency")
    bigram <- arrange(bigram, desc(Frequency))
  }else{
    tokens<-unigram
    bigram<-NULL
  }
  
  if (length(uniGramToken)>=3){
    # trigram
    triGramToken <- NGramTokenizer(formatted_text, Weka_control(min = 3, max = 3))
    trigram <- data.frame(table(triGramToken), 
                          N_token=3, doi=doi)
    colnames(trigram)[1:2] <- c("Word", "Frequency")
    trigram <- arrange(trigram, desc(Frequency))
    tokens<-rbindlist(list(unigram, bigram, trigram))
  }else{
    tokens<-rbindlist(list(unigram, bigram))
    trigram<-NULL
  }
  
  
  tokens$type<-type
  tokens
}
getXmlValue<-function(xml, path, type){
  v<-xpathApply(xml, path, xmlValue)
  if (type!="list"){
    if (length(v)==0){
      v<-NA
    }else{
      v<-v[[1]]
    }
    if (type=="character"){
      if (is.na(v)){
        v<-""
      }
    }
    if (type=="numeric"){
      if (is.na(v)){
        v<- -9999
      }else{
        if (v==""){
          v<- -9999
        }
        v<-as.numeric(v)
      }
    }
    if (type=="boolean"){
      if (is.na(v)){
        v<-0
      }else{
        if (v=="Y"){
          v<-1
        }else{
          v<-0
        }
      }
    }
  }
  if (type=="character"){
    #v<-fix_sql(v)
  }
  return (v)
}


getKeyword<-function(node, type){
  attrs<-xmlAttrs(node)
  text<-gsub("'", "’", xmlValue(node))
  if ("UI" %in% names(attrs)){
    UI<-attrs[which(names(attrs)=="UI")]
  }else{
    UI<-""
  }
  if ("MajorTopicYN" %in% names(attrs)){
    MajorTopicYN<-attrs[which(names(attrs)=="MajorTopicYN")]
  }else{
    MajorTopicYN<-""
  }
  if (MajorTopicYN=="Y"){
    MajorTopicYN<-1
  }else{
    MajorTopicYN<-0
  }
  
  return(data.table(UI=UI, MajorTopicYN=MajorTopicYN, text=text, type=type))
}

colors<-c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")