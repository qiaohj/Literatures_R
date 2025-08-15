#usethis::edit_r_environ()
library(reticulate)
library(httr)
library(data.table)
library(xml2)
library(stringr)
library(stringi)
library(zoo)
library(pdftools)
library(readr)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")


source("RAG.LLM/prompt.r")

args = commandArgs(trailingOnly=TRUE)
api.index<-as.numeric(args[1])
journal<-args[2]
if (is.na(api.index)){
  api.index<-2
}
if (is.na(journal)){
  stop("Must specify a journal name.")
}

apis<-c(Sys.getenv("gemini.key.1"),
        Sys.getenv("gemini.key.2"),
        Sys.getenv("gemini.key.3"))
gemini.key<-apis[api.index]
print(sprintf("Using No.%d api (%s) for %s.",
              api.index, gemini.key, journal))

Sys.setenv("http_proxy"="http://127.0.0.1:7897")
Sys.setenv("https_proxy"="http://127.0.0.1:7897")
Sys.setenv("all_proxy"="http://127.0.0.1:7897")
rep<-GET("https://google.com")
rep

Sys.getenv("http_proxy")
Sys.getenv("https_proxy")
Sys.getenv("all_proxy")
Sys.getenv("no_proxy")

#conda activate rag.literature
use_condaenv("rag.literature", required = TRUE)

py_run_string("import os; print(os.environ.get('http_proxy'))")
py_run_string("import requests; print(requests.get('https://google.com').status_code)")

google_genai <- import("google.generativeai")
google_genai$configure(api_key = gemini.key)


safety_settings <- list(
  dict(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_SEXUAL", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
)

gen_config <- list(
  temperature = 0.1,
  top_p = 0.1,
  max_output_tokens = 100000L
)



models<-c("gemini-2.5-pro", "gemini-2.5-flash")
mstr<-models[1]
model <- google_genai$GenerativeModel(mstr,
                                      generation_config=gen_config,
                                      safety_settings = safety_settings,
                                      system_instruction = system_instruction)


#journal<-"SEED SCIENCE RESEARCH"


csv.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/CSV/%s", journal)
texts<-list.files(csv.folder, pattern="\\.TXT")
log.file<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/LOG/%s.rda", journal)
merged.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/MERGED.CSV/%s", journal)
if (!dir.exists(merged.folder)){
  dir.create(merged.folder)
}

llm.result.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/LLM.RESULT/%s", journal)
if (!dir.exists(llm.result.folder)){
  dir.create(llm.result.folder)
}

llm.response.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/LLM.RESPONSE/%s", journal)
if (!dir.exists(llm.response.folder)){
  dir.create(llm.response.folder)
}


if (file.exists(log.file)){
  log<-readRDS(log.file)
}else{
  log<-data.table(file.name=texts, finished=F, journal=journal, merged.id=0)
  saveRDS(log, log.file)
}
max_token<-250000
token_per_quest<-100000
chat<-model$start_chat()


c.merged.id<-max(log$merged.id)+1
query<-""
index.list<-c()
for (i in c(1:nrow(log))){
  item<-log[i]
  if (item$finished==T){
    next()
  }
  index.list<-c(index.list, i)
  content<-read_file(sprintf("%s/%s", csv.folder, item$file.name))
  query<-str_c(query, content, collapse = "\n")
  n.token<-model$count_tokens(query)
  n.token<-n.token$total_tokens
  if (n.token>=token_per_quest){
    if (length(chat$history)!=0){
      total.tokens<-model$count_tokens(chat$history)
      total.tokens<-total.tokens$total_tokens
      print(sprintf("Tokens in current chat are %d.", total.tokens))
      if (total.tokens>=(max_token-token_per_quest)){
        print(sprintf("Almost reach to the upper limit tokens (%d/%d) in a thread, renew a chat.", total.tokens, max_token))
        chat<-model$start_chat()
      }
    }
    print(sprintf("Sending request from %d to %d of %d with %d tokens.", min(index.list), max(index.list), nrow(log), n.token))
    print(system.time({
      chat$send_message(query)
    }))
    response<-chat$history[[length(chat$history)]]
    dt<-read_csv(response$parts[[0]]$text, col_types = cols(.default = "c"))
    saveRDS(dt, sprintf("%s/%d.rda", llm.result.folder, c.merged.id))
    chat_history_r <- lapply(chat$history, function(content) {
      list(
        role = content$role,
        text = content$parts[[0]]$text
      )
    })
    saveRDS(chat_history_r, sprintf("%s/%d.rda", llm.response.folder, c.merged.id))
    saveRDS(query, sprintf("%s/%d.rda", merged.folder, c.merged.id))
    
    log[index.list, `:=`(c("finished", "merged.id"),
                         list(TRUE, c.merged.id))]
    saveRDS(log, log.file)
    
    c.merged.id<-c.merged.id+1
    query<-""
    index.list<-c()
    
    
  }
}
