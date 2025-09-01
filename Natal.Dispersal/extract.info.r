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
library(pdftools)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

args = commandArgs(trailingOnly=TRUE)
api.index<-as.numeric(args[1])
if (is.na(api.index)){
  api.index<-1
}

apis<-c()
for (ikey in c(1:42)){
  apis<-c(apis, Sys.getenv(sprintf("gemini.key.%d", ikey)))
}
gemini.key<-apis[api.index]

Sys.setenv("http_proxy"="http://127.0.0.1:7897")
Sys.setenv("https_proxy"="http://127.0.0.1:7897")
Sys.setenv("all_proxy"="http://127.0.0.1:7897")

if (F){
  rep<-GET("https://google.com")
  rep
  py_run_string("import os; print(os.environ.get('http_proxy'))")
  py_run_string("import requests; print(requests.get('https://google.com').status_code)")
  
}

use_condaenv("rag.literature", required = TRUE)

google_genai <- import("google.generativeai")
google_genai$configure(api_key = gemini.key)

safety_settings <- list(
  dict(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_SEXUAL", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
)

gen_config <- list(
  temperature = 1,
  top_p = 0.95,
  max_output_tokens = 100000L
)

models<-c("gemini-2.5-pro", "gemini-2.5-flash")
mstr<-models[2]



prompt_ocr<-"接下来我会上传给你一个PDF文件，是一篇学术论文。请将这篇论文的不同部分拆分成题目（Title），摘要（Abstract），
作者（Author），关键词（Keyword），介绍（Introduction），方法（Method）、结果（Result）、讨论（Discussion）、
结论（Conclusion）、致谢（Acknowledgement）、文献（Reference），表格（Table）和图（Figure）和其它（所有不属于上面的类型）。
返回结果请用json格式，以方便我后期用程序处理。返回结果中只包含json内容，不要加入任何你的思考和对话语句。也不要加入`或者json开头的内容。
表格的处理方式，请用CSV来表示，当出现空白内容请用N/A来填充，出现合并单元格的情况时，请拆分单元格，并复制合并前的内容。
图的处理方式，请尽量解析图中的内容，如果图是情景图，直接用图注来代替，如果是流程图，请用文字描述流程，如果是图表图，请尽量提取图中的数据形成数据表，数据表的表现形式同“表格的处理方式”。"

model <- google_genai$GenerativeModel(mstr,
                                      generation_config=gen_config,
                                      safety_settings = safety_settings,
                                      system_instruction = prompt_ocr)

#chat<-model$start_chat()
#chat$send_message(uploaded_file)


pdf_path<-"/media/huijieqiao/WD22T_11/literatures/Data/Natal.Dispersal/pdf/JOURNAL OF ANIMAL ECOLOGY/1365-2656.12017.PDF"
display_name<-basename(pdf_path)
uploaded_file <- google_genai$upload_file(path = pdf_path, display_name = display_name)
message(sprintf("   File uploaded successfully. Name: %s", uploaded_file$name))
print(system.time({
  message(sprintf("2. Generating content with Gemini (%s)...", mstr))
  response <- model$generate_content(uploaded_file)
}))

saveRDS(response, "XXXX")

extracted_text <- tryCatch({response$text},
                           error = function(e) {
                             message("Error: ", e$message)
                             return("")
                           },
                           warning = function(w) {
                             message("Warning: ", w$message)
                             return("")
                           },
                           finally = {
                             
                           })

library(jsonlite)
parsed_data <- fromJSON(gsub("json", "", gsub("`", "", extracted_text)))
parsed_data$Method
parsed_data$Result
parsed_data$Figure
parsed_data$Table
