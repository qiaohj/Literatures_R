library(data.table)
library(reticulate)
library(googleAuthR)
library(httr)
library(gemini.R)
library(stringdist)
library(progress)
library(jsonlite)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
use_condaenv("rag.literature", required = TRUE)
#usethis::edit_r_environ()
setAPI(Sys.getenv("gemini.key"))
#gemini("Explain about the gemini in astrology in one line")

chromadb <- import("chromadb")
google_genai <- import("google.generativeai")
google_genai$configure(api_key = )
gen_config <- list(
  temperature = 0.2,
  max_output_tokens = 8000L
)
model <- google_genai$GenerativeModel("gemini-2.5-pro",
                                      generation_config=gen_config)
extracted_all<-readRDS("../Temp/extracted_all.rda")
extracted_all$ID<-c(1:nrow(extracted_all))
extracted_item<-extracted_all[file.name=="1591-New Phytologist"]
extracted_item<-extracted_item[canonical_name %in% 
                                c("Conclusion", "Discussion", "Introduction", "Methods", "Results")|
                                 label %in% c("title", "abstract")]

text<-paste0(extracted_item$text, collapse = " ")

source("RAG.LLM/prompt.r")
doc_text<-sprintf(prompt_template_cn, text)
#model <- google_genai$GenerativeModel("gemini-2.5-pro")

response <- model$generate_content(doc_text)
raw_text <- response$text
response.pdf <- model$generate_content(list(prompt_template_cn, 
                                            "/media/huijieqiao/WD22T_11/literatures/Temp/TEST/1591-New Phytologist.pdf"))
raw_pdf <- response.pdf$text


dt <- fread(text = raw_pdf, sep = ",", quote = "\"")

View(dt)

if (F){
  embed_text <- function(text_to_embed) {
    result <- google_genai$embed_content(
      model = "models/embedding-001",
      content = text_to_embed,
      task_type = "RETRIEVAL_DOCUMENT" 
    )
    return(result$embedding)
  }
  
  extracted_all<-readRDS("../Temp/extracted_all.rda")
  extracted_all$ID<-c(1:nrow(extracted_all))
  
  persist_dir<-"/home/huijieqiao/chroma/test_chroma_db/"
  client <- chromadb$PersistentClient(path = persist_dir)
  
  #client$delete_collection("text_collection")
  collection <- client$get_or_create_collection(
    name = "text_collection",
    metadata = list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  collection$count()
  
  extracted_item<-extracted_all[canonical_name %in% 
                                  c("Conclusion", "Discussion", "Introduction", "Methods", "Results")]
  extracted_item<-extracted_item[word.length>100]
  documents <- extracted_item$text
  embeddings_to_add <- embed_text(documents)
  metadatas <- lapply(extracted_item$ID, function(x) list(ID = x))
  ids <- as.character(extracted_item$ID)
  
  
  collection$add(
    embeddings = embeddings_to_add,
    documents = as.list(documents),
    metadatas = metadatas,
    ids = ids
  )
}

semantic_query = "plant characteristics measurement"
fuzzy_terms = c("seed weight", "plant hieght")
max_distance = 1
match_all = FALSE
method = "lv"

n_candidates<- as.integer(length(documents))
query_embedding <- embed_text(semantic_query)
candidate_results <- collection$query(
  query_embeddings = list(query_embedding),
  n_results = n_candidates
)
candidate_dt <- data.table(
  id = unlist(candidate_results$ids),
  semantic_distance = unlist(candidate_results$distances), # 重命名以示区分
  text = unlist(candidate_results$documents),
  internal.ID = sapply(candidate_results$metadatas[[1]], function(m) m$ID)
)
new_col_names <- paste0("dist_", gsub(" ", "_", fuzzy_terms))

# 使用 lapply 遍历每个模糊查询词，并计算它与所有候选文本的距离
dist_list <- lapply(fuzzy_terms, function(term) {
  stringdist(tolower(term), tolower(candidate_dt$text), method = method)
})

# 将距离列表转换为data.table
dist_dt <- as.data.table(dist_list)
setnames(dist_dt, new_col_names)

# --- 3. 合并结果并添加摘要信息 ---

# 使用cbind将距离列附加到原始候选集上
final_dt <- cbind(candidate_dt, dist_dt)

# (可选但非常有用) 添加两列摘要：最小距离和对应的最佳匹配词
if (length(fuzzy_terms) > 0) {
  # 计算每一行的最小字符串距离
  final_dt[, min_fuzzy_dist := do.call(pmin, .SD), .SDcols = new_col_names]
  
  # 找到最小距离对应的词
  # which.min 会返回第一个最小值的索引
  best_match_indices <- apply(final_dt[, ..new_col_names], 1, which.min)
  final_dt[, best_match_term := fuzzy_terms[best_match_indices]]
}

# 按最小模糊距离排序，最相关的排在前面
setorder(final_dt, min_fuzzy_dist, semantic_distance)


cat(paste("正在处理文档 ID:", doc_id, "...\n"))

# --- 精心设计的英文提示词模板 ---
# 这个模板是成功的关键。它非常明确地指令了任务、输入和输出格式。
prompt_template <- "
  **Your Task:**
  You are an expert scientific assistant. Your goal is to extract all information specifically related to 'seed size', 'seed width', or 'seed weight' from the provided text passage.
  
  **Instructions:**
  1. Read the text passage carefully.
  2. Identify all sentences or phrases that describe, measure, or analyze seed size, width, or weight.
  3. Extract the species name from the text.
  4. Extract the seed trait (size, width, weight or others) valuess if it is possible.
  5. If no relevant information is found, explicitly state 'No specific information about seed size, width, or weight was found in this context.'
  
  **Output Format:**
  Your response MUST be a single, valid JSON object. Do NOT include any other text, explanations, or markdown formatting like ```json. The JSON object must have the following exact structure:
  {
    \"extraction\": \"<Your concise summary of the findings here.>\"
  }

  **Text Passage to Analyze:**
  ---
  %s
  ---
  "
doc_text<-documents[5]
final_prompt <- sprintf(prompt_template, doc_text)
#model <- google_genai$GenerativeModel("gemini-2.5-pro")

response <- model$generate_content(final_prompt)
raw_json <- response$text

# 使用tryCatch来处理可能的API错误或JSON解析错误
tryCatch({
  # --- 调用Gemini API ---
  
  
  # 清理返回的文本，移除可能的markdown代码块标记
  clean_json <- gsub("`{3,}(json)?\\n?|`{3,}", "", raw_json, perl = TRUE)
  clean_json <- trimws(clean_json) # 移除首尾空格
  
  # --- 解析JSON ---
  parsed_data <- fromJSON(clean_json, simplifyVector = FALSE)
  
  # --- 构建最终的R列表，包含所有要求的信息 ---
  final_result <- list(
    document_id = 1,
    original_context = doc_text,
    extracted_information = parsed_data$extraction
  )
  
  cat(paste("成功提取文档 ID:", doc_id, "\n"))
  return(final_result)
  
}, error = function(e) {
  cat(paste("处理文档 ID:", doc_id, "时发生错误: ", e$message, "\n"))
  return(NULL)
})


