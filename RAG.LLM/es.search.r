library(elastic)
library(purrr)

es_host <- "localhost"
es_port <- 9200
journal<-"METHODS IN ECOLOGY AND EVOLUTION"
es_index_name<-tolower(gsub(" ", "_", journal))
must_terms <- c("seed*") 
should_terms <- c("size", "mass", "weight", "germinat*")
target_labels <- c("Title", "Abstract", "Methods", "Introduction", "Results", "Discussion", "Conclusion", "Unknown")
label_field <- "active_section"


must_clauses <- list()
if (!is.null(must_terms) && length(must_terms) > 0) {
  must_clauses <- c(must_clauses, lapply(must_terms, function(term) {
    list(query_string = list(query = term, default_field = "text"))
  }))
}

if (length(should_terms) > 0) {
  should_clauses <- lapply(should_terms, function(term) {
    if (grepl("[*?]", term)) {
      list(query_string = list(query = term, default_field = "text"))
    } else {
      list(match = list(text = term))
    }
  })
  
  # 将这些 OR 条件用一个 bool/should 结构包起来，并添加到 must_clauses 中
  should_block <- list(bool = list(
    should = should_clauses,
    minimum_should_match = 1
  ))
  must_clauses <- c(must_clauses, list(should_block))
}

if (length(must_clauses) == 0) {
  stop("Error: blank query")
}
query_part <- list(bool = list(must = must_clauses))

filter_part <- NULL
if (length(target_labels) > 0) {
  filter_terms <- list(terms = setNames(list(target_labels), label_field))
  query_part$bool$filter <- list(filter_terms)
}

query_body <- list(
  query = query_part,
  `_source` = c("text", label_field, "filename", "journal"),
  highlight = list(
    fields = list(text = list(fragment_size = 150)),
    pre_tags = "<strong>",
    post_tags = "</strong>"
  )
)

cat("Searching ...\n\n")
search_results <- Search(conn, index = es_index_name, body = query_body, asdf = F, size=10000)

#sapply(search_results$hits$hits, function(x) x$`_source`$text[[1]])


hits <- search_results$hits$hits
all<-list()
for (i in c(1:length(hits))){
  items<-hits[[i]]$`_source`
  item<-data.table(active_section=items$active_section, 
                   filename=items$filename, 
                   text=items$text, 
                   journal=items$journal)
  all[[i]]<-item
}
all_dt<-rbindlist(all)
if (length(hits) == 0) {
  cat("未找到任何匹配结果。\n")
} else {
  cat(sprintf("找到了 %d 个匹配结果：\n", search_results$hits$total$value))
  cat("----------------------------------\n")
  
  # 使用 purrr::walk 遍历 hits 列表
  walk(hits, function(hit) {
    # 'hit' 现在是一个标准的列表，包含 _source, _score, highlight 等
    
    # 从 hit$`_source` 中提取信息
    source_data <- hit$`_source`
    
    # 从 hit$highlight 中提取高亮信息
    highlight_data <- hit$highlight
    
    # 打印文件和标签
    cat(sprintf("文件: %s\n", source_data$file_name))
    cat(sprintf("标签: %s\n", source_data[[label_field]])) # 使用 [[...]] 动态访问字段
    
    # 优先使用高亮文本
    highlighted_text <- unlist(highlight_data$text)
    if (is.null(highlighted_text)) {
      # 如果没有高亮，回退到原始文本
      highlighted_text <- source_data$text
    }
    
    cat(sprintf("内容: ...%s...\n", paste(highlighted_text, collapse = " ... ")))
    cat(sprintf("得分: %.2f\n", hit$`_score`))
    cat("----------------------------------\n")
  })
}
