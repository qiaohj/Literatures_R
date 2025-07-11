search_fuzzy_multiple <- function(semantic_query, 
                                  fuzzy_terms, 
                                  max_distance = 1, 
                                  match_all = FALSE, # 默认为OR逻辑
                                  n_candidates = 20L) { # 增加候选项数量以提高命中率
  
  # --- 1. 先进行一次广泛的语义搜索，召回候选项 ---
  query_embedding <- embed_text(semantic_query)
  candidate_results <- collection$query(
    query_embeddings = list(query_embedding),
    n_results = n_candidates
  )
  
  if (is.null(candidate_results) || length(candidate_results$ids[[1]]) == 0) {
    return(data.table())
  }
  
  # 将结果整理到data.table中
  candidate_dt <- data.table(
    id = unlist(candidate_results$ids),
    distance = unlist(candidate_results$distances),
    text = unlist(candidate_results$documents),
    type = sapply(candidate_results$metadatas[[1]], function(m) m$type)
  )
  
  # --- 2. 在R端对召回的文本进行多关键词模糊匹配 ---
  
  # `sapply` 会对每个关键词进行模糊匹配，返回一个逻辑矩阵
  # 每一列对应一个关键词，每一行对应一个候选文档
  matches_matrix <- sapply(fuzzy_terms, function(term) {
    agrepl(term, candidate_dt$text, max.distance = max_distance, ignore.case = TRUE)
  })
  
  # --- 3. 根据 match_all 的值来确定最终匹配的行 ---
  if (match_all) {
    # AND 逻辑: 只有所有列都为TRUE的行才保留
    # `rowSums` 计算每行的TRUE数量，如果等于关键词数量，则表示全部匹配
    final_match_indices <- which(rowSums(matches_matrix) == length(fuzzy_terms))
  } else {
    # OR 逻辑: 只要有任何一列为TRUE的行就保留
    # `rowSums` 计算每行的TRUE数量，只要大于0，就表示至少有一个匹配
    final_match_indices <- which(rowSums(matches_matrix) > 0)
  }
  
  if (length(final_match_indices) == 0) {
    return(data.table())
  }
  
  # --- 4. 筛选并返回最终结果 ---
  final_dt <- candidate_dt[final_match_indices, ]
  
  # (可选) 可以添加一列来显示匹配了哪些词
  matched_terms_list <- apply(matches_matrix[final_match_indices, , drop = FALSE], 1, function(row) {
    paste(fuzzy_terms[row], collapse = ", ")
  })
  final_dt[, matched_keywords := matched_terms_list]
  
  return(final_dt)
}
