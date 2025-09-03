remove_es_index<-function(conn, es_index_name){
  if (index_exists(conn, index = es_index_name)) {
    cat(sprintf("INDEX '%s' exists, removing...\n", es_index_name))
    index_delete(conn, index = es_index_name)
    cat("INDEX REMOVED\n")
  } else {
    cat(sprintf("INDEX '%s' doesn't exist\n", es_index_name))
  }
}
create_es_index<-function(conn, es_index_name){
  remove_es_index(conn, es_index_name)
  
  mappings_body <- list(
    mappings = list(
      properties = list(
        text = list(
          type = "text",
          analyzer = "ik_max_word",
          search_analyzer = "ik_smart"
        ),
        active_section = list(
          type = "keyword"
        ),
        filename = list(
          type = "keyword"
        ),
        journal = list(
          type = "keyword"
        )
      )
    )
  )
  
  tryCatch({
    response <- index_create(conn, index = es_index_name, body = mappings_body)
    if (response$acknowledged) {
      cat(sprintf("INDEX '%s' CREATE SUCCESSFULLY！\n", es_index_name))
    } else {
      warning("NO RESPONSE")
    }
  }, error = function(e) {
    stop(sprintf("ERROR WHEN CREATING %s", e$message))
  })
}

add_es_index<- function(conn, 
                        es_index_name, 
                        num_shards = 5, 
                        num_replicas = 1) {
  
  if (index_exists(conn, index = es_index_name)) {
    cat(sprintf("INDEX '%s' already exists. Ready for data ingestion.\n", es_index_name))
    return(invisible(NULL))
  }
  
  cat(sprintf("INDEX '%s' does not exist. Creating it now...\n", es_index_name))
  index_body <- list(
    settings = list(
      number_of_shards = num_shards,
      number_of_replicas = num_replicas
    ),
    mappings = list(
      properties = list(
        text = list(
          type = "text",
          analyzer = "ik_max_word",
          search_analyzer = "ik_smart"
        ),
        active_section = list(
          type = "keyword"
        ),
        filename = list(
          type = "keyword"
        ),
        journal = list(
          type = "keyword"
        )
      )
    )
  )
  tryCatch({
    response <- index_create(conn, index = es_index_name, body = index_body)
    if (response$acknowledged) {
      cat(sprintf("INDEX '%s' CREATE SUCCESSFULLY with %d shards!\n", es_index_name, num_shards))
    } else {
      warning(sprintf("INDEX '%s' creation command sent, but not acknowledged.", es_index_name))
    }
  }, error = function(e) {
    if (grepl("resource_already_exists_exception", e$message, ignore.case = TRUE)) {
      cat(sprintf("INDEX '%s' was created by another process concurrently. Continuing...\n", es_index_name))
    } else {
      stop(sprintf("ERROR WHEN CREATING INDEX '%s': %s", es_index_name, e$message))
    }
  })
}


get_count_for_journal <- function(conn, es_index_name, journal_name) {
  
  if (!is.character(journal_name) || length(journal_name) != 1 || nchar(journal_name) == 0) {
    stop("Argument 'journal_name' must be a non-empty string.")
  }
  #cat(sprintf("Counting documents for journal '%s' in index '%s'...\n", journal_name, es_index_name))
  tryCatch({
    query_string <- paste0("journal:", '"', journal_name, '"') # Wrap journal_name in quotes for exact match
    
    response <- count(conn, index = es_index_name, q = query_string)
    
    doc_count <- response
    
    #cat(sprintf("Found %d documents.\n", doc_count))
    
    return(doc_count)
    
  }, error = function(e) {
    if (grepl("index_not_found_exception", e$message)) {
      warning(sprintf("Index '%s' not found. Returning 0.", es_index_name))
      return(0)
    } else {
      stop(sprintf("An error occurred during the count query: %s", e$message))
    }
  })
}
