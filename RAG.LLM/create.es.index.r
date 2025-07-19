create_es_index<-function(conn, es_index_name){
  if (index_exists(conn, index = es_index_name)) {
    cat(sprintf("INDEX '%s' exists, removing...\n", es_index_name))
    index_delete(conn, index = es_index_name)
    cat("INDEX REMOVED\n")
  } else {
    cat(sprintf("INDEX '%s' doesn't exist\n", es_index_name))
  }
  
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