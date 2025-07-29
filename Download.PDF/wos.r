#usethis::edit_r_environ()

library("rwosstarter")
query <- "TS=((seed* OR fruit* OR diaspore* OR propagule*) AND (size* OR mass* OR weight* OR germinat* OR dorman*))"
wos_search(query)
records<-rwosstarter::wos_get_records(query, limit=NULL)

api_url<-function(){
  "https://api.clarivate.com/apis/wos-starter/v1"
}
get_token<-function(){
  wos_token <- Sys.getenv("WOS_STARTER_KEY")
}
function (query, database = "WOS", limit = NULL, sleep = 1) 
{
  if (!is.null(limit)) {
    if (limit == 0) {
      stop("Argument 'limit' must be strictly positive", 
           call. = FALSE)
    }
  }
  query <- utils::URLencode(query, reserved = TRUE)
  n_refs <- wos_search(query, database)
  if (!is.null(limit)) {
    if (n_refs >= limit) {
      n_refs <- limit
    }
    else {
      limit <- n_refs
    }
  }
  if (n_refs == 0) {
    stop("No reference found")
  }
  refs <- data.frame()
  n_records_per_page <- 50
  if (!is.null(limit)) {
    if (limit <= n_records_per_page) {
      n_records_per_page <- limit
    }
  }
  pages <- seq(1, ceiling(n_refs/n_records_per_page), by = 1)
  for (page in pages) {
    target<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/seeds/%d.rda", page)
    
    if (file.exists(target)){
      next()
    }
    tryCatch({
      print(target)
      request <- paste0(api_url(), "/documents", "?db=", database, 
                        "&q=", query, "&limit=", n_records_per_page, "&page=", 
                        page)
      response <- httr::GET(url = request, config = httr::add_headers(accept = "application/json", 
                                                                      `X-ApiKey` = get_token()))
      httr::stop_for_status(response)
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      content <- jsonlite::fromJSON(content)
      saveRDS(content, target)
      
      
    },
    error = function(e) {
      message("Error: ", e$message)
      
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    
    
    if (F){
      content <- content$hits
      uid <- list_to_df(get_field(content, "uid"))
      document_type <- list_to_df(get_field(content, "sourceTypes"))
      title <- list_to_df(get_field(content, "title"))
      authors <- unlist(lapply(get_field(content$names, "authors"), 
                               function(x) {
                                 if (!is.data.frame(x)) {
                                   NA
                                 }
                                 else {
                                   paste0(x[, 1], collapse = " | ")
                                 }
                               }))
      published_year <- list_to_df(get_field(content$source, 
                                             "publishYear"))
      published_month <- list_to_df(get_field(content$source, 
                                              "publishMonth"))
      source <- list_to_df(get_field(content$source, "sourceTitle"))
      volume <- list_to_df(get_field(content$source, "volume"))
      issue <- list_to_df(get_field(content$source, "issue"))
      pages <- list_to_df(get_field(content$source$pages, 
                                    "range"))
      no_article <- list_to_df(get_field(content$source, "articleNumber"))
      supplement_number <- list_to_df(get_field(content$source, 
                                                "supplement"))
      special_issue <- list_to_df(get_field(content$source, 
                                            "specialIssue"))
      book_editors <- unlist(lapply(get_field(content$names, 
                                              "bookEditors"), function(x) {
                                                if (!is.data.frame(x)) {
                                                  NA
                                                }
                                                else {
                                                  paste0(x[, 1], collapse = " | ")
                                                }
                                              }))
      keywords <- list_to_df(get_field(content$keywords, "authorKeywords"))
      doi <- list_to_df(get_field(content$identifiers, "doi"))
      eissn <- list_to_df(get_field(content$identifiers, "eissn"))
      issn <- list_to_df(get_field(content$identifiers, "issn"))
      isbn <- list_to_df(get_field(content$identifiers, "isbn"))
      pmid <- list_to_df(get_field(content$identifiers, "pmid"))
      citations <- unlist(lapply(get_field(content, "citations"), 
                                 function(x) {
                                   ifelse(is.na(x$count), NA, x$count)
                                 }))
      data <- data.frame(uid, document_type, title, authors, 
                         published_year, published_month, source, volume, 
                         issue, pages, no_article, supplement_number, special_issue, 
                         book_editors, keywords, doi, eissn, issn, isbn, 
                         pmid, citations)
      refs <- rbind(refs, data)
    }
    if (length(pages) > 1) 
      Sys.sleep(sample(seq(0, sleep, by = 0.01), 1))
    
  }
  if (!is.null(limit)) 
    refs <- refs[1:limit, ]
  refs
}

