safe_read_html <- function(url, 
                           user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36",
                           timeout_seconds = 10) {
  
  # 使用 purrr::safely() 或 tryCatch() 来捕获所有可能的错误
  # 这里我们使用 tryCatch()，因为它在基础R中，无需额外包
  
  response <- tryCatch(
    {
      # 发起带有 User-Agent 和超时的 GET 请求
      httr::GET(
        url = url, 
        add_headers(`User-Agent` = user_agent),
        timeout(timeout_seconds)
      )
    },
    # 定义当发生任何类型的错误时（如DNS解析失败、超时等）该如何处理
    error = function(e) {
      # 打印一个清晰的错误信息
      message(sprintf("网络请求失败: %s\nURL: %s", e$message, url))
      # 返回 NULL，表示失败
      return(NULL)
    }
  )
  
  # 如果 response 为 NULL，说明上面的 tryCatch 捕获到了错误，直接返回 NULL
  if (is.null(response)) {
    return(NULL)
  }
  
  # 检查 HTTP 状态码
  status <- httr::status_code(response)
  
  if (status == 200) {
    # 状态码 200，请求成功
    # 从响应内容中解析HTML
    # 使用 content() 函数来提取内容，然后传递给 read_html()
    html_content <- httr::content(response, as = "text", encoding = "UTF-8")
    
    # 再次使用 tryCatch 以防止 read_html 解析失败（例如，内容不是有效的HTML）
    webpage <- tryCatch({
      read_html(html_content)
    }, 
    error = function(e){
      message(sprintf("HTML解析失败: %s\nURL: %s", e$message, url))
      return(NULL)
    })
    
    return(webpage)
    
  } else {
    # 其他状态码，表示HTTP错误
    message(sprintf("HTTP请求错误: 状态码 %d - %s\nURL: %s", 
                    status, http_status(status)$reason, url))
    return(NULL)
  }
}

library(urltools)

#' 从一个完整的URL中提取基础URL (协议 + 域名)
#'
#' 例如，从 "https://www.example.com/path/to/page?q=1" 提取 "https://www.example.com"
#'
#' @param url 一个或多个URL字符串。
#'
#' @return 返回一个包含基础URL的字符向量。如果输入URL无效，可能返回NA。
#'
get_base_url <- function(url) {
  # 使用 url_parse() 将URL分解成各个部分
  parsed_urls <- url_parse(url)
  
  # 将协议(scheme)和域名(domain)重新拼接起来
  # paste0 会自动处理向量化的输入
  base_urls <- paste0(parsed_urls$scheme, "://", parsed_urls$domain)
  
  return(base_urls)
}
