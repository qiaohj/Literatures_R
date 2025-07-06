library(data.table)
library(pdftools)
library(stringr)
library(stringi)
library(RCurl)
library(rvest)
library(httr)
library(reticulate)
library(RSelenium)
library(wdman)


rD <- rsDriver(
  browser = "chrome",
  chromever = "latest",
  verbose = TRUE, 
  phantomver = NULL
)

remDr <- rD$client
page_url <- "https://bioone.org/journals/american-museum-novitates/volume-2020/issue-3968/3968.1/On-the-Supposed-Presence-of-Miocene-Tayassuidae-and-Dromomerycinae-Mammalia/10.1206/3968.1.full"
remDr$navigate(page_url)
remDr$close()
rD$server$stop()

http

# 3. 等待！给浏览器足够的时间来执行JavaScript安全检查
#    Cloudflare 通常需要5秒左右来验证和重定向。
cat("等待5秒钟，让JavaScript安全检查完成...\n")
Sys.sleep(5)

reticulate::py_install("undetected-chromedriver", pip = TRUE)
uc_driver <- import("undetected_chromedriver")
driver <- uc_driver$Chrome(
  use_subprocess = TRUE,
  version_main = as.integer(137)
)

options <- import("selenium.webdriver.chrome.options")$Options()
prefs <- dict(`download.default_directory` = "~/Downloads",
              `download.prompt_for_download` = FALSE,
              `download.directory_upgrade` = TRUE,
              `safebrowsing.enabled` = TRUE)
options$add_experimental_option("prefs", prefs)

driver <- uc_driver$Chrome(options = options)

chrome <- driver$Chrome()
driver$get(url)
driver$do
driver$s(url, filename)

#Sys.setenv(http_proxy = "http://127.0.0.1:7897")
#Sys.setenv(https_proxy = "http://127.0.0.1:7897")

Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")
