library(httr2)
library(jsonlite)
library(gemini.R)
model<-"gemini-2.5-flash"
prompt<-read_file("LLM.API/PROMPT/read.paper.md")
setAPI(gemini.key)
gemini("hello", mode="2.5-flash")

gemini_docs("/media/huijieqiao/WD22T_11/literatures/Data/ENM/pdf/PEERJ/PEERJ.13260.PDF",
            prompt,
            type="PDF",
            mode="2.5-flash",
            api_key = gemini.key,
            large=T,
            local=T)

generate_content_async <- function(prompt, system_instruction=null,
                                   gemini.key, model="gemini-2.5-flash",
                                   temperature=1, top_p=0.95, max_output_tokens=65536) {
  
  future({
    
    api_url <- sprintf("https://generativelanguage.googleapis.com/v1beta/models/%s:%s?key=%s", model, "generateContent", gemini.key)
    
    generation_config <- list(
      temperature = temperature,
      top_p = top_p,
      max_output_tokens = max_output_tokens
    )
    
    safety_settings <- list(
      list(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_SEXUAL", threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
    )
    
    request_body <- list(
      contents = list(
        list(
          parts = list(
            list(text = prompt)
          )
        )
      ),
      generationConfig = generation_config,
      safetySettings = safety_settings
    )
    
    if (!is.null(system_instruction)) {
      request_body$systemInstruction <- list(
        parts = list(
          list(text = system_instruction)
        )
      )
    }
    
    tryCatch({
      response <- request(api_url) %>%
        req_headers("Content-Type" = "application/json") %>%
        req_body_json(request_body) %>%
        req_perform()
      
      resp_check_status(response)
      
      # 解析 JSON 响应并提取生成的文本
      content <- resp_body_json(response)
      generated_text <- content$candidates[[1]]$content$parts[[1]]$text
      
      return(generated_text)
      
    }, error = function(e) {
      # 处理错误情况
      error_message <- paste("API 调用失败:", e$message)
      if (exists("response")) {
        # 如果有响应体，尝试提取其中的错误信息
        error_body <- httr2::resp_body_string(response)
        error_message <- paste(error_message, "\nAPI 返回的错误详情:", error_body)
      }
      stop(error_message)
    })
    
  }) # future() 结束
}


# --- 主程序 ---

# 定义你的问题
my_prompt <- "请给我写一首关于星空和代码的短诗。"

cat("正在异步请求 Gemini API...\n")

# 调用异步函数，它会立即返回一个 future 对象（一个承诺）
future_result <- generate_content_async(my_prompt)

cat("请求已发送。主程序可以继续执行其他任务（如果需要）。\n")
# 例如，你可以在这里执行一些不依赖于 API 结果的计算
# Sys.sleep(2)
# print("这是一条在等待期间显示的消息。")

cat("现在，我们等待结果生成...\n\n")

# 使用 value() 函数来等待并获取 future 的最终结果
# 这行代码会阻塞，直到异步任务完成
final_content <- value(future_result)

# 打印最终生成的内容
cat("--- Gemini 生成的内容 ---\n")
cat(final_content)
cat("\n--------------------------\n")

# 清理 future 后端（好习惯）
plan(sequential)