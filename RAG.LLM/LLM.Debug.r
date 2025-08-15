# --- 2. 导入Python模块 ---
# 我们需要导入两个模块：logging 和 google.generativeai
cat("--- 正在导入Python模块 ---\n")
logging <- import("logging")
google_genai <- import("google.generativeai")
cat("导入完成。\n\n")


# --- 3. 设置Python的日志记录器 (关键部分) ---
cat("--- 正在配置Python日志记录器 ---\n")

# 获取 'google.generativeai' 库专属的日志记录器
gai_logger <- logging$getLogger('google.generativeai')

# 设置日志级别为 DEBUG，这样才能捕获最详细的信息
gai_logger$setLevel(logging$DEBUG)

# 创建一个处理器(Handler)，决定日志输出到哪里。
# StreamHandler() 会将日志打印到控制台。
handler <- logging$StreamHandler()

# 将处理器添加到记录器中。没有处理器，日志将不会显示。
gai_logger$addHandler(handler)

cat("日志记录器配置完成，级别为DEBUG。\n\n")


# --- 4. 配置并调用Gemini API ---
# 这部分和之前一样
google_genai$configure(api_key = Sys.getenv("gemini.key"))
model <- google_genai$GenerativeModel('gemini-2.5-pro')

# 我们使用一个简单的prompt，因为重点是看日志，而不是看模型输出
simple_prompt <- "简单介绍一下R语言。"

cat("--- 开始调用Gemini API (请观察下面的DEBUG日志) ---\n\n")
py_output <- py_capture_output({
  # 这里面的所有Python代码生成的标准输出都会被捕获
  # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  
  prompt <- "用一句话简单介绍一下 reticulate 包。"
  
  # 我们甚至可以在这里放一个Python的print语句来测试
  py_run_string("print('这是一个来自Python的测试print语句!')")
  
  # 调用Gemini API
  # 注意：这次我们不把response赋值给R变量，因为它在py_capture_output内部
  # 这个调用的主要目的是触发日志
  model$generate_content(prompt)
  
  # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
})


cat("\n--- API调用结束 ---\n\n")

cat("\n--- 成功捕获到的Python输出 (包含DEBUG日志) ---\n")
# py_output 是一个字符串，包含了所有被捕获的内容
cat(py_output)
cat("--- 捕获结束 ---\n\n")



# --- 5. 打印最终结果 ---
cat("--- Gemini的最终输出 ---\n")
cat(response$text)
