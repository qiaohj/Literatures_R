import os
import logging
import urllib.request
import urllib.error
import google.generativeai as genai

proxy_url = "http://127.0.0.1:7897" # 示例: Clash, v2rayN等本地代理的常见地址和端口
# proxy_url = "http://your_proxy_user:your_proxy_password@proxy.server.com:8080" # 示例: 需要认证的公司代理

print(f"--- 准备设置代理服务器: {proxy_url} ---")
# 通过os.environ设置环境变量，Python的HTTP库会自动使用它们
# 注意：通常需要同时设置HTTP和HTTPS的代理
os.environ['HTTP_PROXY'] = proxy_url
os.environ['HTTPS_PROXY'] = proxy_url
print("代理已设置。\n")

# --- 步骤 2: (新增) 测试代理连通性 ---
def test_proxy_connectivity(test_url="https://www.google.com", timeout=10):
    """
    通过访问一个已知网站来测试代理是否工作正常。
    """
    print(f"--- 正在通过代理测试与 '{test_url}' 的连通性... ---")
    try:
        # 创建一个请求对象
        req = urllib.request.Request(test_url, headers={'User-Agent': 'Mozilla/5.0'})
        
        # urllib会自动使用os.environ中设置的代理
        with urllib.request.urlopen(req, timeout=timeout) as response:
            # 检查HTTP状态码是否为200 (OK)
            if response.status == 200:
                print("✅ 代理工作正常！成功访问 Google.com。\n")
                return True
            else:
                print(f"❌ 代理测试失败。收到状态码: {response.status}\n")
                return False
    except urllib.error.URLError as e:
        # 捕获所有urllib相关的错误，如连接超时、DNS错误等
        print(f"❌ 代理测试失败。无法连接到 '{test_url}'。")
        print(f"   错误原因: {e.reason}\n")
        return False
    except Exception as e:
        # 捕获其他可能的异常
        print(f"❌ 代理测试时发生未知错误: {e}\n")
        return False

# 执行测试，如果失败则退出脚本
if not test_proxy_connectivity():
    print("由于代理测试失败，脚本将终止。请检查你的代理设置和网络连接。")
    exit()

# --- 步骤 1: 配置日志记录器 (这是关键) ---
# 这是你最关心的部分，目的是看到SDK的内部工作日志。

# 获取 'google.generativeai' 库专属的日志记录器
# 这样做的好处是只看这个库的日志，避免被其他库（如urllib3）的日志淹没
gai_logger = logging.getLogger('google.generativeai')

# 设置日志级别为 DEBUG，这样才能捕获最详细的信息
gai_logger.setLevel(logging.DEBUG)

# 创建一个处理器(Handler)，决定日志输出到哪里。
# StreamHandler 会将日志打印到你的终端（控制台）。
handler = logging.StreamHandler()

# 定义日志的输出格式，让它更易读
# asctime: 时间, name: 记录器名称, levelname: 日志级别, message: 日志内容
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
handler.setFormatter(formatter)

# 将处理器添加到记录器中。
# 如果之前有处理器，先清空，防止日志重复打印
if gai_logger.hasHandlers():
    gai_logger.handlers.clear()
gai_logger.addHandler(handler)

print("--- Python日志记录器已配置，级别为DEBUG ---\n")


# --- 步骤 2: 安全地配置API密钥 ---
# 最佳实践：从环境变量中读取API密钥，而不是硬编码在代码里。
# 运行此脚本前，请在终端设置环境变量。
# macOS/Linux: export GOOGLE_API_KEY="你的API密钥"
# Windows (cmd): set GOOGLE_API_KEY="你的API密钥"
# Windows (PowerShell): $env:GOOGLE_API_KEY="你的API密钥"
try:
    
    genai.configure(api_key="XXXXXXX")
    print("--- Gemini API 已成功配置 ---\n")
except ValueError as e:
    print(e)
    exit() # 如果没有密钥，则退出程序


# --- 步骤 3: 调用模型并观察日志 ---
# 创建一个Gemini Pro模型的实例
model = genai.GenerativeModel('gemini-2.5-pro')

prompt = "用一句话简单介绍一下Python的logging模块。"

print(f"--- 准备向Gemini发送请求，Prompt: '{prompt}' ---")
print("--- 请仔细观察下面由logging模块生成的DEBUG信息 ---\n")

try:
    # 执行API调用。由于日志记录器已配置，SDK的内部操作将被打印出来。
    response = model.generate_content(prompt)

    print("\n--- API调用完成 ---\n")

    # --- 步骤 4: 打印最终结果 ---
    print("--- Gemini的最终回复 ---")
    print(response.text)

except Exception as e:
    print(f"\n--- 调用API时发生错误 ---")
    print(e)

