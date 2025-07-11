prompt_template_cn<-"请阅读以下生态学文献pdf内容，并判断其中是否包含对种子性状的研究，特别是以下两类性状：

【1. 种子大小相关性状（Size-related traits）】
- 关键词示例：seed AND (size, mass, weight, length, width, thickness, diameter, volume, area, perimeter, breadth)
- 通常伴随的单位：μg,mg,g,kg,μm,mm,cm 等

【2. 种子萌发相关性状（Germination-related traits）】
- 关键词示例：germination, seed viability, emergence, seed vigor, dormancy
- 常见指标或单位：percentage, rate, speed, time, viability, synchrony, capacity, index, value, type, onset, duration, synchrony

请根据是否出现上述性状的实质性研究内容，提取并填写以下字段：

【输出格式（CSV，表头信息）】
- Reference：文献作者的【姓】 、时间、期刊名的格式，例如（Li et al., (2024) Ecology）
- Species：研究的植物物种（用原文给出的拉丁名）
- Site：研究地点（用原文给出的详细地理信息，如Jingyang, Gudong, Majiang, Qiandongnan, Guizhou, China等）
- Trait：性状（Size/Germination）
- Text：你判断性状依据的原文片段（请指出文中出现的关键词句作为依据）

【请注意】
- 如果未提及具体性状数据或实验（仅泛泛提到概念），请忽略。
-如果数据在pdf的表格中，则要将表格的具体数据输出出来，每一行包含单一物种对应单一地点。
-如果正文中未包含数据，需要在附件中获取完整数据，将该文献的Text一列做上“SP”标记.
- 整个数据以 “CSV”的形式，不同物种、不同地点和不同性状均为独自一行，以便后续分析。
-整个数据的输出语言为英语。
-结果以表格形式呈现。
-不要前后的对话式语言，只输出CSV，以便我用程序来读取

**需要分析的文本:**
---
%s
---
"


prompt_template_en <- "
  **Your Task:**
  You are an expert scientific assistant. Your goal is to extract all information specifically related to 'seed size', 'seed width', or 'seed weight' from the provided text passage.
  
  **Instructions:**
  1. Read the text passage carefully.
  2. Identify all sentences or phrases that describe, measure, or analyze seed size, width, or weight.
  3. Extract the species name from the text.
  4. Extract the seed trait (size, width, weight or others) valuess if it is possible.
  5. If no relevant information is found, explicitly state 'No specific information about seed size, width, or weight was found in this context.'
  
  **Output Format:**
  Your response MUST be a single, valid JSON object. Do NOT include any other text, explanations, or markdown formatting like ```json. The JSON object must have the following exact structure:
  {
    \"extraction\": \"<Your concise summary of the findings here.>\"
  }

  **Text Passage to Analyze:**
  ---
  %s
  ---
  "
