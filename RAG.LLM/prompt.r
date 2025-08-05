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
  You are a literature analysis expert with a strong command of ecological and botanical terminology and research methods. 
  Please read the following document. Your goal is to semantically determine whether these studies object and methodology 
  simultaneously meet both of the following criteria:

1. The paper examines traits of plant’s **seeds, fruits, diaspores, or propagules** (or other semantically related terms 
referring to dispersal units), such as size, mass, germination, dormancy, and other traits.
2. The paper reports an experimental or empirical study. Exclude reviews or syntheses based solely on literature data, 
as these are not considered experimental. Experimental studies may contain one or more of the following characteristics:
* Involve activities such as collecting, processing, sowing, monitoring, or measuring seeds, fruits, diaspores, or propagules;
* Involve experimental treatments, such as temperature, light, humidity, etc.;
* Contain explicit mentions of concepts such as “experimental design” , “germination test” , “conducted fieldwork” , or similar expressions even if the word “experiment” itself is absent, any structured sequence of actions such as collection + treatment + measurement should be regarded as an experiment;
* Demonstrate clearly operational procedures, using verbs like *collected, monitored, exposed, sowed, tested, measured, conducted, carried out, estimated*, or other relevant terms. Do not rely on keyword-matching alone; rather, understand the research context and sentence structure.

The following examples illustrate how experimental studies in seed ecology and related disciplines may be expressed 
using different wording. These expressions all convey an explicit experimental framework: 
**collection → experimental treatment → trait measurement**. 
Learn their semantic structure and research-related expressions so you can identify functionally equivalent expressions 
and judge whether a study meets the required criteria. Your task is to summarise their shared characteristics rather 
than merely memorising word for word. Examples:
* “…individual plants of each species were collected from [site] in [location]”,
* “Seeds of [species] were collected in [site]”,
* “[species] were harvested from [site]”,
* “Freshly harvested [species] seeds from [site] located in [country]”,
* “The distribution of [species] in [site]”,
* “The study was developed with lots of [species] seeds coming from [site]”,
* “The study was conducted in [site]”,
* “The field site was located in [country]”,
* “Seed morphological traits, ..., and seed germination rates were measured …”,
* “Seed size was estimated using seed mass and achene mass measurements”,
* “We measured the fruit size, seed mass, seed viability and germination success”,
* “Seeds were individually weighed …”,
* “In order to determine the seed quality and the seed vigour … the seed germination was performed”,
* “We carried out a seed germination experiment to …”,
* “We performed an in situ seed germination experiment of …”,
* “[species] were chosen to examine their seed germination”.

**[Input Format]:**
The document you receive will consist of approximately 100 articles. I will use [doi] to mark the DOI number before 
each article, which will make it easier for you to include this number in your returned results. 
All text information between two [doi] markers should be considered the content of the preceding article.

### [Trait Data Extraction Requirements]
1. Traits related to seed size
* This includes, but is not limited to, synonyms such as size, mass, weight, length, width, thickness, diameter, 
volume, area, perimeter, breadth specifically for plant’s **seeds, fruits, diaspores, or propagules**. 
Understand their commonality and semantic function, and identify equivalent terms even if phrased differently.
* These traits must be experimentally measured and often accompanied by measurement units (e.g. *μg, mg, g, kg* 
for weight, or *μm, mm, cm, m* for dimensions) and descriptive statistics (e.g. mean, variation, range), 
which can help you determine whether they refer to size-related traits.
* Important Rule: If any such trait is studied on seeds, record it as **Size** in the Trait column in the final 
table, not by the original term.

2. Traits related to seed germination
* This includes, but is not limited to, synonyms such as germination, viability, emergence, vigour, dormancy, 
establishment for plant’s **seeds, fruits, diaspores, or propagules**. Understand their commonality and semantic 
function, and identify equivalent terms even if phrased differently.
* These terms often appear with measurement indices such as *percentage, rate, speed, time, synchrony, capacity, 
value, type, onset, duration*.
* Important Rule: If any such trait is studied, record it as **Germination** in the Trait column in the final 
table, not by the original term.


### [Species Data Extraction Requirements]
* Extract the plant species studied for either *Size* or *Germination* traits. Species names are typically given 
in binomial format (Genus + species epithet), such as *Quercus variabilis*. Do not include authority of the species.
* If plant species are not individually listed in the main text but a full list of species appears in supplementary 
materials (e.g. Supplementary Materials, Appendix, Supporting Information, Online Supporting Information, Data Availability), 
preserve the original amalgamated expression used in the paper. Examples:
  * “163 species were studied” → Species: “163 species”
  * “All species are listed in Supplementary Table S1” → Species: “Species listed in Supplementary”
  * “We sampled 25 woody plant species in...” → Species: “25 woody species”


### [Site Data Extraction Requirements]
* Extract the geographical information related to sample collection locations or study sites such as 
(but not limited to) study site, location, administrative region, county, state, country, longitude, 
latitude, altitude. Your task is to understand the spatial references and sentence structure to determine 
whether a passage contains precise site information, rather than merely memorising word for word. 
Once such information is identified, extract the full geographic details as they appear in the text and 
enter them into the “Site” column in the final table. If coordinates or altitude are included, 
retain them in the same field.
Examples:
* “The study site was located in Cuiabá, MT (15°61’S, 56°65’W)”
* “Freshly harvested *T. roseoalba* seeds from forest fragment located in the municipality 
of Promissão, São Paulo State …”
* “… were collected from the middle region of the canopy of mother trees located in the metropolitan 
region of Natal, RN, Brazil (5°49’12”S and 35°11’16”W, altitude of 70 m)”
* “… developed with lots of *A. cearensis* seeds coming from the municipalities of Soledade, 
PB (7°3’25’’S and 36°21’46’’W; altitude: 521 m) and Petrolina, PE (9°23’34’’S and 40°30’ 8’’W; altitude 376 m)”
* “… harvested in 100 individuals of natural populations of Caatinga vegetation in July 2015, 
at São José de Piranhas, Paraíba, Brazil (07°05’25.98’’S and 38°38’41.14’’W and 350 m altitude)”


### [Output Format and Requirements]
*Important notice: 
**Perform semantic analysis on the provided documents and return ONLY the results in CSV format with Columns: DOI,Species,Site,Trait,Text. 
**Require all fields to be enclosed in double quotes (\"), even for non-string data (e.g., numeric DOI)
**Limit the Text field to 50 words or fewer, truncating while preserving meaning.
**Use commas as separators and ensure each row is complete. 
**Do NOT include the input document, explanations, or any other text outside the CSV data. 

1. General Structure:
* Output should be in **long table** format: each row should contain only 
**one unique combination of Species – Site – Trait**.
* If the same combination appears in multiple sections of the paper, 
retain only one row and avoid duplicates.
* All output should be in **English**.

2. Columns:
* **DOI**: Use [doi] for each article as the reference.
* **Species**: Plant species studied.
* **Site**: Site of study/sample collection.
* **Trait**: “Size” or “Germination” (only these two categories).
* **Text**: Original excerpt from the document supporting your judgement.

  **Document Passage to Analyze:**
  ---
  %s
  ---
  "
