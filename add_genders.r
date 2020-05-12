library("genderizeR")
x = c("Jongkar Grinang",
      "Nazalan Najimudin",
      "Suriani Mohamad")

x = rep(c("Jongkar",
      "Nazalan",
      "Suriani"), 1000)

givenNames = findGivenNames(x, progress = FALSE)

genderize(x, genderDB = givenNames, progress = FALSE)


library("gender")
library("genderdata")

gender(x)
gender(names, years = c(1932, 2012), 
       method = c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo"), 
       countries = c("United States", "Canada", "United Kingdom", "Denmark", "Iceland", "Norway", "Sweden"))

gender(x, method = "ssa", year=c(1932, 2012))
gender(x, method = "genderize")

gender(x, method = "napp", countries = c("Sweden", "Denmark"))
data(package = "genderdata")
