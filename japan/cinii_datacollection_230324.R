## Clean Up Space
rm(list=ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rvest)
library(stringr)
library(readr)
library(pbapply)

p1 <- read_tsv("./bib_japanese/cinii_search_20230324_p1.txt")
p2 <- read_tsv("./bib_japanese/cinii_search_20230324_p2.txt")
p3 <- read_tsv("./bib_japanese/cinii_search_20230324_p3.txt")
p4 <- read_tsv("./bib_japanese/cinii_search_20230324_p4.txt")

d <- data.frame(id = 1:702, 
                as.data.frame(rbind(p1,p2,p3,p4)))
colnames(d) <- c("id","author","title","journal","issn",
                 "publisher","date","volume","number","pages",
                 "url","doi")

## Add Affiliation
getaffil <- function(page) {
  tmp <- read_html(page)
  tmp <- tmp %>% html_nodes("dd") %>%
    .[html_attr(.,"class")%in%"authoraffiliation"] %>% 
    html_text() %>% str_squish() 
  if (length(tmp)==1) {
    if (tmp=="") tmp <- NA
  } 
  if (length(tmp)>1) {
    tmp <- paste(ifelse(tmp == "", NA, tmp), collapse=" and ") %>%
      str_squish()
  }
  tmp
}

d$affiliation <- pbsapply(d$url, 
                          function(k) tryCatch(getaffil(k),  error=function(e) "ERROR"))
# (errorloc <- which(d$affiliation%in%"ERROR"))
# d$affiliation[errorloc] <- pbsapply(d$url[errorloc], 
#                           function(k) tryCatch(getaffil(k),  error=function(e) "ERROR"))

## All Articles
saveRDS(d, "ca_all_articles_japan_230324.rds")
# d <- readRDS("ca_all_articles_japan_230324.rds")

## Detect Language of Text ##

# devtools::install_github("ropensci/cld2")
d$title[which(d$title=="中央アジアの石刻絵画/Petroglyphs in Central Asia(和文英文併記)")] <- 
  "中央アジアの石刻絵画"
d$title[which(d$title=="Sholpan E. Karzhaubayeva, Saule A. Dikanbaeva: カザフスタン共和国における学校健康教育教科開発の動向と課題")] <- 
  "カザフスタン共和国における学校健康教育教科開発の動向と課題"
d$title[which(d$title=="Об<<аралаш>>(смешанном)употреблении кыргызского и русского языков (キルギス語とロシア語の'aralash' (混合)使用について)")] <- 
  "キルギス語とロシア語の'aralash' (混合)使用について"
d$title[which(d$title=="Голоса Женщин: Гендерное исследование в Таджикистане（女性の声：タジキスタンにおけるジェンダー研究）")] <- 
  "女性の声：タジキスタンにおけるジェンダー研究"
d$title_lang <- cld2::detect_language(d$title)
d$title_lang[which(d$title=="Financial Reform in Uzbekistan")] <- "en"
d$title_lang[which(d$title=="ヨルダン南部Jebel Qalkha遺跡，ウズベキスタンAnghilak遺跡，カザフスタンKuzulaus 2 遺跡でのOSL年代の試料と予察結果")] <- "ja"
table(d$title_lang, useNA="always")
d$title[which(d$title_lang=="en")]

## English Dataset ##

p1en <- read_tsv("./bib_japanese/cinii_search_20230325_p1_en.txt")
p2en <- read_tsv("./bib_japanese/cinii_search_20230325_p2_en.txt")
p3en <- read_tsv("./bib_japanese/cinii_search_20230325_p3_en.txt")
p4en <- read_tsv("./bib_japanese/cinii_search_20230325_p4_en.txt")

den <- data.frame(id = 1:702, 
                as.data.frame(rbind(p1en,p2en,p3en,p4en)))
colnames(den) <- c("id","author","title","journal","issn",
                 "publisher","date","volume","number","pages",
                 "url","doi")
## All article matches
all(d$url==den$url)
## Check The Existence of English Titles
den$title[which(d$title=="中央アジアの石刻絵画")] <- 
  "Petroglyphs in Central Asia"
den$title_lang <- cld2::detect_language(den$title)
table(den$title_lang, useNA="always")
den$title[which(den$title_lang=="en")]

## Add English Titles
d$title_en <- NA
d$title_en[which(d$title_lang=="en")] <- 
  d$title[which(d$title_lang=="en")]
d$title_en[which(d$title_lang=="ja" & den$title_lang=="en")] <- 
  den$title[which(d$title_lang=="ja" & den$title_lang=="en")]
table(is.na(d$title_en))

## All Articles
saveRDS(d, "ca_all_articles_japan_230324.rds")
# d <- readRDS("ca_all_articles_japan_230324.rds")
names(d)

## Translate Using DeepL 
### Identifier for Translated text
d$translated <- ifelse(is.na(d$title_en),1,0)
table(d$translated)
### Text vector to Translate
totranslate <- d$title[which(is.na(d$title_en))]
sum(nchar(totranslate))
### Prepare deeplr package
# devtools::install_github("zumbov2/deeplr")
# keyring::key_set("deepL", username="apikey")
dLkey <- keyring::key_get("deepL", username="apikey")
require(deeplr)
as.data.frame(deeplr::available_languages2(dLkey))
deeplr::usage2(dLkey) # how many more characters I can translate
### Translate from Japanese to English
title_en_translated <- 
  deeplr::translate2(totranslate, 
                     source_lang = "JA",target_lang = "EN", 
                     auth_key = dLkey)
head(title_en_translated)
### Store it in the title_en
d$title_en[which(d$translated==1)] <- title_en_translated
## All Articles
saveRDS(d, "ca_all_articles_japan_230324.rds")
# d <- readRDS("ca_all_articles_japan_230324.rds")
names(d)

## Check and fix some title translations ##
table(grepl("central asia|kazakh|kyrgyz|tajik|uzbek|turkmen", tolower(d$title_en)))
d$title_en[!grepl("central asia|kazakh|kyrgyz|tajik|uzbek|turkmen", tolower(d$title_en))]
d$title_en[!grepl("central asia|kazakh|kyrgyz|tajik|uzbek|turkmen", tolower(d$title_en))] <- 
  c("Qing dynasty draft historical documents and research on the history of Central Asia: on the publication of Qing Dynasty Zhong-Ha relations: A Compilation of drafts",
     "On the situations of oral mans in the Republic of Kazakhstan (opening articles)",
     "Overview of Sharq Taronalari International Music Festival: A cross-section on the safeguarding of intangible cultural heritage by the Republic of Uzbekistan")

saveRDS(d, "ca_all_articles_japan_230324.rds")
# d <- readRDS("ca_all_articles_japan_230324.rds")
names(d)

# write.csv(data.frame(journal=unique(d$journal), ssh=1),
#           "journal_coding_20230324.csv", row.names = F)
jdt <- read.csv("journal_coding_20230324_coded.csv")

## Supposedly Non Humanity & Social Science Journal Articles
(droptitle <- 
  d$title[which(d$journal%in%jdt$journal[which(jdt$ssh==0)])])

ds <- subset(d, !title %in% droptitle)

## Save Dataset for Analysis
saveRDS(ds, "ca_articles_japan_230324.rds")



