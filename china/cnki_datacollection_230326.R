## Clean Up Space
rm(list=ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rvest)
library(stringr)
library(readr)
library(pbapply)

# 427 + 446 + 585 + 633 + 756 + 911 + 1014 + 
#   1016 + 869 + 819 + 709 + 644 + 747 + 628 + 
#   568 + 538 + 501 + 436 + 333 + 263 + 286 + 
#   245 + 266

library(readxl)
### Chinese Dataset
filenames <- sort(list.files("bib_chinese")[grep("xls$",list.files("bib_chinese"))],
     decreasing = TRUE)
read_datafile <- function(filename) {
  tmp <- read_html(filename)
  html_node(tmp,"table") %>% html_table(header=TRUE) %>% as.data.frame()
}
tmp <- pblapply(filenames,
                function (k) read_datafile(paste0("bib_chinese/",k))[,1:13])
sapply(tmp, nrow)
d <- do.call("rbind", tmp)
colnames(d)
colnames(d) <- c("src", "title", "author", "affiliation", 
                 "src_literature", "keyword", "abstract", 
                 "time", "first_duty", "fund", 
                 "year", "period")
d$id <- 1:nrow(d)

## All Articles
saveRDS(d, "ca_articles_china_230326.rds")
# d <- readRDS("ca_articles_china_230326.rds")

## Detect Language of Text ##

# devtools::install_github("ropensci/cld2")
d$title_lang <- cld2::detect_language(d$title)
d$title_lang[which(d$title_lang=="zh-Hant")] # all look zh
d$title[is.na(d$title_lang)] # all look zh
d$title_lang <- "zh"
table(d$title_lang, useNA="always")

## Translate Using DeepL 
### Identifier for Translated text
d$title_en <- NA
d$translated <- 1
table(d$translated)
### Text vector to Translate
zhloc <- which(d$translated==1)
totranslate_zh <- d$title[zhloc]
sum(nchar(totranslate_zh))
### Prepare deeplr package
# devtools::install_github("zumbov2/deeplr")
# keyring::key_set("deepL", username="apikey")
dLkey <- keyring::key_get("deepL", username="apikey")
require(deeplr)
as.data.frame(deeplr::available_languages2(dLkey))
deeplr::usage2(dLkey) # how many more characters I can translate
### Translate from Chinese to English
title_en_translated_fromzh <- 
  deeplr::translate2(totranslate_zh, 
                     source_lang = "ZH",target_lang = "EN",
                     split_sentences = FALSE,
                     auth_key = dLkey)
head(title_en_translated_fromzh)
### Store it in the title_en
d$title_en[zhloc] <- title_en_translated_fromzh
## All Articles
saveRDS(d, "ca_articles_china_230326.rds")
# d <- readRDS("ca_articles_china_230326.rds")

## Selecting Social Science & Humanities Journals

# jdt <- data.frame(id = 1:3025, 
#                   journal = unique(d$src_literature),
#                   journal_en = NA,
#                   ssh = 1)
# sum(nchar(jdt$journal))
# ## Translate Journal Title
# # keyring::key_set("deepL", username="apikey")
# dLkey <- keyring::key_get("deepL", username="apikey")
# require(deeplr)
# as.data.frame(deeplr::available_languages2(dLkey))
# deeplr::usage2(dLkey) # how many more characters I can translate
# ### Translate from Chinese to English
# journal_en_translated <- 
#   deeplr::translate2(jdt$journal, 
#                      source_lang = "ZH",target_lang = "EN",
#                      split_sentences = FALSE,
#                      auth_key = dLkey)
# head(journal_en_translated)
# ### Store it in the journal_en
# jdt$journal_en <- journal_en_translated
# 
# saveRDS(jdt, "ca_journals_china_230336.rds")
# write.csv(jdt, "ca_journals_china_230326.csv", row.names=FALSE)
jdt <- read.csv("ca_journals_china_230326_humancoded.csv")  
## Set if SSH or not
d$ssh <- jdt$ssh[match(d$src_literature, jdt$journal)] 
table(d$ssh)
table(grepl("Central Asia|Kazakh|Kyrgyz|Tajik|Uzbek|Turkmen", d$title_en))


## Subset Data ##
ds <- subset(d, ssh==1 & grepl("Central Asia|Kazakh|Kyrgyz|Tajik|Uzbek|Turkmen", title_en))
dim(ds)

saveRDS(ds, "ca_articles_china_230326_ssh.rds")
# ds <- readRDS("ca_articles_china_230326_ssh.rds")
