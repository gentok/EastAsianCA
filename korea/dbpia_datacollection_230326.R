## Clean Up Space
rm(list=ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rvest)
library(stringr)
library(readr)
library(pbapply)

library(readxl)
### Korean Dataset
d <- do.call("rbind", lapply(list.files("bib_korean")[grep("xlsx$",list.files("bib_korean"))],
                               function (k) read_xlsx(paste0("bib_korean/",k))))
### English Dataset
den <- do.call("rbind", lapply(list.files("bib_english")[grep("xlsx$",list.files("bib_english"))],
                             function (k) read_xlsx(paste0("bib_english/",k))))
colnames(d) <- colnames(den) <- c("id", "type", "publisher", "journal", 
                 "volume_issue", "title", "author", 
                 "year_month", "pages", "abstract", 
                 "url", "doi")
d$id <- den$id <- 1:892
all(d$url==den$url)

## All Articles
saveRDS(d, "ca_articles_korea_230326.rds")
# d <- readRDS("ca_articles_korea_230326.rds")

## Detect Language of Text ##

# devtools::install_github("ropensci/cld2")
d$title[which(d$title=="카자흐스탄의 유라시아주의(Eurasianism)와 대외정책")] <- 
  "카자흐스탄의 유라시아주의 와 대외정책"
d$title[which(d$title=="루넷(RuNet)과 유라시아-넷(Eurasia-Net): 중앙아시아에서의 러시아 인터넷의 위상")] <- 
  "루넷 과 유라시아-넷: 중앙아시아에서의 러시아 인터넷의 위상"
d$title_lang <- cld2::detect_language(d$title)
d$title_lang[which(d$title=="카자흐스탄: ESG 문서")] <- "ko"
table(d$title_lang, useNA="always")

## English Title ##

## All article matches
all(d$url==den$url)
## Check The Existence of English Titles
den$title_lang <- cld2::detect_language(den$title)
den$title_lang[which(den$title%in%c(
  "Central Asia", 
  "Russia ․ Uzbekistan ․ N.Korea and Uzbekistan Again:   A Life Story of Fergana Koryeoin Kim Leonid",
  "Migration Process of Корё-сарам of Central Asia and their National Identity"
))] <- "en"
### Titles detected as Uzbek are English
den$title[which(den$title_lang=="uz")]
den$title_lang[which(den$title_lang=="uz")] <- "en" 
### Some Undeteced titles are in English
table(den$title_lang, useNA="always")

## Add English Titles
d$title_en <- NA
d$title_en[which(d$title_lang=="en")] <- d$title[which(d$title_lang=="en")]
d$title_en[which(d$title_lang=="ko" & den$title_lang=="en")] <- 
  den$title[which(d$title_lang=="ko" & den$title_lang=="en")]
table(is.na(d$title_en))

## All Articles
saveRDS(d, "ca_articles_korea_230326.rds")
# d <- readRDS("ca_articles_korea_230326.rds")
names(d)

## Translate Using DeepL 
### Identifier for Translated text
d$translated <- ifelse(is.na(d$title_en),1,0)
table(d$translated)
### Text vector to Translate
koloc <- which(is.na(d$title_en) & d$title_lang=="ko")
ruloc <- which(is.na(d$title_en) & d$title_lang=="ru")
totranslate_ko <- d$title[koloc]
totranslate_ru <- d$title[ruloc]
sum(nchar(totranslate_ko))
sum(nchar(totranslate_ru))
### Prepare deeplr package
# devtools::install_github("zumbov2/deeplr")
# keyring::key_set("deepL", username="apikey")
dLkey <- keyring::key_get("deepL", username="apikey")
require(deeplr)
as.data.frame(deeplr::available_languages2(dLkey))
deeplr::usage2(dLkey) # how many more characters I can translate
### Translate from Japanese to English
title_en_translated_fromko <- 
  deeplr::translate2(totranslate_ko, 
                     source_lang = "KO",target_lang = "EN", 
                     auth_key = dLkey)
head(title_en_translated_fromko)
title_en_translated_fromru <- 
  deeplr::translate2(totranslate_ru, 
                     source_lang = "RU",target_lang = "EN", 
                     auth_key = dLkey)
head(title_en_translated_fromru)
### Store it in the title_en
d$title_en[koloc] <- title_en_translated_fromko
d$title_en[ruloc] <- title_en_translated_fromru
## All Articles
saveRDS(d, "ca_articles_korea_230326.rds")
# d <- readRDS("ca_articles_korea_230326.rds")

tmp <- str_split(d$year_month,"\\.")
d$year <- as.numeric(sapply(tmp, function(k) k[1]))
d$month <- as.numeric(sapply(tmp, function(k) k[2]))

## All Articles
saveRDS(d, "ca_articles_korea_230326.rds")
# d <- readRDS("ca_articles_korea_230326.rds")


## Adjust Some Translations ##

table(grepl("central asia|kazakh|kyrgyz|tajik|uzbek|turkmen", tolower(d$title_en)))
d$title[!grepl("central asia|kazakh|kyrgyz|tajik|uzbek|turkmen", tolower(d$title_en))]

d$title_en[which(d$title_en=="A Construction of Korean Residents Abroad’s Information Resources and DB: A Case Study of China, Central Asis, and Russia")] <- 
  "A Construction of Korean Residents Abroad’s Information Resources and DB: A Case Study of China, Central Asia, and Russia"

d$title_en[which(d$title_en=="A Study on the epic 'Manas' of the Former Soviet Central Russia")] <- 
  "A Study on the epic 'Manas' of the Former Soviet Central Asia"

d$title_en[which(d$title_en=="The Korean Theater’s Meaning for Being and Value")] <- 
  "The Korean Theater in Kazakhstan’s Meaning for Being and Value"

d$title_en[which(d$title_en=="A Study of the Buddhism in Kushan Empire Period   and its Religious Background for Understanding of   the Characteristics of Early Chinese Buddhism")] <-
  "A Study of the Buddhism in Central Asia in Kushan Empire Period and its Religious Background for Understanding of the Characteristics of Early Chinese Buddhism"

d$title_en[which(d$title_en=="Selection of Technology Platform for Mobile Banking Using the Analytical Hierarchy Process")] <- 
  "Selection of Technology Platform for Mobile Banking Using the Analytical Hierarchy Process: Focusing on Uzbek banks"

d$title_en[which(d$title_en=="Reflection on CSRs of Companies at Energy Blocks on Caspian Sea: Centering on Risk Management on Climate and Environmental Problems")] <- 
  "Reflection on CSRs of Companies at Energy Blocks on Caspian Sea in Kazakhstan: Centering on Risk Management on Climate and Environmental Problems"

d$title_en[which(d$title_en=="The Political Implications of the Customs Union - The Changed Scene of the Power Game -")] <- 
  "The Political Implications of the Customs Union - The New Great Game in Central Asia -"

d$title_en[which(d$title_en=="A Study on the national identity in the Poetical works of Chosun")] <- 
  "A Study on the national identity in the Poetical works of Central Asian Koreans"

d$title_en[which(d$title_en=="Research for Northeast Asian medical tourism market strategy (focusing on Russia and CIS country)")] <- 
  "Research for Northeast Asian medical tourism market strategy (focusing on Russia and CIS Central Asia)"

d$title_en[which(d$title_en=="A Survey of Transmission of Korean Traditional Folk Culture in Goryein Community of CIS -Focusing on Folk Culture of Goryein Kolkhoz in Tashkent Province of Uzebekistan-")] <- 
  "A Survey of Transmission of Korean Traditional Folk Culture in Goryein Community of CIS -Focusing on Folk Culture of Goryein Kolkhoz in Tashkent Province of Uzbekistan-"

d$title_en[which(d$title_en=="On the function of discourse markers museuk, museu, museugeo and museun in the Goryeo dialect of Korea")] <- 
  "On the function of discourse markers museuk, museu, museugeo and museun in the Goryeo dialect of Korea in Central Asia"

d$title_en[which(d$title_en=="An analysis of the film ⟪Walnut Tree》")] <- 
  "An analysis of the Kazakh film ⟪Walnut Tree》"

d$title_en[which(d$title_en=="The Development of Multi-Cultural Urban Structure of Tashkent")] <- 
  "The Development of Multi-Cultural Urban Structure of Tashkent, a Traditional Central Asian City"

d$title_en[which(d$title_en=="Linguistic Characteristics of Russia-Eurasia Regions: Focused on the Correlation among ‘Concept of Nation-National Language-Russian’")] <- 
  "Linguistic Characteristics of Russia-Eurasia Regions: Focused on the Correlation among ‘Concept of Nation-National Language-Russian’ in Central Asia"

d$title_en[which(d$title_en=="ODA Effects in Middle Asian Countries and Strategic Revitalization of Economic Cooperation")] <- 
  "ODA Effects in Central Asian Countries and Strategic Revitalization of Economic Cooperation"

d$title_en[which(d$title_en=="A Study on the Soviet-Korean Literary Criticism in “Lenin Gichi”")] <- 
  "A Study on the Central Asian Korean Literary Criticism in “Lenin Gichi”"

d$title_en[which(d$title_en=="The Relationship between GDP and FDI")] <- 
  "The Relationship between GDP and FDI in Kazakhstan" 

d$title_en[which(d$title_en=="The Eurasian Network in the Korean Academic Circle")] <- 
  "The Eurasian Network in the Korean Academic Circle in Central Asia: Its Reflections and Challenges"

d$title_en[which(d$title_en=="Influence of the Second World War on the establishment and Activity of SADUM")] <- 
  "Influence of the Second World War on the establishment and Activity of Central Asian Muslim Servants' Organization (SADUM)"

d$title_en[which(d$title_en=="A Study on the Relations between Social Capital and Development Project Satisfaction in Do’stlik Village: Focused on a Mediation Effect of the level of Interest")] <- 
  "A Study on the Relations between Social Capital and Development Project Satisfaction in Do’stlik Village in Uzbekistan: Focused on a Mediation Effect of the level of Interest"

d$title_en[which(d$title_en=="The Origin and Development of hugui(genuflect in foreign fashion)")] <- 
  "The Origin and Development of hugui (genuflect in foreign fashion): Cases of India, Central Asia, China, and Korea"

d$title_en[which(d$title_en=="A Study on The significance and Value of  Folktale in Korean Cultural Education from the Perspective of Comparative Literature- Focusing on the Heungbu&Nolbu tale of Korea, Vietnam and Uzb")] <- 
  "A Study on The significance and Value of  Folktale in Korean Cultural Education from the Perspective of Comparative Literature- Focusing on the Heungbu&Nolbu tale of Korea, Vietnam and Uzbekistan"

d$title_en[which(d$title_en=="Developing Korean Language Teacher Overseas Training Program for Its Sustainability and Development: focusing on 2018 Korean Language (prospective) Teacher Overseas Practical Training Assistance project by National Institute of Korean Language")] <- 
  "Developing Korean Language Teacher Overseas Training Program for Its Sustainability and Development: focusing on 2018 Korean Language (prospective) Teacher Overseas Practical Training Assistance project (Central Asia Region) by National Institute of Korean Language"

d$title_en[which(d$title_en=="The Legacy of Soviet Union: The Balance between Islam and Nationalism, and Tardy Democratization")] <- 
  "The Legacy of Soviet Union: The Balance between Islam and Nationalism, and Tardy Democratization, in The Present of Islam in Central Asia: Political, Social, and Economic Choices"

d$title_en[which(d$title_en=="New Research on the History of Central Eurasia and West Asia during 2016 to 2019 and Its Prospects - Diversification of Areas, Digitalization, and Challenges for the Globalization of Mongol and Türk Studies -")] <- 
  "[West and Central Asia] New Research on the History of Central Eurasia and West Asia during 2016 to 2019 and Its Prospects - Diversification of Areas, Digitalization, and Challenges for the Globalization of Mongol and Türk Studies -"

d$title_en[which(d$title_en=="Children in Buddhist Monastic Communities in Tarim Basin from 3rd to 7th Century")] <- 
  "Children in Buddhist Monastic Communities in Tarim Basin of Central Asia from 3rd to 7th Century"

d$title_en[which(d$title_en=="The Role and Vision of an Eurasian Korean Art Organization as a Cultural Hub - Focusing on Dance Art Activities of the State Republican Academic Korean Theater of Musical Comedy -")] <- 
  "The Role and Vision of an Eurasian Korean Art Organization as a Cultural Hub - Focusing on Dance Art Activities of the State Republican Academic Korean Theater of Musical Comedy in Kazakhstan -"

d$title_en[which(d$title_en=="Transnational Network and Identity of Bukharan Jews: Focusing on Analysis of Diasporic Media")] <- 
  "Transnational Network and Identity of Bukharan Jews, Central Asia: Focusing on Analysis of Diasporic Media"

table(grepl("central asia|kazakh|kyrgyz|tajik|uzbek|turkmen", tolower(d$title_en)))
d$title_en[!grepl("central asia|kazakh|kyrgyz|tajik|uzbek|turkmen", tolower(d$title_en))]

## Subset Articles to those mentioning keywords
ds <- subset(d, grepl("central asia|kazakh|kyrgyz|tajik|uzbek|turkmen", tolower(title_en)))
saveRDS(d, "ca_articles_korea_230326_key.rds")
# ds <- readRDS("ca_articles_korea_230326_key.rds")
