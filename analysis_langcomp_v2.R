#' ---
#' title: "Replication of Central Asian Studies in East Asian Languages: Quantitative Meta-Analysis"
#' author: "Gento Kato"
#' date: "November 3, 2023"
#' ---
#' 
#' # Preparation 

## Clean Up Space
rm(list=ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Required Packages
library(stringr)
library(pbapply)
library(quanteda)
library(quanteda.textplots)
library(wordcloud)

## Import Data

## Web of Science ##
dws <- readRDS("wos/wos_res_230316.rds")
dws$country <- "Web of Science"
dws$id <- paste0("WS-",sprintf("%.5d",1:nrow(dws)))
dws$year <- dws$PY
dws$title <- dws$TI
dws$title_en <- tolower(dws$TI)
dws$title_eng <- "en"
dws <- subset(dws, year %in% 2000:2022)
dws$journal <- tolower(dws$JI)

## Japan ##
djp <- readRDS("japan/ca_articles_japan_230324.rds")
djp$country <- "Japan"
djp$id <- paste0("JP-",sprintf("%.5d",djp$id))
djp$year <- as.numeric(gsub("\\-.*$", "", djp$date))
djp$title_en <- tolower(djp$title_en)

## Korea ##
dkr <- readRDS("korea/ca_articles_korea_230326_key.rds")
dkr$country <- "Korea"
dkr$id <- paste0("KR-",sprintf("%.5d",dkr$id))
dkr$title_en <- tolower(dkr$title_en)

## China ##
dch <- readRDS("china/ca_articles_china_230326_ssh.rds")
dch$country <- "China"
dch$id <- paste0("CH-",sprintf("%.5d",dch$id))
dch$title_en <- tolower(dch$title_en)
dch$journal <- dch$src_literature
colnames(dch) <- names(dch)

## Combine Data ##
d <- rbind(dws[,c("id","country","year","title_en","title","journal")],
           dch[,c("id","country","year","title_en","title","journal")],
           dkr[,c("id","country","year","title_en","title","journal")],
           djp[,c("id","country","year","title_en","title","journal")])
d$country <- factor(d$country, 
                    levels = c("Web of Science","China",
                               "Korea","Japan"))

#'
#' # Preprocess Dataset for Text Analysis
#'

## Create Character Vector of Title + Abstract
tvec_tgt <- d$title_en %>% 
  str_squish() # clean unnecessary spacing & line breaks
head(tvec_tgt)

## Some Adjustment to the text
## Central Asia Related
tvec_tgt <- gsub("asian", "asia", tvec_tgt)
tvec_tgt <- gsub("\\-asia", " asia", tvec_tgt)
tvec_tgt <- gsub("asia\\-", "asia ", tvec_tgt)
tvec_tgt <- gsub("asia('|)s", "asia", tvec_tgt)
tvec_tgt <- gsub("kazakhstani", "kazakh", tvec_tgt)
tvec_tgt <- gsub("kazakhstan", "kazakh", tvec_tgt)
tvec_tgt <- gsub("\\-kazakh", " kazakh", tvec_tgt)
tvec_tgt <- gsub("kazakh\\-", "kazakh ", tvec_tgt)
tvec_tgt <- gsub("kazakh('|)s", "kazakh", tvec_tgt)
tvec_tgt <- gsub("kyrgyzstani", "kyrgyz", tvec_tgt)
tvec_tgt <- gsub("kyrgyzstan", "kyrgyz", tvec_tgt)
tvec_tgt <- gsub("\\-kyrgyz", " kyrgyz", tvec_tgt)
tvec_tgt <- gsub("kyrgyz\\-", "kyrgyz ", tvec_tgt)
tvec_tgt <- gsub("kyrgyz('|)s", "kyrgyz", tvec_tgt)
tvec_tgt <- gsub("tajikistani", "tajik", tvec_tgt)
tvec_tgt <- gsub("tajikistan", "tajik", tvec_tgt)
tvec_tgt <- gsub("\\-tajik", " tajik", tvec_tgt)
tvec_tgt <- gsub("tajik\\-", "tajik ", tvec_tgt)
tvec_tgt <- gsub("tajik('|)s", "tajik", tvec_tgt)
tvec_tgt <- gsub("uzbekistani", "uzbek", tvec_tgt)
tvec_tgt <- gsub("uzbekistan", "uzbek", tvec_tgt)
tvec_tgt <- gsub("\\-uzbek", " uzbek", tvec_tgt)
tvec_tgt <- gsub("uzbek\\-", "uzbek ", tvec_tgt)
tvec_tgt <- gsub("uzbek('|)s", "uzbek", tvec_tgt)
tvec_tgt <- gsub("turkmenistani", "turkmen", tvec_tgt)
tvec_tgt <- gsub("turkmenistan", "turkmen", tvec_tgt)
tvec_tgt <- gsub("\\-turkmen", " turkmen", tvec_tgt)
tvec_tgt <- gsub("turkmen\\-", "turkmen ", tvec_tgt)
tvec_tgt <- gsub("turkmen('|)s", "turkmen", tvec_tgt)
tvec_tgt <- gsub("afghanistani", "afghan", tvec_tgt)
tvec_tgt <- gsub("afghanistan", "afghan", tvec_tgt)
tvec_tgt <- gsub("\\-afghan", " afghan", tvec_tgt)
tvec_tgt <- gsub("afghan\\-", "afghan ", tvec_tgt)
tvec_tgt <- gsub("afghan('|)s", "afghan", tvec_tgt)
## East Asia Related
tvec_tgt <- gsub("chinese", "china", tvec_tgt)
tvec_tgt <- gsub("\\-china", " china", tvec_tgt)
tvec_tgt <- gsub("china\\-", "china ", tvec_tgt)
tvec_tgt <- gsub("(^| )sino\\-", "china ", tvec_tgt)
tvec_tgt <- gsub("china('|)s", "china", tvec_tgt)
tvec_tgt <- gsub("japanese", "japan", tvec_tgt)
tvec_tgt <- gsub("\\-japan", " japan", tvec_tgt)
tvec_tgt <- gsub("japan\\-", "japan ", tvec_tgt)
tvec_tgt <- gsub("japan('|)s", "japan", tvec_tgt)
tvec_tgt <- gsub("korean", "korea", tvec_tgt)
tvec_tgt <- gsub("\\-korea", " korea", tvec_tgt)
tvec_tgt <- gsub("\\-south korea", "south korea", tvec_tgt)
tvec_tgt <- gsub("\\-north korea", "north korea", tvec_tgt)
tvec_tgt <- gsub("korea\\-", "korea ", tvec_tgt)
tvec_tgt <- gsub("korea('|)s", "korea", tvec_tgt)
tvec_tgt <- gsub("taiwanese", "taiwan", tvec_tgt)
tvec_tgt <- gsub("\\-taiwan", " taiwan", tvec_tgt)
tvec_tgt <- gsub("taiwan\\-", "taiwan ", tvec_tgt)
tvec_tgt <- gsub("taiwan('|)s", "taiwan", tvec_tgt)
tvec_tgt <- gsub("\\-xinjiang", " xinjiang", tvec_tgt)
tvec_tgt <- gsub("xinjiang\\-", "xinjiang ", tvec_tgt)
tvec_tgt <- gsub("xinjiang('|)s", "xinjiang", tvec_tgt)
tvec_tgt <- gsub("\\-uyghur", " uyghur", tvec_tgt)
tvec_tgt <- gsub("uyghur\\-", "uyghur ", tvec_tgt)
tvec_tgt <- gsub("uyghur('|)s", "uyghur", tvec_tgt)
tvec_tgt <- gsub("ili kazakh( |$)", "ili-kazakh ", tvec_tgt) # Ili-Kazakh separate from Kazakh
tvec_tgt <- gsub("ilikazakh( |$)", "ili-kazakh ", tvec_tgt) # Ili-Kazakh separate from Kazakh
tvec_tgt <- gsub("yili\\-kazakh( |$)", "ili-kazakh ", tvec_tgt) # Ili-Kazakh separate from Kazakh
tvec_tgt <- gsub("(^| )yili( |$)", " ili ", tvec_tgt)
tvec_tgt <- gsub("\\-yili", " ili", tvec_tgt)
tvec_tgt <- gsub("yili\\-", "ili ", tvec_tgt)
tvec_tgt <- gsub("ili('|)s", "ili", tvec_tgt)
tvec_tgt <- gsub("mongolian", "mongol", tvec_tgt)
tvec_tgt <- gsub("\\-mongol", " mongol", tvec_tgt)
tvec_tgt <- gsub("mongol\\-", "mongol ", tvec_tgt)
tvec_tgt <- gsub("mongol('|)s", "mongol", tvec_tgt)
## South Asia 
tvec_tgt <- gsub("indian", "india", tvec_tgt)
tvec_tgt <- gsub("\\-india", " india", tvec_tgt)
tvec_tgt <- gsub("india\\-", "india ", tvec_tgt)
tvec_tgt <- gsub("india('|)s", "india", tvec_tgt)
tvec_tgt <- gsub("pakistani", "pakistan", tvec_tgt)
tvec_tgt <- gsub("\\-pakistan", " pakistan", tvec_tgt)
tvec_tgt <- gsub("pakistan\\-", "pakistan ", tvec_tgt)
tvec_tgt <- gsub("pakistan('|)s", "pakistan", tvec_tgt)
#### No Bangladesh
tvec_tgt <- gsub("\\-bhutan", " bhutan", tvec_tgt)
tvec_tgt <- gsub("bhutan\\-", "bhutan ", tvec_tgt)
tvec_tgt <- gsub("bhutan('|)s", "bhutan", tvec_tgt)
tvec_tgt <- gsub("nepali", "nepal", tvec_tgt)
tvec_tgt <- gsub("\\-nepal", " nepal", tvec_tgt)
tvec_tgt <- gsub("nepal\\-", "nepal ", tvec_tgt)
tvec_tgt <- gsub("nepal('|)s", "nepal", tvec_tgt)
#### No Sri Lanka
## Southeast Asia
#### There is asean, no transformation
#### No Brunei
#### No Cambodia
#### No East Timor
#### No Indonesia
#### NO Laos
#### No Malaysia
#### No Myanmar
tvec_tgt <- gsub("philippino", "philippines", tvec_tgt)
tvec_tgt <- gsub("\\-philippines", " philippines", tvec_tgt)
tvec_tgt <- gsub("philippines\\-", "philippines ", tvec_tgt)
tvec_tgt <- gsub("philippines('|)s", "philippines", tvec_tgt)
tvec_tgt <- gsub("thailand", "thai", tvec_tgt)
tvec_tgt <- gsub("\\-thai", " thai", tvec_tgt)
tvec_tgt <- gsub("thai\\-", "thai ", tvec_tgt)
tvec_tgt <- gsub("thai('|)s", "thai", tvec_tgt)
tvec_tgt <- gsub("singaporean", "singapore", tvec_tgt)
tvec_tgt <- gsub("\\-singapore", " singapore", tvec_tgt)
tvec_tgt <- gsub("singapore\\-", "singapore ", tvec_tgt)
tvec_tgt <- gsub("singapore('|)s", "singapore", tvec_tgt)
tvec_tgt <- gsub("vietnamese", "vietnam", tvec_tgt)
tvec_tgt <- gsub("\\-vietnam", " vietnam", tvec_tgt)
tvec_tgt <- gsub("vietnam\\-", "vietnam ", tvec_tgt)
tvec_tgt <- gsub("vietnam('|)s", "vietnam", tvec_tgt)
## Russia and Post Soviet
tvec_tgt <- gsub("russian", "russia", tvec_tgt)
tvec_tgt <- gsub("\\-russia", " russia", tvec_tgt)
tvec_tgt <- gsub("russia\\-", "russia ", tvec_tgt)
tvec_tgt <- gsub("russia('|)s", "russia", tvec_tgt)
tvec_tgt <- gsub("\\-soviet", " soviet", tvec_tgt)
tvec_tgt <- gsub("soviet\\-", "soviet ", tvec_tgt)
tvec_tgt <- gsub("soviet('|)s", "soviet", tvec_tgt)
tvec_tgt <- gsub("(former|post) soviet", "post-soviet", tvec_tgt)
tvec_tgt <- gsub("belarusian", "belarus", tvec_tgt)
tvec_tgt <- gsub("\\-belarus", " belarus", tvec_tgt)
tvec_tgt <- gsub("belarus\\-", "belarus ", tvec_tgt)
tvec_tgt <- gsub("belarus'", "belarus", tvec_tgt)
tvec_tgt <- gsub("moldovan|moldovian", "moldova", tvec_tgt)
tvec_tgt <- gsub("\\-moldova", " moldova", tvec_tgt)
tvec_tgt <- gsub("moldova\\-", "moldova ", tvec_tgt)
tvec_tgt <- gsub("moldova('|)s", "moldova", tvec_tgt)
tvec_tgt <- gsub("ukrainian", "ukraine", tvec_tgt)
tvec_tgt <- gsub("\\-ukraine", " ukraine", tvec_tgt)
tvec_tgt <- gsub("ukraine\\-", "ukraine ", tvec_tgt)
tvec_tgt <- gsub("ukraine('|)s", "ukraine", tvec_tgt)
tvec_tgt <- gsub("armenian", "armenia", tvec_tgt)
tvec_tgt <- gsub("\\-armenia", " armenia", tvec_tgt)
tvec_tgt <- gsub("armenia\\-", "armenia ", tvec_tgt)
tvec_tgt <- gsub("armenia('|)s", "armenia", tvec_tgt)
tvec_tgt <- gsub("azerbaijani", "azerbaijan", tvec_tgt)
tvec_tgt <- gsub("\\-azerbaijan", " azerbaijan", tvec_tgt)
tvec_tgt <- gsub("azerbaijan\\-", "azerbaijan ", tvec_tgt)
tvec_tgt <- gsub("azerbaijan('|)s", "azerbaijan", tvec_tgt)
tvec_tgt <- gsub("georgian", "georgia", tvec_tgt)
tvec_tgt <- gsub("\\-georgia", " georgia", tvec_tgt)
tvec_tgt <- gsub("georgia\\-", "georgia ", tvec_tgt)
tvec_tgt <- gsub("georgia('|)s", "georgia", tvec_tgt)
## Middle East/West Asia (as they appear at least once)
tvec_tgt <- gsub("turkish", "turkey", tvec_tgt)
tvec_tgt <- gsub("\\-turkey", " turkey", tvec_tgt)
tvec_tgt <- gsub("turkey\\-", "turkey ", tvec_tgt)
tvec_tgt <- gsub("turkey('|)s", "turkey", tvec_tgt)
#### No Oman
tvec_tgt <- gsub("saudi arabian", "saudi arabia", tvec_tgt)
tvec_tgt <- gsub("\\-saudi( arabia|)", " saudi arabia", tvec_tgt)
tvec_tgt <- gsub("saudi( arabia|)\\-", "saudi arabia ", tvec_tgt)
tvec_tgt <- gsub("saudi( arabia|)('|)s", "saudi arabia", tvec_tgt)
#### uae appears, no transformation made
tvec_tgt <- gsub("iraqi", "iraq", tvec_tgt)
tvec_tgt <- gsub("\\-iraq", " iraq", tvec_tgt)
tvec_tgt <- gsub("iraq\\-", "iraq ", tvec_tgt)
tvec_tgt <- gsub("iraq('|)s", "iraq", tvec_tgt)
tvec_tgt <- gsub("israeli", "israel", tvec_tgt)
tvec_tgt <- gsub("\\-israel", " israel", tvec_tgt)
tvec_tgt <- gsub("israel\\-", "israel ", tvec_tgt)
tvec_tgt <- gsub("israel('|)s", "israel", tvec_tgt)
tvec_tgt <- gsub("jordanian", "jordan", tvec_tgt)
tvec_tgt <- gsub("\\-jordan", " jordan", tvec_tgt)
tvec_tgt <- gsub("jordan\\-", "jordan ", tvec_tgt)
tvec_tgt <- gsub("jordan('|)s", "jordan", tvec_tgt)
tvec_tgt <- gsub("syrian", "syria", tvec_tgt)
tvec_tgt <- gsub("\\-syria", " syria", tvec_tgt)
tvec_tgt <- gsub("syria\\-", "syria ", tvec_tgt)
tvec_tgt <- gsub("syria('|)s", "syria", tvec_tgt)
tvec_tgt <- gsub("iranian", "iran", tvec_tgt)
tvec_tgt <- gsub("\\-iran", " iran", tvec_tgt)
tvec_tgt <- gsub("iran\\-", "iran ", tvec_tgt)
tvec_tgt <- gsub("iran('|)s", "iran", tvec_tgt)
## West Related (according to Huntington's definition of western world)
tvec_tgt <- gsub("european", "europe", tvec_tgt)
tvec_tgt <- gsub("\\-europe", " europe", tvec_tgt)
tvec_tgt <- gsub("europe\\-", "europe ", tvec_tgt)
tvec_tgt <- gsub("europe('|)s", "europe", tvec_tgt)
tvec_tgt <- gsub("\\-eu( |$)", " eu ", tvec_tgt)
tvec_tgt <- gsub("eu\\-", "eu ", tvec_tgt)
tvec_tgt <- gsub("eu('|)s", "eu", tvec_tgt)
### Specific countries as they appear once or more
tvec_tgt <- gsub("u\\.s\\.( |$)", "usa ", tvec_tgt)
#### also search for america
tvec_tgt <- gsub("canadian", "canada", tvec_tgt)
tvec_tgt <- gsub("\\-canada", " canada", tvec_tgt)
tvec_tgt <- gsub("canada\\-", "canada ", tvec_tgt)
tvec_tgt <- gsub("canada('|)s", "canada", tvec_tgt)
tvec_tgt <- gsub("australian", "australia", tvec_tgt)
tvec_tgt <- gsub("\\-australia", " australia", tvec_tgt)
tvec_tgt <- gsub("australia\\-", "australia ", tvec_tgt)
tvec_tgt <- gsub("australia('|)s", "australia", tvec_tgt)
tvec_tgt <- gsub("french", "france", tvec_tgt)
tvec_tgt <- gsub("\\-france", " france", tvec_tgt)
tvec_tgt <- gsub("france\\-", "france ", tvec_tgt)
tvec_tgt <- gsub("france('|)(e|)s", "france", tvec_tgt)
tvec_tgt <- gsub("spanish", "spain", tvec_tgt)
tvec_tgt <- gsub("\\-spain", " spain", tvec_tgt)
tvec_tgt <- gsub("spain\\-", "spain ", tvec_tgt)
tvec_tgt <- gsub("spain('|)(e|)s", "spain", tvec_tgt)
tvec_tgt <- gsub("swedish", "sweden", tvec_tgt)
tvec_tgt <- gsub("\\-sweden", " sweden", tvec_tgt)
tvec_tgt <- gsub("sweden\\-", "sweden ", tvec_tgt)
tvec_tgt <- gsub("sweden('|)(e|)s", "sweden", tvec_tgt)
#### NO norway
tvec_tgt <- gsub("german", "germany", tvec_tgt)
tvec_tgt <- gsub("\\-germany", " germany", tvec_tgt)
tvec_tgt <- gsub("germany\\-", "germany ", tvec_tgt)
tvec_tgt <- gsub("germany('|)(e|)s", "germany", tvec_tgt)
#### No Finland
tvec_tgt <- gsub("italian", "italy", tvec_tgt)
tvec_tgt <- gsub("\\-italy", " italy", tvec_tgt)
tvec_tgt <- gsub("italy\\-", "italy ", tvec_tgt)
tvec_tgt <- gsub("italy('|)(e|)s", "italy", tvec_tgt)
tvec_tgt <- gsub("(^|)uk( |$)", " britain ", tvec_tgt)
tvec_tgt <- gsub("british", "britain", tvec_tgt)
tvec_tgt <- gsub("\\-britain", " britain", tvec_tgt)
tvec_tgt <- gsub("britain\\-", "britain ", tvec_tgt)
tvec_tgt <- gsub("britain('|)(e|)s", "britain", tvec_tgt)
#### No Iceland
#### No Portugal
tvec_tgt <- gsub("austrian", "austria", tvec_tgt)
tvec_tgt <- gsub("\\-austria", " austria", tvec_tgt)
tvec_tgt <- gsub("austria\\-", "austria ", tvec_tgt)
tvec_tgt <- gsub("austria('|)(e|)s", "austria", tvec_tgt)
#### No Ireland
#### No Denmark
#### No Swizerland 
#### No Netherlands
#### No Belgium
#### No Luxembourg
#### No Andorra
#### No Malta
#### No Liechtenstein
#### NO San Marino
#### No Monaco
#### No Vatican
#### No Cyprus
### Eastern Europe
tvec_tgt <- gsub("polish", "poland", tvec_tgt)
tvec_tgt <- gsub("\\-poland", " poland", tvec_tgt)
tvec_tgt <- gsub("poland\\-", "poland ", tvec_tgt)
tvec_tgt <- gsub("poland('|)(e|)s", "poland", tvec_tgt)
#### No Romania
tvec_tgt <- gsub("bulgarian", "bulgaria", tvec_tgt)
tvec_tgt <- gsub("\\-bulgaria", " bulgaria", tvec_tgt)
tvec_tgt <- gsub("bulgaria\\-", "bulgaria ", tvec_tgt)
tvec_tgt <- gsub("bulgaria('|)(e|)s", "bulgaria", tvec_tgt)
### NO Hungary
tvec_tgt <- gsub("\\-yugoslavia", " yugoslavia", tvec_tgt)
tvec_tgt <- gsub("yugoslavia\\-", "yugoslavia ", tvec_tgt)
tvec_tgt <- gsub("yugoslavia('|)(e|)s", "yugoslavia", tvec_tgt)
tvec_tgt <- gsub("serbian", "serbia", tvec_tgt)
tvec_tgt <- gsub("\\-serbia", " serbia", tvec_tgt)
tvec_tgt <- gsub("serbia\\-", "serbia ", tvec_tgt)
tvec_tgt <- gsub("serbia('|)(e|)s", "serbia", tvec_tgt)
#### Czech republic as it is
#### NO Lithuania
tvec_tgt <- gsub("estonian", "estonia", tvec_tgt)
tvec_tgt <- gsub("\\-estonia", " estonia", tvec_tgt)
tvec_tgt <- gsub("estonia\\-", "estonia ", tvec_tgt)
tvec_tgt <- gsub("estonia('|)(e|)s", "estonia", tvec_tgt)
tvec_tgt <- gsub("latvian", "latvia", tvec_tgt)
tvec_tgt <- gsub("\\-latvia", " latvia", tvec_tgt)
tvec_tgt <- gsub("latvia\\-", "latvia ", tvec_tgt)
tvec_tgt <- gsub("latvia('|)(e|)s", "latvia", tvec_tgt)
#### No Croatia
#### No Bosnia and Herzegovia
#### No Slovakia
tvec_tgt <- gsub("albanian", "albania", tvec_tgt)
tvec_tgt <- gsub("\\-albania", " albania", tvec_tgt)
tvec_tgt <- gsub("albania\\-", "albania ", tvec_tgt)
tvec_tgt <- gsub("albania('|)(e|)s", "albania", tvec_tgt)
#### No Macedonia
#### No Slovenia
#### No Montenegro
### Adjust regions within country (except for korea)
tvec_tgt <- gsub("northern", "north", tvec_tgt)
tvec_tgt <- gsub("southern", "south", tvec_tgt)
tvec_tgt <- gsub("western", "west", tvec_tgt)
tvec_tgt <- gsub("eastern", "east", tvec_tgt)
tvec_tgt <- gsub("(^| )west region", " west-region", tvec_tgt)
tvec_tgt <- gsub("(^| )south region", " south-region", tvec_tgt)
tvec_tgt <- gsub("(^| )north region", " north-region", tvec_tgt)
tvec_tgt <- gsub("(^| )east region", " east-region", tvec_tgt)
tvec_tgt <- gsub("east and central asia", "east asia and central asia", tvec_tgt)
tvec_tgt <- gsub("south and central asia", "south asia and central asia", tvec_tgt)
tvec_tgt <- gsub("west and central asia", "west asia and central asia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) central asia", "central asia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) kazakh", "kazakh", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) kyrgyz", "kyrgyz", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) tajik", "tajik", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) uzbek", "uzbek", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) turkmen", "turkmen", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) xingiang", "xingiang", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) mongol", "mongol", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) afghan", "afghan", tvec_tgt)
tvec_tgt <- gsub("(north|south|east|northeast|southwest|southeast|central) china", "china", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) japan", "japan", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) taiwan", "taiwan", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) india", "india", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) pakistan", "pakistan", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) nepal", "nepal", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) bhutan", "bhutan", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) philippines", "philippines", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) thai", "thai", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) vietnam", "vietnam", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) russia", "russia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) soviet", "soviet", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) post-soviet", "post-soviet", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) belarus", "belarus", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) moldova", "moldova", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) ukraine", "ukraine", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) armenia", "armenia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) azerbaijan", "azerbaijan", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) georgia", "georgia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) iraq", "iraq", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) israel", "israel", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) jordan", "jordan", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) syria", "syria", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) iran", "iran", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) usa", "usa", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) canada", "canada", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) australia", "australia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) france", "france", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) spain", "spain", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) sweden", "sweden", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) germany", "germany", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) italy", "italy", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) britain", "britain", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) austria", "austria", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) poland", "poland", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) romania", "romania", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) bulgaria", "bulgaria", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) yugoslavia", "yugoslavia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) serbia", "serbia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) estonia", "estonia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) latvia", "latvia", tvec_tgt)
tvec_tgt <- gsub("(north|south|west|east|northwest|northeast|southwest|southeast|central) albania", "albania", tvec_tgt)
## Other geographic names fix
tvec_tgt <- gsub("bukharan", "bukhara", tvec_tgt)
tvec_tgt <- gsub("china west", "china-west", tvec_tgt)
tvec_tgt <- gsub("northwest china", "northwest-china", tvec_tgt)
tvec_tgt <- gsub("(^| )west china", " west-china", tvec_tgt)
tvec_tgt <- gsub("(^| )west liao", " liao", tvec_tgt)
tvec_tgt <- gsub("(^| )west half", " west-half", tvec_tgt)
tvec_tgt <- gsub("(^| )west drilling", " western-drilling", tvec_tgt)
tvec_tgt <- gsub("(^| )west station", " station", tvec_tgt)
tvec_tgt <- gsub("(^| )west chu", " chu", tvec_tgt)
tvec_tgt <- gsub("(^| )american west( |$)", " america ", tvec_tgt)
tvec_tgt[grep("西部", d$title)][grep("(^| )west( |$)",tvec_tgt[grep("西部", d$title)])] <- 
  gsub("west", "china-west", tvec_tgt[grep("西部", d$title)][grep("(^| )west( |$)",tvec_tgt[grep("西部", d$title)])])
tvec_tgt[grep("西迁", d$title)][grep("(^| )west( |$)",tvec_tgt[grep("西迁", d$title)])] <- 
  gsub("xibe west", "xibe", tvec_tgt[grep("西迁", d$title)][grep("(^| )west( |$)",tvec_tgt[grep("西迁", d$title)])])
tvec_tgt[grep("西迁", d$title)][grep("(^| )west( |$)",tvec_tgt[grep("西迁", d$title)])] <- 
  gsub("west migration", "migration", tvec_tgt[grep("西迁", d$title)][grep("(^| )west( |$)",tvec_tgt[grep("西迁", d$title)])])
tvec_tgt[grep("西域", d$title)][grep("(^| )west( |$)",tvec_tgt[grep("西域", d$title)])] <- 
  gsub("west", "west-part", tvec_tgt[grep("西域", d$title)][grep("(^| )west( |$)",tvec_tgt[grep("西域", d$title)])])
tvec_tgt <- gsub("(^| )west-part asia", " west asia", tvec_tgt)
## Other fixes
tvec_tgt <- gsub("one belt\\-one road", "one belt one road", tvec_tgt) 
tvec_tgt <- gsub("belt (and|\\&) road", "one belt one road", tvec_tgt) 
tvec_tgt <- gsub("one belt (and|\\&) one road", "one belt one road", tvec_tgt) 
tvec_tgt <- gsub("belt road", "one belt one road", tvec_tgt) 
tvec_tgt <- gsub("opening to the west", "opening-to-the-west", tvec_tgt) 
tvec_tgt <- gsub("web site", "website", tvec_tgt)
tvec_tgt <- gsub("great powers", "great power", tvec_tgt)
tvec_tgt <- gsub("great games", "great game", tvec_tgt)
tvec_tgt <- gsub("great influences", "great influence", tvec_tgt)
tvec_tgt <- gsub("geopolitical and economic game", "geopolitical game and economic game", tvec_tgt)
tvec_tgt <- gsub("post\\-9\\/11", "post-9-11", tvec_tgt)

## Squish Everything
tvec_tgt <- str_squish(tvec_tgt)

## Generate Corpus
cs_tgt <- corpus(tvec_tgt, docnames=d$id)
head(cs_tgt)

## Generate Tokens
### Multi-word terms to be detected.
multiword <- c("central asia", "south korea", "north korea",
               "silk road", "economic belt", "higher education",
               "nuclear power", 
               "power industry","power sector",
               "power industries","power sectors",
               "one belt one road", "one belt", "hong kong",
               "prime minister", 
               "west asia", "east asia", "southeast asia", 
               "south asia",
               "middle east", "far east", "saudi arabia",
               "st petersburg",
               # "west europe", 
               "east europe", "central europe", 
               "west world", #"opening to west", 
               "west countries","west broadcasting", 
               #"west sky", "red west",
               "west fanned",
               "latin america", "north america", 
               "central america",
               "small game",
               "soft power","power politics","state power",
               "great power game","great game","big game",
               "great power","great influence contest",
               "power contest",
               "central asia game","geopolitical game",
               "security game","energy game", "strategic game",
               "economic game","military base game",
               "military game", "multi-state game",
               "russia american game","russia us game",
               "usa russia game", "political game",
               "energy cooperation game","geostrategic game",
               "chess game") 
tk_tgt <- tokens(cs_tgt, remove_punct = TRUE) %>% 
  tokens_compound(pattern=phrase(multiword))

## Keywords in Context (show some examples)
head(kwic(tk_tgt, "central_asia"))
head(kwic(tk_tgt, "kazakh"))
head(kwic(tk_tgt, "ili-kazakh"))
head(kwic(tk_tgt, "uzbek"))
head(kwic(tk_tgt, "road"))
head(kwic(tk_tgt, "situation"))
head(kwic(tk_tgt, "belt"), 30)
head(kwic(tk_tgt, "political"))
head(kwic(tk_tgt, "relations"))
head(kwic(tk_tgt, "characteristics"))
head(kwic(tk_tgt, "system"))
head(kwic(tk_tgt, "current"))
head(kwic(tk_tgt, "role"))
head(kwic(tk_tgt, "influence"))
head(kwic(tk_tgt, "higher"))
head(kwic(tk_tgt, "evaluation"))
head(kwic(tk_tgt, "site"))
head(kwic(tk_tgt, "east"))
tail(kwic(tk_tgt, "western"),10)
head(kwic(sample(tk_tgt), "east"))
head(kwic(sample(tk_tgt), "system"),50)

# ## Stem Tokens (Not used, but for reference)
# SnowballC::getStemLanguages()
# tk_tgt_stemmed <- tokens_wordstem(tk_tgt, language="english")
# head(kwic(tk_tgt_stemmed, "evalu"))

## Document Feature Matrix with All Words
dfm_tgt <-  dfm(tk_tgt) %>% 
  dfm_remove(stopwords("english"))
dfm_tgt[1:5,1:5]

#'
#' # Detecting Research Sites
#'

## Count total number of keyword appearance per document
d$key_central_asia <- ((dfm_tgt[,c("central_asia")])>0)*1
d$key_kazakh <- 
  (rowSums(dfm_tgt[,which(colnames(dfm_tgt)%in%
                                          c("kazakh", "astana", "nur-sultan", 
                                            "almaty", "shymkent", 
                                            "aktobe", "karaganda", 
                                            "taraz", "semey", "pavlodar", "oskemen",
                                            "kostanay", "kyzylorda", "petropavl"))])>0)*1
d$key_kyrgyz <- 
  (rowSums(dfm_tgt[,which(colnames(dfm_tgt)%in% 
                                          c("kyrgyz", "bishkek", "osh"))])>0)*1
d$key_tajik <- 
  (rowSums(dfm_tgt[,which(colnames(dfm_tgt)%in% 
                                          c("tajik", "dushanbe"))])>0)*1
d$key_uzbek <- 
  (rowSums(dfm_tgt[,which(colnames(dfm_tgt)%in%
                                          c("uzbek", "tashkent", "samarkand", 
                                            "namangan", "andijan", "nukus", 
                                            "fergana", "bukhara", "qarshi", 
                                            "kokand", "margilan"))])>0)*1
d$key_turkmen <- 
  (rowSums(dfm_tgt[,which(colnames(dfm_tgt)%in% 
                                          c("turkmen", "ashgabat", 
                                            "turkmenabat", "dasoguz"))])>0)*1

d$key_east_asia <- 
  (rowSums(dfm_tgt[,c("east_asia","far_east")])>0)*1
d$key_china <- 
  (rowSums(dfm_tgt[,c("china","beijing","shanghai","hong_kong", "guangzhou",
                      "chengdu", "tianjin", "chongqing", "wuhan", "xian", "xi'an",
                      "hangzhou", "harbin", "dalian", "qingdao", "hefei", "xiamen",
                      "fuzhou", "changzhou", "lanzhou", "tibet", "fujian", "guangdong",
                      "gansu", "guizhou", "henan", "hubei", "hebei", "heilongjiang",
                      "jiangsu", "liaoning", "qinghai", "sichuan", "shandong",
                      "shaanxi", "shanxi", "yunnan", "zhejiang")])>0)*1
d$key_xinjiang <- 
  (rowSums(dfm_tgt[,c("xinjiang","uyghur","ili-kazakh","ili",
                    "china-west","northwest-china","west-china",
                    "urumqi", "karamay", "aksu", "altay", 
                    "alashankou", "changji", "fukang", "hotan", 
                    "kashgar", "shihezi", "tacheng", "yining")])>0)*1
d$key_japan <- 
  (rowSums(dfm_tgt[,c("japan","tokyo", "hokkaido", "nara")])>0)*1
d$key_korea <- 
  (rowSums(dfm_tgt[,c("south_korea", "korea", 
                      "seoul", "daegu", "gwangju")])>0)*1
d$key_taiwan <- 
  (rowSums(dfm_tgt[,c("taiwan")])>0)*1
d$key_north_korea <- 
  (rowSums(dfm_tgt[,c("north_korea")])>0)*1
d$key_mongol <- 
  (rowSums(dfm_tgt[,c("mongol")])>0)*1
d$key_afghan <- 
  (rowSums(dfm_tgt[,c("afghan")])>0)*1
d$key_south_asia <- 
  (rowSums(dfm_tgt[,c("south_asia","india","pakistan","nepal","bhutan")])>0)*1
d$key_southeast_asia <- 
  (rowSums(dfm_tgt[,c("southeast_asia","asean","philippines","thai","singapore","vietnam")])>0)*1
d$key_middle_east <- 
  (rowSums(dfm_tgt[,c("middle_east", "west_asia",
                      "iraq","israel","jordan","syria","iran",
                      "turkey", #"oman",
                      "saudi_arabia")])>0)*1
d$key_russia <- (rowSums(dfm_tgt[,c("russia",
                                    "russia_american_game",
                                    "russia_us_game",
                                    "usa_russia_game",
                                    "moscow", "st_petersburg", "chelyabinsk",
                                    "volgograd", "vladivostok")])>0)*1
d$key_soviet <- 
  (rowSums(dfm_tgt[,c("post-soviet", "cis",
                      "soviet","belarus","moldova",
                      "ukraine","armenia","azerbaijan","georgia",
                      "estonia","latvia")])>0)*1


d$key_west <- 
  (rowSums(dfm_tgt[,c("usa","us","america","american","americans",
                    "canada","australia",
                    "europe","eu", #"west_europe",
                    "france","spain","sweden","germany",
                    "italy","britain", "austria",
                    "west_world", "opening-to-the-west",
                    "west_countries","west_broadcasting",
                    #"west_sky","red_west",
                    "west_fanned",
                    "russia_american_game","russia_us_game",
                    "usa_russia_game")])>0)*1
d$key_east_europe <- 
  (rowSums(dfm_tgt[,c("east_europe", "poland",
                    "bulgaria","yugoslavia",
                    "serbia","czech","albania")])>0)*1
d$key_great_game <- 
  (rowSums(dfm_tgt[,c("great_power_game","great_game","big_game",
                      "great_power","great_influence_contest",
                      # "power_contest",
                      "central_asia_game","geopolitical_game",
                      "security_game","energy_game",
                      "strategic_game",
                      "economic_game","military_base_game",
                      "military_game","multi-state_game",
                      "russia_american_game","russia_us_game",
                      "usa_russia_game","political_game",
                      "energy_cooperation_game",
                      "geostrategic_game")])>0)*1

## Generate Annual Dataset for plotting 
keydt <- data.frame(country = levels(d$country))
## Add key-count average per country
keydt$central_asia <- tapply(d$key_central_asia, d$country, mean)
keydt$kazakh <- tapply(d$key_kazakh, d$country, mean)
keydt$kyrgyz <- tapply(d$key_kyrgyz, d$country, mean)
keydt$tajik <- tapply(d$key_tajik, d$country, mean)
keydt$uzbek <- tapply(d$key_uzbek, d$country, mean)
keydt$turkmen <- tapply(d$key_turkmen, d$country, mean)
## East Asia
keydt$east_asia <- tapply(d$key_east_asia, d$country, mean)
keydt$china <- tapply(d$key_china, d$country, mean)
keydt$xinjiang <- tapply(d$key_xinjiang, d$country, mean)
keydt$mongol <- tapply(d$key_mongol, d$country, mean)
keydt$japan <- tapply(d$key_japan, d$country, mean)
keydt$korea <- tapply(d$key_korea, d$country, mean)
keydt$north_korea <- tapply(d$key_north_korea, d$country, mean)
keydt$taiwan <- tapply(d$key_taiwan, d$country, mean)
## Middle East & South Asia
keydt$south_asia <- tapply(d$key_south_asia, d$country, mean)
keydt$southeast_asia <- tapply(d$key_southeast_asia, d$country, mean)
keydt$afghan <- tapply(d$key_afghan, d$country, mean)
keydt$middle_east <- tapply(d$key_middle_east, d$country, mean)
## Post Soviet & East Europe & West
keydt$great_game <- tapply(d$key_great_game, d$country, mean)
keydt$russia <- tapply(d$key_russia, d$country, mean)
keydt$soviet <- tapply(d$key_soviet, d$country, mean)
keydt$east_europe <- tapply(d$key_east_europe, d$country, mean)
keydt$west <- tapply(d$key_west, d$country, mean)

require(reshape2)
keydt_long <- melt(keydt, id = "country")
keydt_long$country <- factor(keydt_long$country, 
                             levels = rev(unique(keydt_long$country)),
                             labels = rev(c("Web of Science", 
                                            "CNKI (China)",
                                            "DBpia (Korea)",
                                            "CiNii (Japan)")))
keydt_long$variable <- 
  factor(keydt_long$variable, levels=(unique(keydt_long$variable)),
         labels = (c("Central Asia\n(as a region)", "Kazakhstan", 
                     "Kyrgyz", "Tajikistan", 
                      "Uzbekistan", "Turkmenistan",
                     "East Asia\n(as a region)",
                     "China", "Xinjiang\n(China-West)",
                     "Mongol", 
                     "Japan", "Korea\n(South Korea)", 
                     "North Korea","Taiwan",
                     "South Asia", "Southeast Asia", 
                     "Afghanistan", "Middle East",
                     "Great Game",
                     "Russia", "(Post-)Soviet",
                     "East Europe", "West")))

#'
#' ## Figure 1 
#'

## Plot Central Asia ##

#+ fig.width = 7, fig.height = 4.5
require(ggplot2)
ggplot(subset(keydt_long, variable%in%
                c("Central Asia\n(as a region)",
                  "Kazakhstan", "Kyrgyz",
                  "Tajikistan", "Uzbekistan",
                  "Turkmenistan") & 
                !country %in% "TCI (Taiwan)"), aes(x=country, y=value)) + 
  geom_col() + 
  coord_flip(ylim = c(0, 0.65)) + 
  facet_wrap(vars(variable)) + 
  labs(y = "Propotion of article titles contatining keywords",
       x = "Publication Database",
       title = "Cross-National Variations of \n Central Asian Studies' Research Sites",
       caption = "Note: Keywords for each country include name of the country and its major cities.") + 
  theme_bw() + 
  theme(legend.position = c(0.75,0.8),
        legend.background = element_rect(color="black"),
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=1, size=8))

#+ eval=FALSE
ggsave("cv_central_asia.png", width = 7, height = 4.5)

#'
#' ## Figure 2 
#'

## Plot East Asia ##

#+ fig.width = 7, fig.height = 4.5
require(ggplot2)
ggplot(subset(keydt_long, variable%in%
                c("East Asia\n(as a region)",
                  "China", "Xinjiang\n(China-West)",
                  "Mongolia", 
                  "Korea\n(South Korea)", 
                  #"North Korea",
                  "Japan")#, 
                  #"Taiwan")
                  ), 
       aes(x=country, y=value)) + 
  geom_col() + 
  coord_flip(ylim = c(0, 0.325)) + 
  facet_wrap(vars(variable)) + 
  labs(y = "Propotion of article titles contatining keywords",
       x = "Publication Database",
       title = "Cross-National Variations of East Asian Countries'\nPresence in Central Asian Studies",
       caption = "Note: Keyword is each country's name (for Korea, it includes 'Korea' and 'South Korea,' but not 'North Korea.')") + 
  theme_bw() + 
  theme(legend.position = c(0.75,0.8),
        legend.background = element_rect(color="black"),
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=1, size=8))

#+ eval = FALSE
ggsave("cv_east_asia.png", width = 7, height = 4.5)

## Plot South/West/Southeast Asia (not in the chapter) ##

# require(ggplot2)
# ggplot(subset(keydt_long, variable%in%
#                 c("South Asia", "Southeast Asia", 
#                   "Afghanistan", "Middle East")), 
#        aes(x=country, y=value)) + 
#   geom_col() + 
#   coord_flip(ylim = c(0, 0.025)) + 
#   facet_wrap(vars(variable)) + 
#   labs(y = "Propotion of article titles contatining keywords",
#        x = "Publication Database",
#        title = "Cross-National Variations of Other Asian Countries'\nPresence in Central Asian Studies",
#        caption = "Note: Keyword is each country's name (for Korea, it includes 'Korea' and 'South Korea,' but not 'North Korea.')") + 
#   theme_bw() + 
#   theme(legend.position = c(0.75,0.8),
#         legend.background = element_rect(color="black"),
#         plot.title = element_text(hjust=0.5),
#         plot.caption = element_text(hjust=1, size=8))
# 
# ggsave("cv_other_asia.png", width = 7, height = 4.5)

#'
#' ## Figure 3
#'

## Plot Post-Soviet + West ##

#+ fig.width = 7, fig.height = 4.5
require(ggplot2)
ggplot(subset(keydt_long, variable%in%
                c("Great Game",
                  "Russia", "(Post-)Soviet",
                  "East Europe", "West")), 
       aes(x=country, y=value)) + 
  geom_col() + 
  coord_cartesian(xlim=c(0, 0.13)) +
  coord_flip() + 
  facet_wrap(vars(variable)) + 
  labs(y = "Propotion of article titles contatining keywords",
       x = "Publication Database",
       title = "Cross-National Variations of Post-Communist and Western Countries'\nPresence in Central Asian Studies",
       caption = "Note: Keywords are country's name or region's name + country's names in a region.") + 
  theme_bw() + 
  theme(legend.position = c(0.75,0.8),
        legend.background = element_rect(color="black"),
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=1, size=8))

#+ eval = FALSE
ggsave("cv_power_countries.png", width = 7, height = 4.5)

#'
#' # Identifying Topics
#'

## Generate Tokens
### Multi-word terms to be detected.
multiword_clean <- c("central asia", "south korea", "north korea",
               "silk road", "economic belt", "higher education",
               "nuclear power", 
               "power industry","power sector",
               "power industries","power sectors",
               "one belt one road", "one belt", "hong kong",
               "prime minister", 
               "west asia", "east asia", "southeast asia", 
               "south asia",
               "middle east", "far east", "saudi arabia",
               "st petersburg",
               # "west europe", 
               "east europe", "central europe", 
               "west world", #"opening to west", 
               "west countries","west broadcasting", 
               #"west sky", "red west",
               "west fanned",
               "latin america", "north america", 
               "central america")#,
               # "small game",
               # "soft power","power politics","state power",
               # "great power game","great game","big game",
               # "great power","great influence contest",
               # "power contest",
               # "central asia game","geopolitical game",
               # "security game","energy game", "strategic game",
               # "economic game","military base game",
               # "military game", "multi-state game",
               # "russia american game","russia us game",
               # "usa russia game", "political game",
               # "energy cooperation game","geostrategic game",
               # "chess game") 
## Recreate the Document Removing More Stop Words
addstop <- c(# Generic research related words
             "analysis", "study",  "example",
             "case", "situation","perspective","context",
             "research","based","republic","review",
             "characteristics", 
             "factors","studies",
             "survey","problems","region","regions",
             "among","status","model","brief","implications",
             "role","challenge","challenges",
             "introduction","features","formation","issues","use",
             "structure","first","process","discussion",
             "held","evaluation","overview","assessment","toward",
             "patterns","preliminary","significance","post-9-11",
             "impact","effects",
             "focusing","exploring","taking","building",
             # Border-line words (not removed)
             "relations","system","systems",
             "state","states","country","countries","five",
             "new","current","prospects",
             "policy","policies","countermeasures",
             ## Central Asia
             "central_asia","asia",
             "kazakh", "astana", "nur-sultan", 
             "almaty", "shymkent", 
             "aktobe", "karaganda", "taraz", 
             "semey", "pavlodar", "oskemen",
             "kostanay", "kyzylorda", "petropavl",
             "kyrgyz", "bishkek", "osh",
             "tajik", "dushanbe",
             "uzbek", "tashkent", "samarkand", "namangan",
             "andijan", "nukus",
             "fergana", "bukhara", "qarshi", "kokand", "margilan",
             "turkmen", "ashgabat", "turkmenabat", "dasoguz",
             ## Other central asia related regions
             "turkic", "urals","han","dungan","hui",
             ## East asia and others
             "east_asia","far_east","china","beijing",
             "shanghai","hong_kong","guangzhou",
             "chengdu", "tianjin", "chongqing", "wuhan", "xian", "xi'an",
             "hangzhou", "harbin", "dalian", "qingdao", "hefei", "xiamen",
             "fuzhou", "changzhou", "lanzhou", "tibet", "fujian", "guangdong",
             "gansu", "guizhou", "henan", "hubei", "hebei", "heilongjiang",
             "jiangsu", "liaoning", "qinghai", "sichuan", "shandong",
             "shaanxi", "shanxi", "yunnan", "zhejiang", 
             "xinjiang","uyghur",
             "ili-kazakh","china-west","northwest-china","ili",
             "west-china",
             "urumqi", "karamay", "aksu", "altay", 
             "alashankou", "changji", "fukang", "hotan", 
             "kashgar", "shihezi", "tacheng", "yining",
             "japan","tokyo","hokkaido", "nara", 
             "south_korea", 
             "korea", "seoul", "daegu", "gwangju", 
             "taiwan","north_korea",
             "mongol","afghan","south_asia","india",
             "pakistan","nepal","bhutan", "southeast_asia",
             "asean","philippines","thai","singapore",
             "vietnam","russia","post-soviet","soviet",
             "st_petersburg",
             "belarus","moldova",
             "ukraine","armenia","azerbaijan","georgia",
             "middle_east", "west_asia",
             "iraq","israel","jordan","syria","iran",
             "turkey", "saudi_arabia","usa",
             "america","american","americans",
             "canada","australia",
             "europe","eu",
             "france","spain","sweden","germany",
             "italy","britain", "austria",
             "west_world", "opening-to-the-west",
             "west_countries","west_broadcasting",
             # "west_sky", "red_west",
             "west_fanned", 
             "east_europe", "poland",
             "bulgaria","yugoslavia",
             "serbia","czech","estonia","latvia","albania")

## Cleaned Tokens
tk_tgt_clean <- tokens(cs_tgt, remove_punct = TRUE) %>% 
  tokens_compound(pattern=phrase(multiword_clean)) %>% 
  tokens_remove(c(stopwords("english"),addstop))
  
## Document Feature Matrix with All Words
dfm_tgt_clean <-  dfm(tk_tgt_clean) 
dfm_tgt_clean[1:5,1:5]

## Top Words
topfeatures(dfm_tgt_clean, 50)  # top words
# write.csv(as.data.frame(topfeatures(dfm_tgt, 3000)), "popularwordslist.csv")  # top words

## Visualize through Wordcloud (not implemented)
# set.seed(130)
# textplot_wordcloud(dfm_tgt_clean, min_count = 50, #random_order = FALSE,
#                    # max_size = 14,
#                    # rotation = .25, 
#                    color = RColorBrewer::brewer.pal(8,"Dark2"))

## Topic Analysis ##

## Prepare Data for the Analysis
dt_tgt_clean <- data.frame(
  id = unlist(lapply(1:length(tk_tgt_clean), 
                     function(i) rep(names(tk_tgt_clean)[i],length(tk_tgt_clean[[i]])))),
  token = unlist(tk_tgt_clean)
)
dim(dt_tgt_clean)
head(dt_tgt_clean)

## Estimate Biterm Topic Model ##

library(BTM)

## Code file to implement BTM model (Slightly revised the one from Bitterman et al. 2020)
source("BTM_grid_rev.R") 

## Run BTM model for a specificed number of topics (not implemented)

# ntopics <- 8
# set.seed(100)
# tgt_btm_fit8 <- BTM(dt_tgt_clean, k = ntopics, trace=100, window = 50)
# saveRDS(tgt_btm_fit, "topic_analysis_fit8.rds")
# # tgt_btm_fit <- readRDS("topic_analysis_fit.rds")

## Grid Search

klist <- 6:16 # range of k based on preliminary examinations
# for each k, BTM_grid returns best model (coherence * exclusivity) w.r.t. alpha and seed
# *The code below TAKES TIME to be executed.
#+ eval = FALSE
btm_gridsearch <- BTM_grid(dt_tgt_clean,
                        convert(dfm_tgt_clean, to = "topicmodels"),
                        klist, window = 50, iter=1000)
#+ echo = FALSE, eval = FALSE
save(btm_gridsearch, file = "btm_gridsearch.RData")
#+ echo = FALSE
load("btm_gridsearch.RData")

#+
## evaluation metrics ##

#+ fig.width = 7, fig.height = 5
require(ggplot2) 
ggplot(btm_gridsearch$params_bestk,
       aes(x=k)) + 
  geom_point(aes(y=combinedstat, shape="1"), size=2.5) + 
  geom_line(aes(y=combinedstat, linetype="1")) + 
  geom_point(aes(y=scale(exclusivity), shape="2"), size=2.5, alpha=0.7) + 
  geom_line(aes(y=scale(exclusivity), linetype="2")) + 
  geom_point(aes(y=scale(coherence), shape="3"), size=2.5, alpha=0.7) + 
  geom_line(aes(y=scale(coherence), linetype="3")) + 
  scale_shape_discrete(name="Topic Quality", 
                       labels=c("Combined Statistics",
                                "Exclusivity (Standardized)",
                                "Coherence (Standardized)")) +
  scale_linetype_discrete(name="Topic Quality", 
                          labels=c("Combined Statistics",
                                   "Exclusivity (Standardized)",
                                   "Coherence (Standardized)")) +
  theme_bw()

#+ eval = FALSE
ggsave("btm_gridsearch_plot.png", width=7, height=5)

#'
#' Based on the figure above, combined statistics of exclusivity and coherence (the solid line with circle points) show a significant improvement between K = 10 and K = 11, but not a lot of improvement between 11 and 16. After the qualitative assessment of the identified topics of K=11 or above, it is decided to use 12 topics. K=12 is chosen because (1) the topic quality based on exclusivity and coherence statistics is sufficiently high, and (2) the identified topics are relatively easy to interpret. 
#'

## Use 12 topics
ntopics <- 12

## Topic labels (manually given)
if (ntopics==12) {
  topiclabs = c("1. National Identity & Literature",
                "2. Language & Education",
                "3. Medieval and Modern History",
                "4. Culture & Art",
                "5. Water Resources",
                "6. Economic Trade & Cooperation",
                "7. Migration & Societal Transition",
                "8. Mineral Resources & Investment",
                "9. Local Autonomy",
                "10. Power and Security",
                "11. Official Political Events",
                "12. Ancient History")
}

# set.seed(100)
tgt_btm_fit <- 
  btm_gridsearch$models_bestk[[which(btm_gridsearch$params_bestk$k==ntopics)]]

#'
#' ## Figure 4
#'

#+ fig.width = 8, fig.height = 5
## Plot Model Results
library(textplot)
library(ggraph)
library(concaveman)
set.seed(251)
plot(tgt_btm_fit,
    top_n = 6,
    labels = as.character(c(1:ntopics)),
    title = "Biterm topic model",
    subtitle = "K = 12, 1,000 Training Iterations")

#+ eval = FALSE
ggsave("topic_network.png", width=8, height=5)

#'
#' ## Figure 5
#'

## In ggplot
tadt <- data.frame(topic = NA, 
                   prop = colMeans(predict(tgt_btm_fit, dt_tgt_clean)))

# topiclabset <- 
#   sapply(1:ntopics, function(i) {
#     paste0("Topic ", i, " (",(round(tadt$prop[i]*100,1)),"%): ", 
#            paste(terms(tgt_btm_fit, top_n = 5)[[i]]$token, collapse=", "))
#   })
# topiclabset
topiclabset <-
  sapply(1:ntopics, function(i) {
    paste0(topiclabs[i], " (",(round(tadt$prop[i]*100,1)),"%)")#,
           # paste(terms(tgt_btm_fit, top_n = 5)[[i]]$token, collapse=", "))
  })
topiclabset
tadt$topic <- factor(topiclabset, levels=topiclabset)

require(dplyr)
tadtlab <- tadt %>% 
  mutate(csum = rev(cumsum(rev(prop))), 
         pos = prop/2 + lead(csum, 1),
         pos = if_else(is.na(pos), prop/2, pos))

#+ fig.width = 7, fig.height = 4
require(ggplot2)
ggplot(tadt, aes(x="", y=prop, fill=topic)) +
  geom_bar(stat="identity", width=1, 
           color="white", alpha=0.8) + 
  scale_fill_brewer(name = "", type="qual", palette=3) +
  coord_polar("y", start=0, direction = -1) +
  geom_text(data = tadtlab, 
                  aes(y = pos, x = "", label=c(1:12)),
                  nudge_x = 0.3, hjust = 0.2,
                  show.legend = FALSE) + 
  labs(title = "The Distribution of Topics") + 
  theme_void() + 
  theme(plot.title = element_text(hjust=0),
        plot.background = element_rect(fill="white", color=NA))

#+ eval = FALSE
ggsave("topic_dist.png", width = 8.2, height = 5.125)

# head(tgt_btm_fit$phi[,1])
# apply(tgt_btm_fit$phi, 2, function(k) paste0( paste(round(sort(k, decreasing = TRUE)[1:5]*100,2),collapse="%, "),"%"))
# apply(tgt_btm_fit$phi, 2, function(k) paste(names(sort(k, decreasing = TRUE)[1:10]),collapse=", "))

#'
#' ## Figure 6
#'

## Topic by Document
tgt_btm_pred <- predict(tgt_btm_fit, dt_tgt_clean)
colMeans(tgt_btm_pred)
tgt_btm_pred <- data.frame(id = row.names(tgt_btm_pred),
                           tgt_btm_pred)
head(tgt_btm_pred)
length(unique(tgt_btm_pred$id))
length(unique(d$id))

## Combine Metadata with Topic Proportions
## And Get Cross National Variations
require(tidyr)
require(dplyr)
dim(d[,c("id","country","year")])
length(unique(tgt_btm_pred$id))
dscvdt <- merge(d[,c("id","country","year")], tgt_btm_pred, by = "id") %>% 
  pivot_longer(cols=paste0("X",c(1:ntopics)),
               names_to='topic',
               values_to='prop') %>% 
  group_by(country, topic) %>% summarise(prop = mean(prop)) 
dscvdt$topic <- as.factor(as.numeric(gsub("X","",dscvdt$topic)))
dscvdt$country <- factor(dscvdt$country, 
                             levels = rev(unique(dscvdt$country)),
                             labels = rev(c("Web of Science", 
                                            "CNKI (China)",
                                            "DBpia (Korea)",
                                            "CiNii (Japan)")))
head(dscvdt)

dscvdt$Topics <- factor(dscvdt$topic, labels=topiclabs)

#+ fig.width = 9, fig.height = 6
## Plot Cross National Variations
require(ggplot2)
ggplot(dscvdt, aes(x=country)) + 
  #geom_line(aes(y=prop, color=topic)) + 
  # geom_point(aes(y=prop, shape=topic, color=topic), size=3) + 
  geom_col(aes(y=prop, fill=Topics), color="white") + 
  coord_flip() + 
  facet_wrap(vars(Topics), ncol = 3) + 
  scale_color_brewer(name = "Topic", type="qual", palette=3,
                    labels = paste0("Topic ", c(1:ntopics))) + 
  scale_shape_manual(name = "Topic", 
                     values = c(1:ntopics)-1,
                     labels = paste0("Topic ", c(1:ntopics))) + 
  labs(x = "Publication Database", 
       y = "Topic Proportion",
       title = "Cross-Country Variations in Topics") + 
  theme_bw() + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5),
        strip.text = element_text(face = "bold"))

#+ eval = FALSE
ggsave("topic_cv.png", width = 9, height = 6)

#'
#' ## Generating Examples
#'

## Generate Some Examples

sort(tgt_btm_pred$X1, decreasing = TRUE)[1:100]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X1, decreasing = TRUE)][1:100]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X2, decreasing = TRUE)[1:100]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X2, decreasing = TRUE)][1:100]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X3, decreasing = TRUE)[1:100]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X3, decreasing = TRUE)][1:100]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X4, decreasing = TRUE)[1:100]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X4, decreasing = TRUE)][1:100]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X5, decreasing = TRUE)[1:100]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X5, decreasing = TRUE)][1:100]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X6, decreasing = TRUE)[1:300]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X6, decreasing = TRUE)][1:300]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X7, decreasing = TRUE)[1:200]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X7, decreasing = TRUE)][1:200]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X8, decreasing = TRUE)[1:176]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X8, decreasing = TRUE)][1:176]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X9, decreasing = TRUE)[1:100]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X9, decreasing = TRUE)][1:100]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X10, decreasing = TRUE)[1:200]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X10, decreasing = TRUE)][1:200]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X11, decreasing = TRUE)[1:250]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X11, decreasing = TRUE)][1:250]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

sort(tgt_btm_pred$X12, decreasing = TRUE)[1:150]
tmp <- tgt_btm_pred$id[order(tgt_btm_pred$X12, decreasing = TRUE)][1:150]
cbind(tmp,d$title_en[match(tmp,d$id)])
cbind(tmp,d$title[match(tmp,d$id)])

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('analysis_langcomp_v2.R', 'github_document', clean=FALSE)
# tmp <- list.files(paste0("./"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$|\\.log$|\\.tex$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0("./",tmp[i]))
