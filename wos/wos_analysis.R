setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(bibliometrix)

# d <- convert2df(c("wos_res0001to0500_230316.txt",
#                   "wos_res0501to1000_230316.txt",
#                   "wos_res1001to1500_230316.txt",
#                   "wos_res1501to2000_230316.txt",
#                   "wos_res2001to2500_230316.txt",
#                   "wos_res2501to2791_230316.txt"))
# saveRDS(d, "wos_res_230316.rds")
d <- readRDS("wos_res_230316.rds")
out <- biblioAnalysis(d, sep = ";")

d2 <- metaTagExtraction(d,Field="AU_CO",";")
?metaTagExtraction
lapply(d$AU)

require(stringr)
d2$NofAU <- sapply(d2$AU, function(k) length(str_split(k,";")[[1]]))
d2$NofAU_UN <- sapply(d2$C1, function(k) length(str_split(k,";")[[1]]))
d2$NofAU_CO <- sapply(d2$AU_CO, function(k) length(str_split(k,";")[[1]]))

d2[1:30,c("NofAU","NofAU_UN","NofAU_CO")]
d2[1,c("AU","AU_UN","AU_CO")]
d2$AU[1:10]

## Author Level Data
d2x <- do.call("rbind",lapply(1:nrow(d2), function(k) do.call("rbind",replicate(d2$NofAU[k], d2[k,], simplify=FALSE))))
## Auhor Level Variables
d2x$AU_CO <- unlist(str_split(ifelse(is.na(d2$AU_CO),"",d2$AU_CO),";"))

str_split(ifelse(is.na(d2$AU_CO[1:10]),"",d2$AU_CO[1:10]),";")

?convert2df
