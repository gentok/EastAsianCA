# BTM grid search

# modified for BTM from LDA_grid.R, A. Bittermann, ZPID
# then modified by Gento Kato

BTM_grid <- function(x_token, 
                     DTM, klist, 
                     window = 15,
                     alphalist = c(0.001, 0.01), 
                     seedlist = c(12, 4321, 28, 3323), 
                     cores = parallel::detectCores()-1,
                     iter = 2000,
                     trace = 100,
                     top_n = 10,
                     biterms = NULL,
                     detailed = TRUE){
  
  require(pbapply)
  source("exclusivityBTM_rev.R")
  source("coherenceBTM_rev.R")

  ## Values for Grid Search
  searchlist <- list()
  for(o in 1:length(klist)) {
    for (i in 1:length(alphalist)) {
      for (j in 1:length(seedlist)) {
        searchlist <- 
          append(searchlist, list(list(k=klist[o],a=alphalist[i],s=seedlist[j])))
      }
    }
  }
  
  ## Grid Search
  btm_grid <- pbapply::pblapply(X = searchlist, FUN = function(x) {
    set.seed(x$s)
    if (is.null(biterms)) {
      BTM::BTM(x_token, 
               window = window,
               k = x$k, alpha = x$a, beta = 1/x$k, 
               iter = iter, trace = trace, detailed = detailed)
    } else {
      BTM::BTM(x_token, 
               window = window,
               biterms = biterms,
               k = x$k, alpha = x$a, beta = 1/x$k, 
               iter = iter, trace = trace, detailed = detailed)
    }
  }, cl=cores)
  
  ## Information on the Best Model for Each K
  searchdt <- 
    data.frame(k = sapply(searchlist, function(x) x$k),
               alpha = sapply(searchlist, function(x) x$a),
               seed = sapply(searchlist, function(x) x$s))
  searchdt$exclusivity <- 
    sapply(1:nrow(searchdt), 
           function(i) mean(exclusivityBTM(btm_grid[[i]])))
  searchdt$coherence <- 
    sapply(1:nrow(searchdt), 
           function(i) mean(coherenceBTM(btm_grid[[i]], DTM, N=top_n)))
  searchdt$combinedstat <- 
    (scale(searchdt$exclusivity) + scale(searchdt$coherence))/2
  
  ## Best Model for Each K
  bestkdt <- data.frame(k = klist,
                        exclusivity = NA,
                        coherence = NA,
                        combinedstat = NA)
  btm_bestk_a <- list()
  for (o in 1:length(klist)) {
    kloc <- which(searchdt$k==klist[o])
    bestkloc <- kloc[which.max(searchdt$combinedstat[kloc])]
    btm_bestk_a[[o]] <- btm_grid[[bestkloc]]
    bestkdt$exclusivity[o] <- searchdt$exclusivity[bestkloc]
    bestkdt$coherence[o] <- searchdt$coherence[bestkloc]
    bestkdt$combinedstat[o] <- searchdt$combinedstat[bestkloc]
  }
  
  list(models_bestk = btm_bestk_a,
       params_bestk = bestkdt,
       models_grid = btm_grid,
       params_grid = searchdt)
}


