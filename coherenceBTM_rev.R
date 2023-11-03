# modified from tmca_coherence() function (A. Niekler & G. Wiedemann)
# slightly modified by Gento Kato

coherenceBTM <- function(model, DTM, N = 10) {
  
  # Ensure matrix or Matrix-format (convert if slam)
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(DTM)) {
    DTM <- sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  
  K <- model$K
  
  DTMBIN <- DTM > 0
  
  documentFrequency <- colSums(DTMBIN)
  names(documentFrequency) <- colnames(DTMBIN)
  
  topNtermsPerTopic <- terms(model, top_n = N)
  
  termcollect <- list()
  for (i in 1:K){
    termcollect[[i]] <- topNtermsPerTopic[[i]][,1]
  }
  
  allTopicModelTerms <- unique(as.vector(unlist(termcollect)))
  
  DTMBIN <- DTMBIN[, allTopicModelTerms]
  DTMBINCooc <- t(DTMBIN) %*% DTMBIN
  DTMBINCooc <- t((DTMBINCooc + 1) / colSums(DTMBIN))
  DTMBINCooc <- log(DTMBINCooc)
  DTMBINCooc <- as.matrix(DTMBINCooc)
  
  coherence <- rep(0, K)
  #pb <- txtProgressBar(max = K)
  for (topicIdx in 1:K) {
    # setTxtProgressBar(pb, topicIdx)
    topWordsOfTopic <- topNtermsPerTopic[[topicIdx]][,1]
    
    coherence[topicIdx] <- 0
    for (m in 2:length(topWordsOfTopic)) {
      for (l in 1:(m-1)) {
        mTerm <- topWordsOfTopic[m]
        lTerm <- topWordsOfTopic[l]
        coherence[topicIdx] <- coherence[topicIdx] + DTMBINCooc[mTerm, lTerm]
      }
    }
  }
  #close(pb)
  
  print(paste("Mean Cohearence:", mean(coherence)))
  
  return(coherence)
}
