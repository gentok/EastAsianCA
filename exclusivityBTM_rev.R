# source: https://github.com/bnosac/BTM/issues/3

exclusivityBTM <- function (model, M = 30, frexw = 0.7){
  w <- frexw
  phidf <- t(as.matrix(model$phi))
  phi <- list(phidf)
  row.names(phi) <- NULL
  if (length(phi) != 1)
    stop("Exclusivity calculation only designed for models without content covariates")
  tbeta <- t(exp(phi[[1]]))
  s <- rowSums(tbeta)
  mat <- tbeta/s
  ex <- apply(mat, 2, rank)/nrow(mat)
  fr <- apply(tbeta, 2, rank)/nrow(mat)
  frex <- 1/(w/ex + (1 - w)/fr)
  index <- apply(tbeta, 2, order, decreasing = TRUE)[1:M, ]
  out <- vector(length = ncol(tbeta))
  for (i in 1:ncol(frex)) {
    out[i] <- sum(frex[index[, i], i])
  }
  print(paste("Mean Exclusivity:", mean(out)))
  return(out)
}