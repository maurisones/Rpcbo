
computeExtents <- function(df, ncpus = 1, minsupport = 0){
  
  # transforma a matriz em valores esparsos
  sparse <- apply(df, 1, function(x){ which(x == 1)})
  
  # cria um único vetor separanado cada linha por -1
  vetorToPCBO <- unlist(lapply(sparse, function(x){c(x, -1)}))
  names(vetorToPCBO) <- NULL
  
  #Rcpp::sourceCpp("pcbo.cpp") 
  
  retorno <- as.numeric(pcbo(vetorToPCBO, ncpus, minsupport))
  sepindexes <- which(retorno == -1)
  retlist <- list();
  for (i in 2:length(sepindexes)){
    retlist[[i-1]] <- sort(retorno[(sepindexes[i-1] + 1): (sepindexes[i] - 1)  ])
  }
  
  # tem que somar um em cada elemento pois o pcbo trabalha com base 0
  retlist <- lapply(retlist, function(x){as.numeric(x) + 1})
  
  return(retlist)
}