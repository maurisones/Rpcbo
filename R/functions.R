
computeExtents <- function(df, ncpus = 1, minsupport = 0){
  
  # transforma a matriz em valores esparsos
  sparse <- apply(df, 1, function(x){ which(x == 1)})
  
  # cria um único vetor separanado cada linha por -1
  vetorToPCBO <- unlist(lapply(sparse, function(x){c(x, -1)}))
  names(vetorToPCBO) <- NULL
  
  
  retorno <- pcbo(vetorToPCBO, ncpus, minsupport)
  retorno <- unlist(lapply(retorno, function(x){gsub("\n", "", x)}))
  retorno <- lapply(retorno, function(x){as.numeric(unlist(strsplit(x, " "))) + 1})
  
  
  return(retorno)
}