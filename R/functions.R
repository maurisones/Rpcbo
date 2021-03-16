
computeExtents <- function(df, ncpus = 1, minsupport = 0){
  
  # transforma a matriz em valores esparsos
  sparse <- apply(df, 1, function(x){ which(x == 1)})
  
  # cria um único vetor separanado cada linha por -1
  vetorToPCBO <- unlist(lapply(sparse, function(x){c(x, -1)}))
  names(vetorToPCBO) <- NULL
  
  
  extents <- pcbo(vetorToPCBO, ncpus, minsupport)
  extents <- unlist(lapply(extents, function(x){gsub("\n", "", x)}))
  extents <- lapply(extents, function(x){as.numeric(unlist(strsplit(x, " "))) + 1})
  
  
  return(extents)
}


computeIntents <- function(df, extents){
  
  intents <- list()
  for (i in 1:length(extents)){
    extent <- extents[[i]]
    if (length(extent) > 1){
      intents[[i]] <- which(rowSums(df[,extent]) == length(extent))
    } else {
      intents[[i]] <- which(df[,extent] == length(extent))
    }
  }
  
  # verifica quando o root é vazio
  if (length(extents[[1]]) == 0){
    intents[[1]] <- as.numeric(rownames(df))
  }
  
  return(intents)
}