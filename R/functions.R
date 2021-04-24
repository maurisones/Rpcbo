#' Find the extents in the given data.frame
#'
#' This function call the pcbo algorithm to find the concepts in the context 
#' represented in the dataframe passed as a parameter.
#'
#' @param df A data.frame with the forma concept data
#' @param ncpus The number of cpu cores to be used for parallel processing
#' @param minsupport The minumum support threshold
#' @return A list containing the extents found in the given context
#' @export
computeExtents2 <- function(df, threads = 1, minsupport = 0){
  
  # transform the matrix to a list of sparse values
  sparse <- apply(df, 1, function(x){ which(x == 1)})
  
  # creates a vector using -1 as line separator
  vetorToPCBO <- unlist(lapply(sparse, function(x){c(x, -1)}))
  names(vetorToPCBO) <- NULL
  
  # call the cpp pcbo
  extents <- pcbo(vetorToPCBO, threads, minsupport)
  # replaces the line breaks in results
  extents <- unlist(lapply(extents, function(x){gsub("\n", "", x)}))
  # convert values to numeric and add 1 - pcbo uses base 0
  extents <- lapply(extents, function(x){as.numeric(unlist(strsplit(x, " "))) + 1})
  
  
  return(extents)
}



#' Find the extents in the given data.frame
#'
#' This function call the pcbo algorithm to find the concepts in the context 
#' represented in the dataframe passed as a parameter.
#'
#' @param df A data.frame with the forma concept data
#' @param ncpus The number of cpu cores to be used for parallel processing
#' @param minsupport The minumum support threshold
#' @return A list containing the extents found in the given context
#' @export
computeExtents <- function(df, threads = 1, minsupport = 0){
  
  # transform the matrix to a list of sparse values
  sparse <- apply(df, 1, function(x){ which(x == 1)})
  
  # creates a vector using -1 as line separator
  #vetorToPCBO <- unlist(lapply(sparse, function(x){c(x, -1)}))
  #names(vetorToPCBO) <- NULL
  strToPCBO <- paste(unlist(lapply(sparse, function(x){c(as.character(x), '\n')})), collapse = " ")
  
  
  # call the cpp pcbo
  extents <- pcbo(strToPCBO, threads, minsupport)
  # replaces the line breaks in results
  extents <- unlist(lapply(extents, function(x){gsub("\n", "", x)}))
  # convert values to numeric and add 1 - pcbo uses base 0
  extents <- lapply(extents, function(x){as.numeric(unlist(strsplit(x, " "))) + 1})
  
  
  return(extents)
}




computeIntents <- function(df, extents, threads = 1){

  cl_computeintents <- parallel::makeCluster(threads)
  doParallel::registerDoParallel(cl_computeintents)
  
  intents <- foreach (i = 1:length(extents)) %dopar%{
    extent <- extents[[i]]
    if (length(extent) > 1){
      intent <- which(rowSums(df[,extent]) == length(extent))
    } else {
      intent <- which(df[,extent] == length(extent))
    }
    
  }
  
  # verifica quando o root é vazio
  if (length(extents[[1]]) == 0){
    intents[[1]] <- as.numeric(rownames(df))
  }
  
  doParallel::stopImplicitCluster()
  
  return(intents)
}



computeIntentsSerial <- function(df, extents){
  
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

