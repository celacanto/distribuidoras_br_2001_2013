makeDumb  <- function(dfColuna){
  listValues  <- strsplit(dfColuna, " / ")
  uValues  <- unique(unlist(listValues))
  mtxDump  <- t(sapply(listValues, function(x) uValues %in%  x))
  colnames(mtxDump)  <- uValues
  mtxDump
}