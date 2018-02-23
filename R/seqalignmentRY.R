#' RY code a sleqalignment object
#' @return A seqalignment object of RY coded sites
#' @param x An object of class seqalignment of RY coded bases.
#' @details 
#' This function takes an object of class seqalignment and RY codes nucleotides. Purine nucleotides
#' (A,G) are coded as R while pyrimidines (C,T)  are coded as Y. N bases and gaps remain as N. The
#' function is insensitive to to case (i.e. nucleotides T and t will both be converted to Y). It 
#' returns a seqalignment object with RY coded bases.
#' @seealso \code{\link{seqalignment}}
#' @author Samuel R. Borstein
#' @examples 
#' library(ape)
#' data(woodmouse)#load woodmouse data
#' mice.raw <- as.character(woodmouse)
#' mice.seqalignment <- seqalignment(mice.raw)#convert woodmouse to seqalignment
#' seqalignmentRY<-SleqAlign.RY(mice.seqalignment)

seqalignmentRY<-function(x){
  #stopifnot(class(x)=="seqalignment")
  if(class(x) != "seqalignment") stop("Error: Object must be of class seqalignment")
  alignment2RY<-x$sequences
  R.align<-gsub("G|A|g|a","R",alignment2RY)  
  RY.align<-gsub("C|T|c|t","Y",R.align)
  x$sequences<-RY.align
  x
}
