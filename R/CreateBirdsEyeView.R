#' Create a birds' eye view of the data
#' @param data A character matrix
#' @param colors A vector of colors
#' @return TRUE if the plotting worked
#' @examples
#' \dontrun{
#' test.sequences <- as.matrix(read.alignment(file = system.file("sequences/test.phylip",
#' package = "seqinr"), format = "phylip"))
#' CreateBirdsEyeView(test.sequences)
#' }
CreateBirdsEyeView <- function(data, colors=NULL) {
  plot(x=c(1,dim(data)[2]), y=c(1,dim(data)[1]), type="n", xlab="", ylab="", yaxt="n", bty="n")
  
  return(TRUE)
}
