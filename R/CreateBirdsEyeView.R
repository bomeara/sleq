#' Create a birds' eye view of the data
#' @param data A character matrix
#' @param colors A vector of colors, with colnames matching states
#' @examples
#' \dontrun{
#' test.sequences <- as.matrix(read.alignment(file = system.file("sequences/test.phylip",
#' package = "seqinr"), format = "phylip"))
#' CreateBirdsEyeView(test.sequences)
#' }
CreateBirdsEyeView <- function(data, colors=NULL) {
  plot(x=c(.5,.5+dim(data)[2]), y=c(-.5,-.5-dim(data)[1]), type="n", xlab="", ylab="", yaxt="n", bty="n")
  text(x=rep(0,dim(data)[1]), y=-sequence(dim(data)[1]), labels=rownames(data), adj = c(1, NA), cex=0.5)

  if(is.null(colors)) {
  	colors <- rainbow(n=length(unique(data))) #primitive way of doing now. Later, do gaps, N, differently
  	names(colors) <- unique(data)
  }
  for (taxon in sequence(dim(data)[1])) {
    for (character in sequence(dim(data)[2])) {
    	rect(xleft=-.5+character, ybottom=-.45-taxon, xright=0.5+character, ytop=.45-taxon, col=colors[data[taxon,character]], border=NA)
    }
  }
}
