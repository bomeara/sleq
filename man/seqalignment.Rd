% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seqalignment.R
\name{seqalignment}
\alias{seqalignment}
\title{Create seqalignment class object}
\usage{
seqalignment(x)
}
\arguments{
\item{x}{A character matrix}
}
\value{
A seqalignment object
}
\description{
Create seqalignment class object
}
\details{
This class is, internally, a list containing character matrix \code{$sequences},
\code{$type} (a guess at what kind of sequence it is: dna, rna, or aa), and 
\code{$genes} (a character vector with labels for the genes; initialized at "1" for all.
However, one cool thing about this class is that if you use \code{[]} to access elements,
this returns elements from within the \code{$sequences} object, not the class object itself.
}
\examples{
library(ape)
data(woodmouse)
mice.raw <- as.character(woodmouse)
print(mice.raw[1:3, 1:20])
mice.seqalignment <- seqalignment(mice.raw)
print(mice.seqalignment[1:3, 1:20])
print(mice.seqalignment$type)
}

