test_that("GetCodonPhase is calculating the number of stop codons correctly", {
  consensusSequence <- c("a", "a", "a", "a", "a", "a", "a", "c", "a", "a", 
    "g", "g", "c", "a", "a", "c", "c", "g", "c", "c", "t", "c", "g", 
    "c", "c", "n", "t", "t", "c", "c", "g", "t", "t", "t", "t", "g", 
    "g", "g", "t", "t", "g", "g", "g", "g", "g", "c", "c", "c", "c", 
    "c", "a", "a", "c", "c", "c", "t", "g", "g", "g", "g", "t", "t", 
    "t", "t", "g", "t", "g", "t", "t", "t", "c", "c", "c", "a", "a", 
    "a", "a", "a", "c", "c", "g", "g", "g", "g", "g", "g", "g", "g", 
    "c", "c", "g", "g", "g", "t", "t", "t", "t", "t", "t", "t", "g", 
    "g", "a", "a", "a", "a", "a", "c", "a", "a", "g", "g", "a", "a", 
    "a", "c", "c", "g", "c", "c", "c", "c", "g", "c", "c", "c", "g", 
    "t", "g", "a", "g", "t", "t", "a", "t", "g", "g", "g", "g", "t", 
    "g", "g", "g", "g", "g", "c", "c", "c", "c", "c", "a", "c", "c", 
    "c", "c", "a", "g", "g", "g", "g", "t", "g", "t", "g", "g", "a", 
    "g", "t", "g", "t", "c", "c", "c", "a", "a", "a", "a", "a", "c", 
    "c", "g", "c", "g", "a", "g", "g", "g", "g", "c", "c", "g", "g", 
    "g", "t", "t", "t", "t", "t", "c", "t", "a", "a", "a", "a", "a", 
    "t", "t", "a", "t", "a")
  output <- GetCodonPhase(consensusSequence, return.all=FALSE)
  expect_equal(length(output$stopCodonPostions), 6)
  expect_equal(min(output$NumStopCodons), 0)
  expect_equal(max(output$NumStopCodons), 2)
  expect_less_than(length(output$translation), 71) #the length of the translation can vary depending on where you start the codon
  expect_more_than(length(output$translation), 68)
  expect_match(class(output), "list")
  output2 <- GetCodonPhase(consensusSequence, return.all=TRUE)
  expect_equal(length(output2$translation),2)
})



