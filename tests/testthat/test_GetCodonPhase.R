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
  output <- GetCodonPhase(consensusSequence)
  expect_match(length(output$stopCodonPositions), 6)
  expect_less_than(output$numStopCodons, ncol(translation))
  expect_more_than(output$numStopCodons, 0)
  expect_more_than(ncol(output$translation), 1)
  expect_match(class(output), "list")
})



