test.sequences <- structure(c("a", "a", "a", "a", "a", "a", "a", "c", "a", "a",
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
"t", "t", "a", "t", "a"), .Dim = c(5L, 42L), .Dimnames = list(
    c("Turkey    ", "Salmo gair", "H. Sapiens", "Chimp     ",
    "Gorilla   "), c("1", "2", "3", "4", "5", "6", "7", "8",
    "9", "10", "11", "12", "13", "14", "15", "16", "17", "18",
    "19", "20", "21", "22", "23", "24", "25", "26", "27", "28",
    "29", "30", "31", "32", "33", "34", "35", "36", "37", "38",
    "39", "40", "41", "42")))
  test.sequences2 <- structure(c("a", "a", "a", "a", "a", "a", "a", "c", "a", "a",
    "g", "g", "c", "a", "a", "c", "c", "g", "c", "c", "t", "c", "g",
    "c", "c", "n", "t", "t", "c", "c", "g", "t", "t", "t", "t", "g",
    "g", "g", "t", "t", "g", "g", "g", "g", "g", "c", "c", "c", "c",
    "c", "a", "a", "c", "c", "c", "t", "g", "g", "g", "g", "t", "t",
    "t", "t", "g", "t", "g", "t", "t", "t", "c", "c", "c", "a", "a",
    "a", "a", "a", "c", "c", "g", "g", "g", "g", "g", "g", "g", "g",
    "c", "c", "g", "g", "g", "t", "t", "t", "t", "t", "t", "t", "g",
    "g", "a", "a", "a", "a", "a", "c", "a", "a", "g", "g", "a", "a",
    "a", "c", "c", "g", "c", "c", "c", "c", "g", "c", "c", "c", "g",
    "t", "g", "a", "g", "t", "t", "a", "X", "g", "g", "g", "g", "t",
    "g", "g", "g", "g", "g", "c", "c", "c", "c", "c", "a", "c", "c",
    "c", "c", "a", "g", "g", "g", "g", "t", "g", "t", "g", "g", "a",
    "g", "t", "g", "t", "c", "c", "c", "a", "a", "a", "a", "a", "c",
    "c", "g", "c", "g", "a", "g", "g", "g", "g", "c", "c", "g", "g",
    "g", "t", "t", "t", "t", "t", "c", "t", "a", "a", "a", "a", "a",
    "t", "t", "a", "t", "a"), .Dim = c(5L, 42L), .Dimnames = list(
        c("Turkey    ", "Salmo gair", "H. Sapiens", "Chimp     ",
        "Gorilla   "), c("1", "2", "3", "4", "5", "6", "7", "8",
        "9", "10", "11", "12", "13", "14", "15", "16", "17", "18",
        "19", "20", "21", "22", "23", "24", "25", "26", "27", "28",
        "29", "30", "31", "32", "33", "34", "35", "36", "37", "38",
        "39", "40", "41", "42")))
# Temporary to make faulty results to check for errors
# Nr1 <- matrix(data=c(4, 6, 8, 9, 0, 4, 5, 9, 3, 4, 8, 1), nrow=6, ncol=2)
# Nr2 <- matrix(data=c(4, 6, 8, 9, 0, 4, 5, 9, 3, 4, 8, 9), nrow=6, ncol=2)
# Nr3 <- matrix(data=c(4, 6, 8, 9, 0, 4, 5, 9, 3, 4, 8, 1, 5, 9, 3, 4, 8, 1), nrow=6, ncol=3)
# Nr4 <- matrix(data=c(4, 6, 8, 9, 5, 9, 3, 4, 8, 1), nrow=5, ncol=2)
# faulty.output <- list(Nr1, Nr2, Nr3, Nr4)
test_that("SingleTaxonCompare runs without failure", {
   expect_that(SingleTaxonCompare(test.sequences), not(throws_error()))
})
test_that("SingleTaxonCompare has correct dimensions", {
   expect_equal(length(SingleTaxonCompare(test.sequences)), 42)
   testresult <- SingleTaxonCompare(test.sequences)
   for (i in 1:length(testresult)) {
     expect_equal(dim(testresult[[i]]), c(6,2))
   }
})
test_that("SingleTaxonCompare adds up to 100%", {
   testresult <- SingleTaxonCompare(test.sequences)
   for (i in 1:length(testresult)) {
     expect_equal(testresult[[i]][6,2], 1)
   }
})
test_that("SingleTaxonCompare throws warning for non-standard bases", {
  expect_warning(SingleTaxonCompare(test.sequences2), "The sequences contain characters different from 'a', 'c', 'g', 't' or 'n'. They will be counted under 'n'. The characters in question are: X")
  })

# ## Temporary to test for picking up of errors
# test_that("SingleTaxonCompare runs without failure", {
#    expect_that(SingleTaxonCompare(faulty.output), not(throws_error()))
# })
# test_that("SingleTaxonCompare has correct dimensions", {
#    expect_equal(length(faulty.output), 42)
#    testresult <- faulty.output
#    for (i in 1:length(testresult)) {
#      expect_equal(dim(testresult[[i]]), c(6,2))
#    }
# })
# test_that("SingleTaxonCompare adds up to 100%", {
#    testresult <- faulty.output
#    for (i in 1:length(testresult)) {
#      expect_equal(testresult[[i]][6,2], 1)
#    }
# })
