test_that("CreateBirdsEyeView runs without failure", {
  test.sequences <- as.matrix(read.alignment(file = system.file("sequences/test.phylip",
   package = "seqinr"), format = "phylip"))
  expect_true(CreateBirdsEyeView(test.sequences))
})
