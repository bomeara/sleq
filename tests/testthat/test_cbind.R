test_that("bind DNA together", {
raw.sequences.A <- structure(c("a", "a", "a", "a", "a", "a", "a", "c", "a", "a", 
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
    c("Turkey", "Salmo gair", "H. Sapiens", "Chimp", 
    "Gorilla"), c("1", "2", "3", "4", "5", "6", "7", "8", 
    "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
    "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", 
    "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", 
    "39", "40", "41", "42")))
 raw.sequences.B <- structure(c("-", "a", "-", "a", "a", "a", "a", "c", "a", "a", 
"g", "g", "c", "a", "a", "c", "c", "g", "c", "c", "t", "c", "g", 
"c", "c", "n", "t", "t", "c", "c", "g", "t", "t", "t", "t", "g", 
"g", "g", "t", "t", "g", "g", "g", "-", "g", "c", "c", "c", "c", 
"c", "a", "a", "c", "c", "c", "t", "g", "g", "g", "g", "t", "t", 
"t", "t", "g", "t", "-", "t", "t", "t", "c", "c", "c", "a", "a", 
"a", "a", "a", "c", "c", "g", "g", "g", "g", "g", "g", "g", "g", 
"c", "c", "g", "g", "g", "t", "t", "t", "t", "t", "t", "t"), .Dim = c(4L, 25L), .Dimnames = list(
    c("Turkey", "Lemur", "Chimp", 
    "Orangutan"), c("1", "2", "3", "4", "5", "6", "7", "8", 
    "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
    "19", "20", "21", "22", "23", "24", "25")))
    new.format.sequences.A <- seqalignment(raw.sequences.A)
    new.format.sequences.B <- seqalignment(raw.sequences.B)
    
    combined.sequences <- cbind(new.format.sequences.A, new.format.sequences.B)
    expect_equal(dim(combined.sequences)[2], dim(raw.sequences.A)[2]+dim(raw.sequences.B)[2])
    expect_equal(dim(combined.sequences)[1], length(unique(c(rownames(raw.sequences.A), rownames(raw.sequences.B)))))
    expect_equal(unique(sort(c(rownames(raw.sequences.A), rownames(raw.sequences.B)))), sort(rownames(combined.sequences)))
})







