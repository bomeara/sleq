test_that("gets percent data present per site", {
raw.sequences <- structure(c("a", "a", "a", "a", "a", "a", "a", "c", "a", "a", 
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
    new.format.sequences <- seqalignment(raw.sequences)
    expect_true(sum(SiteMissing(new.format.sequences)[,2])==42)
})

test_that("checks that proper sites are culled based on threshold", {
    raw.sequences <- structure(c("a", "a", "a", "a", "a", "a", "a", "c", "a", "a",
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
    new.format.sequences = raw.sequences
    new.format.sequences[,2] = "-"
    new.format.sequences[,13] = "-"
    cleaned.new.sequences <- CleanSeqs(new.format.sequences, seq.type="dna", cutoff=0.5)
    expect_true(dim(cleaned.new.sequences)[2]==40)
    new.format.sequences <- seqalignment(raw.sequences)
    new.format.sequences$sequences[,2] = "-"
    new.format.sequences$sequences[,13] = "-"
    expect_true(dim(cleaned.new.sequences)[2]==40)
})



