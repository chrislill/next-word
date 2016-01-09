library(testthat)

source("tokenise-functions.R")

if(!exists("dev.corpora")) {
  load("data\\dev-corpora.RData")
}

# Test TokeniseText() ---------------------------------------------------------
expect_equal(unlist(TokeniseText("Just like")), c("just", "like"))
expect_equal(unlist(TokeniseText("coast, the")), c("coast", "the"))
expect_equal(unlist(TokeniseText("don't")), "don't")
expect_equal(unlist(TokeniseText("It's")), "it's")
expect_equal(unlist(TokeniseText("thick-walled")), "thick-walled")
expect_equal(unlist(TokeniseText("thick -walled")), c("thick", "walled"))
expect_equal(unlist(TokeniseText("thick- walled")), c("thick", "walled"))
expect_equal(unlist(TokeniseText("-Boston")), "boston")
expect_equal(unlist(TokeniseText("--Boston")), "boston")

dev.tokens <- TokeniseText(dev.corpora)
expect_equal(length(dev.tokens), 1000)
expect_equal(dev.tokens[[19]][1], "that's")
             

# Test CreateDictionary() -----------------------------------------------------

dev.dictionary <- CreateDictionary(dev.tokens, 10000)
short.dictionary <- CreateDictionary(dev.tokens, 10)

expect_equal(nrow(dev.dictionary[word == "the"]), 1)
expect_equal(dev.dictionary[word == "the", hash], hash("the"))
expect_equal(nrow(dev.dictionary[word == "mixologymashup"]), 0)
expect_equal(nrow(short.dictionary), 10)


# Test RemoveUnknownWords() ---------------------------------------------------







