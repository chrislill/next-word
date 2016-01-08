library(testthat)

source("model-functions.R")

long.phrase <- c("if", "this", "year", "would", "just", "end", "i", "just", "think", "who", "are", "you")
short.phrase <- c("who", "are", "you")
two.phrase <- c("who", "are")


# Test CreateValNgrams() ------------------------------------------------------
long.ngrams <- CreateValNgrams(long.phrase)
short.ngrams <- CreateValNgrams(short.phrase)
two.ngrams <- CreateValNgrams(two.phrase)

# TODO: Test for <UNK>
expect_equal(long.ngrams[1, 1], hash("if"))
expect_equal(nrow(long.ngrams), 10)
expect_true(is.na(long.ngrams[9, 5]))
expect_true(is.na(long.ngrams[10, 4]))

expect_equal(nrow(short.ngrams), 1)
expect_equal(nrow(two.ngrams), 0)


# Test BuildValNgramTable() ---------------------------------------------------
ngram.table <- BuildValNgramTable(list(long.phrase, short.phrase))
expect_equal(nrow(ngram.table), 10)
expect_equal(max(ngram.table$count), 2)
expect_equal(sum(is.na(ngram.table$word4)), 1)

