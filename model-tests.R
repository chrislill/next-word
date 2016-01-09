library(testthat)

source("model-functions.R")

long.phrase <- c("if", "this", "year", "would", "just", "end", "i", "think", "if", "this", "year", "would")
short.phrase <- c("if", "this", "year")


# Test CreateValNgrams() ------------------------------------------------------
long.ngrams <- CreateValNgrams(long.phrase)
short.ngrams <- CreateValNgrams(short.phrase)
two.ngrams <- CreateValNgrams(c("who", "are"))
one.ngrams <- CreateValNgrams("who")

# TODO: Test for <UNK>
expect_equal(long.ngrams[3, 1], hash("if"))
expect_equal(nrow(long.ngrams), 11)
expect_true(is.na(long.ngrams[1, 2]))
expect_true(is.na(long.ngrams[2, 1]))

expect_equal(nrow(short.ngrams), 2)
expect_equal(nrow(two.ngrams), 1)
expect_equal(nrow(one.ngrams), 0)


# Test BuildValNgramTable() ---------------------------------------------------
ngram.table <- BuildValNgramTable(list(long.phrase, short.phrase))
expect_equal(nrow(ngram.table), 10)
expect_equal(max(ngram.table$count), 2)
expect_equal(sum(is.na(ngram.table$word2)), 1)

