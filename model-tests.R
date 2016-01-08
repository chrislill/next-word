library(testthat)

source("model-functions.R")

long.phrase <- c("if", "this", "year", "would", "just", "end", "i", "think", "we'd", "all", "be", "okay")
short.phrase <- c("who", "are", "you")
two.phrase <- c("who", "are")

long.ngrams <- CreateValNgrams(long.phrase)
short.ngrams <- CreateValNgrams(short.phrase)
two.ngrams <- CreateValNgrams(two.phrase)

# TODO: Test for <UNK>
# TODO: Test for two words

expect_equal(long.ngrams[1, 1], hash("if"))
expect_equal(nrow(long.ngrams), length(long.phrase) - 2)
expect_equal(length(long.ngrams), (length(long.phrase) - 2) * 5)
expect_true(is.na(long.ngrams[9, 5]))
expect_true(is.na(long.ngrams[10, 4]))

expect_equal(nrow(short.ngrams), 1)
expect_equal(nrow(two.ngrams), 0)
