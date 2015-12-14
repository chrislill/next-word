library(testthat)

expect_equal(punctuationRegex("Just like"), "Just like")
expect_equal(punctuationRegex("coast, the"), "coast the")
expect_equal(punctuationRegex("don't"), "don't")
expect_equal(punctuationRegex("you???re"), "you're")
expect_equal(punctuationRegex("thick-walled"), "thick-walled")
expect_equal(punctuationRegex("thick -walled"), "thick walled")
expect_equal(punctuationRegex("thick- walled"), "thick walled")
