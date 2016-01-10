library(testthat)
library(hashr)

source("tokenise-functions.R")

# Test DLookup() --------------------------------------------------------------
h <- c(1154509973, 1136549914, 82571174, NA, 759823308, 1178941954, -1923859448, 2094660954)

expect_equal(DLookup(h[1]), "return")
expect_equal(DLookup(h[1:2]), c("return", "it"))
expect_true(is.na(DLookup(NA)))
