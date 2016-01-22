source("tokenise-functions.R")
source("model-functions.R")

# Load data ------------------------------------------------------------------
load("models\\training-dictionary.RData")
load("data\\validation-tokens.RData")
# load("data\\test-tokens.RData")


# Create data.table of ngrams to be easily evaluated -------------------------
val.ngrams <- BuildValNgramTable(validation.tokens)
# Note: tried using ReplaceUnknownHashes on outcome, but it caused a bug
val.ngrams[, 1:3] <- lapply(val.ngrams[, .(word.3, word.2, word.1)],
                            ReplaceUnknownHashes)
val.ngrams <- unique(val.ngrams[, count := sum(count), 
                                by = .(word.1, word.2, word.3, outcome)])
setkey(val.ngrams, "word.1", "word.2", "word.3")
save(val.ngrams, file = "data\\val-ngrams.RData")


# Repeat for test set ---------------------------------------------------------
# test.ngrams <- BuildValNgramTable(test.tokens)
# test.ngrams[, 1:3] <- lapply(test.ngrams[, .(word.3, word.2, word.1)],
#                             ReplaceUnknownHashes)
# test.ngrams <- unique(test.ngrams[, count := sum(count), 
#                                 by = .(word.1, word.2, word.3, outcome)])
# setkey(test.ngrams, "word.1", "word.2", "word.3")
# save(test.ngrams, file = "data\\test-ngrams.RData")

