source("tokenise-functions.R")
source("model-functions.R")

# Load data ------------------------------------------------------------------
# load("models\\training-dictionary.RData")
# load("data\\validation-tokens.RData")
# load("data\\test-tokens.RData")


# Create data.table of ngrams to be easily evaluated -------------------------
val.ngrams <- BuildValNgramTable(validation.tokens)
val.ngrams[, 1:3] <- lapply(val.ngrams[, .(word.1, word.2, word.3)],
                            ReplaceUnknownHashes,
                            dictionary = training.dictionary$hash)
val.ngrams <- unique(val.ngrams[, count := sum(count), by = .(word.1, word.2, word.3,
                                                              outcome)])
setkey(val.ngrams, "word.1", "word.2", "word.3")
save(val.ngrams, file = "data\\val-ngrams.RData")


# Repeat for test set ---------------------------------------------------------
# test.ngrams <- BuildValNgramTable(test.tokens)
# test.ngrams[, 1:3] <- lapply(test.ngrams[, .(word.1, word.2, word.3)],
#                             ReplaceUnknownHashes,
#                             dictionary = training.dictionary$hash)
# test.ngrams <- unique(test.ngrams[, count := sum(count), by = .(word.1, word.2, word.3,
#                                                               outcome)])
# save(test.ngrams, file = "data\\test-ngrams.RData")

