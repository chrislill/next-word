library(dplyr)

source("tokenise-functions.R")
source("model-functions.R")

# Load model ------------------------------------------------------------------
load("models\\training-quadgram-model.RData")
load("data\\training-dictionary.RData")


# Load validation data --------------------------------------------------------
if(!file.exists("data\\val-ngrams.RData")) {
  load("data\\validation-tokens.RData")
  val.ngrams <- BuildValNgramTable(validation.tokens)
  val.ngrams[, 1:3] <- lapply(val.quadgrams[unmatched, .(word1, word2, word3)],
                              ReplaceUnknownHashes,
                              dictionary = training.dictionary$hash)
  val.ngrams <- unique(val.ngrams[, count = sum(count), by = .(word1, word2, word3,
                                                       answer)])
  save(val.ngrams, file = "data\\val-ngrams.RData")
} else {
  load("data\\val-ngrams.RData")
}

# Evaluate --------------------------------------------------------------------
eval.start <- Sys.time()

val.results <- quadgram.model[val.ngrams[!is.na(word1)]]
val.results[, accuracy:=(answer == word4_1)]

# TODO: There is definitely a more elegant way to do this...
val.results[, top.3.accuracy:=(answer == word4_1 | 
                                 answer == word4_2 | 
                                 answer == word4_3 )]
val.eval <- val.results[, list(sum(count, na.rm = TRUE),
                               sum(accuracy * count, na.rm = TRUE),
                               sum(top.3.accuracy * count, na.rm = TRUE))]
accuracy <- round(val.eval[[1, 2]] / val.eval[[1, 1]], 3)
top.3.accuracy <- round(val.eval[[1, 3]] / val.eval[[1, 1]], 3)

# Metrics for evaluation accuracy ---------------------------------------------
runtime <- format(Sys.time() - eval.start, digits = 3)
this.eval <- cbind(start.time = format(start.time),
                   records = nrow(val.results),
                   accuracy,
                   top.3.accuracy,
                   runtime,
                   comment = "Reduced validation")
if(file.exists("data\\eval-accuracy.RData")) {
  load("data\\eval-accuracy.RData")
  eval.accuracy <- rbind(eval.accuracy, this.eval)
} else {
  eval.accuracy <- data.frame(this.eval, stringsAsFactors = FALSE)
}
save(eval.accuracy, file = "data\\eval-accuracy.RData")

load("data\\metrics.RData")
metrics[metrics$start.time == format(start.time),c("accuracy", "top.3.accuracy")] =
  c(accuracy, top.3.accuracy)
save(metrics, file = "data\\metrics.RData")




