library(dplyr)

source("tokenise-functions.R")
source("model-functions.R")

# Load model ------------------------------------------------------------------
load("models\\training-dictionary.RData")
# load("models\\training-bigram-model.RData")
# load("models\\training-trigram-model.RData")
load("models\\training-quadgram-model.RData")
model <- quadgram.model


# Load validation data --------------------------------------------------------
if(!file.exists("data\\val-ngrams.RData")) {
  load("data\\validation-tokens.RData")
  val.ngrams <- BuildValNgramTable(validation.tokens)
  val.ngrams[, 1:3] <- lapply(val.quadgrams[unmatched, .(word.1, word.2, word.3)],
                              ReplaceUnknownHashes,
                              dictionary = training.dictionary$hash)
  val.ngrams <- unique(val.ngrams[, count = sum(count), by = .(word.1, word.2, word.3,
                                                       outcome)])
  save(val.ngrams, file = "data\\val-ngrams.RData")
} else {
  load("data\\val-ngrams.RData")
}

# Evaluate --------------------------------------------------------------------
eval.start <- Sys.time()

val.results <- model[val.ngrams]
val.results[, accuracy:=(outcome == answer_1)]

# TODO: There is definitely a more elegant way to do this...
val.results[, top.5.accuracy:=(outcome == answer_1 | 
                                 outcome == answer_2 | 
                                 outcome == answer_3 |
                                 outcome == answer_4 |
                                 outcome == answer_5 )]
val.eval <- val.results[, list(sum(count, na.rm = TRUE),
                               sum(accuracy * count, na.rm = TRUE),
                               sum(top.5.accuracy * count, na.rm = TRUE))]
accuracy <- round(val.eval[[1, 2]] / val.eval[[1, 1]], 3)
top.5.accuracy <- round(val.eval[[1, 3]] / val.eval[[1, 1]], 3)

# Metrics for evaluation accuracy ---------------------------------------------
runtime <- format(Sys.time() - eval.start, digits = 3)
this.eval <- cbind(eval.start = format(eval.start),
                   records = nrow(val.results),
                   accuracy,
                   top.5.accuracy,
                   runtime,
                   comment = "Rerun models with 30000 dictionary to get top.5")
if(file.exists("data\\eval-accuracy.RData")) {
  load("data\\eval-accuracy.RData")
  eval.accuracy <- rbind(eval.accuracy, this.eval)
} else {
  eval.accuracy <- data.frame(this.eval, stringsAsFactors = FALSE)
}
save(eval.accuracy, file = "data\\eval-accuracy.RData")

load("data\\metrics.RData")
metrics[metrics$start.time == format(start.time),c("accuracy", "top.5.accuracy")] =
  c(accuracy, top.5.accuracy)
save(metrics, file = "data\\metrics.RData")



