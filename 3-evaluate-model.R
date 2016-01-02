library(dplyr)

source("next-word-functions.R")

# Load model ------------------------------------------------------------------
if(!exists("trigram.model")) {
  load(file = "models\\training-model.RData")
}

# Load validation data --------------------------------------------------------
if(!exists("val.trigrams")) {
  if(!file.exists("data\\val-trigrams.RData")) {
    load("data\\validation-tokens.RData")
    val.trigrams <- CountTrigrams(validation.tokens)
    save(val.trigrams, file = "data\\val-trigrams.RData")
  }
  load("data\\val-trigrams.RData")
} 

# Evaluate --------------------------------------------------------------------
# TODO: Load the 30% dataset and compare accuracy
eval.start <- Sys.time()
set.seed(1234)
val.results <- trigram.model[val.trigrams[sample(nrow(val.trigrams), 200000), ]]
val.results[, accuracy:=(word3 == word3_1)]

# TODO: There is definitely a more elegant way to do this...
val.results[, first.5.accuracy:=(word3 == word3_1 | 
                                   word3 == word3_2 | 
                                   word3 == word3_3 | 
                                   word3 == word3_4 | 
                                   word3 == word3_5 )]
val.results[, perplexity:=(sum((word3 == word3_1) / pr_1,
                               (word3 == word3_2) / pr_2,
                               (word3 == word3_3) / pr_3,
                               (word3 == word3_4) / pr_4,
                               (word3 == word3_5) / pr_5,
                               na.rm = TRUE)), by=.(word1, word2, word3)]
val.eval <- val.results[, list(sum(count),
                               sum(accuracy * count, na.rm = TRUE),
                               sum(first.5.accuracy * count, na.rm = TRUE),
                               prod(perplexity ^ count))]
accuracy <- round(val.eval[[1, 2]] / val.eval[[1, 1]], 3)
first.5.accuracy <- round(val.eval[[1, 3]] / val.eval[[1, 1]], 3)
perplexity <- round(val.eval[[1, 4]] ^ (1 / val.eval[[1, 1]]), 3)

# Metrics for evaluation accuracy ---------------------------------------------
runtime <- format(Sys.time() - eval.start, digits = 3)
this.eval <- cbind(start.time = format(start.time),
                   records = nrow(val.results),
                   accuracy,
                   first.5.accuracy,
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
metrics[metrics$start.time == format(start.time),c("accuracy", "first.5.accuracy")] =
  c(accuracy, first.5.accuracy)
save(metrics, file = "data\\metrics.RData")




