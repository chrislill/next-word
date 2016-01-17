library(dplyr)

# Load model ------------------------------------------------------------------
# load("models\\training-dictionary.RData")
# load("models\\training-bigram-model.RData")
# load("models\\training-trigram-model.RData")
# load("models\\training-quadgram-model.RData")
# load("data\\val-ngrams.RData")
model <- bigram.model

eval.start <- Sys.time()

# Evaluate --------------------------------------------------------------------
val.results <- model[val.ngrams]
val.results[, accuracy:=(outcome == answer_1)]

# TODO: There is probably a more elegant way to do this...
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
                   comment = "Try 8000 dictionary")
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



