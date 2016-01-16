library(dplyr)
library(data.table)

# source("tokenise-functions.R")
# source("model-functions.R")

# Load model ------------------------------------------------------------------
load("models\\training-dictionary.RData")
load("models\\training-bigram-model.RData")
load("models\\training-trigram-model.RData")
load("models\\training-quadgram-model.RData")
load("data\\val-ngrams.RData")

# These coefficients will need to be tuned
lambda.trigram <- 0.6
lambda.quadgram <- 0.3

eval.start <- Sys.time()

# Build a set of answers for a unique set of val.inputs------------------------
val.inputs <- unique(val.ngrams[, .(word.3, word.2, word.1)])
ngram.answers <- bigram.model[trigram.model[quadgram.model[val.inputs]]]
setkey(ngram.answers, "word.1", "word.2", "word.3")

cols.bigram.answers <- 2:6
cols.bigram.probs <- 7:11
cols.trigram.answers <- 13:17
cols.trigram.probs <- 18:22
cols.quadgram.answers <- 24:28
cols.quadgram.probs <- 29:33

# Modify the probabilities by the lambda coefficients in our interpolated model
# It doesn't matter if the higher order models have no answers, because we're
# only looking for the top 5, not a calculation of probabilities
for(j in cols.quadgram.probs){
  set(ngram.answers, i = NULL, j = j, 
      value = ngram.answers[[j]] * lambda.quadgram)
}
for(j in cols.trigram.probs){
  set(ngram.answers, i = NULL, j = j, 
      value = ngram.answers[[j]] * (lambda.trigram * (1 - lambda.quadgram)))
}
for(j in cols.bigram.probs){
  set(ngram.answers, i = NULL, j = j, 
      value = ngram.answers[[j]] * ((1 -lambda.trigram) * (1 - lambda.quadgram)))
}
name <- names(ngram.answers)

# Combine ngram probabilities for each answer ---------------------------------
# Loop through each trigram answer and copy it across to the bigram answer
for(right.col in cols.trigram.answers){
  for(left.col in cols.bigram.answers){
    answer.match <- ngram.answers[, get(name[left.col]) == get(name[right.col])]
    ngram.answers[answer.match, 
                  (name[left.col + 5]) := 
                    get(name[left.col + 5]) + get(name[right.col + 5])]
    ngram.answers[answer.match, (name[right.col]) := NA]
  }
}

# Loop through each quadgram answer and copy it across to the bigram or 
# trigram answer
for(right.col in cols.quadgram.answers){
  for(left.col in append(cols.bigram.answers, cols.trigram.answers)){
    answer.match <- ngram.answers[, get(name[left.col]) == get(name[right.col])]
    ngram.answers[answer.match, 
                  (name[left.col + 5]) := 
                    get(name[left.col + 5]) + get(name[right.col + 5])]
    ngram.answers[answer.match, (name[right.col]) := NA]
  }
}

# Find the top 5 probabilities ------------------------------------------------
ngram.answers[, order := sort()]






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



