library(data.table)


# Load model ------------------------------------------------------------------
load("models\\training-dictionary.RData")
load("models\\training-bigram-model.RData")
load("models\\training-trigram-model.RData")
load("models\\training-quadgram-model.RData")
load("data\\val-ngrams.RData")

# These coefficients will need to be tuned
lambda.trigram <- 0.8
lambda.quadgram <- 0.5


# Initialise metrics
start.time <- Sys.time()


# Build a set of answers for a unique set of val.inputs------------------------
val.inputs <- unique(val.ngrams[, .(word.3, word.2, word.1)])


# Melt each set of answers ----------------------------------------------------
bigram.inputs <- melt(bigram.model[val.inputs], 
                      id = c("word.3", "word.2", "word.1"), 
                      measure = patterns("^answer_", "^pr_"), 
                      value.name = c("answer", "pr"),
                      na.rm = TRUE)
bigram.inputs[, variable := NULL]
bigram.inputs[, pr := pr * (1 -lambda.trigram) * (1 - lambda.quadgram)]

trigram.inputs <- melt(trigram.model[val.inputs], 
                       id = c("word.3", "word.2", "word.1"), 
                       measure = patterns("^answer_", "^pr_"), 
                       value.name = c("answer", "pr"),
                       na.rm = TRUE)
trigram.inputs[, variable := NULL]
trigram.inputs[, pr := pr * lambda.trigram * (1 - lambda.quadgram)]

quadgram.inputs <- melt(quadgram.model[val.inputs], 
                        id = c("word.3", "word.2", "word.1"), 
                        measure = patterns("^answer_", "^pr_"), 
                        value.name = c("answer", "pr"),
                        na.rm = TRUE)
quadgram.inputs[, variable := NULL]
quadgram.inputs[, pr := pr * lambda.quadgram]

# Combine data.tables ---------------------------------------------------------
ngram.inputs <- rbindlist(list(bigram.inputs, trigram.inputs, quadgram.inputs), 
                          use.names = TRUE)
rm(bigram.inputs, trigram.inputs, quadgram.inputs)
setkey(ngram.inputs, word.1, word.2, word.3)
setorder(ngram.inputs, word.1, word.2, word.3, answer)
ngram.inputs <- unique(ngram.inputs[, pr := sum(pr),
                                     by = .(word.1, word.2, word.3, answer)])


# Add rank, so that we can return the top 5 rows 
setorder(ngram.inputs, word.1, word.2, word.3, -pr)
ngram.inputs[, rank:=(1:.N), by=.(word.1, word.2, word.3)]


# Cast the data.table, so there is a single row for each trigram
interpolated.answers <- dcast(ngram.inputs[rank <= 5], 
                              word.1 + word.2 + word.3 ~ rank,
                              value.var = c("answer", "pr"))

# ngram.inputs takes a lot of memory
rm(ngram.inputs)

# Merge with the answers from the validation set and evaluate -----------------
val.results <- interpolated.answers[val.ngrams]
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


# Add metrics -----------------------------------------------------------------
runtime <- format(Sys.time() - start.time, digits = 3)
this.metric <- cbind(start.time = format(start.time),
                     records = nrow(val.ngrams),
                     runtime,
                     mem.before = NA,
                     mem.after = NA,
                     mem.model = NA,
                     accuracy,
                     top.5.accuracy,
                     comment = paste("Interpolated model with 10000 Dictionary size"))
load("data\\metrics.RData")
metrics <- rbind(metrics, this.metric)
save(metrics, file = "data\\metrics.RData")

