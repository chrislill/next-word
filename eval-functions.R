require(dplyr, warn.conflicts = FALSE)
require(data.table, warn.conflicts = FALSE)
require(hashr)

# load("models\\training-dictionary.RData")
# load("models\\training-bigram-model.RData")
# load("models\\training-trigram-model.RData")
# load("models\\training-quadgram-model.RData")

# Interpolate models ----------------------------------------------------------
# These coefficients will need to be tuned
coefficient.trigram <- 0.6
coefficient.quadgram <- 0.3

InterpolateModels <- function(input.trigram) {
  # Predicts a single word from a vector of three words, interpolated from the
  # bigram, trigram and quadgram models 
  #
  # Args:
  #   tokens: A vector of three words 
  #
  # Returns:
  #   A single word
  

  a1.bigram <- unlist(bigram.model[word.1 == hash(input.trigram[3]),
                                   2:11, with = FALSE])
  a1.trigram <- unlist(trigram.model[word.1 == hash(input.trigram[3]) &
                                       word.2 == hash(input.trigram[2]), 
                                     3:12, with = FALSE])
  a1.quadgram <- unlist(quadgram.model[word.1 == hash(input.trigram[3]) & 
                                         word.2 == hash(input.trigram[2]) &
                                         word.3 == hash(input.trigram[1]),
                                       4:13, with = FALSE])

  a2.bigram <- matrix(a1.bigram, ncol = 2) 
  a2.trigram <- matrix(a1.trigram, ncol = 2) 
  a2.quadgram <- matrix(a1.quadgram, ncol = 2) 
  
  # Modify probabilities using the coefficients for each quadgram, so they can be combined
  answers <- a2.bigram
  if(any(!is.na(a2.trigram[, 2]))) {
    answers[, 2] <- answers[, 2] * (1 - coefficient.trigram)
    a2.trigram[, 2] <- a2.trigram[, 2] * coefficient.trigram
    answers <- rbind(answers, a2.trigram)
    if(any(!is.na(a2.quadgram[, 2]))) {
      answers[, 2] <- answers[, 2] * (1 - coefficient.quadgram)
      a2.quadgram[, 2] <- a2.quadgram[, 2] * coefficient.quadgram
      answers <- rbind(answers, a2.quadgram)
    }
  }
  a3 <- data.frame(answers) %>%
    rename(answer = X1, prob = X2) %>%
    group_by(answer) %>%
    summarise(prob = sum(prob)) %>%
    arrange(desc(prob))
  
  # a3$answer <- DLookup(a3$answer)

  DLookup(a3[1,1])
 
}







# Dictionary Lookup -----------------------------------------------------------
DLookup <- function(x) {
  training.dictionary[data.table(x), word]
}



# Decode ngram models ---------------------------------------------------------
# t <- trigram.model[!is.na(answer_5)]
# t2 <- t[1:1000]
# sea <- mutate(sea,
#              word.1 = DLookup(word.1), 
#              word.2 = DLookup(word.2), 
#              word.3 = DLookup(word.3), 
#              answer_1 = DLookup(answer_1), 
#              answer_2 = DLookup(answer_2), 
#              answer_3 = DLookup(answer_3), 
#              answer_4 = DLookup(answer_4), 
#              answer_5 = DLookup(answer_5)) 
# 
# v <- val.results[!is.na(answer_5)]
# v2 <- v[1:1000]
# v3 <- mutate(v2,
#              word.1 = DLookup(word.1), 
#              word.2 = DLookup(word.2), 
#              word.3 = DLookup(word.3), 
#              answer_1 = DLookup(answer_1), 
#              answer_2 = DLookup(answer_2), 
#              answer_3 = DLookup(answer_3), 
#              answer_4 = DLookup(answer_4), 
#              answer_5 = DLookup(answer_5),
#              outcome = DLookup(outcome))