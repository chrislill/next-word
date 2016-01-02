source("next-word-functions.R")

quiz <- matrix(c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd", 
                 "eat", "die", "give", "sleep",
                 "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
                 "horticultural", "spiritual", "financial", "marital",
                 "I'd give anything to see arctic monkeys this",
                 "weekend", "decade", "month", "morning",
                 "Talking to your mom has the same effect as a hug and helps reduce your",
                 "hunger", "stress", "happiness", "sleepiness",
                 "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
                 "look", "walk", "minute", "picture",
                 "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
                 "incident", "matter", "account", "case",
                 "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
                 "arm", "hand", "toe", "finger",
                 "Every inch of you is perfect from the bottom to the",
                 "center", "middle", "side", "top",
                 "I'm thankful my childhood was filled with imagination and bruises from playing",
                 "inside", "daily", "weekly", "outside",
                 "I like how the same people are in almost all of Adam Sandler's",
                 "pictures", "stories", "movies", "novels"),
               ncol = 5, byrow = TRUE)

load(file = "models\\training-model - Counts.RData")
load(file = "data\\training-tokens.RData")
quiz.tokens <- sapply(quiz[, 1], TokeniseText)
quiz.tokens <- lapply(quiz.tokens, ReplaceUnknownWords, 
                      dictionary = training.dictionary)

LastWord <- function(tokens, offset = 0) {
  tokens[length(tokens) - offset]
}

word1 <- sapply(quiz.tokens, LastWord, offset = 1)
word2 <- sapply(quiz.tokens, LastWord)

trigram.model <- training.trigram
# trigram.model <- dev.trigram.model
bigrams <- matrix(c(word1, word2), ncol = 2)


a1 <- QuizProbabilities(quiz, bigrams)
# Scores 5.5