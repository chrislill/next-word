source("next-word-functions.R")

quiz <- matrix(c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
                 "pretzels", "beer", "cheese", "soda",
                 "You're the reason why I smile everyday. Can you follow me please? It would mean the",
                 "universe", "best", "world", "most",
                 "Hey sunshine, can you follow me and make me the",
                 "bluest" , "smelliest" , "saddest" , "happiest", 
                 "Very early observations on the Bills game: Offense still struggling but the",
                 "referees"  , "players" , "defense" , "crowd",
                 "Go on a romantic date at the", "mall" , "movies" , "grocery" , "beach",
                 "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
                 "motorcycle" , "horse" , "phone" , "way", 
                 "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
                 "thing" , "time" , "weeks" , "years",
                 "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
                 "fingers" , "eyes" , "toes" , "ears",
                 "Be grateful for the good times and keep the faith during the",
                 "hard" , "worse" , "sad" , "bad", 
                 "If this isn't the cutest thing you've ever seen, then you must be",
                 "insensitive" , "asleep" , "insane" , "callous"),
               ncol = 5, byrow = TRUE)

load(file = "models\\training-model.RData")

quiz.tokens <- sapply(quiz[, 1], TokeniseText)

LastWord <- function(tokens, offset = 0) {
  tokens[length(tokens) - offset]
}

word1 <- sapply(quiz.tokens, LastWord, offset = 1)
word2 <- sapply(quiz.tokens, LastWord)

trigram.model <- training.trigram.model
# trigram.model <- dev.trigram.model
bigrams <- matrix(c(word1, word2), ncol = 2)


a1 <- QuizProbabilities(quiz, bigrams)
