require(data.table, warn.conflicts = FALSE)

load("data\\training-dictionary.RData")

DLookup <- function(x) {
  training.dictionary[data.table(x), word]
}


t <- trigram.model[!is.na(answer_5)]
t2 <- t[1:1000]
t3 <- mutate(t2,
             word.1 = DLookup(word.1), 
             word.2 = DLookup(word.2), 
             answer_1 = DLookup(answer_1), 
             answer_2 = DLookup(answer_2), 
             answer_3 = DLookup(answer_3), 
             answer_4 = DLookup(answer_4), 
             answer_5 = DLookup(answer_5)) 

v <- val.results[!is.na(answer_5)]
v2 <- v[1:1000]
v3 <- mutate(v2,
             word.1 = DLookup(word.1), 
             word.2 = DLookup(word.2), 
             word.3 = DLookup(word.3), 
             answer_1 = DLookup(answer_1), 
             answer_2 = DLookup(answer_2), 
             answer_3 = DLookup(answer_3), 
             answer_4 = DLookup(answer_4), 
             answer_5 = DLookup(answer_5),
             outcome = DLookup(outcome))