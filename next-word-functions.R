require(tm)
require(dplyr, warn.conflicts = FALSE)
require(data.table, warn.conflicts = FALSE)

# TODO: Sentence segmentation <S>
# TODO: <UNK> to handle missing words
TokeniseText <- function(documents) {
  # Tokenises each word in a collection of documents, removing numbers, 
  # most punctuation, excess whitespace and changing to lowercase.
  #
  # Args:
  #   documents: Vector of strings
  #
  # Returns:
  #   List containing vectors of words (token.list)
  
  x <- gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", documents)
  x <- gsub("[^a-zA-Z '-]", " ", x)
  x <- gsub("^[-']+|[-']+$| [-']+|[-']+ [-']*", " ", x)
  x <- tolower(x)
  x <- trimws(stripWhitespace(x))
  strsplit(x, " ", fixed = TRUE)
}


CreateDictionary <- function(word.token.list) {
  # Creates a dictionary of the 30000 most frequent words, sorted alphabetically
  #
  # Args:
  #   word.token.list: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A word vector, to be used as a dictionary
  
  # Test data:
  # word.token.list <- dev2.tokens
  
  word <- unlist(word.token.list, use.names = FALSE)
  
  word.count <- unique(data.table(word)[, count:=.N, by = word])
  setorder(word.count, -count)

  # Calculate the minimum word frequency to achieve a dictionary size of 30,000
  # If there are less than 30,000 records, use 1, so we omit unique words
  min.frequency <- word.count[30000, count]
  if(is.na(min.frequency)) min.frequency = 1
  
  dictionary <- sort(word.count[count > min.frequency, word])

  dictionary
}


ReplaceUnknownWords <- function(word.vector, dictionary) {
  # Replaces words that aren't in the dictionary with <UNK>
  #
  # Args:
  #   word.vector: A tokenised vector of words
  #   dictionary: A vector of frequent words
  #
  # Returns:
  #   A word vector

  word.vector[!(word.vector %in% dictionary)] = "<UNK>"
  word.vector
}


# TODO: Only keep top 5 next words
CreateTrigrams <- function(tokens) {
  # Creates a simple matrix with a row for each trigram and a column 
  # for each word. 
  #
  # Args:
  #   tokens: A vector of words
  #
  # Returns:
  #   A matrix with a row for each trigram
  
  # tokens <- dev.tokens[[5]]
  
  word1 <- vector(mode = "character")
  word2 <- vector(mode = "character")
  word3 <- vector(mode = "character")
    
  if(length(tokens) > 2) {
    for(i in 1:(length(tokens) - 2)) {
      word1 <- append(word1, tokens[i])
      word2 <- append(word2, tokens[i+1])
      word3 <- append(word3, tokens[i+2])
    }
  }
  m <-matrix(c(word1, word2, word3), ncol = 3)
  m[word3 != "<UNK>", ]
  
}


CountTrigrams <- function(token.list) {
  # Creates a data.table of unique trigrams and a count of occurances
  #
  # Args:
  #   tokens: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A trigram frequency data.table
  
  # Test data:
  # token.list <- dev.tokens
  
  trigram.list <- sapply(token.list, 
                         CreateTrigrams, 
                         simplify = "array", 
                         USE.NAMES = FALSE)
  
  trigram.dt <- data.table(do.call(rbind, trigram.list))
  names(trigram.dt) <- c("word1", "word2", "word3")

  trigram.count <- trigram.dt[, count:=.N, by = .(word1, word2, word3)]
  
  # TODO: Filter out more low frequency trigrams
  trigram.count <- unique(trigram.count[count > 1])
  setorder(trigram.count, word1, word2, -count)
  setkey(trigram.count, word1, word2)
  
  trigram.count
}


BuildTrigramModel <- function(trigram.count) {
  # Creates a model for use in the application. For each bigram it suggests the
  # top five answers and gives the percentage probabilities.
  #
  # Args:
  #   trigram.count: A trigram frequency data.table
  #
  # Returns:
  #   A data.table with a row for each bigram
  
  # Return the top 5 rows for each bigram - Data tables are awesome! 
  trigram.top5 <- trigram.count[, .SD[1:min(5, .N)], by=.(word1, word2)]
  trigram.top5[, answer:=(1:.N), by=.(word1, word2)]
  
  # Cast the data.table, so there is a single row for each bigram
  trigram.wide <- dcast(trigram.top5, word1 + word2 ~ answer, 
                         value.var = c("word3", "count"))
  trigram.totals <- trigram.count[, sum(count),by=.(word1, word2)]
  trigram.model <- trigram.wide[trigram.totals] %>%
    mutate(pr_1 = signif(count_1 / V1, 2),
           pr_2 = signif(count_2 / V1, 2),
           pr_3 = signif(count_3 / V1, 2),
           pr_4 = signif(count_4 / V1, 2),
           pr_5 = signif(count_5 / V1, 2)) %>%
    select(starts_with("word"), starts_with("pr"))
}  
  

PredictWord <- function(word1, word2) { 
  # Calculates the most likely next word 
  # Assumes that trigram.model is loaded in memory 
  # 
  # Args: 
  #   word1: First word in the trigram 
  #   word2: Second word in the trigram 
  # 
  # Returns: 
  #   The most likely third word in the trigram 

  # Test Function:
#   word1 <- as.character(validation.trigrams[[101, "word1"]]) 
#   word2 <- as.character(validation.trigrams[[101, "word2"]]) 

  # Function cannot be called by dplyr - change the structure of the model.
  prediction <- which(trigram.model$word1 == word1 & 
                             trigram.model$word2 == word2)[1]
  
  # TODO: Handle the case where the first match is actually 1
  if (is.na(prediction)) {
    "<UNK>"
  } else {
    as.character(trigram.model$word3[prediction])
  }
}
  

QuizProbabilities <- function (quiz, bigrams) {
  # Calculates the top 5 probabilities for the next word
  # Assumes that trigram.model is loaded in memory
  #
  # Args:
  #   bigrams: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A dataframe of words and their probability
  
  # bigram <- bigrams[1,]
  
  for(i in 1:nrow(quiz)) {
    suggestions <- trigram.model[trigram.model$word1 == bigrams[i, 1] & 
                                   trigram.model$word2 == bigrams[i, 2], ]
    
    # Remove this when we fix the model to return desc(count)
    suggestions <- arrange(suggestions, desc(count))
    
    total.records <- sum(suggestions$count)
    
    if (nrow(suggestions) > 5) {
      suggestions <- rbind(suggestions[1:5,],
                           suggestions[suggestions$word3 %in% quiz[i, 2:5],])
    }
    
    suggestions$question <- quiz[i, 1]
    suggestions$answer <- as.character(suggestions$word3)
    suggestions$pr <- signif(suggestions$count / total.records, 2)

    if(exists("answers")) {
      answers <- rbind(answers, suggestions[,5:7])
    } else {
      answers <- suggestions[,5:7]
    }
    
  }
  
  answers
}

