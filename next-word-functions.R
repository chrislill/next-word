library(tm)
library(dplyr)

# TODO: Sentence segmentation <S>
# TODO: <UNK> to handle missing words
# TODO: Modify regex to correct the wrong apostrophe ' > '
TokeniseText <- function(documents) {
  # Tokenises each word in a collection of documents, removing numbers, 
  # most punctuation, excess whitespace and changing to lowercase.
  #
  # Args:
  #   documents: Vector of strings
  #
  # Returns:
  #   List containing vectors of words (token.list)
  
  x <- gsub("[^a-zA-Z '-]", " ", documents)
  x <- gsub("^[-']+|[-']+$| [-']+|[-']+ [-']*", " ", x)
  x <- tolower(x)
  x <- trimws(stripWhitespace(x))
  strsplit(x, " ", fixed = TRUE)
}


CreateIndex <- function(word.token.list) {
  # Creates a vector of words, omitting words that only occur once 
  # and sorted alphabetically
  #
  # Args:
  #   word.token.list: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A word vector, to be used as an index
  
  word <- unlist(word.token.list, use.names = FALSE)
  
  word.count <- data.frame(word, stringsAsFactors = FALSE) %>%
    group_by(word) %>%
    summarise(count = length(word)) %>%
    filter(count > 5) %>%
#    arrange(word)
  arrange(count)
    
  word.count$word
  
}


CountWords <- function(token.list) {
  # Creates a word frequency table, sorted by frequency
  #
  # Args:
  #   tokenlist: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A word frequency dataframe
  
  word <- unlist(token.list, use.names = FALSE)
  
  word.count <- data.frame(word) %>%
    group_by(word) %>%
    summarise(count = length(word)) %>%
    arrange(desc(count))
  
  word.count
}


# TODO: Remove single trigrams
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
  
  #tokens <- dev.tokens[[1]]
  
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
  matrix(c(word1, word2, word3), ncol = 3)
}


CountTrigrams <- function(token.list) {
  # Creates a data-frame for predicting trigrams using the bigram,  
  # associated next word, and frequency
  #
  # Args:
  #   tokens: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A trigram frequency dataframe
  
  #token.list <- dev2.tokens
  
  trigram.list <- sapply(token.list, 
                         CreateTrigrams, 
                         simplify = "array", 
                         USE.NAMES = FALSE)
  
  trigram.df <- data.frame(do.call(rbind, trigram.list))
  names(trigram.df) <- c("word1", "word2", "word3")
  
  trigram.count <- trigram.df %>%
    group_by(word1, word2, word3) %>%
    summarise(count = length(word1)) %>%
    filter(count > 1) %>%
    arrange(word1, word2, count)

  trigram.count
}




