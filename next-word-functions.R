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
  x <- gsub("^-|-$|--| -|- | '|' ", " ", x)
  x <- tolower(x)
  x <- trimws(stripWhitespace(x))
  strsplit(x, " ", fixed = TRUE)
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
  # Creates a simple dataframe with a row for each trigram. The first column is
  # the first two words. The second column is the next word.
  #
  # Args:
  #   tokens: A vector of words
  #
  # Returns:
  #   A dataframe with a row for each trigram
  
  #tokens <- dev.tokens[[1]]
  
  bigram <- vector(mode = "character")
  next.word <- vector(mode = "character")
    
  if(length(tokens) > 2) {
    for(i in 1:(length(tokens) - 2)) {
      bigram <- append(bigram, paste(tokens[i:(i+1)], collapse = " "))
      next.word <- append(next.word, tokens[i+2])
    }
  }
  #data.frame(bigram, next.word, stringsAsFactors = FALSE)
  matrix(c(bigram, next.word), ncol = 2)
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
  
  #token.list <- dev.tokens[1:100]
  
  trigram.list <- sapply(token.list, 
                         CreateTrigrams, 
                         simplify = "array", 
                         USE.NAMES = FALSE)
  
  trigram.df <- data.frame(do.call(rbind, trigram.list),
                              stringsAsFactors = FALSE)
  names(trigram.df) <- c("bigram", "next.word")
  
  trigram.count <- trigram.df %>%
    group_by(bigram, next.word) %>%
    summarise(count = length(bigram)) %>%
    arrange(bigram, count)

  trigram.count
}




