require(tm)
require(data.table, warn.conflicts = FALSE)

CreateDictionary <- function(word.token.list) {
  # Creates a dictionary of the 30000 most frequent words, sorted alphabetically
  #
  # Args:
  #   word.token.list: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A word vector, to be used as a dictionary
  
  # Test data:
  word.token.list <- training.tokens
  
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
