require(tm)
require(data.table, warn.conflicts = FALSE)
require(hashr)

CreateDictionary <- function(word.token.list, max.length) {
  # Creates a dictionary of the most frequent words, sorted alphabetically
  #
  # Args:
  #   word.token.list: A tokenised list, containing vectors of words
  #   max.length: Maximum length of the dictionary
  #
  # Returns:
  #   A data.table with the words and their hash, to be used as a dictionary

  
  word <- unlist(word.token.list, use.names = FALSE)
  
  word.count <- unique(data.table(word)[, count:=.N, by = word])
  setorder(word.count, -count)

  # Calculate the minimum word frequency to achieve a dictionary size of max.length
  # If there are less than max.length records, use 2, so we omit unique words
  # Remove a couple (around 3) of lower frequency words with a duplicate hash
  min.frequency <- word.count[max.length, count]
  if(is.na(min.frequency)) min.frequency = 2
  
  selected.words <- sort(word.count[count >= min.frequency, word])
  dictionary <- data.table(hash = hash(selected.words), word = selected.words)
  setkey(dictionary, hash)
  unique(dictionary)
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
