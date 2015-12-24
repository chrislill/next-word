library(tm)
library(ddply)

# TODO: Sentence segmentation <S>
# TODO: <UNK> to handle missing words
# TODO: Modify regex to correct the wrong apostrophe ' > '
myTokenise <- function(x) {
  # Tokenises each word in a collection of documents, removing numbers, 
  # most punctuation, excess whitespace and changing to lowercase.
  #
  # Args:
  #   x: Vector of strings
  #
  # Returns:
  #   List containing vectors of words
  
  x <- gsub("[^a-zA-Z '-]", " ", x)
  x <- gsub("^-|-$| -|- ", " ", x)
  x <- tolower(x)
  x <- stripWhitespace(x)
  strsplit(x, " ", fixed = TRUE)
}


myUnigramCount <- function(tokens) {
  # Creates a word frequency table, sorted by frequency
  #
  # Args:
  #   tokens: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A word frequency table
  
  word.list 
  
}

