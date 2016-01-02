require(dplyr, warn.conflicts = FALSE)
require(data.table, warn.conflicts = FALSE)


CreateBigrams <- function(tokens) {
  # Creates a simple matrix with a row for each bigram and a column 
  # for each word. 
  #
  # Args:
  #   tokens: A vector of words
  #
  # Returns:
  #   A matrix with a row for each bigram
  
  # tokens <- dev.tokens[[5]]
  
  word1 <- vector(mode = "character")
  word2 <- vector(mode = "character")

  if(length(tokens) > 1) {
    for(i in 1:(length(tokens) - 1)) {
      word1 <- append(word1, tokens[i])
      word2 <- append(word2, tokens[i+1])
    }
  }
  m <-matrix(c(word1, word2), ncol = 2)
}


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
}


CountBigrams <- function(token.list) {
  # Creates a data.table of unique bigrams and a count of occurances
  #
  # Args:
  #   tokens: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A bigram frequency data.table
  
  # Test data:
  # token.list <- dev.tokens
  
  bigram.list <- sapply(token.list, 
                         CreateBigrams, 
                         simplify = "array", 
                         USE.NAMES = FALSE)
  
  bigram.dt <- data.table(do.call(rbind, bigram.list))
  names(bigram.dt) <- c("word1", "word2")
  
  bigram.count <- unique(bigram.dt[, count:=.N, by = .(word1, word2)])
  setorder(bigram.count, word1, -count)
  setkey(bigram.count, word1)
  
  bigram.count
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
  
  trigram.count <- unique(trigram.dt[, count:=.N, by = .(word1, word2, word3)])
  setorder(trigram.count, word1, word2, -count)
  setkey(trigram.count, word1, word2)
  
  trigram.count
}


BuildBigramModel <- function(bigram.count) {
  # Creates a model for use in the application. For each unigram it suggests the
  # top five answers and gives the percentage probabilities.
  #
  # Args:
  #   bigram.count: A bigram frequency data.table
  #
  # Returns:
  #   A data.table with a row for each unigram
  
  bigram.rows <- bigram.count[, sum(count)]
  bigram.totals <- bigram.count[, sum(count),by=.(word1)]
  
  # Return the top 5 rows for each unigram - Data tables are awesome! 
  bigram.top5 <- bigram.count[count != 1 & word2 != "<UNK>", 
                                .SD[1:min(5, .N)], by=.(word1)]
  bigram.top5[, answer:=(1:.N), by=.(word1)]
  
  # Cast the data.table, so there is a single row for each unigram
  bigram.wide <- dcast(bigram.top5, word1 ~ answer, 
                        value.var = c("word2", "count"))
  bigram.model <- bigram.totals[bigram.wide] %>%
    mutate(pr_1 = signif(count_1 / V1, 2),
           pr_2 = signif(count_2 / V1, 2),
           pr_3 = signif(count_3 / V1, 2),
           pr_4 = signif(count_4 / V1, 2),
           pr_5 = signif(count_5 / V1, 2)) %>%
    select(starts_with("word"), starts_with("pr"))
  
  # TODO: Is perplexity calculated correctly?
  bigram.perplexity <- bigram.top5[bigram.totals, 
                                   perplexity:=((count / V1) ^ (count / bigram.rows))]
  perplexity <<- signif(bigram.perplexity[, prod(perplexity)], 3)
  
  bigram.model
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

  trigram.rows <- trigram.count[, sum(count)]
  trigram.totals <- trigram.count[, sum(count),by=.(word1, word2)]
    
  # Return the top 5 rows for each bigram - Data tables are awesome! 
  trigram.top5 <- trigram.count[count != 1 & word3 != "<UNK>", 
                                .SD[1:min(5, .N)], by=.(word1, word2)]
  trigram.top5[, answer:=(1:.N), by=.(word1, word2)]
  
  # Cast the data.table, so there is a single row for each bigram
  trigram.wide <- dcast(trigram.top5, word1 + word2 ~ answer, 
                         value.var = c("word3", "count"))
  trigram.model <- trigram.totals[trigram.wide] %>%
    mutate(pr_1 = signif(count_1 / V1, 2),
           pr_2 = signif(count_2 / V1, 2),
           pr_3 = signif(count_3 / V1, 2),
           pr_4 = signif(count_4 / V1, 2),
           pr_5 = signif(count_5 / V1, 2)) %>%
    select(starts_with("word"), starts_with("pr"))
  
  # TODO: Is perplexity calculated correctly?
  trigram.perplexity <- trigram.top5[trigram.totals, 
                                     perplexity:=((count / V1) ^ (count / trigram.rows))]
  perplexity <<- signif(trigram.perplexity[, prod(perplexity)], 3)
  
  trigram.model
}  
  