require(dplyr, warn.conflicts = FALSE)
require(data.table, warn.conflicts = FALSE)
require(hashr)


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


CreateQuadgrams <- function(tokens) {
  # Creates a simple matrix with a row for each quadgram and a column 
  # for each word. 
  #
  # Args:
  #   tokens: A vector of words
  #
  # Returns:
  #   A matrix with a row for each quadgram
  
  # tokens <- dev3.tokens[[5]]
  
  tokens <- hash(tokens)
  
  word1 <- vector(mode = "integer")
  word2 <- vector(mode = "integer")
  word3 <- vector(mode = "integer")
  word4 <- vector(mode = "integer")
    
  if(length(tokens) > 3) {
    for(i in 1:(length(tokens) - 3)) {
      word1 <- append(word1, tokens[i])
      word2 <- append(word2, tokens[i+1])
      word3 <- append(word3, tokens[i+2])
      word4 <- append(word4, tokens[i+3])
    }
  }
  m <-matrix(c(word1, word2, word3, word4), ncol = 4)
}


CreateValNgrams <- function(tokens) {
  # Creates a simple matrix with a row for each 5-gram and a column 
  # for each word. The last two rows contain NA values so they can be used to 
  # validate bigrams and trigrams.
  #
  # Args:
  #   tokens: A vector of words
  #
  # Returns:
  #   A matrix with a row for each 5-gram
  
  tokens <- hash(tokens)
  tokens <- append(tokens, c(NA, NA))
  
  word1 <- vector(mode = "integer")
  word2 <- vector(mode = "integer")
  word3 <- vector(mode = "integer")
  word4 <- vector(mode = "integer")
  word5 <- vector(mode = "integer")
  
  if(length(tokens) >= 5) {
    for(i in 1:(length(tokens) - 4)) {
      word1 <- append(word1, tokens[i])
      word2 <- append(word2, tokens[i+1])
      word3 <- append(word3, tokens[i+2])
      word4 <- append(word4, tokens[i+3])
      word5 <- append(word5, tokens[i+4])
    }
  }
  m <-matrix(c(word1, word2, word3, word4, word5), ncol = 5)
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
  # token.list <- dev3.tokens
  
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


CountQuadgrams <- function(token.list) {
  # Creates a data.table of unique quadgrams and a count of occurances
  #
  # Args:
  #   tokens: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A quadgram frequency data.table
  
  # Test data:
  # token.list <- dev3.tokens
  
  # TODO: Refactor CreateQuadgrams() to hash word1, word2 and word3
  # TODO: Or hash word4 and use the dictionary as a lookup
  quadgram.list <- sapply(token.list, 
                         CreateQuadgrams, 
                         simplify = "array", 
                         USE.NAMES = FALSE)

  quadgram.dt <- data.table(do.call(rbind, quadgram.list))
  names(quadgram.dt) <- c("word1", "word2", "word3", "word4")
  
  quadgram.count <- unique(quadgram.dt[, count:=.N,
                                       by = .(word1, word2, word3, word4)])
  setorder(quadgram.count, word1, word2, word3, -count)
  setkey(quadgram.count, word1, word2, word3)
  
  quadgram.count
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


BuildQuadgramModel <- function(quadgram.count) {
  # Creates a model for use in the application. For each trigram it suggests the
  # top five answers and gives the percentage probabilities.
  #
  # Args:
  #   quadgram.count: A quadgram frequency data.table
  #
  # Returns:
  #   A data.table with a row for each trigram
  
  quadgram.rows <- quadgram.count[, sum(count)]
  quadgram.totals <- quadgram.count[, sum(count),by=.(word1, word2, word3)]
  
  # Return the top 5 rows for each trigram - Data tables are awesome! 
  quadgram.top5 <- quadgram.count[count != 1 & word4 != "<UNK>", 
                                .SD[1:min(5, .N)], by=.(word1, word2, word3)]
  quadgram.top5[, answer:=(1:.N), by=.(word1, word2, word3)]
  
  # Cast the data.table, so there is a single row for each trigram
  quadgram.wide <- dcast(quadgram.top5, word1 + word2 + word3 ~ answer, 
                        value.var = c("word4", "count"))
  quadgram.model <- quadgram.totals[quadgram.wide] %>%
    mutate(pr_1 = signif(count_1 / V1, 2),
           pr_2 = signif(count_2 / V1, 2),
           pr_3 = signif(count_3 / V1, 2),
           pr_4 = signif(count_4 / V1, 2),
           pr_5 = signif(count_5 / V1, 2)) %>%
    select(starts_with("word"), starts_with("pr"))
  
  # TODO: Is perplexity calculated correctly?
  quadgram.perplexity <- quadgram.top5[quadgram.totals, 
                           perplexity:=((count / V1) ^ (count / quadgram.rows))]
  perplexity <<- signif(quadgram.perplexity[, prod(perplexity)], 3)
  
  quadgram.model
} 
  