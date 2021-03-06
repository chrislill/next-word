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
  
  tokens <- hash(tokens)
  
  word.1 <- vector(mode = "integer")
  answer <- vector(mode = "integer")

  if(length(tokens) > 1) {
    for(i in 1:(length(tokens) - 1)) {
      word.1 <- append(word.1, tokens[i])
      answer <- append(answer, tokens[i+1])
    }
  }
  m <-matrix(c(word.1, answer), ncol = 2)
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

  tokens <- hash(tokens)
    
  word.2 <- vector(mode = "integer")
  word.1 <- vector(mode = "integer")
  answer <- vector(mode = "integer")
  
  if(length(tokens) > 2) {
    for(i in 1:(length(tokens) - 2)) {
      word.2 <- append(word.2, tokens[i])
      word.1 <- append(word.1, tokens[i+1])
      answer <- append(answer, tokens[i+2])
    }
  }
  m <-matrix(c(word.2, word.1, answer), ncol = 3)
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
  
  word.3 <- vector(mode = "integer")
  word.2 <- vector(mode = "integer")
  word.1 <- vector(mode = "integer")
  answer <- vector(mode = "integer")
    
  if(length(tokens) > 3) {
    for(i in 1:(length(tokens) - 3)) {
      word.3 <- append(word.3, tokens[i])
      word.2 <- append(word.2, tokens[i+1])
      word.1 <- append(word.1, tokens[i+2])
      answer <- append(answer, tokens[i+3])
    }
  }
  m <-matrix(c(word.3, word.2, word.1, answer), ncol = 4)
}


CreateValNgrams <- function(tokens) {
  # Creates a simple matrix with a row for each quadgram and a column 
  # for each word. The first two rows contain NA values so they can be used to 
  # validate bigrams and trigrams.
  #
  # Args:
  #   tokens: A vector of words
  #
  # Returns:
  #   A matrix with a row for each 4-gram
  
  tokens <- hash(tokens)
  tokens <- append(c(NA, NA), tokens)
  
  word1 <- vector(mode = "integer")
  word2 <- vector(mode = "integer")
  word3 <- vector(mode = "integer")
  outcome <- vector(mode = "integer")
  
  if(length(tokens) >= 4) {
    for(i in 1:(length(tokens) - 3)) {
      word1 <- append(word1, tokens[i])
      word2 <- append(word2, tokens[i+1])
      word3 <- append(word3, tokens[i+2])
      outcome <- append(outcome, tokens[i+3])
    }
  }
  m <-matrix(c(word1, word2, word3, outcome), ncol = 4)
}


CountBigrams <- function(token.list) {
  # Creates a data.table of unique bigrams and a count of occurances
  #
  # Args:
  #   tokens: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A bigram frequency data.table

  bigram.list <- sapply(token.list, 
                         CreateBigrams, 
                         simplify = "array", 
                         USE.NAMES = FALSE)
  
  bigram.dt <- data.table(do.call(rbind, bigram.list))
  setnames(bigram.dt, 1:2, c("word.1", "answer"))
  
  bigram.count <- unique(bigram.dt[, count:=.N, by = .(word.1, answer)])
  setorder(bigram.count, word.1, -count)
  setkey(bigram.count, word.1)
  
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
  
  trigram.list <- sapply(token.list, 
                         CreateTrigrams, 
                         simplify = "array", 
                         USE.NAMES = FALSE)
  
  trigram.dt <- data.table(do.call(rbind, trigram.list))
  setnames(trigram.dt, 1:3, c("word.2", "word.1", "answer"))
  
  trigram.count <- unique(trigram.dt[, count:=.N, by = .(word.1, word.2, answer)])
  setorder(trigram.count, word.1, word.2, -count)
  setkey(trigram.count, word.1, word.2)
  
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
  
  quadgram.list <- sapply(token.list, 
                         CreateQuadgrams, 
                         simplify = "array", 
                         USE.NAMES = FALSE)

  quadgram.dt <- data.table(do.call(rbind, quadgram.list))
  setnames(quadgram.dt, 1:4, c("word.3", "word.2", "word.1", "answer"))

  quadgram.count <- unique(quadgram.dt[, count:=.N,
                                       by = .(word.3, word.2, word.1, answer)])
  setorder(quadgram.count, word.1, word.2, word.3, -count)
  setkey(quadgram.count, word.1, word.2, word.3)
  
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
  bigram.totals <- bigram.count[, sum(count),by=.(word.1)]
  
  # Return the top 5 rows for each unigram - Data tables are awesome! 
  bigram.top5 <- bigram.count[count != 1 & answer != hash("<UNK>"), 
                                .SD[1:min(5, .N)], by=.(word.1)]
  bigram.top5[, rank:=(1:.N), by=.(word.1)]
  
  # Cast the data.table, so there is a single row for each unigram
  bigram.wide <- dcast(bigram.top5, word.1 ~ rank, 
                        value.var = c("answer", "count"))
  bigram.model <- bigram.totals[bigram.wide] %>%
    mutate(pr_1 = signif(count_1 / V1, 2),
           pr_2 = signif(count_2 / V1, 2),
           pr_3 = signif(count_3 / V1, 2),
           pr_4 = signif(count_4 / V1, 2),
           pr_5 = signif(count_5 / V1, 2)) %>%
    select(starts_with("word"), starts_with("answer"), starts_with("pr"))
  
  # TODO: Is perplexity calculated correctly?
#   bigram.perplexity <- bigram.top5[bigram.totals, 
#                                    perplexity:=((count / V1) ^ (count / bigram.rows))]
#   perplexity <<- signif(bigram.perplexity[, prod(perplexity)], 3)
  
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
  trigram.totals <- trigram.count[, sum(count),by=.(word.1, word.2)]
    
  # Return the top 5 rows for each bigram - Data tables are awesome! 
  trigram.top5 <- trigram.count[count != 1 & answer != hash("<UNK>"), 
                                .SD[1:min(5, .N)], by=.(word.1, word.2)]
  trigram.top5[, rank:=(1:.N), by=.(word.1, word.2)]
  
  # Cast the data.table, so there is a single row for each bigram
  trigram.wide <- dcast(trigram.top5, word.1 + word.2 ~ rank, 
                         value.var = c("answer", "count"))
  trigram.model <- trigram.totals[trigram.wide] %>%
    mutate(pr_1 = signif(count_1 / V1, 2),
           pr_2 = signif(count_2 / V1, 2),
           pr_3 = signif(count_3 / V1, 2),
           pr_4 = signif(count_4 / V1, 2),
           pr_5 = signif(count_5 / V1, 2)) %>%
    select(starts_with("word"), starts_with("answer"), starts_with("pr"))
  
  # TODO: Is perplexity calculated correctly?
#   trigram.perplexity <- trigram.top5[trigram.totals, 
#                                      perplexity:=((count / V1) ^ (count / trigram.rows))]
#   perplexity <<- signif(trigram.perplexity[, prod(perplexity)], 3)
  
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
  quadgram.totals <- quadgram.count[, sum(count),by=.(word.1, word.2, word.3)]
  
  # Return the top 5 rows for each trigram - Data tables are awesome! 
  quadgram.top5 <- quadgram.count[count != 1 & answer != hash("<UNK>"), 
                                .SD[1:min(5, .N)], by=.(word.1, word.2, word.3)]
  quadgram.top5[, rank:=(1:.N), by=.(word.1, word.2, word.3)]
  
  # Cast the data.table, so there is a single row for each trigram
  quadgram.wide <- dcast(quadgram.top5, word.1 + word.2 + word.3 ~ rank, 
                        value.var = c("answer", "count"))
  quadgram.model <- quadgram.totals[quadgram.wide] %>%
    mutate(pr_1 = signif(count_1 / V1, 2),
           pr_2 = signif(count_2 / V1, 2),
           pr_3 = signif(count_3 / V1, 2),
           pr_4 = signif(count_4 / V1, 2),
           pr_5 = signif(count_5 / V1, 2)) %>%
    select(starts_with("word"), starts_with("answer"), starts_with("pr"))
  
  # TODO: Is perplexity calculated correctly?
#   quadgram.perplexity <- quadgram.top5[quadgram.totals, 
#                            perplexity:=((count / V1) ^ (count / quadgram.rows))]
#   perplexity <<- signif(quadgram.perplexity[, prod(perplexity)], 3)
  
  quadgram.model
} 


BuildValNgramTable <- function(token.list) {
  # Creates a Ngram data.table with counts which can be used for evaluation of 
  # bigrams, trigrams and quadgrams
  #
  # Args:
  #   token.list: A tokenised list, containing vectors of words
  #
  # Returns:
  #   A data.table with a row for each 5-gram
  
  ngram.list <- sapply(token.list, 
                          CreateValNgrams, 
                          simplify = "array", 
                          USE.NAMES = FALSE)
  
  ngram.dt <- data.table(do.call(rbind, ngram.list))
  names(ngram.dt) <- c("word.3", "word.2", "word.1", "outcome")
  
  ngram.count <- unique(ngram.dt[, count:=.N, by = .(word.1, word.2, word.3, outcome)])
  setkey(ngram.count, word.1, word.2, word.3, outcome)
  
  ngram.count
} 