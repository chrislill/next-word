source("tokenise-functions.R")

# Skip the first half of this file if training.all.tokens exists --------------
if(!file.exists("data\\training-all-tokens.RData")) {

  # Download file and unzip -----------------------------------------------------
  if(!file.exists("Coursera-SwiftKey.zip")) {
    training.url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(training.url, "Coursera-SwiftKey.zip")
    unzip("Coursera-SwiftKey.zip")
  }
  
  
  # Load data -------------------------------------------------------------------
  con.twitter <- file("final\\en_US\\en_US.twitter.txt", open="rb")
  con.blogs <- file("final\\en_US\\en_US.blogs.txt", open="rb")
  con.news <- file("final\\en_US\\en_US.news.txt", open="rb")
  twitter <- readLines(con.twitter, encoding = "UTF-8", skipNul = TRUE)
  blogs <- readLines(con.blogs, encoding = "UTF-8", skipNul = TRUE)
  news <- readLines(con.news, encoding = "UTF-8", skipNul = TRUE)
  close(con.twitter)
  close(con.news)
  close(con.blogs)
  
  
  # Create folders for data -----------------------------------------------------
  if (!file.exists("models")) {
    dir.create("data")
    dir.create("models")
  }
  
  
  # Partition the data ----------------------------------------------------------
  # 60% training set
  # 30% validation set (Currently set to 10%)
  # 10% test set
  # Use smaller dev sets to get the models working and optimise performance
  corpora <- append(twitter, append(blogs, news))
  boundary <- c(floor(0.6 * length(corpora)),
                floor(0.9 * length(corpora)),
                length(corpora))
  set.seed(1234)
  corpora.index <- sample(length(corpora), length(corpora))
  training.corpora <- corpora[corpora.index[1:boundary[1]]]
  validation.corpora <- corpora[corpora.index[(boundary[1] + 1):boundary[2]]]                          
  # test.corpora <- corpora[corpora.index[(boundary[2] + 1):boundary[3]]]  
  # dev.corpora <- training.corpora[1:1000]
  # dev2.corpora <- training.corpora[1:10000]
  # dev3.corpora <- training.corpora[1:100000]
  # save(corpora, file = "data\\corpora.RData")
  # save(dev.corpora, dev2.corpora, dev3.corpora, file = "data\\dev-corpora.RData")
  
  
  # Tokenise --------------------------------------------------------------------
  # dev.tokens <- sapply(dev.corpora, TokeniseText)
  # dev2.tokens <- sapply(dev2.corpora, TokeniseText)
  # dev3.tokens <- sapply(dev3.corpora, TokeniseText)
  training.all.tokens <- sapply(training.corpora, TokeniseText, USE.NAMES = FALSE)
  validation.tokens <- sapply(validation.corpora, TokeniseText, USE.NAMES = FALSE)
  # test.tokens <- sapply(test.corpora, TokeniseText, USE.NAMES = FALSE)
  
  save(training.all.tokens, file = "data\\training-all-tokens.RData")
  save(validation.tokens, file = "data\\validation-tokens.RData")
  # save(test.tokens, file = "data\\test-tokens.RData")
} else {
  load("data\\training-all-tokens.RData")
}

# Create dictionary with frequent words ---------------------------------------
# dev.dictionary <- CreateDictionary(dev.tokens, 8000)
# dev2.dictionary <- CreateDictionary(dev2.tokens, 8000)
# dev3.dictionary <- CreateDictionary(dev3.tokens, 8000)
training.dictionary <- CreateDictionary(training.all.tokens, 16000)

save(training.dictionary, file = "models\\training-dictionary.RData")


# Replace tokens that aren't in the dictionary with <UNK> ---------------------
# dev.tokens <- lapply(dev.tokens, ReplaceUnknownWords, 
#                        dictionary = dev.dictionary$word)
# dev2.tokens <- lapply(dev2.tokens, ReplaceUnknownWords, 
#                       dictionary = dev2.dictionary$word)
# dev3.tokens <- lapply(dev3.tokens, ReplaceUnknownWords, 
#                       dictionary = dev3.dictionary$word)
training.tokens <- lapply(training.all.tokens, ReplaceUnknownWords,
                          dictionary = training.dictionary$word)

# save(dev.tokens, dev.dictionary, file = "data\\dev-tokens.RData")
# save(dev2.tokens, dev2.dictionary, file = "data\\dev2-tokens.RData")
# save(dev3.tokens, dev3.dictionary, file = "data\\dev3-tokens.RData")
save(training.tokens, file = "data\\training-tokens.RData")
