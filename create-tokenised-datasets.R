# Load myTokenise function
source("next-word-functions.R")

# Download file and unzip
if(!file.exists("Coursera-SwiftKey.zip")) {
  training.url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(training.url, "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}


# Load data
# TODO: Get more data!
if(!exists("twitter")) {
  con.twitter <- file("final\\en_US\\en_US.twitter.txt", open="rb")
  con.blogs <- file("final\\en_US\\en_US.blogs.txt", open="rb")
  con.news <- file("final\\en_US\\en_US.news.txt", open="rb")
  twitter <- readLines(con.twitter, encoding = "UTF-8", skipNul = TRUE)
  blogs <- readLines(con.blogs, encoding = "UTF-8", skipNul = TRUE)
  news <- readLines(con.news, encoding = "UTF-8", skipNul = TRUE)
  close(con.twitter)
  close(con.news)
  close(con.blogs)
}

# Partition the data as follows:
# 60% training set
# 30% test set
# 10% validation set
# 1000 record dev set (from training)
corpora <- append(twitter, append(blogs, news))
boundary <- c(floor(0.6 * length(corpora)),
              floor(0.9 * length(corpora)),
              length(corpora))
set.seed(1234)
corpora.index <- sample(length(corpora), length(corpora))
training.corpora <- corpora[corpora.index[1:boundary[1]]]
# test.corpora <- corpora[corpora.index[(boundary[1] + 1):boundary[2]]]                          
# validation.corpora <- corpora[corpora.index[(boundary[2] + 1):boundary[3]]]  
dev.corpora <- training.corpora[1:1000]
dev2.corpora <- training.corpora[1:10000]
dev3.corpora <- training.corpora[1:100000]

# Tokenise
dev.word.tokens <- sapply(dev.corpora, TokeniseText)
dev2.word.tokens <- sapply(dev2.corpora, TokeniseText)
dev3.word.tokens <- sapply(dev3.corpora, TokeniseText)
# training.word.tokens <- sapply(training.corpora, TokeniseText, USE.NAMES = FALSE)
# test.word.tokens <- sapply(test.corpora, TokeniseText, USE.NAMES = FALSE)
# validation.word.tokens <- sapply(validation.corpora, TokeniseText, USE.NAMES = FALSE)

# Index
dev.index <- CreateIndex(dev.word.tokens)
dev2.index <- CreateIndex(dev2.word.tokens)
dev3.index <- CreateIndex(dev3.word.tokens)

# Change Tokens to use index
index <- dev.index
dev.tokens <- sapply(dev.word.tokens, match, table = dev.index)
dev2.tokens <- sapply(dev2.word.tokens, match, table = dev2.index)
# TODO: Fix paging error with match which causes next line to hang
# dev3.tokens <- sapply(dev3.word.tokens, match, table = dev3.index)

# Save tokens
if (!file.exists("data")) {
  dir.create("data")
}
save(dev.corpora, file = "data\\dev-corpora.RData")
save(dev.tokens, dev.index, file = "data\\dev-tokens.RData")
save(dev2.tokens, dev2.index, file = "data\\dev2-tokens.RData")
# save(dev3.tokens, dev3.index, file = "data\\dev3-tokens.RData")
# save(training.tokens, file = "data\\training-tokens.RData")
# save(test.tokens, file = "data\\test-tokens.RData")
# save(validation.tokens, file = "data\\validation-tokens.RData")
