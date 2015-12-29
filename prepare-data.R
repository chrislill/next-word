source("next-word-functions.R")

# Download file and unzip
if(!file.exists("Coursera-SwiftKey.zip")) {
  training.url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(training.url, "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}

# Load data
con.twitter <- file("final\\en_US\\en_US.twitter.txt", open="rb")
con.blogs <- file("final\\en_US\\en_US.blogs.txt", open="rb")
con.news <- file("final\\en_US\\en_US.news.txt", open="rb")
twitter <- readLines(con.twitter, encoding = "UTF-8", skipNul = TRUE)
blogs <- readLines(con.blogs, encoding = "UTF-8", skipNul = TRUE)
news <- readLines(con.news, encoding = "UTF-8", skipNul = TRUE)
close(con.twitter)
close(con.news)
close(con.blogs)

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
# validation.corpora <- corpora[corpora.index[(boundary[1] + 1):boundary[2]]]                          
# test.corpora <- corpora[corpora.index[(boundary[2] + 1):boundary[3]]]  
dev.corpora <- training.corpora[1:1000]
dev2.corpora <- training.corpora[1:10000]
dev3.corpora <- training.corpora[1:100000]

# Tokenise
dev.tokens <- sapply(dev.corpora, TokeniseText)
dev2.tokens <- sapply(dev2.corpora, TokeniseText)
dev3.tokens <- sapply(dev3.corpora, TokeniseText)
training.tokens <- sapply(training.corpora, TokeniseText, USE.NAMES = FALSE)
# validation.tokens <- sapply(validation.corpora, TokeniseText, USE.NAMES = FALSE)
# test.tokens <- sapply(test.corpora, TokeniseText, USE.NAMES = FALSE)

# Word Counts
dev.word.count <- CountWords(dev.tokens)
dev2.word.count <- CountWords(dev2.tokens)
dev3.word.count <- CountWords(dev3.tokens)
training.word.count <- CountWords(training.tokens)

# Save tokens
if (!file.exists("data")) {
  dir.create("data")
}
save(dev.corpora, dev2.corpora, dev3.corpora, file = "data\\dev-corpora.RData")
save(dev.tokens, dev.word.count, file = "data\\dev-tokens.RData")
save(dev2.tokens, dev2.word.count, file = "data\\dev2-tokens.RData")
save(dev3.tokens, dev3.word.count, file = "data\\dev3-tokens.RData")
save(training.tokens, training.word.count, 
     file = "data\\training-tokens.RData")
# save(validation.tokens, validation.word.count, 
#      file = "data\\validation-tokens.RData")
# save(test.tokens, test.word.count, file = "data\\test-tokens.RData")

