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
test.corpora <- corpora[corpora.index[(boundary[1] + 1):boundary[2]]]                          
validation.corpora <- corpora[corpora.index[(boundary[2] + 1):boundary[3]]]  
dev.corpora <- training.corpora[1:1000]

# Tokenise
training.tokens <- sapply(training.corpora, myTokenise, USE.NAMES = FALSE)
test.tokens <- sapply(test.corpora, myTokenise, USE.NAMES = FALSE)
validation.tokens <- sapply(validation.corpora, myTokenise, USE.NAMES = FALSE)
dev.tokens <- sapply(dev.corpora, myTokenise)


# Save tokens
if (!file.exists("data")) {
  dir.create("data")
}
save(training.tokens, file = "data\\training-tokens.RData")
save(test.tokens, file = "data\\test-tokens.RData")
save(validation.tokens, file = "data\\validation-tokens.RData")
save(dev.tokens, file = "data\\dev-tokens.RData")
