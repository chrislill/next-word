library(tm)

# Optimize PCorpus
# Load PCorpus
# Regex for punctuation

# Download file and unzip
if(!file.exists("Coursera-SwiftKey.zip")) {
  training.url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(training.url, "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}

# Load the the first 10000 rows of each data file
# TODO: Create the whole corpora directly from the files
if(!exists("twitter")) {
  con.twitter <- file("final\\en_US\\en_US.twitter.txt", open="rb")
  con.blogs <- file("final\\en_US\\en_US.blogs.txt", open="rb")
  con.news <- file("final\\en_US\\en_US.news.txt", open="rb")
  twitter <- readLines(con.twitter, 10000)
  blogs <- readLines(con.blogs, 10000, encoding = "UTF-8")
  news <- readLines(con.news, 10000, encoding = "UTF-8")
  close(con.twitter)
  close(con.news)
  close(con.blogs)
}

corpus.raw <- VCorpus(VectorSource(c(twitter, blogs, news)))

# corpus.raw <- PCorpus(VectorSource(c(twitter, blogs, news)),
#                       dbControl = list(dbName = "part_corpus", dbType = "DB1"))

# Select 1000 documents from the sample for training and exploration
set.seed(1234)
train.sample <- floor(runif(1000, min = 1, max = length(corpus.raw)))
corpus.train <- corpus.raw[train.sample]

# TODO: Modify regex to remove non-intra-word dashes
# TODO: Modify regex to correct the wrong apostrophe ' > '
# There must be a better way to do this than nested regexes
punctuationRegex <- function(x) {
  gsub("[^a-zA-Z ''-]", "", x)
  # gsub("[^a-zA-Z '-]| -|- ", "", x)
  }






# Transform the corpus
# TODO: Do this in a single step
# TODO: Stopwords
# TODO: Tokenisation - what does this mean?
corpus.clean <- tm_map(corpus.train, content_transformer(punctuationRegex))
corpus.clean <- tm_map(corpus.clean, content_transformer(tolower))

words <- TermDocumentMatrix(corpus.clean)

head(corpus.clean[[1]]$content)

findFreqTerms(words)
findAssocs(words, "west", 0.1)

for(i in 1:20) print(corpus.clean[[i]]$content)






