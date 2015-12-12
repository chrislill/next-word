# Download file and unzip
if(!file.exists("Coursera-SwiftKey.zip")) {
  training.url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(training.url, "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}

# Load the data
if(!exists("twitter")) {
  twitter <- readLines("final\\en_US\\en_US.twitter.txt")
  blogs <- readLines("final\\en_US\\en_US.blogs.txt")
  news <- readLines("final\\en_US\\en_US.news.txt")
}
news[56]
