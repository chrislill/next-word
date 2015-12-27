library(pryr)

# Load myUnigramCount and myTrigramCount functions
source("next-word-functions.R")

# Load data
# load("data\\dev-corpora.RData")
load("data\\dev-tokens.RData")
load("data\\dev2-tokens.RData")
load("data\\dev3-tokens.RData")
# load("data\\training-tokens.RData")
# load("data\\test-tokens.RData")


# Initialise metrics
start.time <- Sys.time()
mem.before <- mem_used()


# Create model
dev.trigram.model <- CountTrigrams(dev3.tokens)


# Add metrics
runtime <- format(Sys.time() - start.time, digits = 3)
mem.after <- format(capture.output(mem_used()))
mem.model <- format(capture.output(object_size(dev.trigram.model)))
this.metric <- cbind(start.time = format(start.time),
                     records = length(dev3.tokens),
                     runtime,
                     mem.before = format(capture.output(mem.before)),
                     mem.after,
                     mem.model,
                     comment = "Remove words occuring <6 from index")
if(file.exists("data\\metrics.RData")) {
  load("data\\metrics.RData")
  metrics <- rbind(metrics, this.metric)
} else {
  metrics <- data.frame(this.metric)
  }
save(metrics, file = "data\\metrics.RData")







