library(pryr)

source("model-functions.R")

# Load data
# load("data\\dev-corpora.RData")
# load("data\\dev-tokens.RData")
# load("data\\dev2-tokens.RData")
# load("data\\dev3-tokens.RData")
load("data\\training-tokens.RData")
load("models\\training-dictionary.RData")

# Initialise metrics
start.time <- Sys.time()
gc()
mem.before <- mem_used()

# Declare Perplexity as a system level variable which can be modified from 
# within BuildTrigramModel()
# perplexity <- numeric()

# Create model
bigram.count <- CountBigrams(training.tokens)
bigram.model <- BuildBigramModel(bigram.count)

# Add metrics
runtime <- format(Sys.time() - start.time, digits = 3)
mem.after <- format(capture.output(mem_used()))
mem.model <- format(capture.output(object_size(bigram.model)))
this.metric <- cbind(start.time = format(start.time),
                     records = length(training.tokens),
                     runtime,
                     mem.before = format(capture.output(mem.before)),
                     mem.after,
                     mem.model,
                     accuracy = NA,
                     top.5.accuracy = NA,
                     comment = "Hashed bigram model")
if(file.exists("data\\metrics.RData")) {
  load("data\\metrics.RData")
  metrics <- rbind(metrics, this.metric)
} else {
  metrics <- data.frame(this.metric, stringsAsFactors = FALSE)
  }
save(metrics, file = "data\\metrics.RData")

save(bigram.model, start.time, file = "models\\training-bigram-model.RData")






