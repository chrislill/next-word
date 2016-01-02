library(pryr)

source("model-functions.R")

# Load data
# load("data\\dev-corpora.RData")
load("data\\dev-tokens.RData")
load("data\\dev2-tokens.RData")
load("data\\dev3-tokens.RData")
load("data\\training-tokens.RData")

# Initialise metrics
start.time <- Sys.time()
gc()
mem.before <- mem_used()

# Declare Perplexity as a system level variable which can be modified from 
# within BuildTrigramModel()
perplexity <- numeric()

# Create model
trigram.count <- CountTrigrams(dev.tokens)
trigram.model <- BuildTrigramModel(trigram.count)

# Add metrics
runtime <- format(Sys.time() - start.time, digits = 3)
mem.after <- format(capture.output(mem_used()))
mem.model <- format(capture.output(object_size(trigram.model)))
this.metric <- cbind(start.time = format(start.time),
                     records = length(training.tokens),
                     runtime,
                     mem.before = format(capture.output(mem.before)),
                     mem.after,
                     mem.model,
                     accuracy = NA,
                     first.5.accuracy = NA,
                     perplexity,
                     comment = "Change join in trigram model, and measure perplexity")
if(file.exists("data\\metrics.RData")) {
  load("data\\metrics.RData")
  metrics <- rbind(metrics, this.metric)
} else {
  metrics <- data.frame(this.metric, stringsAsFactors = FALSE)
  }
save(metrics, file = "data\\metrics.RData")

# Save model
if (!file.exists("models")) {
  dir.create("models")
}
save(trigram.model, start.time, file = "models\\training.RData")






