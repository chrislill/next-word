library(pryr)
library(hashr)

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
# within BuildNgramModel()
# perplexity <- numeric()

# Create model
quadgram.count <- CountQuadgrams(training.tokens)
quadgram.model <- BuildQuadgramModel(quadgram.count)

# Add metrics
runtime <- format(Sys.time() - start.time, digits = 3)
mem.after <- format(capture.output(mem_used()))
mem.model <- format(capture.output(object_size(quadgram.model)))
this.metric <- cbind(start.time = format(start.time),
                     records = length(training.tokens),
                     runtime,
                     mem.before = format(capture.output(mem.before)),
                     mem.after,
                     mem.model,
                     accuracy = NA,
                     top.5.accuracy = NA,
                     comment = "Quadgram with 16000 Dictionary size")
if(file.exists("data\\metrics.RData")) {
  load("data\\metrics.RData")
  metrics <- rbind(metrics, this.metric)
} else {
  metrics <- data.frame(this.metric, stringsAsFactors = FALSE)
  }
save(metrics, file = "data\\metrics.RData")

save(quadgram.model, start.time, file = "models\\training-quadgram-model.RData")





