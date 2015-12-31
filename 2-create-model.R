library(pryr)

source("next-word-functions.R")

# Load data
# load("data\\dev-corpora.RData")
# load("data\\dev-tokens.RData")
# load("data\\dev2-tokens.RData")
# load("data\\dev3-tokens.RData")
load("data\\training-tokens.RData")
# load("data\\validation-tokens.RData")


# Initialise metrics
start.time <- Sys.time()
mem.before <- mem_used()


# Create model
dev.trigram <- CountTrigrams(dev3.tokens)
# training.trigram.model <- CountTrigrams(training.tokens)


# Add metrics
runtime <- format(Sys.time() - start.time, digits = 3)
mem.after <- format(capture.output(mem_used()))
mem.model <- format(capture.output(object_size(dev.trigram)))
this.metric <- cbind(start.time = format(start.time),
                     records = length(dev3.tokens),
                     runtime,
                     mem.before = format(capture.output(mem.before)),
                     mem.after,
                     mem.model,
                     comment = "Remove trigrams with word3 = <UNK>")
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
save(dev.trigram, start.time, file = "models\\dev-model.RData")






