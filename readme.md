# Next Word Prediction
A model and website for Natural Language Prediction. Created for the Data Science Capstone project, from Coursera and John Hopkins University.

## Features
1. Read in the HC text corpus with over 4 million tweets, blogs and news articles
1. Tokenise the text by
Changing to lowercase
Replace numeric characters with a space
Replace different apostrophe characters with a standard apostrophe
Removing punctuation except hyphens and apostrophes with a space
Split the text into tokens
Select a dictionary of words and hashed values of a configured size
Dictionary size has been set to 16000 words to optimize accuracy 
Replace words not in the dictionary with '<UNK>'. This allows the model to handle unknown words in the future
Hash the tokens (to reduce processing time and memory usage)
Extract every 2-gram, 3-gram and 4-gram from the text
Store the counts of each n-gram in a data.table (to reduce processing time)
Summarise the top 5 answers and probabilities for each 1-gram, 2-gram and 3-gram

## Execution
