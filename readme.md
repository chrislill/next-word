# Next Word Prediction
A model and website for Natural Language Prediction. Created for the Data Science Capstone project run by Coursera and John Hopkins University.

## Methodology
The R scripts in the root of this repository provide the following functionality:

1. Read in the HC text corpus with over 4 million tweets, blogs and news articles
2. Split the text corpus into
    * 60% training set
    * 30% validation set
    * 10% test set
    * Note: The final model uses a 90% training set for improved accuracy
2. Tokenise the text by
    * Changing to lowercase
    * Replace numeric characters with a space
    * Replace different apostrophe characters with a standard apostrophe
    * Removing punctuation except hyphens and apostrophes with a space
3. Split the text into tokens
4. Select a dictionary of words and hashed values of a configured size
    * Dictionary size has been set to 16000 words to optimize accuracy 
6. Replace words not in the dictionary with '<UNK>'. This allows the model to handle unknown words in the future
7. Hash the tokens (to reduce processing time and memory usage)
8. Extract every 2-gram, 3-gram and 4-gram from the text
9. Store the counts of each n-gram in a data.table (to reduce processing time)
10. Summarise the top 5 answers and probabilities for each 1-gram, 2-gram and 3-gram
11. Create a summary of the unique 4-grams in the validation or test set, and their frequency
12. Evaluate the accuracy of each ngram model, and [record metrics](http://rpubs.com/chrislill/next-word-metrics)
13. Evaluate the accuracy of an interpolated model, and [record metrics](http://rpubs.com/chrislill/next-word-metrics)
    * The coefficients selected to optimise efficiency on the validation set are:
    * lambda.trigram = 0.6
    * lambda.quadgram = 0.8
14. Provide a function for the website to call to interpolate the three models

## Execution
To reproduce this model use the following steps. On a PC with 8GB RAM you may need to restart R studio after steps 1, 2, 3, 4, 6 and 8.

1. Run 1-prepare-data.R
2. Run 2d-create-evaluation-ngrams.R
3. Run 2a-create-bigram-model.R
4. Evaluate the bigram model using 3a-evaluate-ngram.R
5. Run 2b-create-trigram-model.R
6. Evaluate the trigram model using 3a-evaluate-ngram.R
7. Run 2a-create-quadgram-model.R
8. Evaluate the quadgram model using 3a-evaluate-ngram.R
9. Evaluate the interpolated model using 3a-evaluate-ngram.R
10. Copy the content of the models into the next-word-app folder
11. Open UI.R in the next-word-app folder and run the app
