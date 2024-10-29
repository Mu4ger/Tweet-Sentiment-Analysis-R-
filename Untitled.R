setwd("~/Desktop/BUS285A Coding")
# Install necessary Packages
install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
install.packages("readtext")
install.packages("ggplot2")
install.packages("text2vec")
install.packages("dplyr")
install.packages("tidytext")  
# Ensure this is installed as well
# Call the isntalled packages
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(readtext)
library(ggplot2)
library(text2vec)
library(dplyr)
library(tidytext)

trump_tweets <- read.csv("Tweets.csv")
View(trump_tweets) #notice that View has an uppercase V. The lettercase matters - if you get an error with a prompt you suspect should work, check the letter case as a starting point.
head(trump_tweets) # Shows the first 6 rows
tail(trump_tweets) # Shows the last 6 rows
#head and tail are useful for very big datasets

# 2.Keep a new dataframe (dataset) of only the tweets
## Do this using the "select" prompt for the relevant column - "text"
## We are going to pull out features from Trump’s tweets (e.g. substantive themes, topics, sentiment)
tweet_text <- trump_tweets %>% select(text)

### Now, create a CORPUS of all the words in the dataset of tweet text.
# Create a corpus from your text object
tweet_corpus <- corpus(tweet_text)
# Check it out
ndoc(tweet_corpus) #number of documents --> "tweets" in this example.
summary(tweet_corpus) #shows you the first 100 documents in your corpus; types means unique token; token means the token-count; sentences means count of sentences in the documents.

##### Next, your TOKENS
# Convert your corpus to tokens
tweet_toks <- tokens(tweet_corpus) #treats every single word and punctuation mark as a unit of analysis.
tweet_toks
# Convert your tokens object to a document-feature-matrix (dfm)
tweet_dfm <- dfm(tweet_toks)
tweet_dfm #matrix of tokens with counts

#################### CLEANING #################### Deciding what matters and what doesn't
# Generate a list of all unique tokens by term frequency and document frequency
textstat_frequency(tweet_dfm) 
# feature is the token
# Are all of these useful words? What about symbols and punctuation?
# most of the top words are stop words and punctuation.
# it really depends on the research question.
# remove the words that are common and would be rather neutral (used by "good" and "bad people/tweets)
# Re-run the tokenization step with cleaning steps added
tweet_toks_clean <- tokens(tweet_corpus,
                           remove_punct = TRUE,
                           remove_numbers = TRUE,
                           remove_symbols = TRUE)
# Lower case the tokens
tweet_toks_clean <- tokens_tolower(tweet_toks_clean)
#Remove words from the standard stopwords list
my_stopwords <- stopwords("en")
my_stopwords

# Add words to the standard stopwords list
my_stopwords <- c("amp", "00pme", my_stopwords)
my_stopwords

# Remove stopwords after lowercasing. Why?
##### we do this because the standard stopwords list is LOWERCASED - so it is important for MATCHING.
tweet_toks_clean <- tokens_select(tweet_toks_clean, pattern = my_stopwords, selection = "remove")

# Recreate your dfm
tweet_dfm2 <- dfm(tweet_toks_clean)
# Regenerate your frequency list
textstat_frequency(tweet_dfm2)
# Remove single letters
# there are some letters like "w" just written by itself
tweet_toks_clean2 <- tokens_select(tweet_toks_clean, min_nchar=2L)
# Recreate your dfm
tweet_dfm3 <- dfm(tweet_toks_clean2)
# Regenerate your frequency list
textstat_frequency(tweet_dfm3)

#################### VISUALIZATIONS ####################
# Once you are happy with your word list, explore some visualizations: plots and word clouds
# Frequency plot
# Assign your textstat_frequency results to an object -> doing this allows you to create visualizations

tstat_ttweet <- textstat_frequency(tweet_dfm3)
tstat_ttweet

# Generate a plot -- top 20 words
ggplot(tstat_ttweet[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency")

# Word cloud
# Open a new plot window for better viewing
dev.new(width = 1000, height = 1000, unit = "px") 
#to make the word cloud look less distorted
# Generate the word cloud
textplot_wordcloud(tweet_dfm3, max_words = 100) # Top 100 words #word clouds are fun for slides, but not enough for a paper.

#################### NGRAMS ####################
# Move from single word tokens to ngrams
# Create phrases e.g. "social security"
### Do it before and after cleaning ###
# Generate n-grams of any length
# https://rdrr.io/cran/quanteda/man/tokens_ngrams.html

toks_ngram <- tokens_ngrams(tweet_toks_clean, n = 2)
# Look at the first few documents' tokens now
head(toks_ngram)
View(toks_ngram)
# Create a dfm
ngram_dfm <- dfm(toks_ngram)
textstat_frequency(ngram_dfm)

# Generate plots and word clouds using the same code as above
# Frequency plot
# Assign your textstat_frequency results to an object -> doing this allows you to create visualizations

tstat_ttweet_2ngram <- textstat_frequency(ngram_dfm)
tstat_ttweet_2ngram

# Generate a plot -- top 20 words
ggplot(tstat_ttweet_2ngram[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency")

# Word cloud
# Open a new plot window for better viewing
dev.new(width = 1000, height = 1000, unit = "px") #to make the word cloud look less distorted
# Generate the word cloud
textplot_wordcloud(ngram_dfm, max_words = 100) # Top 100 words #word clouds are fun for slides, but not enough for a paper.

#################### Sentiment Analysis ####################
# https://quanteda.io/reference/data_dictionary_LSD2015.html
# https://tutorials.quanteda.io/advanced-operations/targeted-dictionary-analysis/
# There are MANY ways to perform sentiment analysis.
# Use quanteda's built-in sentiment analysis dictionary: http://www.lexicoder.com/ data_dictionary_LSD2015
# Compound neg_negative and neg_positive tokens before creating a dfm object

toks_ngram_compound <- tokens_compound(toks_ngram, data_dictionary_LSD2015)
toks_ngram_sentiment <- dfm_lookup(dfm(toks_ngram_compound), data_dictionary_LSD2015)
toks_ngram_sentiment
toks_ngram[["text1"]]
View(toks_ngram_sentiment)
# Convert toks_ngram_sentiment to a dataframe
toks_ngram_sentiment <- convert(toks_ngram_sentiment, to = "data.frame")
View(toks_ngram_sentiment)
# What can you do with this?
# Compute a net score? (positive + neg_negative) - (negative + neg_positive)
colnames(toks_ngram_sentiment)

# Create a new column in the dataframe
toks_ngram_sentiment$net_sentiment <- ((toks_ngram_sentiment$positive + toks_ngram_sentiment$neg_negative) - (toks_ngram_sentiment$negative + toks_ngram_sentiment$neg_positive))
colnames(toks_ngram_sentiment)
View(toks_ngram_sentiment)
# Sort negative to positive
toks_ngram_sentiment <-
  toks_ngram_sentiment[order(toks_ngram_sentiment$net_sentiment),]
View(toks_ngram_sentiment)

### Repeat it for single word tokens ###
# Compound neg_negative and neg_positive tokens before creating a dfm object
tweet_toks_clean2_compound <- tokens_compound(tweet_toks_clean2, data_dictionary_LSD2015)
tweet_toks_clean2_sentiment <- dfm_lookup(dfm(tweet_toks_clean2_compound), data_dictionary_LSD2015)

tweet_toks_clean2_sentiment
tweet_toks_clean2[["text1"]]
View(tweet_toks_clean2_sentiment)
# Convert tweet_toks_clean2_sentiment to a dataframe
tweet_toks_clean2_sentiment <- convert(tweet_toks_clean2_sentiment, to = "data.frame")
View(tweet_toks_clean2_sentiment)
# What can you do with this?
# Compute a net score? (positive + neg_negative) - (negative + neg_positive)
colnames(tweet_toks_clean2_sentiment)

# Create a new column in the dataframe
tweet_toks_clean2_sentiment$net_sentiment <- ((tweet_toks_clean2_sentiment$positive + tweet_toks_clean2_sentiment$neg_negative) - (tweet_toks_clean2_sentiment$negative + tweet_toks_clean2_sentiment$neg_positive))
colnames(tweet_toks_clean2_sentiment)
View(tweet_toks_clean2_sentiment)
# Sort negative to positive
tweet_toks_clean2_sentiment <- tweet_toks_clean2_sentiment[order(tweet_toks_clean2_sentiment$net_sentiment),]

View(tweet_toks_clean2_sentiment)

# Shortcomings with dictionary-based sentiment analysis?
# Alternatives: machine learning classification
##### Tweets with the Top and Bottom Sentiments #####
View(tweet_text)

# Generate a plot -- lowest 20 sentiments -- NGRAM tokens
ggplot(toks_ngram_sentiment[1:20,], aes(x = reorder(doc_id, net_sentiment), y = net_sentiment)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "net_sentiment")
tweet_text #will display all the tweets text in the console
tweet_text[c(371, 337, 402,876), ] #to see specific tweets
# Generate a plot -- lowest 20 sentiments -- Word tokens
ggplot(tweet_toks_clean2_sentiment[1:20,], aes(x = reorder(doc_id, net_sentiment), y = net_sentiment)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "net_sentiment")
tweet_text[c(686, 630, 628, 431), ] #to see specific tweets

# Generate a plot -- highest 20 sentiments -- NGRAM tokens
ggplot(toks_ngram_sentiment[3176:3196,], aes(x = reorder(doc_id, net_sentiment), y =
                                               net_sentiment)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "net_sentiment")
# Generate a plot -- highest 20 sentiments -- Word tokens
ggplot(tweet_toks_clean2_sentiment[3176:3196,], aes(x = reorder(doc_id, net_sentiment), y =
                                                      net_sentiment)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "net_sentiment")
tweet_text[c(1047), ]

# Generate a plot -- all the sentiments -- NGRAM tokens
ggplot(toks_ngram_sentiment[1:3196,], aes(x = reorder(doc_id, net_sentiment), y =
                                            net_sentiment)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "net_sentiment")
# Generate a plot -- all the sentiments -- Word tokens
ggplot(tweet_toks_clean2_sentiment[1:3196,], aes(x = reorder(doc_id, net_sentiment), y =
                                                   net_sentiment)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "net_sentiment")

# Unigram (single word tokens) sentiment counts
negative_count_unigram <- sum(tweet_toks_clean2_sentiment$net_sentiment < 0)
neutral_count_unigram <- sum(tweet_toks_clean2_sentiment$net_sentiment == 0)
positive_count_unigram <- sum(tweet_toks_clean2_sentiment$net_sentiment > 0)

# Display counts
cat("Unigram Sentiment Counts:\n")
cat("Negative:", negative_count_unigram, "\n")
cat("Neutral:", neutral_count_unigram, "\n")
cat("Positive:", positive_count_unigram, "\n")

# Bigram (2-gram tokens) sentiment counts
negative_count_bigram <- sum(toks_ngram_sentiment$net_sentiment < 0)
neutral_count_bigram <- sum(toks_ngram_sentiment$net_sentiment == 0)
positive_count_bigram <- sum(toks_ngram_sentiment$net_sentiment > 0)

# Display counts
cat("Bigram Sentiment Counts:\n")
cat("Negative:", negative_count_bigram, "\n")
cat("Neutral:", neutral_count_bigram, "\n")
cat("Positive:", positive_count_bigram, "\n")


