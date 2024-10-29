# Twitter Sentiment Analysis on Donald Trump Tweets

This project performs sentiment analysis on tweets from former U.S. President Donald Trump, utilizing R and the `quanteda` package. The project aims to uncover patterns in sentiment, identify frequently used terms, and visualize insights using frequency plots and word clouds. The code includes data cleaning, tokenization, and a dictionary-based approach to classify sentiment as positive, negative, or neutral.

## Project Overview

The project involves the following steps:
1. **Data Import**: Load tweet data from a CSV file.
2. **Data Cleaning**: Remove unnecessary elements (e.g., punctuation, symbols, stopwords).
3. **Tokenization**: Break down tweet text into individual words and bigrams (two-word combinations).
4. **Sentiment Analysis**: Use a dictionary-based approach to assign positive, negative, and neutral sentiments to each tweet.
5. **Visualizations**: Create frequency plots and word clouds for data visualization.

## Requirements

Ensure that the following packages are installed:
- `quanteda`
- `quanteda.textstats`
- `quanteda.textplots`
- `readtext`
- `ggplot2`
- `text2vec`
- `dplyr`
- `tidytext`

You can install these packages using the following R code:
```R
install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
install.packages("readtext")
install.packages("ggplot2")
install.packages("text2vec")
install.packages("dplyr")
install.packages("tidytext")
