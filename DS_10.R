# The text mining package
library(tm)

## Load Data
blogs_raw <- readLines(connect <- file("R/en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(connect)

news_raw <- readLines(connect <- file("R/en_US.news.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(connect)

twitter_raw <- readLines(connect <- file("R/en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(connect)


## merge into a single corpus a smaller set (10%) for training
## set seed to always use the same sample across runs
merged_train <- paste(sample(blogs_raw, size = length(blogs_raw) / 10), 
                      sample(news_raw, size = length(news_raw) / 10), 
                      sample(twitter_raw, size = length(twitter_raw) / 10))


# Use tm package to clean the data instead of using RE, & convert to lowercase
# corpus = a collection of documents
# you can type "corpus" to see how many documents it contains
corpus <- VCorpus(VectorSource(merged_train))


corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <-tm_map(corpus, tolower)

# remove profanity
profanity <- read.csv("http://www.bannedwordlist.com/lists/swearWords.txt", header=FALSE)
corpus <- tm_map(corpus, removeWords, profanity[,1]) 

# Do stemming to collapse different forms of similar words
# Requires the SnowballC package
# Maybe rather crude but I suppose the benefits outweigh the disadvantages
# You can see the sample of the words after stemming 
# Some of the words, the ending has been chopped off 
# >writeLines(as.character(corpus[[1]]))
corpus <- tm_map(corpus, stemDocument)


# Next : Creation of the term document matrix (TDM)
# A matrix that lists all occurrences of words in the corpus, by document.
