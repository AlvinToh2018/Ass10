library(quanteda)
library(dplyr)
#library(tidyr)


## Load Data
blogs_raw <- readLines(connect <- file("R/en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(connect)
blogs_raw <- iconv(blogs_raw, from = "UTF-8", to = "ascii", sub = "")
blogs_raw <- gsub("_","",blogs_raw)  # remove '_' as it is used by quanteda later as a word separator

news_raw <- readLines(connect <- file("R/en_US.news.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(connect)
news_raw <- iconv(news_raw, from = "UTF-8", to = "ascii", sub = "")
news_raw <- gsub("_","",news_raw)

twitter_raw <- readLines(connect <- file("R/en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(connect)
twitter_raw <- iconv(twitter_raw, from = "UTF-8", to = "ascii", sub = "")
twitter_raw <- gsub("_","",twitter_raw)

# list of profanity for removal
profanity <- read.csv("http://www.bannedwordlist.com/lists/swearWords.txt", header=FALSE)


set.seed(1)

# merge into a single corpus a smaller set (10%) for training
# set seed to always use the same sample across runs
merged_train <- paste(sample(blogs_raw, size = length(blogs_raw) / 1.5), 
                      sample(news_raw, size = length(news_raw) / 1.5), 
                      sample(twitter_raw, size = length(twitter_raw) / 1.5))


# merged_train <- paste(blogs_raw, news_raw, twitter_raw)




# Change to quanteda corpus format
train_corpus <- corpus(merged_train)
# summary(train_corpus, 5)

# do tokens, remove unwanted tokens and form ngrams

toks <- train_corpus %>% 
  tokens(remove_numbers = TRUE,
         remove_punct = TRUE, 
         remove_symbols = TRUE,
         remove_separators = TRUE,
         remove_twitter = TRUE,
         remove_hyphens = TRUE,
         remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords('en')) %>%
  tokens_remove(pattern = profanity)
  

unigram <- toks %>% tokens_ngrams(n=1)
bigram <- toks %>% tokens_ngrams(n=2)
trigram <- toks %>% tokens_ngrams(n=3)


# create the freq tables for lookup of suggestion
# as the freq tables are loaded into Shiny for word lookup, 
# try to minimise the size of the lookup / frequency table
uni.freq <- unigram %>% 
  dfm() %>% 
  textstat_frequency()

bi.freq <- bigram %>% 
  dfm() %>% 
  textstat_frequency()

tri.freq <- trigram %>% 
  dfm() %>% 
  textstat_frequency()

# remove the cols not needed from frequency table to save memory space when loaded into Shiny
# keep only the 1st 2 cols - 1) feature (ngram), 2) frequency
uni.freq[3:5]  <- list(NULL)
bi.freq[3:5] <- list(NULL)
tri.freq[3:5] <- list(NULL)

# split the bigram feature into 2 separate 'tokens' 
# to compare against the input and output the last word as suggested word
# strsplit returns a list, so need to unlist, create matrix and add to the bi.freq dataframe
bi.split = matrix(unlist(strsplit(bi.freq$feature,"_")), ncol = 2, byrow = TRUE)

# add these as cols back into bi.freq
# added col names are "1" & "2"
bi.freq <- cbind(bi.freq, bi.split)
# after spliting, remove original feature, and frequency count to save memory space
bi.freq[1:2] <- list(NULL)


# do the same for the trigram
tri.split = matrix(unlist(strsplit(tri.freq$feature,"_")), ncol = 3, byrow = TRUE)
tri.freq <- cbind(tri.freq, tri.split)
tri.freq[1:2] <- list(NULL)


# do the same for unigram (remove frequency)
uni.freq[2] <- list(NULL)

#
# Algo takes 1st match so no need to have more than 1 row have the same n-1 words as these will
# not be reached and thus taking up memory without adding to prediction accuracy
# go thru the freq_table and remove such rows





    

# same the computed objects so that we can load the results in shiny app, no need to recompute 
write.csv(tri.freq, "R/trigram_freq_table.csv")
write.csv(bi.freq, "R/bigram_freq_table.csv")
write.csv(uni.freq, "R/unigram_freq_table.csv")


#####################################

tri.freq <- read.csv("R/trigram_freq_table.csv", stringsAsFactors=FALSE)
bi.freq <- read.csv("R/bigram_freq_table.csv", stringsAsFactors=FALSE)
uni.freq <- read.csv("R/unigram_freq_table.csv", stringsAsFactors=FALSE)

tri.freq[1]  <- list(NULL)  # remove the row number - save memory and more intuitive decrement/increment in algo
bi.freq[1]  <- list(NULL)
uni.freq[1] <- list(NULL)


new_bi <- bi.freq[1,]

for (i in 2:nrow(bi.freq)) {
  same = FALSE
  for (j in 1:nrow(new_bi))  {
    if (all(bi.freq[i, 1:1] == new_bi[j,1:1])) {
        same = TRUE
        break
   }
  }   
  if (isFALSE(same)){
    # did not find similar terms
    # add that row to the new_tri  
    new_bi <- rbind(new_bi,bi.freq[i,])
  } 
}


new_tri <- tri.freq[1,]

for (i in 2:nrow(tri.freq)) {
  same = FALSE
  for (j in 1:nrow(new_tri))  {
    if (all(tri.freq[i, 1:2] == new_tri[j,1:2])) {
      same = TRUE
      break
    }
  }   
  if (isFALSE(same)){
    # did not find similar terms
    # add that row to the new_tri  
    new_tri <- rbind(new_tri,tri.freq[i,])
  } 
}


# Process the input word
input <- "country singer"

input_words <- strsplit(tolower(input),"\\s+")[[1]]
# input_words
wc=length(input_words)

done = FALSE

while (wc >= 0) {

  if (done) {
    print("Done at top of loop. break")
    break
  }
  
  if (wc == 2) {
    freq_table = tri.freq
    print("freq_table is tri")
  }
  else if (wc ==1) {
      freq_table = bi.freq
      print("freq_table is bi")
    } 
    else {
      uni.freq[1,1]   # output the most common unigram
      print ("uni printed")
      done <- TRUE
    }
  
  
  if (isFALSE(done)) {
    for (i in 1:nrow(freq_table)) {

      if (all(input_words == freq_table[i,1:wc])) {
        output <- freq_table[i,wc+1]
        print("matched to freq table word")
        print(i)
        print(wc+1)
        print(output)
        ###############
        # while matched and less than 10
        # display n+1 word (display 1st ten or less next words)
        done <- TRUE
        break # break from for loop
        }     
    }
  }

  wc <- wc - 1
  input_words <- input_words[-1]  # remove the 1st word from input and try again - stupid backoff mtd
  
}

