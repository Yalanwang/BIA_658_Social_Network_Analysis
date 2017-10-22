## homework 6


install.packages('tidytext')

library(readr)
library(dplyr)
library(tidytext)
library(igraph)

song_lyrics = read_csv("F:/2017 fall third semester/BIA 658/week7/cooccurrence network/data/billboard_lyrics_1964-2015.csv")
names(song_lyrics)# the coloum names of the song_lyrics
states = read_csv("F:/2017 fall third semester/BIA 658/week7/cooccurrence network/data/nst-est2016-alldata.csv")[-(1:5), ]
state_names = tolower(states$NAME)

# create one observation per artist by concatenating all his/her songs
artist_lyrics = song_lyrics %>% group_by(Artist) %>% summarise(Lyrics= paste(Lyrics, collapse=" "))

# unnest_tokens() restructure the lyrics as one-token-per-row format
artist_unigram = artist_lyrics %>% unnest_tokens(word, Lyrics, to_lower = TRUE)
artist_bigram = artist_lyrics %>% unnest_tokens(ngram, Lyrics, to_lower = TRUE, token = "ngrams", n = 2)
artist_bigram = artist_bigram %>% rename(word = ngram)
artist_tokens = rbind(artist_unigram, artist_bigram)
rm(artist_bigram)
artist_tokens = artist_tokens %>% filter(word %in% state_names)
state_counts = artist_tokens %>% group_by(word) %>% summarise(count = n())
barplot(table(artist_tokens$word))

create_adj_list = function(df){
  # Input: a dataframe with a column "word"
  # Output: all possible 2-combinations (sorted) of the unique word
  unique_tokens = unique(df$word)
  adj_list = data.frame()
  if(length(unique_tokens) >= 2) {
    all_combins = t(combn(unique_tokens, 2))
    all_combins = t(apply(all_combins, 1, sort))
    adj_list = data.frame(all_combins, stringsAsFactors = FALSE)
  }
  return(adj_list)
}

# create unweighted and weighted adjacency lists
adj_list = artist_tokens %>% group_by(Artist) %>% do(create_adj_list(.))
adj_list_weighted = data.frame(adj_list) %>% group_by(X1, X2) %>% summarise(weight = n())

# plot the unweighted graph
state_graph = graph_from_data_frame(adj_list[, c("X1", "X2")], directed = FALSE)
plot(state_graph, layout=layout.fruchterman.reingold)
plot(simplify(state_graph))
# plot the weighted graph
state_graph_weighted = graph_from_data_frame(adj_list_weighted[, c("X1", "X2")], directed = FALSE)
E(state_graph_weighted)$weight = adj_list_weighted$weight
plot(state_graph_weighted, layout=layout.fruchterman.reingold, edge.width=E(state_graph_weighted)$weight)

##  Q1 Plot the unweighted graph
plot(state_graph, layout = layout.circle, edge.arrow.size=.2, edge.color="black",
     vertex.color="purple",
     vertex.label=V(state_graph)$states_names, vertex.label.color="black")

## Q1 plot the weighted graph
plot(state_graph_weighted, layout = layout.circle, edge.arrow.size=.2, edge.color="black",
     vertex.color="purple", edge.width=E(state_graph_weighted)$weight,
     vertex.label=V(state_graph)$states_names, vertex.label.color="black")



## Q2 Visualize the lyrics-state network 
## so that the size of the nodes correspond to the population of the states


state_names_in_graph = data.frame(state = as.vector(V(state_graph_weighted)$name), stringsAsFactors = F)

state_names = filter(states, tolower(NAME) %in% state_names_in_graph$state)

state_names_size=subset(states,tolower(states$NAME) %in% state_names_in_graph$state)$POPESTIMATE2016/5000000


plot(state_graph_weighted, layout = layout.fruchterman.reingold, edge.color="orange",
     edge.width=E(state_graph_weighted)$weight,
     vertex.color="red", vertex.frame.color="black",
     vertex.size= as.matrix(state_names_size, state_name),
     vertex.label=V(state_graph)$states_names, vertex.label.color="black")




## Q3. For the sentiment analysis of movie reviews, use the tf-idf weighted 
## document-term matrix (instead of the term-frequency weight I showed in 
## the demonstration) and a support vector machine classifier. 
## Do you get better misclassification rate on the test set?

install.packages(c('tm', 'SnowballC', 'wordcloud'))
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)

reviews = read.csv("F:/2017 fall third semester/BIA 658/week7/sentiment_analysis (1)/data/movie_reviews.csv", stringsAsFactors = F, row.names = 1)

## stringsAsFactor = f: to make the data you put in are not all the categorical number.

# A collection of text documents is called a Corpus
review_corpus = Corpus(VectorSource(reviews$content))
# View content of the first review
review_corpus[[1]]
review_corpus[1]
# Change to lower case, not necessary here
review_corpus = tm_map(review_corpus, content_transformer(tolower))
# Remove numbers
review_corpus = tm_map(review_corpus, removeNumbers)
# Remove punctuation marks and stopwords
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("duh", "whatever", stopwords("english")))
## remove thses words which are not meaningful
# Remove extra whitespaces
review_corpus =  tm_map(review_corpus, stripWhitespace)

review_corpus[[1]]$content

# tf-idf(term frequency-inverse document frequency) instead of the frequencies of the term as entries, 
# tf-idf measures the relative importance of a word to a document
## idf

review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf
# The first 10 document
inspect(review_dtm_tfidf[1:10,1:20])


# Precompiled list of words with positive and negative meanings
# Source: http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
neg_words = read.table("F:/2017 fall third semester/BIA 658/week7/sentiment_analysis (1)/data/negative-words.txt", header = F, stringsAsFactors = F)[, 1]
pos_words = read.table("F:/2017 fall third semester/BIA 658/week7/sentiment_analysis (1)/data/positive-words.txt", header = F, stringsAsFactors = F)[, 1]

# neg, pos contain the number of positive and negative words in each document
# Note: newer version of tm package updated the syntax, now tm_term_score takes a document-term matrix as input
reviews$neg = tm_term_score(DocumentTermMatrix(review_corpus), neg_words)
reviews$pos = tm_term_score(DocumentTermMatrix(review_corpus), pos_words)

# remove the actual texual content for statistical models
reviews$content = NULL
# construct the dataset for models
reviews = cbind(reviews, as.matrix(review_dtm_tfidf))

## cbind:
reviews$polarity = as.factor(reviews$polarity)
## y label

# Split to testing and training set
## train 80%, 20% test
id_train = sample(nrow(reviews),nrow(reviews)*0.80)
reviews.train = reviews[id_train,]
reviews.test = reviews[-id_train,]


# support vector machine (SVM)

install.packages("e1071")
library(e1071) # for Support Vector Machine

reviews.svm = svm(polarity~., data = reviews.train);
## support vector machine
## (y ; ". "means: every other x variables)

# Performance in the test set

pred.svm = predict(reviews.svm, reviews.test)
table(reviews.test$polarity,pred.svm,dnn=c("Observed","Predicted"))
mean(ifelse(reviews.test$polarity != pred.svm, 1, 0))












