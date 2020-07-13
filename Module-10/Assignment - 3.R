# QUESTION 1.1

library(tm)

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

corpus = VCorpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

str(allTweets)

# QUESTION 2.1

install.packages("wordcloud")

library(wordcloud)

colnames(allTweets)

# QUESTION 2.2

colSums(allTweets)

# QUESTION 2.3

wordcloud(colnames(allTweets), colSums(allTweets))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))

# QUESTION 2.4

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

wordcloud(colnames(allTweets), colSums(allTweets))

For a much smaller plot, we could have used:

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

# QUESTION 3.1

negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

# QUESTION 4.1

install.packages("RColorBrewer")

library(RColorBrewer)

# QUESTION 4.2

display.brewer.all()

# QUESTION 4.3

brewer.pal(9, "Blues")[-1:-4]

brewer.pal(9, "Blues")[5:9]