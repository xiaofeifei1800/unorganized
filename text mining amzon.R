library(RSQLite)
library(tm)
library(wordcloud)
setwd("I:/R Data")
db = dbConnect(SQLite(), "amazon.sqlite")
dbListTables(db)


reviews <- dbGetQuery(db, "SELECT * FROM Reviews
                           LIMIT 10000")


make_word_cloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[400]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE)  
}


make_word_cloud(reviews$Text)

# igraphe
corpus = Corpus(VectorSource(tolower(reviews$Text[1:100])))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
  
frequencies = DocumentTermMatrix(corpus)
          
findFreqTerms(frequencies, 1000)

data = as.data.frame(inspect(frequencies))
data.scale = scale(data)
d = dist(data.scale, method = "euclidean")
fit = hclust(d, method = "ward")
plot(fit)
  

