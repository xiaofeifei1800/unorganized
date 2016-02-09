library(RSQLite)
library(tm)
library(wordcloud)
library(igraph)
setwd("I:/R Data")
db = dbConnect(SQLite(), "amazon.sqlite")

dbListTables(db)

reviews <- dbGetQuery(db, "
SELECT *
FROM Reviews
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

corpus = Corpus(VectorSource(tolower(reviews$Text[1:100])))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  
tmx = t(frequencies)
mdtm = as.matrix(tmx)  
mdtm[mdtm >= 1] = 1
termMatrix = mdtm %*% t(mdtm)

g = graph.adjacency(termMatrix, weighted = T, mode = "undirected")
g = simplify(g)

V(g)$label = V(g)$name
V(g)$degree = degree(g)

set.seed(123)
layout1 = layout.fruchterman.reingold(g)
tkplot(g, layout = layout1)

#########################
test = mdtm[rowSums(mdtm)>=10,]
nrow(test)


termMatrix = test %*% t(test)

g = graph.adjacency(termMatrix, weighted = T, mode = "undirected")
g = simplify(g)

V(g)$label = V(g)$name
V(g)$degree = degree(g)

set.seed(123)
layout1 = layout.fruchterman.reingold(g)
tkplot(g, layout = layout1)

