library(rJava)
library(Rwordseg)
library(wordcloud)
library(tm)
library(igraph)

setwd("I:/R Data")

# get the text
text = readLines("data mining.txt")
add_word = readLines("dict.txt")
stopwords = readLines("stopword.txt")
insertWords(add_word)
stopWord = function(strs) 
{
  theflags <- strs %in% stopwords
  strs[!theflags]
}

# clean the text
text = lapply(text, function(x) unlist(segmentCN(x)))
text = unlist(text)
text = stopWord(text)

# wordcloud

name = names(table(text))
freq = as.numeric(table(text))

word_data = data.frame(word = name, freq = freq)
word_data = word_data[word_data$freq>2,]
wordcloud(word_data$word, word_data$freq, scale = c(3,0.5), max.words = 30, random.order = F)


# networking graph
test = Corpus(VectorSource(text),readerControl = list(reader = reader(text), language = "cn"))
test = Corpus(DirSource(directory = "I:/R Data/text mining", encoding = "UTF-8"), readerControl = list(reader = readPlain, language = "cn"))
inspect(test)
dtmatrix = DocumentTermMatrix(test)
test = Corpus(VectorSource(text), readerControl = list(reader = readPlain, language = "cn",tolower = TRUE))
temp = tm_map(test, removePunctuation)
temp = tm_map(test, removeNumbers)
temp = tm_map(test, stripWhitespace)


d = DocumentTermMatrix(test)
td = t(d)
mtd = as.matrix(td)
mtd[mtd >= 1] = 1
termMatrix = mtd %*% t(mtd)

g = graph.adjacency(termMatrix, weighted = T, mode = 'directed')
g = simplify(g)
V(g)$label = V(g)$name
V(g)$degree = degree(g)
set.seed(22)
layout1 = layout.fruchterman.reingold(g)
tkplot(V(g)$name, layout = layout1)
plot.igraph(layout1)
layout1
plot(g, layout = layout1, vertex.size = 1, asp = F)

g1 = graph.data.frame(layout1[1:50,],directed=T)
plot(g1,layout=layout.kamada.kawai,vertex.shape='circle',vertex.size = 1,edge.arrow.size=0,vertex.label = V(g)$name[1:50])
