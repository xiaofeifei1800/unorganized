library(rJava)
library(Rwordseg)
library(XML)
library(wordcloud)
setwd("I:/R Data")

############################### web scraping###########################
## first page
u = "http://movie.douban.com/subject/11589036/comments"
doc = htmlParse(u)

# get the comment
comment = function(html)
{
  comment = xpathSApply(html, "//div[@class='comment']//p", xmlValue)
  clean_com = lapply(comment, function(x) gsub(" ", "\\1",x))
  clean_com = lapply(clean_com, function(x) gsub("\n", "\\1",x))
  return(clean_com)
}

# find the next page
next_page = function(html)
{  
  link = unlist(getNodeSet(html, "//div[@id='paginator']//a[@class = 'next']/@href"))
  nxt = paste0("http://movie.douban.com/subject/11589036/comments",link)
  nxt = htmlParse(nxt)
  return(nxt)
}

# start web scraping
page = list()
for(i in 1:6)
{
  page[i] = list(comment(doc))
  next_p = next_page(doc)
  doc = next_p
}

######################### text segmentation ###############################
page1 = unlist(page)

# add stopwords and invallid words
stopwords = readLines("stopword.txt")

invalid.words = c ("��Ӱ", "��Ա", "����", "����", "����", "һ��", "û��",
                   "����", "����", "ӰƬ", "����", "��Ϊ", "ʲô", "�Լ�",
                   "���", "����", "���", "����", "����", "Ϊ��", "һ��",
                   "�ⲿ", "Ƭ��", "��ʵ", "��Ȼ", "ʱ��", "����", "�Ѿ�",
                   "����", "֪��", "��Щ", "һ��", "���", "����", "����",
                   "��ʼ", "��ô", "�Ǹ�", "����", "���", "���", "��β",
                   "���", "����", "����", "�е�", "����", "֮��", "��ô",
                   "һ��", "����", "��Ʒ", "�ط�", "��Ƭ", "һЩ", "һ��",
                   "֮ǰ", "����", "��Ȼ", "��ô", "��ɫ", "��ô", "����",
                   "����", "��Ϊ", "�Ե�", "����", "����", "����", "��Щ")
# add word dict
insertWords(c("����", "ˮī��","�й���","����")) 
installDict("hah.scel", "dict1", dicttype = "scel", load = T)

# filter function
filterWord = function(strs) 
{
  theflags = strs %in% invalid.words
  strs[!theflags]
}

# stop function
stopWord = function(strs) 
{
  theflags <- strs %in% stopwords
  strs[!theflags]
}

# start segment
comment.words = lapply(page1, segmentCN)
comment.words = lapply(comment.words, filterWord)
comment.words = lapply(comment.words, stopWord)

saveRDS(comment.words, file = "douban_comment.rds")

# count the freq

comment.words = readRDS("douban_comment.rds")
freq <- sort(table(unlist(comment.words)), decreasing = T)
comment.words.name <- names(freq)
comment.words.freq <- as.numeric(freq)
wordsData <- data.frame(words =comment.words.name, freq = comment.words.freq) 

# plot wordcould
library(wordcloud) 
top20 <- wordsData[wordsData$freq>15,]   
colors=brewer.pal(8,"Dark2")
wordcloud(top20$words,top20$freq,c(3,.3),colors=colors,random.order=F) 

####################### text cluster #############
#kmeans()

library(tm)
corpus  <-Corpus(VectorSource(comment.words))
comment_dtm<- DocumentTermMatrix(corpus,control=list(wordLengths=c(1,Inf))) 
comment.matrix <- as.matrix(comment_dtm) 
head(comment.matrix)

k <- 5  
kmeansRes <- kmeans(comment.matrix,k) #k�Ǿ�����  
mode(kmeansRes)
names(kmeansRes)  
head(kmeansRes$cluster,10)  
kmeansRes$size

hlzj.kmeansRes <- list(content=page1,type=kmeansRes$cluster)  
write.csv(hlzj.kmeansRes,"hlzj_kmeansRes.csv")  
fix(hlzj.kmeansRes)  


#hclust()
d <- dist(comment.matrix,method="euclidean")  
hclustRes <- hclust(d,method="complete")  
hclustRes.type <- cutree(hclustRes,k=5) #����������5�����  
length(hclustRes.type)  
hclustRes.type[1:10]  

hlzj.hclustRes <- list(content=page1,type=hclustRes.type)  
hlzj.hclustRes <- as.data.frame(hlzj.hclustRes)  
fix(hlzj.hclustRes) 

#specc()
stringkern <-stringdot(type="string")  
kernelRes <-specc(hlzj.matrix,centers=5,kernel=stringkern)  
mode(kernelRes)  

length(kernelRes)  
kernelRes[1:10]  
table(kernelRes)  
temp <-t(kernelRes) #����ת��  
hlzj.kernelRes<-list(cotent=hlzj,type=temp[1:1639]  
hlzj.kernelRes <-as.data.frame(hlzj.kernelRes)  
fix(hlzj. kernelRes) 