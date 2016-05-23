library(dplyr)
library(tm)
library(Amelia)
library(cluster)
library(proxy)

setwd("D:/Semester 2/Data Analytics/Project")

airline <- read.csv("airline.csv", header = TRUE, na.strings = c("","NA"))
airport <- read.csv("airport.csv", header = TRUE)
lounge <- read.csv("lounge.csv", header = TRUE)
seat <- read.csv("seat.csv", header = TRUE, quote="",row.names = NULL, stringsAsFactors = FALSE)
aairlines <- read.csv("name.csv")

airlines <- table(aairlines)
aairlines <- data.frame(airlines)
colnames(aairlines) <- c("airline_name","repeats")
merge <- left_join(airline,aairlines,by="airline_name")
responses <- airline %>%
  group_by(airline_name) %>%
  summarise(response = length(which(overall_rating!="NA")))
merged <- left_join(merge,responses,by="airline_name")
merged_airline <- merged %>%
  mutate(percentage = response/repeats)
aairline <- merged_airline %>%
  filter(percentage >= 0.9,repeats >= 100)
airline_rating <- aairline[,c(1,12:16,19)]

aairline_imputed <- amelia(airline_rating[,c(2:7)])
aairline_imp <- aairline_imputed$imputations[[1]]
abc <- data.frame(aairline[,c(1)])
aairline_rating <- bind_cols(abc,aairline_imp)

mmnormalize <- function(a){
  m <- max(a,na.rm = TRUE)
  n <- min(a,na.rm = TRUE)
  mmnormalized <- a
  mmnormalized <- (mmnormalized-n)/(m-n)
  return (mmnormalized)
}
aairline_normalized <- mmnormalize(aairline_rating[,c(2:7)])
airline_proc <- bind_cols(aairline_rating[,c(1)],aairline_normalized)
airline_proc <- airline_proc[complete.cases(airline_proc),]

colnames(airline_proc)[1] <- "airline_name"
airline_data <- airline_proc %>%
  group_by(airline_name) %>%
  summarise_each(funs(mean))


wss <- (nrow(airline_data)-1)*sum(apply(airline_data[,c(2:7)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(airline_data[,c(2:7)], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

km <- kmeans(airline_data[,c(2:7)],6)
clusplot(airline_data[,c(2:7)],
         km$cluster,
         color = TRUE,
         shade = TRUE,
         lines = 47,
         main = "k-means-Cluster")

names <- as.vector(airline_data$airline_name)
distance <- dist(airline_data[,c(2:7)], method = "euclidean")
hcluster <- hclust(distance,method = "ward.D")
plot(hcluster,labels =names, main = "Cluster Dendrogram- Airlines")
rect.hclust(hcluster,k=4,border = "blue")

names <- as.vector(airline_data$airline_name)
distance <- dist(airline_data[,c(2:7)], method = "cosine")
hcluster <- hclust(distance,method = "ward.D")
plot(hcluster,labels =names, main = "Cluster Dendrogram- Airlines - Cosine Distance")
rect.hclust(hcluster,k=6,border = "blue")

groups <- cutree(hcluster, k=6)
clustered_data <- cbind(airline_data[,c(1:7)], groups)
clustered <- left_join(aairline, clustered_data, by="airline_name")

write.csv(clustered, file = "joined.csv")

g1 <- subset(clustered, groups == 1)
g2 <- subset(clustered, groups == 2)
g3 <- subset(clustered, groups == 3)
g4 <- subset(clustered, groups == 4)
g5 <- subset(clustered, groups == 5)
g6 <- subset(clustered, groups == 6)

write.csv(g5, file = "underperforming.csv")

g7 <- subset(g5, g5$airline_name == "air-canada-rouge" & g5$recommended == 0)
write.csv(g7, file = "g7.csv")
g8 <- subset(g5, g5$airline_name == "sunwing-airlines" & g5$recommended == 0)
g9 <- subset(g5, g5$airline_name == "spirit-airlines" & g5$recommended == 0)
g10 <- subset(g5, g5$airline_name == "american-airlines" & g5$recommended == 0)
g11 <- subset(g5, g5$airline_name == "united-airlines" & g5$recommended == 0)
write.csv(g11,file = "g11.csv")

write.table(g1$content, "D:/Semester 2/Data Analytics/Project/comment1.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g2$content, "D:/Semester 2/Data Analytics/Project/comment2.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g3$content, "D:/Semester 2/Data Analytics/Project/comment3.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g4$content, "D:/Semester 2/Data Analytics/Project/comment4.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)

write.table(g1$content, "D:/Semester 2/Data Analytics/Project/tm1/comment1.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g2$content, "D:/Semester 2/Data Analytics/Project/tm2/comment2.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g3$content, "D:/Semester 2/Data Analytics/Project/tm3/comment3.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g4$content, "D:/Semester 2/Data Analytics/Project/tm4/comment4.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)

write.table(g7$content, "D:/Semester 2/Data Analytics/Project/tm/comment1.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g8$content, "D:/Semester 2/Data Analytics/Project/tm/comment2.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g9$content, "D:/Semester 2/Data Analytics/Project/tm/comment3.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g10$content, "D:/Semester 2/Data Analytics/Project/tm/comment4.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g11$content, "D:/Semester 2/Data Analytics/Project/tm/comment5.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)

write.table(g7$content, "D:/Semester 2/Data Analytics/Project/tm/g7/comment1.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g8$content, "D:/Semester 2/Data Analytics/Project/tm/g8/comment2.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g9$content, "D:/Semester 2/Data Analytics/Project/tm/g9/comment3.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g10$content, "D:/Semester 2/Data Analytics/Project/tm/g10/comment4.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(g11$content, "D:/Semester 2/Data Analytics/Project/tm/g11/comment5.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)

dirname <- file.path("D:/Semester 2/Data Analytics/Project/tm", "g7")
comment <- Corpus(DirSource(dirname, encoding = "UTF-8"))
meta(comment[[1]])

# The following steps pre-process the raw text documents.
# Remove punctuations and numbers because they are generally uninformative.
comment <- tm_map(comment, removePunctuation)
comment <- tm_map(comment, removeNumbers)
# Convert all words to lowercase.
comment <- tm_map(comment, content_transformer(tolower))
# Remove stopwords such as "a", "the", etc.
comment <- tm_map(comment, removeWords, stopwords("english"))

comment <- tm_map(comment, removeWords, c("flight","airline","plane")) 

# Use the SnowballC package to do stemming.
library(SnowballC)
comment <- tm_map(comment, stemDocument)
# Remove excess white spaces between words.
comment <- tm_map(comment, stripWhitespace)
# Inspect the first document to see what it looks like.
comment[["comment1.txt"]]$content
# Convert all documents to a term frequency matrix.
tfm <- DocumentTermMatrix(comment)
# We can check the dimension of this matrix by calling dim()
print(dim(tfm))

dtm <- TermDocumentMatrix(comment)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#findAssocs only works when there's more than one document
findAssocs(dtm, terms = "uncomfortable", corlimit = 0.95)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most Frequent Words",
        ylab = "Word Frequencies")

# Use topicmodels package to conduct LDA analysis.
library(topicmodels)
results <- LDA(tfm, k = 5, method = "Gibbs")
# Obtain the top five words (i.e., the 5 most probable words) for each topic.
Terms <- terms(results, 20)
# Obtain the most likely topic assignment for each document.
Topic <- topics(results, 5)
# Get the posterior probability for each document over each topic
posterior <- posterior(results)[[2]]

#look at the posterior topic distribution for the first document and plot it visually
posterior(results)[[2]][1,]
barplot(posterior(results)[[2]][1,])

#examine the main topic for document 1
Terms[,2]

# Calculate the entropy for each document to quantify keyword ambiguity
CalcEntropy <- function(document) {
  entropy = 0
  for (i in 1:length(document)) {
    entropy = entropy - document[i]*log(document[i])
  }
  return(entropy)
}

Entropy <- apply(posterior, 1, CalcEntropy) #posterior is matrix, 1 indicates rows
newKeywordConstruct <- data.frame(Entropy, Topic)

library(wordcloud)
makeWordCloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE)  
}

# wordcloud for cluster
makeWordCloud(g1[["content"]])
makeWordCloud(g2[["content"]])
makeWordCloud(g3[["content"]])
makeWordCloud(g4[["content"]])
makeWordCloud(g5[["content"]])
makeWordCloud(g6[["content"]])

# wordcloud for airline
makeWordCloud(g7[["content"]])
makeWordCloud(g8[["content"]])
makeWordCloud(g9[["content"]])
makeWordCloud(g10[["content"]])
makeWordCloud(g11[["content"]])
