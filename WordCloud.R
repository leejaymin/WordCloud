#Loading the required libraries and setting initial parameters#

setwd("E:/Data Analysis With R/WordCloud/WordCloud")
needed_libs <- c("tm", "wordcloud")
install_missing <- function(lib){
  install.packages(lib,repos="https://cran.r-project.org/", dependencies = TRUE); 
  library(lib, character.only = TRUE)}
for (lib in needed_libs) 
  tryCatch(library(lib, character.only=TRUE), error = function(e) install_missing(lib))

set.seed(8)


# Reading the text in the PDF file

#my_pdf <- readPDF(control=list(text="-layout"))(elem=list(uri="./papers/allPapers.pdf"), language="en")
my_pdf <- readPDF(control=list(text="-layout"))(elem=list(uri="./papers/TCE.pdf"), language="en")

text_raw <- my_pdf$content

text_raw <- text_raw[-c(1:2)] # remove title
text_raw <- text_raw[-c(4:12)] # remove title
text_raw <- text_raw[-c(176:201)] #remove list of references

# Clean up
text_corpus <- Corpus(VectorSource(text_raw))
corpus_clean <- tm_map(text_corpus, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
my_stopwords <- c("e-ii","can","due","will", # additional user-defined stop words
                  "fig","figs","figure","online", # stop words related to figure captions
                  "work", "paper") # stop words related to the journal
corpus_clean <- tm_map(corpus_clean, removeWords, my_stopwords)
corpus_clean <- tm_map(corpus_clean, removePunctuation)

# construct word cloud
wordcloud(corpus_clean, min.freq=25, max.words=500, random.order=FALSE, scale= c(5, 0.1), colors=brewer.pal(8,"Dark2"))



# frequency analysis
library(SnowballC)   
docs <- tm_map(corpus_clean, stemDocument) 
docs <- tm_map(docs, stripWhitespace)  
docs <- tm_map(docs, PlainTextDocument)   
dtm <- DocumentTermMatrix(docs) 

tdm <- TermDocumentMatrix(docs)   
tdm   

# Explore your data
freq <- colSums(as.matrix(dtm))   
length(freq)  

ord <- order(freq)   

m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv")   

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)  
freq[head(ord)]  
freq[tail(ord)]   
head(table(freq), 20)   

freq <- colSums(as.matrix(dtms))   
freq  
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) 
findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data.

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

library(ggplot2)   
p <- ggplot(subset(wf, freq>150), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) # specifying a correlation limit of 0.98   
findAssocs(dtms, "contrast", corlimit=0.90) # specifying a correlation limit of 0.95   

