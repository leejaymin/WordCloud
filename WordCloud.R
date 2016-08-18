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

my_pdf <- readPDF(control=list(text="-layout"))(elem=list(uri="./papers/allPapers.pdf"), language="en")


text_raw <- my_pdf$content

text_raw <- text_raw[-c(1:2)] # remove title
text_raw <- text_raw[-c(4:12)] # remove title
text_raw <- text_raw[-c(176:201)] #remove list of references

text_corpus <- Corpus(VectorSource(text_raw))
corpus_clean <- tm_map(text_corpus, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
my_stopwords <- c("e-ii","can","due","will", # additional user-defined stop words
                  "fig","figs","figure","online", # stop words related to figure captions
                  "work", "paper") # stop words related to the journal
corpus_clean <- tm_map(corpus_clean, removeWords, my_stopwords)

# punctuation
corpus_clean <- tm_map(corpus_clean, removePunctuation)
wordcloud(corpus_clean, min.freq=25, max.words=500, random.order=FALSE, scale= c(5, 0.1), colors=brewer.pal(8,"Dark2"))


