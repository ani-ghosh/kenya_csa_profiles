# install.packages("pdftools")
# pacman::p_load_gh("trinker/entity")
library(pdftools)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(quanteda)
library(tm)
library(spacyr)
library(entity)

datadir <- "G:/My Drive/opportunities/1CG/OneCGIAR Initiatives First Batch/September 30 _ Final Submissions"

ff <- list.files(datadir, pattern = "pdf$", full.names = TRUE)
ff <- grep("SAPLING", ff, invert = TRUE, value = TRUE)
dd <- lapply(ff, pdftools::pdf_text)
dds <- unlist(dd)

dd <- lapply(ff, function(f){
  d <- pdf_data(f)
  d <- bind_rows(d)
})
dds <- bind_rows(dd)

cx <- dds$text[!tolower(dds$text) %in% tolower(get_stopwords()$word)]
cx <- str_replace_all(tolower(cx), "[[:punct:]]", "")
cx <- str_replace_all(tolower(cx), "[[:digit:]]+", "")
cx <- trimws(cx)
cx <- cx[nchar(cx) > 2 ]

cc <- data.frame(table(cx))
df <- cc[cc$Freq < 10000,]

set.seed(1234) # for reproducibility 
wordcloud(words = df$cx, freq = df$Freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# locations?
pp <- location_entity(cx)

cx %>%
  table() %>%
  with(wordcloud(word, n, max.words = 100))






docs <- Corpus(VectorSource(cx))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
