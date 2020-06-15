#########################################################################################
#	MIT License
#
#	Copyright (c) [2020] [Gonzalo Daniel Garcia: https://github.com/gonzalofichero/]
#
#	Permission is hereby granted, free of charge, to any person obtaining a copy
#	of this software and associated documentation files (the "Software"), to deal
#	in the Software without restriction, including without limitation the rights
#	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#	copies of the Software, and to permit persons to whom the Software is
#	furnished to do so, subject to the following conditions:
#
#	The above copyright notice and this permission notice shall be included in all
#	copies or substantial portions of the Software.
#
#	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#	SOFTWARE.
##########################################################################################

# Title of code: "Mala_Educacion_Narrativas_Banco_Mundial_desde_enfoque_lexico-relacional.R"
# DOI (master): https://doi.org/10.5281/zenodo.3836126


# Following this methodology of analysis:
# https://rpubs.com/williamsurles/316682


# Load the necessary libraries for Text Mining
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(rJava)
library(qdap)
library(qdapDictionaries)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(readr)
library(lubridate)


############################################
# IMPORTING DATA

# Loading all txt files from BM
data_path <- "C:/Users/Gonzalo/Desktop/Master UAB/TFM/BM_scraping/Data/Data_Edu/all_together/"
files <- dir(data_path, pattern="*.txt")
bm <- files %>%
          map(~ read_file(file.path(data_path,.))) %>%
              reduce(rbind) %>%
                data.frame()

# Loading 2010-2014 txts
data_path_14 <- "C:/Users/Gonzalo/Desktop/Master UAB/TFM/BM_scraping/Data/Data_Edu/all_2010_2014/"
files_14 <- dir(data_path_14, pattern="*.txt")
bm_14 <- files_14 %>%
  map(~ read_file(file.path(data_path_14,.))) %>%
  reduce(rbind) %>%
  data.frame()

# Loading 2015-2020 txts
data_path_20 <- "C:/Users/Gonzalo/Desktop/Master UAB/TFM/BM_scraping/Data/Data_Edu/all_2015_2020/"
files_20 <- dir(data_path_20, pattern="*.txt")
bm_20 <- files_20 %>%
  map(~ read_file(file.path(data_path_20,.))) %>%
  reduce(rbind) %>%
  data.frame()


############################################
# CREATING CORPUS IN R


#bm_corpus <- VCorpus(DirSource("C:/Users/Gonzalo/Desktop/Master UAB/TFM/BM_scraping/Data/", pattern="*.txt",encoding = 'UTF-8'), readerControl = list(language="english"))
#bm_titles <- read.csv("WB_Titles_Education.csv", header = T, sep="|", encoding = 'UTF-8')

bm_corpus <- VCorpus(VectorSource(bm$.), readerControl = list(language="english", readPlain, load=TRUE))
bm_corpus_14 <- VCorpus(VectorSource(bm_14$.), readerControl = list(language="english", readPlain, load=TRUE))
bm_corpus_20 <- VCorpus(VectorSource(bm_20$.), readerControl = list(language="english", readPlain, load=TRUE))


##################################
# WORKING THE EDUCATION TITLES
glimpse(bm_titles)

bm_titles$date_2 <- mdy(bm_titles$Date)

bm_titles$year <- year(bm_titles$date_2)

# Ploting the qty of blogs by year
bm_titles %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(x=as.factor(year))) + geom_bar(aes(weight=count)) +
  labs(title = "Artículos de Blog por año", x = "Año", y = "")




################################################
# Let's clean the data!

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

# Cleaning the Corpus and reformatting the Corpus (PlainText tends to lose the Corpus data structure)
clean_bm <- clean_corpus(bm_corpus)
clean_bm_14 <- clean_corpus(bm_corpus_14)
clean_bm_20 <- clean_corpus(bm_corpus_20)


#################################
# Merging words
clean_bm <- tm_map(clean_bm, content_transformer(gsub), pattern = "world bank", replacement = "world_bank", fixed=TRUE)
clean_bm_14 <- tm_map(clean_bm_14, content_transformer(gsub), pattern = "world bank", replacement = "world_bank", fixed=TRUE)
clean_bm_20 <- tm_map(clean_bm_20, content_transformer(gsub), pattern = "world bank", replacement = "world_bank", fixed=TRUE)

# Deleting by hand the characters UTF-8 doesn't recognize
clean_bm <- tm_map(clean_bm, content_transformer(gsub), pattern = "”", replacement = "", fixed=TRUE)
clean_bm_14 <- tm_map(clean_bm_14, content_transformer(gsub), pattern = "”", replacement = "", fixed=TRUE)
clean_bm_20 <- tm_map(clean_bm_20, content_transformer(gsub), pattern = "”", replacement = "", fixed=TRUE)

clean_bm <- tm_map(clean_bm, content_transformer(gsub), pattern = "…", replacement = "", fixed=TRUE)
clean_bm_14 <- tm_map(clean_bm_14, content_transformer(gsub), pattern = "…", replacement = "", fixed=TRUE)
clean_bm_20 <- tm_map(clean_bm_20, content_transformer(gsub), pattern = "…", replacement = "", fixed=TRUE)

clean_bm <- tm_map(clean_bm, content_transformer(gsub), pattern = "“", replacement = "", fixed=TRUE)
clean_bm_14 <- tm_map(clean_bm_14, content_transformer(gsub), pattern = "“", replacement = "", fixed=TRUE)
clean_bm_20 <- tm_map(clean_bm_20, content_transformer(gsub), pattern = "“", replacement = "", fixed=TRUE)

clean_bm <- tm_map(clean_bm, content_transformer(gsub), pattern = "‘", replacement = "", fixed=TRUE)
clean_bm_14 <- tm_map(clean_bm_14, content_transformer(gsub), pattern = "‘", replacement = "", fixed=TRUE)
clean_bm_20 <- tm_map(clean_bm_20, content_transformer(gsub), pattern = "‘", replacement = "", fixed=TRUE)

clean_bm <- tm_map(clean_bm, content_transformer(gsub), pattern = "’", replacement = "", fixed=TRUE)
clean_bm_14 <- tm_map(clean_bm_14, content_transformer(gsub), pattern = "’", replacement = "", fixed=TRUE)
clean_bm_20 <- tm_map(clean_bm_20, content_transformer(gsub), pattern = "’", replacement = "", fixed=TRUE)

clean_bm <- tm_map(clean_bm, content_transformer(gsub), pattern = "-", replacement = "", fixed=TRUE)
clean_bm_14 <- tm_map(clean_bm_14, content_transformer(gsub), pattern = "-", replacement = "", fixed=TRUE)
clean_bm_20 <- tm_map(clean_bm_20, content_transformer(gsub), pattern = "-", replacement = "", fixed=TRUE)


########################
# Time to delete
# Removing words common but not important
clean_bm2 <- tm_map(clean_bm, 
                      removeWords, 
                      c("also","may","one","can"))

clean_bm2_14 <- tm_map(clean_bm_14, 
                    removeWords, 
                    c("also","may","one","can"))

clean_bm2_20 <- tm_map(clean_bm_20, 
                    removeWords, 
                    c("also","may","one","can"))



#########################
# Analytics

# Now that the data is cleaned, we can create a Document Term object
bm_dtm2 <- DocumentTermMatrix(clean_bm2)

bm_dtm2_14 <- DocumentTermMatrix(clean_bm2_14)
bm_dtm3_14 <- removeSparseTerms(bm_dtm2_14, 0.6)

bm_dtm2_20 <- DocumentTermMatrix(clean_bm2_20)
bm_dtm3_20 <- removeSparseTerms(bm_dtm2_20, 0.6)


# Frequent Terms
findFreqTerms(bm_dtm2, lowfreq=300)
findFreqTerms(bm_dtm3_14, lowfreq=100)
findFreqTerms(bm_dtm3_20, lowfreq=100)


# Convert to dataframe for exporting to Visone
bm_dtm_frame <- tidy(bm_dtm2)
bm_dtm_frame_14 <- tidy(bm_dtm2_14)
write.csv(bm_dtm_frame_14, "attribute_table_2010_2014.csv")
bm_dtm_frame_20 <- tidy(bm_dtm2_20)
write.csv(bm_dtm_frame_14, "attribute_table_2015_2020.csv")



# And now goes into a matrix
# colSums IF DocumentTermMatrix; rowSums IF TermDocumentMatrix !
term.freq <- sort(colSums(as.matrix(bm_dtm2)),decreasing=TRUE)
term.freq_14 <- sort(colSums(as.matrix(bm_dtm2_14)),decreasing=TRUE)
term.freq_20 <- sort(colSums(as.matrix(bm_dtm2_20)),decreasing=TRUE)


# data.framing the previous objects
bm_word_freqs <- data.frame(word = names(term.freq), freq=term.freq)
bm_word_freqs_14 <- data.frame(word = names(term.freq_14), freq=term.freq_14)
bm_word_freqs_20 <- data.frame(word = names(term.freq_20), freq=term.freq_20)

##########################################
# Word freq for not usual words
bm_common_freqs <- inner_join(bm_word_freqs_14, bm_word_freqs_20, by = "word") %>% select(word)

bm_word_freqs_14_notcommon <- anti_join(bm_word_freqs_14, bm_common_freqs, by = "word")
bm_word_freqs_20_notcommon <- anti_join(bm_word_freqs_20, bm_common_freqs, by = "word")

# Ploting
dev.new(width = 1600, height = 1600, unit = "px")
wordcloud(bm_word_freqs_14_notcommon$word, bm_word_freqs_14_notcommon$freq,
          # With random False then most important words in the centre
          random.order = F,
          rot.per = 0.2,
          scale=c(5,.2),
          min.freq=3,
          max.words = 100,
          colors = dark2)


dev.new(width = 1600, height = 1600, unit = "px")
wordcloud(bm_word_freqs_20_notcommon$word, bm_word_freqs_20_notcommon$freq,
          # With random False then most important words in the centre
          random.order = F,
          rot.per = 0.2,
          scale=c(5,.2),
          min.freq=3,
          max.words = 100,
          colors = dark2)


##########################################
# Word after #100 most used
bm_word_freqs_14_top100 <- top_n(bm_word_freqs_14, 15, bm_word_freqs_14$freq)
bm_word_freqs_20_top100 <- top_n(bm_word_freqs_20, 15, bm_word_freqs_20$freq)

bm_word_freqs_14_m100 <- anti_join(bm_word_freqs_14, bm_word_freqs_14_top100, by = "word")
bm_word_freqs_20_m100 <- anti_join(bm_word_freqs_20, bm_word_freqs_20_top100, by = "word")

# Ploting
dev.new(width = 1600, height = 1600, unit = "px")
wordcloud(bm_word_freqs_14_m100$word, bm_word_freqs_14_m100$freq,
          # With random False then most important words in the centre
          random.order = T,
          rot.per = 0.2,
          scale=c(5,.2),
          min.freq=10,
          max.words = 400,
          colors = dark2)


dev.new(width = 1600, height = 1600, unit = "px")
wordcloud(bm_word_freqs_20_m100$word, bm_word_freqs_20_m100$freq,
          # With random False then most important words in the centre
          random.order = T,
          rot.per = 0.2,
          scale=c(5,.2),
          min.freq=10,
          max.words = 400,
          colors = dark2)



# Plotting frequencies to find cutting point
bm_word_freqs_14 %>% 
  ggplot(aes(x=freq)) + geom_histogram()

quantile(bm_word_freqs_14$freq, probs=c(0.1,0.5,0.75,0.85,0.9,0.95,0.975))
# 26 or more for top 5% in 2010-2014

quantile(bm_word_freqs_20$freq, probs=c(0.1,0.5,0.75,0.85,0.9,0.95,0.975))
# 41 or more for top 5% in 2015-2020


# Selection down 90% to filter in word_association
down_90_2014 <- bm_word_freqs_14 %>% filter(freq < 26) %>% select(word) %>% list()
down_90_2020 <- bm_word_freqs_20 %>% filter(freq < 41) %>% select(word) %>% list()



#################################3
# GENERATING ADJACENCY MATRIX
library(Matrix)
bm_tdm_14 <- t(bm_dtm2_14)

Xt <- sparseMatrix(j=bm_dtm2_14$i, i=bm_dtm2_14$j, x=bm_dtm2_14$v)
X <- sparseMatrix(j=bm_tdm_14$i, i=bm_tdm_14$j, x=bm_tdm_14$v)
adj_matrix_14 <- Xt %*% X
write.table(summary(adj_matrix_14), "adj_14.txt")


#####################
# Basic Bag of Words
# Freq plot for words in all documents
ggplot(subset(bm_word_freqs, freq>300), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  ylab("Frecuencia entre los 20 blogs analizados") + xlab("") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


ggplot(subset(bm_word_freqs_14, freq>200), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  ylab("Frecuencia entre los 20 blogs analizados") + xlab("") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


ggplot(subset(bm_word_freqs_20, freq>200), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  ylab("Frecuencia entre los 20 blogs analizados") + xlab("") +
  theme(axis.text.x=element_text(angle=45, hjust=1))




##################################
# CLUSTERING
library(cluster)   
d_14 <- dist(t(bm_dtm3_14), method="euclidian")   
fit_14 <- hclust(d=d_14, method="complete")   # for a different look try substituting: method="ward.D"
fit_14

plot.new()
plot(fit_14, hang=-1)
ggdendogram(fit_14)
#groups <- cutree(fit_14, h=40) #cutree(fit_14, k=4)   # "k=" defines the number of clusters you are using   
rect.hclust(fit_14, h=25, border="red") # draw dendogram with red borders around the 6 clusters 

d_20 <- dist(t(bm_dtm3_20), method="euclidian")   
fit_20 <- hclust(d=d_20, method="complete")   # for a different look try substituting: method="ward.D"
fit_20

plot.new()
plot(fit_20, hang=-1)
#groups <- cutree(fit_14, k=4)   # "k=" defines the number of clusters you are using   
rect.hclust(fit_20, h=35, border="red") # draw dendogram with red borders around the 6 clusters 


#####################
# Cloud of Words :)
dark2 <- brewer.pal(6,"Dark2")

dev.new(width = 1600, height = 1600, unit = "px")
wordcloud(bm_word_freqs_14$word, bm_word_freqs_14$freq,
          # With random False then most important words in the centre
          random.order = F,
          rot.per = 0.2,
          scale=c(5,.2),
          min.freq=50,
          max.words = 400,
          colors = dark2)


# Or a pretty plot
word_associate(
  bm_dtm3_14,
  match.string = c("educ", "learn"), # palabra clave!
  # si en vez de Corpus hay q agregar un df, entonces sí usar la expresión de abajo
  stopwords = c(stopwords(kind = "en")),
  network.plot = T,
  cloud.colors = c("gray85", "darkred")
)

title(main = "")


#######################
# Word relations
#source('http://bioconductor.org/biocLite.R')
#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
library(Rgraphviz)
plot(bm_dtm3_14,
     terms = findFreqTerms(bm_dtm3_14, lowfreq = 10)[1:30],
     corThreshold = 0.2,
     weighting = T,
     attrs=list(node=list(label="foo", fillcolor="lightpink", fontsize = 25),
                edge=list(color="black")
                #graph=list(rankdir="LR")
                )
     )


plot(bm_dtm3_20,
     terms = findFreqTerms(bm_dtm3_20, lowfreq = 20)[1:30],
     corThreshold = 0.15, 
     weighting = T,
     attrs=list(node=list(label="foo", fillcolor="lightpink", fontsize = 20),
                edge=list(color="black")
                #graph=list(rankdir="LR")
                )
    )



###################################################################
# Coeficiente de Inclusión Mutua para grupo de palabras selectas

# Education
findAssocs(bm_dtm2, "education", 0.25)

asoc_education_14 <- findAssocs(bm_dtm2_14, "education", 0.25) %>% data.frame()
asoc_education_14_bis <- cbind(data.frame(rownames(asoc_education_14)), asoc_education_14$education) %>%  data.frame()
names(asoc_education_14_bis) <- c("word","corr")
write.table(asoc_education_14_bis, "education_2010_2014.txt", sep = "\t")

asoc_education_20 <- findAssocs(bm_dtm2_20, "education", 0.25) %>% data.frame()
asoc_education_20_bis <- cbind(data.frame(rownames(asoc_education_20)), asoc_education_20$education) %>%  data.frame()
names(asoc_education_20_bis) <- c("word","corr")
write.table(asoc_education_20_bis, "education_2015_2020.txt", sep = "\t")


# Learning
findAssocs(bm_dtm2, "learning", 0.25)
findAssocs(bm_dtm2_14, "learning", 0.3)
findAssocs(bm_dtm2_20, "learning", 0.3)

asoc_learning_14 <- findAssocs(bm_dtm2_14, "learning", 0.25) %>% data.frame()
asoc_learning_14_bis <- cbind(data.frame(rownames(asoc_learning_14)), asoc_learning_14$learning) %>%  data.frame()
names(asoc_learning_14_bis) <- c("word","corr")
write.table(asoc_learning_14_bis, "learning_2010_2014.txt", sep = "\t")

asoc_learning_20 <- findAssocs(bm_dtm2_20, "learning", 0.25) %>% data.frame()
asoc_learning_20_bis <- cbind(data.frame(rownames(asoc_learning_20)), asoc_learning_20$learning) %>%  data.frame()
names(asoc_learning_20_bis) <- c("word","corr")
write.table(asoc_learning_20_bis, "learning_2015_2020.txt", sep = "\t")


# Private
findAssocs(bm_dtm2, "private", 0.25)
findAssocs(bm_dtm2_14, "privat", 0.40)
findAssocs(bm_dtm2_20, "privat", 0.35)

asoc_private_14 <- findAssocs(bm_dtm2_14, "privat", 0.4) %>% data.frame()
asoc_private_14_bis <- cbind(data.frame(rownames(asoc_private_14)), asoc_private_14$privat) %>%  data.frame()
names(asoc_private_14_bis) <- c("word","corr")
write.table(asoc_private_14_bis, "private_2010_2014.txt", sep = "\t")

asoc_private_20 <- findAssocs(bm_dtm2_20, "privat", 0.35) %>% data.frame()
asoc_private_20_bis <- cbind(data.frame(rownames(asoc_private_20)), asoc_private_20$privat) %>%  data.frame()
names(asoc_private_20_bis) <- c("word","corr")
write.table(asoc_private_20_bis, "private_2015_2020.txt", sep = "\t")


# Public
findAssocs(bm_dtm2, "public", 0.25)
findAssocs(bm_dtm2_14, "public", 0.25)
findAssocs(bm_dtm2_20, "public", 0.25)

asoc_public_14 <- findAssocs(bm_dtm2_14, "public", 0.25) %>% data.frame()
asoc_public_14_bis <- cbind(data.frame(rownames(asoc_public_14)), asoc_public_14$public) %>%  data.frame()
names(asoc_public_14_bis) <- c("word","corr")
write.table(asoc_public_14_bis, "public_2010_2014.txt", sep = "\t")

asoc_public_20 <- findAssocs(bm_dtm2_20, "public", 0.25) %>% data.frame()
asoc_public_20_bis <- cbind(data.frame(rownames(asoc_public_20)), asoc_public_20$public) %>%  data.frame()
names(asoc_public_20_bis) <- c("word","corr")
write.table(asoc_public_20_bis, "public_2015_2020.txt", sep = "\t")


# Teach
findAssocs(bm_dtm2, "teach", 0.2)
findAssocs(bm_dtm3_14, "teacher", 0.1)
findAssocs(bm_dtm3_20, "teacher", 0.1)

asoc_teach_14 <- findAssocs(bm_dtm3_14, "teacher", 0.1) %>% data.frame()
asoc_teach_14_bis <- cbind(data.frame(rownames(asoc_teach_14)), asoc_teach_14$teacher) %>%  data.frame()
names(asoc_teach_14_bis) <- c("word","corr")
write.table(asoc_teach_14_bis, "teach_2010_2014.txt", sep = "\t")

asoc_teach_20 <- findAssocs(bm_dtm3_20, "teacher", 0.1) %>% data.frame()
asoc_teach_20_bis <- cbind(data.frame(rownames(asoc_teach_20)), asoc_teach_20$teacher) %>%  data.frame()
names(asoc_teach_20_bis) <- c("word","corr")
write.table(asoc_teach_20_bis, "teach_2015_2020.txt", sep = "\t")


# Student
findAssocs(bm_dtm2, "student", 0.1)
findAssocs(bm_dtm3_14, "student", 0.2)
findAssocs(bm_dtm3_20, "student", 0.15)


asoc_student_14 <- findAssocs(bm_dtm3_14, "student", 0.2) %>% data.frame()
asoc_student_14_bis <- cbind(data.frame(rownames(asoc_student_14)), asoc_student_14$student) %>%  data.frame()
names(asoc_student_14_bis) <- c("word","corr")
write.table(asoc_student_14_bis, "student_2010_2014.txt", sep = "\t")

asoc_student_20 <- findAssocs(bm_dtm3_20, "student", 0.15) %>% data.frame()
asoc_student_20_bis <- cbind(data.frame(rownames(asoc_student_20)), asoc_student_20$student) %>%  data.frame()
names(asoc_student_20_bis) <- c("word","corr")
write.table(asoc_student_20_bis, "student_2015_2020.txt", sep = "\t")


##############################
# Topic Modeling
library(topicmodels)
bm_14_lda <- LDA(bm_dtm2_14, k = 6, control = list(seed = 31416))
term_14 <- terms(bm_14_lda, 12) #first 5 terms of every topic
term_14 #checking

bm_20_lda <- LDA(bm_dtm2_20, k = 6, control = list(seed = 31416))
term_20 <- terms(bm_20_lda, 12) #first 5 terms of every topic
term_20 #checking

