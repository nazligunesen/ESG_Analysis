

#Downloading all the required libraies
#install.packages(stm)
library(readr)
library(tm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wordcloud)
require(pdftools)#reads pdf documents
require(tm)#text mining analysis

#install.packages(stm)
#library(stm)              # For structural topic models
#library(stminsights)      # For visual exploration of STM

library(wordcloud)        # To generate wordclouds
#library(topicmodels)      # For topicmodels
library(quanteda)         # For NLP
#library(stminsights)      # For visual exploration of STM
library(wordcloud)        # To generate wordclouds
#library(topicmodels)      # For topicmodels
#library(gsl)              # Required for the topicmodels package
library(SnowballC)        # For stemming
#files<-list.files(pattern="pdf$")#creates vector of pdf file names
#opinions<-lapply(files,pdf_text)#loads pdfs
#length(opinions)#verify how many files have been loaded
library(igraph)
library(ggraph)


#journals2010<-list.files(pattern="2010$")#creates vector of pdf file names
#pdf_files2010 <- journals2010[grepl(".pdf$", all_files, ignore.case = TRUE)]

# Setting the path for each folder containing PDF files
#folder_path2010 <- "~/Desktop/ESG Text Analytics/2010"
#folder_path2011 <- "~/Desktop/ESG Text Analytics/2011"
#folder_path2012 <- "~/Desktop/ESG Text Analytics/2012"
#folder_path2013 <- "~/Desktop/ESG Text Analytics/2013"
#folder_path2014 <- "~/Desktop/ESG Text Analytics/2014"
#folder_path2015 <- "~/Desktop/ESG Text Analytics/2015"
#folder_path2016 <- "~/Desktop/ESG Text Analytics/2016"
#folder_path2017 <- "~/Desktop/ESG Text Analytics/2017"
#folder_path2018 <- "~/Desktop/ESG Text Analytics/2018"
#folder_path2019 <- "~/Desktop/ESG Text Analytics/2019"
#folder_path2020 <- "~/Desktop/ESG Text Analytics/2020"
#folder_path2021 <- "~/Desktop/ESG Text Analytics/2021"
#folder_path2022 <- "~/Desktop/ESG Text Analytics/2022"
#folder_path2023 <- "~/Desktop/ESG Text Analytics/2023"


# List all files in the folder
#pdf_files2010 <- list.files(folder_path2010, pattern = ".pdf$", full.names = TRUE)
#pdf_files2011 <- list.files(folder_path2011, pattern = ".pdf$", full.names = TRUE)
#pdf_files2012 <- list.files(folder_path2012, pattern = ".pdf$", full.names = TRUE)
#pdf_files2013 <- list.files(folder_path2013, pattern = ".pdf$", full.names = TRUE)
#pdf_files2014 <- list.files(folder_path2014, pattern = ".pdf$", full.names = TRUE)
#pdf_files2015 <- list.files(folder_path2015, pattern = ".pdf$", full.names = TRUE)
#pdf_files2016 <- list.files(folder_path2016, pattern = ".pdf$", full.names = TRUE)
#pdf_files2017 <- list.files(folder_path2017, pattern = ".pdf$", full.names = TRUE)
#pdf_files2018 <- list.files(folder_path2018, pattern = ".pdf$", full.names = TRUE)
#pdf_files2019 <- list.files(folder_path2019, pattern = ".pdf$", full.names = TRUE)
#pdf_files2020 <- list.files(folder_path2020, pattern = ".pdf$", full.names = TRUE)
#pdf_files2021 <- list.files(folder_path2021, pattern = ".pdf$", full.names = TRUE)
#pdf_files2022 <- list.files(folder_path2022, pattern = ".pdf$", full.names = TRUE)
#pdf_files2023 <- list.files(folder_path2023, pattern = ".pdf$", full.names = TRUE)



# Print the list of PDF files
#print(pdf_files2010)


# Identifying the pdf files that cause parsing error and elminating them for year 2021
#2021
#pdf_texts2021 <- lapply(pdf_files2021, function(file_path) {
#  tryCatch(
#   {
#      pdf_text(file_path)
#    },
#   error = function(e) {
#      # Handle error, e.g., print the file path causing the error
#      cat("Error parsing:", file_path, "\n")
#      return(NULL) # Return NULL or handle the error as needed
#    }
#  )
#})

# Filter out NULL entries (failed parsing) if needed
#parsed_texts2021<- Filter(Negate(is.null), pdf_texts)

# Filenames or paths of PDF files to remove
#files_to_remove <- c("/Users/nazligunesen/Desktop/ESG Text Analytics/2021/Ridley‐Duff and Bull - 2021 - Common pool resource institutions The rise of int.pdf", "/Users/nazligunesen/Desktop/ESG Text Analytics/2021/T. M. et al. - 2021 - What motivates the adoption of green restaurant pr.pdf")  
# Remove specific files from the list
#pdf_files2021 <- pdf_files2021[!pdf_files2021 %in% files_to_remove]





#Creating a corpus for each year to be used for further analysis
#WARNING: Takes long
#pdfdatabase2010 <- Corpus(URISource(pdf_files2010),readerControl = list(reader = readPDF))
#pdfdatabase2011 <- Corpus(URISource(pdf_files2011),readerControl = list(reader = readPDF))
#pdfdatabase2012 <- Corpus(URISource(pdf_files2012),readerControl = list(reader = readPDF))
#pdfdatabase2013 <- Corpus(URISource(pdf_files2013),readerControl = list(reader = readPDF))
#pdfdatabase2014 <- Corpus(URISource(pdf_files2014),readerControl = list(reader = readPDF))
#pdfdatabase2015 <- Corpus(URISource(pdf_files2015),readerControl = list(reader = readPDF))
#pdfdatabase2016 <- Corpus(URISource(pdf_files2016),readerControl = list(reader = readPDF))
#pdfdatabase2017 <- Corpus(URISource(pdf_files2017),readerControl = list(reader = readPDF))
#pdfdatabase2018 <- Corpus(URISource(pdf_files2018),readerControl = list(reader = readPDF))
#pdfdatabase2019 <- Corpus(URISource(pdf_files2019),readerControl = list(reader = readPDF))
#pdfdatabase2020 <- Corpus(URISource(pdf_files2020),readerControl = list(reader = readPDF))
#pdfdatabase2021 <- Corpus(URISource(pdf_files2021),readerControl = list(reader = readPDF))
#pdfdatabase2022 <- Corpus(URISource(pdf_files2022),readerControl = list(reader = readPDF))
#pdfdatabase2023 <- Corpus(URISource(pdf_files2023),readerControl = list(reader = readPDF))





# Saving the generated corpus for each year so that we dont need to generate them again

#saveRDS(pdfdatabase2010, file = "pdfdatabase2010.rds")
#saveRDS(pdfdatabase2011, file = "pdfdatabase2011.rds")
#saveRDS(pdfdatabase2012, file = "pdfdatabase2012.rds")
#saveRDS(pdfdatabase2013, file = "pdfdatabase2013.rds")
#saveRDS(pdfdatabase2014, file = "pdfdatabase2014.rds")
#saveRDS(pdfdatabase2015, file = "pdfdatabase2015.rds")
#saveRDS(pdfdatabase2016, file = "pdfdatabase2016.rds")
#saveRDS(pdfdatabase2017, file = "pdfdatabase2017.rds")
#saveRDS(pdfdatabase2018, file = "pdfdatabase2018.rds")
#saveRDS(pdfdatabase2019, file = "pdfdatabase2019.rds")
#saveRDS(pdfdatabase2020, file = "pdfdatabase2020.rds")
#saveRDS(pdfdatabase2021, file = "pdfdatabase2021.rds")
#saveRDS(pdfdatabase2022, file = "pdfdatabase2022.rds")
#saveRDS(pdfdatabase2023, file = "pdfdatabase2023.rds")

#saveRDS(pdfdatabase1, file = "pdfdatabase1.rds")
#saveRDS(pdfdatabase2, file = "pdfdatabase2.rds")

# Calling the saved corpus for each year
corpus2010 <- readRDS("pdfdatabase2010.rds")
corpus2011 <- readRDS("pdfdatabase2011.rds")
corpus2012 <- readRDS("pdfdatabase2012.rds")
corpus2013 <- readRDS("pdfdatabase2013.rds")
corpus2014 <- readRDS("pdfdatabase2014.rds")
corpus2015 <- readRDS("pdfdatabase2015.rds")
corpus2016 <- readRDS("pdfdatabase2016.rds")
corpus2017 <- readRDS("pdfdatabase2017.rds")
corpus2018 <- readRDS("pdfdatabase2018.rds")
corpus2019 <- readRDS("pdfdatabase2019.rds")
corpus2020 <- readRDS("pdfdatabase2020.rds")
corpus2021 <- readRDS("pdfdatabase2021.rds")
corpus2022 <- readRDS("pdfdatabase2022.rds")
corpus2023 <- readRDS("pdfdatabase2023.rds")


# Merging corpus between 2010-2019
#merged_corpus20102019 <- Corpus(VectorSource(c(corpus2010,corpus2011,corpus2012,corpus2013,corpus2014,corpus2015,corpus2016,corpus2017,corpus2018,corpus2019)))

# Merging corpus between 2020-2023
#merged_corpus20202023 <- Corpus(VectorSource(c(corpus2020,corpus2021,corpus2022,corpus2023)))

#Saving merged corpuses

#saveRDS(merged_corpus20102019, file = "merged_corpus20102019.rds")
#saveRDS(merged_corpus20202023, file = "merged_corpus20202023.rds")

# Calling the saved corpus for each cluster
#merged_corpus1 <- readRDS("merged_corpus20102019.rds")
#merged_corpus2 <- readRDS("merged_corpus20202023.rds")




#PREPROCESSING#


# Custom function to remove punctuation
removePunctuation <- function(text) {
  text <- gsub("[[:punct:]]", " ", text)
  return(text)
}

# Removing punctuation
corpus2010 <- tm_map(corpus2010, content_transformer(removePunctuation))
corpus2011 <- tm_map(corpus2011, content_transformer(removePunctuation))
corpus2012 <- tm_map(corpus2012, content_transformer(removePunctuation))
corpus2013 <- tm_map(corpus2013, content_transformer(removePunctuation))
corpus2014 <- tm_map(corpus2014, content_transformer(removePunctuation))
corpus2015 <- tm_map(corpus2015, content_transformer(removePunctuation))
corpus2016 <- tm_map(corpus2016, content_transformer(removePunctuation))
corpus2017 <- tm_map(corpus2017, content_transformer(removePunctuation))
corpus2018 <- tm_map(corpus2018, content_transformer(removePunctuation))
corpus2019 <- tm_map(corpus2019, content_transformer(removePunctuation))
corpus2020 <- tm_map(corpus2020, content_transformer(removePunctuation))
corpus2021 <- tm_map(corpus2021, content_transformer(removePunctuation))
corpus2022 <- tm_map(corpus2022, content_transformer(removePunctuation))
corpus2023 <- tm_map(corpus2023, content_transformer(removePunctuation))

#Removing numbers and stopwords  
corpus2010 <-tm_map(corpus2010,content_transformer(tolower))#convert all text to lowercase
corpus2010 <-tm_map(corpus2010,removeNumbers)#remove numbers from document
corpus2010 <-tm_map(corpus2010,removeWords,stopwords("english"))#remove stopwords in English
corpus2010 <-tm_map(corpus2010,removeWords,c("journal"))#remove stopwords in English

corpus2011 <-tm_map(corpus2011,content_transformer(tolower))#convert all text to lowercase
corpus2011 <-tm_map(corpus2011,removeNumbers)#remove numbers from document
corpus2011 <-tm_map(corpus2011,removeWords,stopwords("english"))#remove stopwords in English
corpus2011 <-tm_map(corpus2011,removeWords,c("journal"))#remove stopwords in English

corpus2012 <-tm_map(corpus2012,content_transformer(tolower))#convert all text to lowercase
corpus2012 <-tm_map(corpus2012,removeNumbers)#remove numbers from document
corpus2012 <-tm_map(corpus2012,removeWords,stopwords("english"))#remove stopwords in English
corpus2012 <-tm_map(corpus2012,removeWords,c("journal"))#remove stopwords in English

corpus2013 <-tm_map(corpus2013,content_transformer(tolower))#convert all text to lowercase
corpus2013 <-tm_map(corpus2013,removeNumbers)#remove numbers from document
corpus2013 <-tm_map(corpus2013,removeWords,stopwords("english"))#remove stopwords in English
corpus2013 <-tm_map(corpus2013,removeWords,c("journal"))#remove stopwords in English

corpus2014 <-tm_map(corpus2014,content_transformer(tolower))#convert all text to lowercase
corpus2014 <-tm_map(corpus2014,removeNumbers)#remove numbers from document
corpus2014 <-tm_map(corpus2014,removeWords,stopwords("english"))#remove stopwords in English
corpus2014 <-tm_map(corpus2014,removeWords,c("journal"))#remove stopwords in English

corpus2015 <-tm_map(corpus2015,content_transformer(tolower))#convert all text to lowercase
corpus2015 <-tm_map(corpus2015,removeNumbers)#remove numbers from document
corpus2015 <-tm_map(corpus2015,removeWords,stopwords("english"))#remove stopwords in English
corpus2015 <-tm_map(corpus2015,removeWords,c("journal"))#remove stopwords in English

corpus2016 <-tm_map(corpus2016,content_transformer(tolower))#convert all text to lowercase
corpus2016 <-tm_map(corpus2016,removeNumbers)#remove numbers from document
corpus2016 <-tm_map(corpus2016,removeWords,stopwords("english"))#remove stopwords in English
corpus2016 <-tm_map(corpus2016,removeWords,c("journal"))#remove stopwords in English

corpus2017 <-tm_map(corpus2017,content_transformer(tolower))#convert all text to lowercase
corpus2017 <-tm_map(corpus2017,removeNumbers)#remove numbers from document
corpus2017 <-tm_map(corpus2017,removeWords,stopwords("english"))#remove stopwords in English
corpus2017 <-tm_map(corpus2017,removeWords,c("journal"))#remove stopwords in English

corpus2018 <-tm_map(corpus2018,content_transformer(tolower))#convert all text to lowercase
corpus2018 <-tm_map(corpus2018,removeNumbers)#remove numbers from document
corpus2018 <-tm_map(corpus2018,removeWords,stopwords("english"))#remove stopwords in English
corpus2018 <-tm_map(corpus2018,removeWords,c("journal"))#remove stopwords in English

corpus2019 <-tm_map(corpus2019,content_transformer(tolower))#convert all text to lowercase
corpus2019 <-tm_map(corpus2019,removeNumbers)#remove numbers from document
corpus2019 <-tm_map(corpus2019,removeWords,stopwords("english"))#remove stopwords in English
corpus2019 <-tm_map(corpus2019,removeWords,c("journal"))#remove stopwords in English

corpus2020 <-tm_map(corpus2020,content_transformer(tolower))#convert all text to lowercase
corpus2020 <-tm_map(corpus2020,removeNumbers)#remove numbers from document
corpus2020 <-tm_map(corpus2020,removeWords,stopwords("english"))#remove stopwords in English
corpus2020 <-tm_map(corpus2020,removeWords,c("journal"))#remove stopwords in English

corpus2021 <-tm_map(corpus2021,content_transformer(tolower))#convert all text to lowercase
corpus2021 <-tm_map(corpus2021,removeNumbers)#remove numbers from document
corpus2021 <-tm_map(corpus2021,removeWords,stopwords("english"))#remove stopwords in English
corpus2021 <-tm_map(corpus2021,removeWords,c("journal"))#remove stopwords in English

corpus2022 <-tm_map(corpus2022,content_transformer(tolower))#convert all text to lowercase
corpus2022 <-tm_map(corpus2022,removeNumbers)#remove numbers from document
corpus2022 <-tm_map(corpus2022,removeWords,stopwords("english"))#remove stopwords in English
corpus2022 <-tm_map(corpus2022,removeWords,c("journal"))#remove stopwords in English

corpus2023 <-tm_map(corpus2023,content_transformer(tolower))#convert all text to lowercase
corpus2023 <-tm_map(corpus2023,removeNumbers)#remove numbers from document
corpus2023 <-tm_map(corpus2023,removeWords,stopwords("english"))#remove stopwords in English
corpus2023 <-tm_map(corpus2023,removeWords,c("journal"))#remove stopwords in English




#FREQUENCY ANALYSIS#

#Step 1: Finding top 10 used words

dtm2010 <-TermDocumentMatrix(corpus2010)
m2010 <-as.matrix(dtm2010)
v2010 <- sort(rowSums(m2010),decreasing = TRUE)
d2010 <- data.frame(word=names(v2010), frequency=v2010)
head(d2010,10)

dtm2011 <-TermDocumentMatrix(corpus2011)
m2011 <-as.matrix(dtm2011)
v2011 <- sort(rowSums(m2011),decreasing = TRUE)
d2011 <- data.frame(word=names(v2011), frequency=v2011)
head(d2011,10)

dtm2012 <-TermDocumentMatrix(corpus2012)
m2012 <-as.matrix(dtm2012)
v2012 <- sort(rowSums(m2012),decreasing = TRUE)
d2012 <- data.frame(word=names(v2012), frequency=v2012)
head(d2012,10)

dtm2013 <-TermDocumentMatrix(corpus2013)
m2013 <-as.matrix(dtm2013)
v2013 <- sort(rowSums(m2013),decreasing = TRUE)
d2013 <- data.frame(word=names(v2013), frequency=v2013)
head(d2013,10)

dtm2014 <-TermDocumentMatrix(corpus2014)
m2014 <-as.matrix(dtm2014)
v2014 <- sort(rowSums(m2014),decreasing = TRUE)
d2014 <- data.frame(word=names(v2014), frequency=v2014)
head(d2014,10)

dtm2015 <-TermDocumentMatrix(corpus2015)
m2015 <-as.matrix(dtm2015)
v2015 <- sort(rowSums(m2015),decreasing = TRUE)
d2015 <- data.frame(word=names(v2015), frequency=v2015)
head(d2015,10)

dtm2016 <-TermDocumentMatrix(corpus2016)
m2016 <-as.matrix(dtm2016)
v2016 <- sort(rowSums(m2016),decreasing = TRUE)
d2016 <- data.frame(word=names(v2016), frequency=v2016)
head(d2016,10)

dtm2017 <-TermDocumentMatrix(corpus2017)
m2017 <-as.matrix(dtm2017)
v2017 <- sort(rowSums(m2017),decreasing = TRUE)
d2017 <- data.frame(word=names(v2017), frequency=v2017)
head(d2017,10)

dtm2018 <-TermDocumentMatrix(corpus2018)
m2018 <-as.matrix(dtm2018)
v2018 <- sort(rowSums(m2018),decreasing = TRUE)
d2018 <- data.frame(word=names(v2018), frequency=v2018)
head(d2018,10)

dtm2019 <-TermDocumentMatrix(corpus2019)
m2019 <-as.matrix(dtm2019)
v2019 <- sort(rowSums(m2019),decreasing = TRUE)
d2019 <- data.frame(word=names(v2019), frequency=v2019)
head(d2019,10)

dtm2020 <-TermDocumentMatrix(corpus2020)
m2020 <-as.matrix(dtm2020)
v2020 <- sort(rowSums(m2020),decreasing = TRUE)
d2020 <- data.frame(word=names(v2020), frequency=v2020)
head(d2020,10)

dtm2021 <-TermDocumentMatrix(corpus2021)
m2021 <-as.matrix(dtm2021)
v2021 <- sort(rowSums(m2021),decreasing = TRUE)
d2021 <- data.frame(word=names(v2021), frequency=v2021)
head(d2021,10)

dtm2022 <-TermDocumentMatrix(corpus2022)
m2022 <-as.matrix(dtm2022)
v2022 <- sort(rowSums(m2022),decreasing = TRUE)
d2022 <- data.frame(word=names(v2022), frequency=v2022)
head(d2022,10)

dtm2023 <-TermDocumentMatrix(corpus2023)
m2023 <-as.matrix(dtm2023)
v2023 <- sort(rowSums(m2023),decreasing = TRUE)
d2023 <- data.frame(word=names(v2023), frequency=v2023)
head(d2023,10)


#CREATING WORD CLOUDS#

set.seed(123)
wordcloud(words=d2010$word, freq=d2010$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2011$word, freq=d2011$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2012$word, freq=d2012$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2013$word, freq=d2013$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2014$word, freq=d2014$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2015$word, freq=d2015$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2016$word, freq=d2016$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2017$word, freq=d2017$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2018$word, freq=d2018$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2019$word, freq=d2019$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2020$word, freq=d2020$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2021$word, freq=d2021$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2022$word, freq=d2022$freq, min.freq=1000, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
wordcloud(words=d2023$word, freq=d2023$freq, min.freq=10, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))






######COUNTING SPECIFIC WORDS#######

### Words to count ESG ###

# Load necessary libraries
# Load necessary libraries
library(tm)
library(ggplot2)

# List of corpus names
corpus_names <- paste0("corpus", 2010:2023)

# Words to count (ESG)
words_to_count <- c("sri", "csr", "esg", "environment", "social", "governance", 
                    "impact", "sustain", "sustainability", "responsibility", "disclosure")

# Function to ensure that all documents in the corpus are PlainTextDocuments
convert_to_text_document <- function(corpus) {
  corpus <- tm_map(corpus, function(doc) {
    if (is.character(doc)) {
      return(PlainTextDocument(doc))
    } else if (inherits(doc, "PlainTextDocument")) {
      return(doc)
    } else {
      stop("Document is neither character nor PlainTextDocument.")
    }
  })
  return(corpus)
}

# Function to count occurrences of multiple words in a VCorpus
count_words_occurrences <- function(corpus) {
  if (!inherits(corpus, "VCorpus")) {
    stop("Input is not a valid VCorpus object.")
  }
  
  # Convert any character objects back to PlainTextDocument
  corpus <- convert_to_text_document(corpus)
  
  # Step 1: Convert to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # Step 2: Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  
  # Ensure the documents are still PlainTextDocuments after transformations
  corpus <- convert_to_text_document(corpus)
  
  # Initialize counts for each word
  word_counts <- numeric(length(words_to_count))
  
  # Loop through each word and count occurrences
  for (i in seq_along(words_to_count)) {
    word <- words_to_count[i]
    word_counts[i] <- sum(sapply(corpus, function(doc) {
      if (!inherits(doc, "PlainTextDocument")) {
        stop("Error: Document is not a PlainTextDocument.")
      }
      text <- as.character(doc$content)
      sum(grepl(word, text, fixed = TRUE))
    }))
  }
  
  return(word_counts)  # Return the counts for each word
}

# Initialize an empty data frame to store results
results_df <- data.frame(Year = numeric(), Word = character(), Occurrences = numeric())

# Iterate through each corpus and count occurrences of the words
for (corpus_name in corpus_names) {
  try({
    # Retrieve the corpus object
    corpus_data <- get(corpus_name)
    
    if (inherits(corpus_data, "VCorpus")) {
      print(paste("Processing", corpus_name))
      
      # Count word occurrences in the VCorpus
      word_counts <- count_words_occurrences(corpus_data)
      
      # Extract the year from the corpus name
      year <- as.numeric(substr(corpus_name, 7, 10))
      
      # Store the results: one row per word
      for (i in seq_along(words_to_count)) {
        results_df <- rbind(results_df, data.frame(Year = year, Word = words_to_count[i], Occurrences = word_counts[i]))
      }
      
    } else {
      print(paste("Corpus", corpus_name, "is not a valid VCorpus object."))
    }
    
  }, silent = FALSE)  # Capture any errors and continue
}

# Output the final results
print(results_df)

# Optional: You can save the data frame to a CSV file if needed
# write.csv(results_df, "results.csv", row.names = FALSE)

#Normalization

require(ggplot2)

# Example number of PDF articles per corpus (replace with actual data)
num_articles <- c(688, 881, 631, 1019, 752, 1709, 1135, 1334, 1586, 1539, 1478, 1987, 2273, 2268)


# Add the number of articles to the results data frame
results_df$NumArticles <- rep(num_articles, each = length(words_to_count))

# Assuming `num_articles` holds the number of articles in each corpus, for example:
# num_articles <- c(688, 720, 650, ...)  # One value per year or corpus

# After filling the results dataframe, make sure it has the correct number of rows:
if (nrow(results_df) > 0) {
  # Replicate the `num_articles` value to match the number of rows in `results`
  results_df$NumArticles <- rep(num_articles, each = length(words_to_count))
  
  # Check the structure of the results dataframe to ensure it looks as expected
  print(head(results_df))
} else {
  print("Results data frame is empty, check word counting step.")
}


# Normalize the occurrences by the number of articles
results_df$NormalizedOccurrences <- results_df$Occurrences / results_df$NumArticles

# Load ggplot2 if not already loaded
library(ggplot2)

# Plot occurrences using ggplot
ggplot(results_df, aes(x = factor(Year), y = Occurrences, color = Word, group = Word)) +
  geom_line() +
  geom_point() +  # Add points at each year for better visibility
  labs(title = "Occurrences of ESG-related Keywords by Year",
       x = "Year", y = "Occurrences") +
  scale_x_discrete(labels = function(x) substr(x, 3, 4)) +  # Display only the last two digits of the year
  scale_color_discrete(name = "Keywords") +  # Legend title for the keywords
  theme_minimal()  # Use a minimal theme for cleaner visualization


# Plot normalized occurrences using ggplot
ggplot(results_df, aes(x = factor(Year), y = NormalizedOccurrences, color = Word, group = Word)) +
  geom_line() +
  geom_point() +  # Add points at each year for better visibility
  labs(title = "Normalized Occurrences of ESG-related Keywords by Year",
       x = "Year", y = "Normalized Occurrences") +
  scale_x_discrete(labels = function(x) substr(x, 3, 4)) +  # Display only the last two digits of the year
  scale_color_discrete(name = "Keywords") +  # Legend title for the keywords
  theme_minimal()  # Use a minimal theme for cleaner visualization



### Words to count (M&A) ###

# Load necessary libraries
# Load necessary libraries
library(tm)
library(ggplot2)

# List of corpus names
corpus_names <- paste0("corpus", 2010:2023)

# Words to count (ESG)
words_to_count <- c("mergersandacquisitions", "duediligence", "transaction", "merger", "acquisition", "target", 
                    "takeover", "buyside", "sellside", "acquire", "valuation","invest","diligence","deal")

# Function to ensure that all documents in the corpus are PlainTextDocuments
convert_to_text_document <- function(corpus) {
  corpus <- tm_map(corpus, function(doc) {
    if (is.character(doc)) {
      return(PlainTextDocument(doc))
    } else if (inherits(doc, "PlainTextDocument")) {
      return(doc)
    } else {
      stop("Document is neither character nor PlainTextDocument.")
    }
  })
  return(corpus)
}

# Function to count occurrences of multiple words in a VCorpus
count_words_occurrences <- function(corpus) {
  if (!inherits(corpus, "VCorpus")) {
    stop("Input is not a valid VCorpus object.")
  }
  
  # Convert any character objects back to PlainTextDocument
  corpus <- convert_to_text_document(corpus)
  
  # Step 1: Convert to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # Step 2: Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  
  # Ensure the documents are still PlainTextDocuments after transformations
  corpus <- convert_to_text_document(corpus)
  
  # Initialize counts for each word
  word_counts <- numeric(length(words_to_count))
  
  # Loop through each word and count occurrences
  for (i in seq_along(words_to_count)) {
    word <- words_to_count[i]
    word_counts[i] <- sum(sapply(corpus, function(doc) {
      if (!inherits(doc, "PlainTextDocument")) {
        stop("Error: Document is not a PlainTextDocument.")
      }
      text <- as.character(doc$content)
      sum(grepl(word, text, fixed = TRUE))
    }))
  }
  
  return(word_counts)  # Return the counts for each word
}

# Initialize an empty data frame to store results
results_df <- data.frame(Year = numeric(), Word = character(), Occurrences = numeric())

# Iterate through each corpus and count occurrences of the words
for (corpus_name in corpus_names) {
  try({
    # Retrieve the corpus object
    corpus_data <- get(corpus_name)
    
    if (inherits(corpus_data, "VCorpus")) {
      print(paste("Processing", corpus_name))
      
      # Count word occurrences in the VCorpus
      word_counts <- count_words_occurrences(corpus_data)
      
      # Extract the year from the corpus name
      year <- as.numeric(substr(corpus_name, 7, 10))
      
      # Store the results: one row per word
      for (i in seq_along(words_to_count)) {
        results_df <- rbind(results_df, data.frame(Year = year, Word = words_to_count[i], Occurrences = word_counts[i]))
      }
      
    } else {
      print(paste("Corpus", corpus_name, "is not a valid VCorpus object."))
    }
    
  }, silent = FALSE)  # Capture any errors and continue
}

# Output the final results
print(results_df)

# Optional: You can save the data frame to a CSV file if needed
# write.csv(results_df, "results.csv", row.names = FALSE)

#Normalization

require(ggplot2)

# Example number of PDF articles per corpus (replace with actual data)
num_articles <- c(688, 881, 631, 1019, 752, 1709, 1135, 1334, 1586, 1539, 1478, 1987, 2273, 2268)


# Add the number of articles to the results data frame
results_df$NumArticles <- rep(num_articles, each = length(words_to_count))

# Assuming `num_articles` holds the number of articles in each corpus, for example:
# num_articles <- c(688, 720, 650, ...)  # One value per year or corpus

# After filling the results dataframe, make sure it has the correct number of rows:
if (nrow(results_df) > 0) {
  # Replicate the `num_articles` value to match the number of rows in `results`
  results_df$NumArticles <- rep(num_articles, each = length(words_to_count))
  
  # Check the structure of the results dataframe to ensure it looks as expected
  print(head(results_df))
} else {
  print("Results data frame is empty, check word counting step.")
}


# Normalize the occurrences by the number of articles
results_df$NormalizedOccurrences <- results_df$Occurrences / results_df$NumArticles

# Load ggplot2 if not already loaded
library(ggplot2)

# Plot occurrences using ggplot
ggplot(results_df, aes(x = factor(Year), y = Occurrences, color = Word, group = Word)) +
  geom_line() +
  geom_point() +  # Add points at each year for better visibility
  labs(title = "Occurrences of M&A-related Keywords by Year",
       x = "Year", y = "Occurrences") +
  scale_x_discrete(labels = function(x) substr(x, 3, 4)) +  # Display only the last two digits of the year
  scale_color_discrete(name = "Keywords") +  # Legend title for the keywords
  theme_minimal()  # Use a minimal theme for cleaner visualization


# Plot normalized occurrences using ggplot
ggplot(results_df, aes(x = factor(Year), y = NormalizedOccurrences, color = Word, group = Word)) +
  geom_line() +
  geom_point() +  # Add points at each year for better visibility
  labs(title = "Normalized Occurrences of M&A-related Keywords by Year",
       x = "Year", y = "Normalized Occurrences") +
  scale_x_discrete(labels = function(x) substr(x, 3, 4)) +  # Display only the last two digits of the year
  scale_color_discrete(name = "Keywords") +  # Legend title for the keywords
  theme_minimal()  # Use a minimal theme for cleaner visualization


# Word Association Analysis


# Load necessary libraries
library(tm)
library(Matrix)
library(ggplot2)

# Define the keywords you want to track
ma_keywords <- c("mergersandacquisitions", "duediligence", "transaction", "merger", "acquisition", "target", 
                 "takeover", "buyside", "sellside", "acquire", "valuation", "invest", "diligence", "deal")
esg_keywords <- c("esg", "environment", "social", "governance")
esg_focus_keyword <- c("esg")

# Merge the two keyword vectors
keywords <- c(ma_keywords, esg_keywords)

# Print the merged list of keywords
print(keywords)

# Initialize a list to store co-occurrence matrices by year
association_matrices <- list()

# List of corpora names
corpora <- list(
  '2010' = corpus2010,
  '2011' = corpus2011,
  '2012' = corpus2012,
  '2013' = corpus2013,
  '2014' = corpus2014,
  '2015' = corpus2015,
  '2016' = corpus2016,
  '2017' = corpus2017,
  '2018' = corpus2018,
  '2019' = corpus2019,
  '2020' = corpus2020,
  '2021' = corpus2021,
  '2022' = corpus2022,
  '2023' = corpus2023
)

# Loop through each year to create DTM and co-occurrence matrices
for (year in names(corpora)) {
  
  # Access the corpus for the specific year
  corpus <- corpora[[year]]
  
  # Create a Document-Term Matrix with only the keywords
  dtm <- DocumentTermMatrix(corpus, control = list(dictionary = keywords))
  
  # Convert to a matrix and calculate co-occurrence (dtm transpose %*% dtm)
  dtm_matrix <- as.matrix(dtm)
  co_occurrence_matrix <- t(dtm_matrix) %*% dtm_matrix
  
  # Normalize by the number of documents
  normalized_matrix <- co_occurrence_matrix / nrow(dtm_matrix)
  
  # Store the matrix in the list
  association_matrices[[year]] <- normalized_matrix
}




# Now let's extract and plot the association over years for specific keyword pairs

keyword_pairs_esg_ma <- list(
  c('esg', 'mergersandacquisitions'), c('esg', 'duediligence'), c('esg', 'transaction'), 
  c('esg', 'merger'), c('esg', 'acquisition'), c('esg', 'target'), 
  c('esg', 'takeover'), c('esg', 'buyside'), c('esg', 'sellside'), 
  c('esg', 'acquire'), c('esg', 'valuation'), c('esg', 'invest'), 
  c('esg', 'diligence'), c('esg', 'deal'))

years <- names(corpora)

# Prepare a data frame for plotting
plot_data <- data.frame()

for (pair in keyword_pairs_esg_ma) {
  associations <- sapply(association_matrices, function(mat) {
    if (all(pair %in% colnames(mat))) {
      mat[pair[1], pair[2]]
    } else {
      NA  # If a pair doesn't exist in that year
    }
  })
  
  # Add to the data frame
  plot_data <- rbind(plot_data, data.frame(
    Year = as.numeric(years),
    Association = associations,
    Pair = paste(pair[1], "and", pair[2])
  ))
}

# Plot the associations over time
ggplot(plot_data, aes(x = Year, y = Association, color = Pair)) +
  geom_line() +
  geom_point() +
  labs(title = "Association of ESG and M&A Keywords Over Time",
       x = "Year", y = "Association Strength") +
  theme_minimal()

#Lets repeat the process for other keyword pairs

keyword_pairs_env_ma <- list(
  c('environment', 'mergersandacquisitions'), c('environment', 'duediligence'), c('environment', 'transaction'), 
  c('environment', 'merger'), c('environment', 'acquisition'), c('environment', 'target'), 
  c('environment', 'takeover'), c('environment', 'buyside'), c('environment', 'sellside'), 
  c('environment', 'acquire'), c('environment', 'valuation'), c('environment', 'invest'), 
  c('environment', 'diligence'), c('environment', 'deal'))

years <- names(corpora)

# Prepare a data frame for plotting
plot_data <- data.frame()

for (pair in keyword_pairs_env_ma) {
  associations <- sapply(association_matrices, function(mat) {
    if (all(pair %in% colnames(mat))) {
      mat[pair[1], pair[2]]
    } else {
      NA  # If a pair doesn't exist in that year
    }
  })
  
  # Add to the data frame
  plot_data <- rbind(plot_data, data.frame(
    Year = as.numeric(years),
    Association = associations,
    Pair = paste(pair[1], "and", pair[2])
  ))
}

# Plot the associations over time
ggplot(plot_data, aes(x = Year, y = Association, color = Pair)) +
  geom_line() +
  geom_point() +
  labs(title = "Association of Environment and M&A Keywords Over Time",
       x = "Year", y = "Association Strength") +
  theme_minimal()


# Now let's extract and plot the association over years for specific keyword pairs

keyword_pairs_gov_ma <- list(
  c('governance', 'mergersandacquisitions'), c('governance', 'duediligence'), c('governance', 'transaction'), 
  c('governance', 'merger'), c('governance', 'acquisition'), c('governance', 'target'), 
  c('governance', 'takeover'), c('governance', 'buyside'), c('governance', 'sellside'), 
  c('governance', 'acquire'), c('governance', 'valuation'), c('governance', 'invest'), 
  c('governance', 'diligence'), c('governance', 'deal')
)

years <- names(corpora)

# Prepare a data frame for plotting
plot_data <- data.frame()

for (pair in keyword_pairs_gov_ma) {
  associations <- sapply(association_matrices, function(mat) {
    if (all(pair %in% colnames(mat))) {
      mat[pair[1], pair[2]]
    } else {
      NA  # If a pair doesn't exist in that year
    }
  })
  
  # Add to the data frame
  plot_data <- rbind(plot_data, data.frame(
    Year = as.numeric(years),
    Association = associations,
    Pair = paste(pair[1], "and", pair[2])
  ))
}

# Plot the associations over time
ggplot(plot_data, aes(x = Year, y = Association, color = Pair)) +
  geom_line() +
  geom_point() +
  labs(title = "Association of Governance and M&A Keywords Over Time",
       x = "Year", y = "Association Strength") +
  theme_minimal()


# Now let's extract and plot the association over years for specific keyword pairs

keyword_pairs_social_ma <- list(
  c('social', 'mergersandacquisitions'), c('social', 'duediligence'), c('social', 'transaction'), 
  c('social', 'merger'), c('social', 'acquisition'), c('social', 'target'), 
  c('social', 'takeover'), c('social', 'buyside'), c('social', 'sellside'), 
  c('social', 'acquire'), c('social', 'valuation'), c('social', 'invest'), 
  c('social', 'diligence'), c('social', 'deal')
)

years <- names(corpora)

# Prepare a data frame for plotting
plot_data <- data.frame()

for (pair in keyword_pairs_social_ma) {
  associations <- sapply(association_matrices, function(mat) {
    if (all(pair %in% colnames(mat))) {
      mat[pair[1], pair[2]]
    } else {
      NA  # If a pair doesn't exist in that year
    }
  })
  
  # Add to the data frame
  plot_data <- rbind(plot_data, data.frame(
    Year = as.numeric(years),
    Association = associations,
    Pair = paste(pair[1], "and", pair[2])
  ))
}

# Plot the associations over time
ggplot(plot_data, aes(x = Year, y = Association, color = Pair)) +
  geom_line() +
  geom_point() +
  labs(title = "Association of Social and M&A Keywords Over Time",
       x = "Year", y = "Association Strength") +
  theme_minimal()


#Combined Word Association Exercise


# Initialize an empty data frame to hold the combined association strengths for each ESG keyword
combined_plot_data <- data.frame()

# Define the ESG-related keywords
esg_focus_keywords <- c('esg', 'environment', 'governance', 'social')

# Loop through each ESG keyword and calculate combined association strength for all M&A-related keywords
for (esg_keyword in esg_focus_keywords) {
  
  # Initialize an empty vector to hold the combined association strength for each year
  combined_association <- numeric(length(years))
  
  # Loop through each year
  for (i in seq_along(years)) {
    year <- years[i]
    
    # Initialize a sum for the combined association strength for this ESG keyword in this year
    sum_association <- 0
    count <- 0
    
    # Loop through all M&A-related keywords and sum the association strengths
    for (ma_keyword in ma_keywords) {
      if (all(c(esg_keyword, ma_keyword) %in% colnames(association_matrices[[year]]))) {
        sum_association <- sum_association + association_matrices[[year]][esg_keyword, ma_keyword]
        count <- count + 1
      }
    }
    
    # Calculate the average (or sum) association strength for this year
    combined_association[i] <- sum_association / count  # or just sum_association if you want to use the sum
  }
  
  # Add the combined association data to the plot data
  combined_plot_data <- rbind(combined_plot_data, data.frame(
    Year = as.numeric(years),
    Association = combined_association,
    ESG = esg_keyword
  ))
}

# Plot the combined association strength over time for each ESG keyword
ggplot(combined_plot_data, aes(x = Year, y = Association, color = ESG)) +
  geom_line() +
  geom_point() +
  labs(title = "Combined Association Strength of ESG, Environment, Governance, and Social with M&A Keywords",
       x = "Year", y = "Combined Association Strength") +
  theme_minimal()

View(combined_plot_data)



# Heat Map Visualization

install.packages("reshape2")
library(reshape2)

# Assuming combined_plot_data has "Year", "Association", and "ESG" columns
# Reshape the data to wide format using dcast

heatmap_data <- dcast(combined_plot_data, Year ~ ESG, value.var = "Association")

# Print the reshaped data to confirm it looks correct
print(heatmap_data)

# Create the heatmap using ggplot2
ggplot(melt(heatmap_data, id.vars = "Year"), aes(x = Year, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Combined Association Strength of ESG and M&A Keywords",
       x = "Year", y = "ESG Keywords", fill = "Association Strength") +
  theme_minimal()


# Bubble Chart Visualization

ggplot(combined_plot_data, aes(x = Year, y = ESG, size = Association, color = ESG)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Bubble Chart of ESG and M&A Keyword Association Strength",
       x = "Year", y = "ESG Keywords", size = "Association Strength") +
  theme_minimal()

#Box Plot Visualization

ggplot(combined_plot_data, aes(x = ESG, y = Association, fill = ESG)) +
  geom_boxplot() +
  labs(title = "Distribution of Association Strengths for ESG Keywords",
       x = "ESG Keywords", y = "Association Strength") +
  theme_minimal()

#Bar Chart Visualization

ggplot(combined_plot_data, aes(x = ESG, y = Association, fill = ESG)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Year) +
  labs(title = "Association Strength of ESG with M&A Keywords by Year",
       x = "ESG Keywords", y = "Association Strength") +
  theme_minimal()





