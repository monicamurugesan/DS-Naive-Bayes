
sms_spam <- read.csv(file.choose(), stringsAsFactors = FALSE)
# examine the structure of the sms data
str(sms_spam)
# convert spam/ham to factor.
sms_spam$type <- factor(sms_spam$type)
# build a corpus using the text mining (tm) package
library(tm)
sms_corpus <- Corpus(VectorSource(sms_spam$text))
# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean)
# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)

# creating training and test datasets
sms_raw_train <- sms_spam[1:4169, ]
sms_raw_test  <- sms_spam[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# word cloud visualization
library(wordcloud)


wordcloud(sms_corpus_train, min.freq = 30)
# subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 100, scale = c(3, .2),colors = 'blue')
wordcloud(ham$text, max.words = 100, scale = c(3, 0.2))

# indicator features for frequent words
sms_dict<-findFreqTerms(sms_dtm_train, 3)
#sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

confusionMatrix(table(sms_test_pred, sms_raw_test$type))


##  model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
confusionMatrix(table(sms_test_pred2, sms_raw_test$type))
