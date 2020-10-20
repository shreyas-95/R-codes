require(caret)
require(tm)
require(wordcloud)
require(e1071)
require(MLmetrics)
rawData <-  read.csv("C:\\Users\\Minal\\Downloads\\New folder\\Naive Bayes\\sms_raw_NB.csv", 
                     header = FALSE, 
                     stringsAsFactors = FALSE)
str(rawData)
colnames(rawData) <- c("type", "text")
rawData$text <- iconv(rawData$text, to = "utf-8")
rawData$type <- factor(rawData$type)
summary(rawData)
table(rawData$type)
prop.table(table(rawData$type)) * 100
set.seed(1234)
trainIndex <- createDataPartition(rawData$type, p = .75, 
                                  list = FALSE, 
                                  times = 1)
trainData <- rawData[trainIndex,]
testData <- rawData[-trainIndex,]
prop.table(table(trainData$type)) * 100
prop.table(table(testData$type)) * 100
trainData_ham <- trainData[trainData$type == "ham",]
head(trainData_ham$text)

Sys.setlocale('LC_ALL','C')

tail(trainData_ham$text)
trainData_spam <- trainData[trainData$type == "spam",]
head(trainData_spam$text)

tail(trainData_spam$text)
trainData_spam <- NULL
trainData_ham <- NULL
corpus <- Corpus(VectorSource(trainData$text))
print(corpus)
corpus[[1]]$content
corpus[[2]]$content
corpus[[50]]$content
corpus[[100]]$content
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
#3. remove stopwords e.g. to, and, but, or (using predefined set of word in tm package)
corpus <- tm_map(corpus, removeWords, stopwords())
#4. remove punctuation
corpus <- tm_map(corpus, removePunctuation)
#5. normalize whitespaces
corpus <- tm_map(corpus, stripWhitespace)
#transformed
corpus[[1]]$content
## [1] "ok lar joking wif u oni"
corpus[[2]]$content
## [1] "u dun say early hor u c already say"
corpus[[50]]$content
## [1] " part checking iq"
corpus[[100]]$content

pal1 <- brewer.pal(9,"YlGn")
pal1 <- pal1[-(1:4)]

pal2 <- brewer.pal(9,"Reds")
pal2 <- pal2[-(1:4)]

#min.freq initial settings -> around 10% of the number of docs in the corpus (40 times)
par(mfrow = c(1,2))
wordcloud(corpus[trainData$type == "ham"], min.freq = 40, random.order = FALSE, colors = pal1)
wordcloud(corpus[trainData$type == "spam"], min.freq = 40, random.order = FALSE, colors = pal2)

sms_dtm <- DocumentTermMatrix(corpus, control = list(global = c(2, Inf)))
print(sms_dtm)
inspect(sms_dtm[1:10, 5:13])
sms_features <- findFreqTerms(sms_dtm, 5) #find words that appears at least 5 times
summary(sms_features)
##    Length     Class      Mode 
##      1224 character character
head(sms_features)
## [1] "abiola"  "able"    "abt"     "accept"  "access"  "account"
sms_dtm_train <- DocumentTermMatrix(corpus, list(global = c(2, Inf), dictionary = sms_features))
print(sms_dtm_train)
## <<DocumentTermMatrix (documents: 4182, terms: 1224)>>
## Non-/sparse entries: 24197/5094571
## Sparsity           : 100%
## Maximal term length: 15
## Weighting          : term frequency (tf)
convert_counts <- function(x){
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
  return (x)
}
sms_dtm_train <- apply(sms_dtm_train, MARGIN = 2, convert_counts)
head(sms_dtm_train[,1:5])
sms_classifier <- naiveBayes(sms_dtm_train, trainData$type)

sms_classifier[[2]][1:5]
corpus <- Corpus(VectorSource(testData$text))
#1. normalize to lowercase (not a standard tm transformation)
corpus <- tm_map(corpus, content_transformer(tolower))
#2. remove numbers
corpus <- tm_map(corpus, removeNumbers)
#3. remove stopwords e.g. to, and, but, or (using predefined set of word in tm package)
corpus <- tm_map(corpus, removeWords, stopwords())
#4. remove punctuation
corpus <- tm_map(corpus, removePunctuation)
#5. normalize whitespaces
corpus <- tm_map(corpus, stripWhitespace)
sms_dtm_test <- DocumentTermMatrix(corpus, list(global = c(2, Inf), dictionary = sms_features))
print(sms_dtm_test)
sms_dtm_test <- apply(sms_dtm_test, MARGIN = 2, convert_counts)
sms_dtm_test[1:10, 5:12]
sms_test_pred <- predict(sms_classifier, sms_dtm_test)
table(testData$type, sms_test_pred)
ConfusionMatrix(sms_test_pred, testData$type)
Accuracy(sms_test_pred, testData$type)
F1_Score(sms_test_pred, testData$type)
