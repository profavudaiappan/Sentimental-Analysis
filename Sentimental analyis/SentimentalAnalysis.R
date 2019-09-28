getwd()
setwd(choose.dir())
getwd()

install.packages('plyr')
install.packages('dplyr')
install.packages('e1071')
install.packages('twitteR')
install.packages('ROAuth')
install.packages('SentimentAnalysis')
install.packages('sentimentr')
install.packages('ggplot2')
install.packages('stringr')
install.packages('wordcloud')
install.packages('tokenizers')
install.packages('tm')
install.packages('RColorBrewer')
install.packages('RCurl')
install.packages('RJSONIO')
install.packages('qdapDictionaries')
install.packages('RSentiment')
install.packages('httk')
install.packages('httpuv')
install.packages('tidyverse')
install.packages('text2vec')
install.packages('caret')
install.packages('glmnet')
install.packages('ggrepel')
install.packages('purrr')
install.packages('purrrlyr')

library(plyr)
library(dplyr)
library(e1071)
library(twitteR)
library(ROAuth)
library(SentimentAnalysis)
library(sentimentr)
library(ggplot2)
library(stringr)
library(wordcloud)
library(tokenizers)
library(tm)
library(RColorBrewer)
library(RCurl)
library(RJSONIO)
library(qdapDictionaries)
library(RSentiment)
require('devtools')
require('SentimentAnalysis')
library(httpuv)
library(tidyverse)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)
library(purrr)
library(purrrlyr)

#PROVIDE USER AUTHENTICATION
consumer_key<- "f81UjW3gzLk5n5OfhC1yEymdb"
consumer_secret<- "	630rNg1ykPhN7JJtpMmRqZckXekSqdxxLwBhPmISOuhBC2MFKM"
access_token<- "941580010841878528-R8CRUlinKSMC923b8Ixioi5bqhbSeLh"
access_secret<- "vzvSHuQ2qJFZikpiOqcCmKvaE9gmIS8Nn85keo6xzX07h"
requrl<- "https://api.twitter.com/oauth/request_token"

authurl<- "https://api.twitter.com/oauth/authorize"
accessurl<- "https://api.twitter.com/oauth/access_token"
setup_twitter_oauth(consumer_key="f81UjW3gzLk5n5OfhC1yEymdb",
                    consumer_secret="630rNg1ykPhN7JJtpMmRqZckXekSqdxxLwBhPmISOuhBC2MFKM", 
                    access_token=NULL, access_secret=NULL)

#TWEETS RETRIEVAL FROM TWITTER API
sentiment_tweets = searchTwitter('#BJP',n=2500,lang = "en")
sentiment_txt = sapply(sentiment_tweets,function(t) t$getText())
sentiment_txt<- str_replace_all(sentiment_txt,"[^[:graph:]]", " ") 
sentiment_txt = gsub("&amp", " ",sentiment_txt)
# tolower
sentiment_txt = tolower(sentiment_txt)
# remove rt
sentiment_txt = gsub("rt", " ", sentiment_txt)
sentiment_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",sentiment_txt)
# remove at
sentiment_txt = gsub("@\\w+", " ", sentiment_txt)
# remove punctuation
sentiment_txt = gsub("[[:punct:]]", " ", sentiment_txt)
# remove numbers
sentiment_txt = gsub("[[:digit:]]", " ", sentiment_txt)
# remove links http
sentiment_txt = gsub("http\\w+", " ", sentiment_txt)
# Get rid of hashtags
sentiment_txt = gsub("#[a-z,A-Z]*"," ",sentiment_txt)
# Get rid of references to other screennames
sentiment_txt = gsub("@[a-z,A-Z]*"," ",sentiment_txt)
# Get rid of stopwords,commonwords,prepositions,interjections
sentiment_txt<- removeWords(sentiment_txt,c(stopwords("english")))
sentiment_txt<- removeWords(sentiment_txt,c(preposition))
# remove tabs 
sentiment_txt = gsub("[ |\t]{2,}", " ", sentiment_txt)
# remove \n
sentiment_txt = gsub("[\r\n]", "", sentiment_txt)
sentiment_txt = gsub("\\W*\\b\\w\\b\\W*", " ", sentiment_txt)
# remove blank spaces at the beginning
sentiment_txt = gsub("^ ", "", sentiment_txt)
# remove blank spaces at the end
sentiment_txt = gsub(" $", "", sentiment_txt)


tweets <- do.call("rbind", lapply(sentiment_tweets, as.data.frame))
noof_tweets = c(length(sentiment_txt))
sentiment_classification<- c(sentiment_txt)

#TWEETS CLEANING AND PREPROCESSING
score.sentiment = function(sentences,pos.words,neg.words,.progress= 'none')
{
  scores = laply(sentences,
                 function(sentiment_txt,pos.words,neg.words)
                 {
                   
                   tryTolower = function(x)
                   {
                     y=NA
                     try_error = tryCatch(tolower(x),
                                          error=function(e) e)
                     if(!inherits(try_error,"error"))
                       y=tolower(x)
                     return(y)
                   }
                   sentiment_txt = sapply(sentiment_txt,tryTolower)
                   word.list = str_split(sentiment_txt,"\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words,pos.words)
                   neg.matches = match(words,neg.words)
                   pos.matches= !is.na(pos.matches)
                   neg.matches= !is.na(neg.matches)
                   score=sum(pos.matches)-sum(neg.matches)
                   return(score)
                 }, pos.words,neg.words, .progress=.progress)
  scores.df = data.frame(text=sentences,score=scores)
  return(scores.df)
}  
sentiment_txt

#TRAINING DATA SET (COLLECTION OF POSITIVE AND NEGATIVE WORDS)
posText<- read.delim("pwords1.txt",header=TRUE,sep="\t",blank.lines.skip=TRUE,
                     encoding="UTF-8",na.strings="",stringsAsFactors = FALSE,skipNul = TRUE,
                     fill = T,quote = "",comment.char = "")
posText<- posText$v1
posText<- unlist(lapply(posText,function(x) {str_split(x,"\n")}))
negText<- read.delim("nwords1.txt",header=TRUE,sep="\t",blank.lines.skip=TRUE,
                     encoding="UTF-8",na.strings="",stringsAsFactors = FALSE,skipNul = TRUE,
                     fill = T,quote = "",comment.char = "")
negText<- negText$v1
negText<- unlist(lapply(negText,function(x) {str_split(x,"\n")}))
pos.words = scan('pwords1.txt', what = 'character',comment.char = ':')
neg.words = scan('nwords1.txt', what = 'character',comment.char = ':')

#SENTIMENT ANALYSIS 
scores = score.sentiment(sentiment_classification,pos.words,neg.words,.progress = 'text')
scores$sentiment_classification = factor(rep(c("sentimental_mining"),noof_tweets))
scores$positive<- as.numeric(scores$score>0)
scores$negative<- as.numeric(scores$score<0)
scores$neutral<- as.numeric(scores$score==0)
print(scores$positive)
print(scores$negative)
print(scores$neutral)

sentiment_text_out<- subset(scores,scores$sentiment_classification=='sentimental_mining')
sentiment_text_out
tweetfile<- cbind.data.frame (sentiment_text_out$score,tweets$id,tweets$created,tweets$screenName, sentiment_txt)
colnames(tweetfile)<-NULL
write.csv(tweetfile,file="tweetfile.CSV",row.names = FALSE)
sentiment_text_out$polarity<- ifelse(sentiment_text_out$score> 0,"positive",
                                     ifelse(sentiment_text_out$score< 0,"negative",
                                            ifelse(sentiment_text_out$score == 0,"neutral",0)))
qplot(factor(polarity),data = sentiment_text_out,geom = "bar",fill=factor(polarity))+
  xlab("polarity_categories")+ylab("frequency")+ggtitle("sentiments")

#CNN
# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

tweets_classified <- read_csv('tweetfile.csv',
                              col_names = c('sentiment', 'id', 'date', 'user', 'text')) %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))

# data splitting on train and test

set.seed(100)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.6,list = FALSE, times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = tweets_train$id,
                   progressbar = TRUE)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = tweets_test$id,
                  progressbar = TRUE)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf, y = tweets_train[['sentiment']], 
                               family = 'binomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # k-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

#plot(glmnet_classifier)
#print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
glmnet:::auc(as.numeric(tweets_test$sentiment), preds)

# save the model for future using
saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')

df_tweets <- twListToDF(sentiment_tweets) %>% dmap_at('text', conv_fun)

# preprocessing and tokenization
it_tweets <- itoken(sentiment_txt,preprocessor = prep_fun,tokenizer = tok_fun,ids = df_tweets$id,progressbar = TRUE)

# creating vocabulary and document-term matrix
dtm_tweets <- create_dtm(it_tweets, vectorizer)

# transforming data with tf-idf
dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)

# loading classification model
glmnet_classifier <- readRDS('glmnet_classifier.RDS')

# predict probabilities of positiveness
preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]

# adding rates to initial dataset
df_tweets$sentiment <- preds_tweets


# color palette
cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")

set.seed(932)
samp_ind <- sample(c(1:nrow(df_tweets)), nrow(df_tweets) * 0.1) # 10% for labeling

# plotting
ggplot(df_tweets, aes(x = created, y = sentiment, color = sentiment)) +
  theme_minimal() +
  scale_color_gradientn(colors = cols, limits = c(0, 1),
                        breaks = seq(0, 1, by = 1/4),
                        labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
                        guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_point(aes(color = sentiment), alpha = 0.8) +
  geom_hline(yintercept = 0.75, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_hline(yintercept = 0.25, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  geom_label_repel(data = df_tweets[samp_ind, ],
                   aes(label = round(sentiment, 2)),
                   fontface = 'bold',
                   size = 2.5,
                   max.iter = 100) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
  ggtitle("Probability of positiveness")

#total score calculation: positive / negative / neutral
df_tweets$created <- as.Date(df_tweets$created , format = "%m/%d/%Y")
df_tweets <- mutate(df_tweets, score=ifelse(df_tweets$sentiment > 0.75, 'positive', ifelse(df_tweets$sentiment < 0.25, 'negative', 'neutral')))
by.tweet <- group_by(df_tweets, score, created)
by.tweet <- summarise(by.tweet, number=n())
posc<-df_tweets$sentiment > 0.90
negc<-df_tweets$sentiment < 0.10
#Sentimentanalysis using Dictionary based approach
mat <- confusionMatrix(sentiment_text_out$positive,sentiment_text_out$negative)
mat
precision <- mat$byClass['Pos Pred Value'] 
recall <- mat$byClass['Sensitivity']
accuracy<-round(mat$overall["Accuracy"]*100)

#CNN analysis
cnn <- confusionMatrix(posc,negc)
cnn
mat1 <- as.matrix(preds_tweets,what="overall")
print("Percentage of positiveness:")
colMeans(round(mat1))*100
precision1 <- cnn$byClass['Pos Pred Value'] 
recall1 <- cnn$byClass['Sensitivity']
accuracy1<-round(cnn$overall["Accuracy"]*100)

#Print values
precision
precision1
recall
recall1
accuracy
accuracy1
