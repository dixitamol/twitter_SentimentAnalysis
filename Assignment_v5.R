rm(list=ls()) # clears all objects in "global environment"
cat("\014") # clears the console area
getwd()
########################################################################

### install packages and load Libraries
pkgs <- c("twitteR", "ROAuth", "httr", "tidyverse", "quanteda", "tm","wordcloud",
          "openNLP","openNLPdata","tidytext","dplyr","ggplot2","reshape","plotrix","stringr","plyr")
library(pacman)
pacman::p_load(char=pkgs,install=TRUE,character.only=TRUE)
## Installing OpenNLP package from local folder as it is not available for R 3.5.2
# install.packages("openNLPmodels.en_1.5-1.tar.gz", repos = NULL, type = "source")
library(openNLPmodels.en)
######################################################################## 

### twitterConnection

consumer_key <- 'yBZ9gWqoSGzC7dwhTuPquPtNi'
consumer_secret <- '1V9v6DJcauhDm32xQ4dZD6e0Y6CHVWO7SN48kO5bnmXnm6a9VC'
access_token <- '2806191819-Ej4Kpj1evGWD4IugbG3B7acqx9xcprHS8nxfvaf'
access_secret <- 'ikVFCYuMdj2W3d2yJOKQzMNp9FSxjXcMIB2PgfjkqP9SK'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

######################################################################## 

### searchTweet
## Below code is commented as the dataframe is made static post one extraction and used as is. Dataset is shared.

# fn_twitter <- searchTwitter("@Nike",n=2500,lang="en")
# fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame
# fn_twitter_df_1000 <- head(fn_twitter_df, n = 1000)
# write.csv(fn_twitter_df_1000, 'Raw_Data.csv')

######################################################################## 

### import_Select
nike <- read.csv("Raw_Data.csv", stringsAsFactors = FALSE) #read the CSV file

nike_eText <- as.vector(nike[,2]) # get only the relevant text i.e second column
nike_eText <- sapply(nike_eText,function(row) iconv(row, "latin1", "ASCII", sub="")) # convert the corpora to vector

######################################################################## 

### preprocessing
##Data preprocessing: POSIX characters Handling
nike_eText <- gsub("<.*>","",nike_eText) #1. get rid hex characters
nike_eText <- gsub("https.*","",nike_eText) #2. get rid of urls
nike_eText <- gsub("[\n\r]","",nike_eText) #3. get rid of new line and carriage return
nike_eText <- gsub("@[a-z,A-Z]*","",nike_eText) #4. get rid twitter names
nike_eText <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", nike_eText) # get rid of other odd chars

######################################################################## 

##consolidate the original text file the text into the single corpora
text <- paste(nike_eText,collapse = "")
## get the data into the Volatile corpus
corpus_Nike <- tm::VCorpus(VectorSource(nike_eText)) 

######################################################################## 

### textMining
corpus_L <- tm::tm_map(corpus_Nike, tm::content_transformer(tolower)) #1. convert the text to the lower case
corpus_Punc <- tm::tm_map(corpus_L, removePunctuation) #2. remove punc with tm package
corpus_Numbers <- tm::tm_map(corpus_Punc, tm::removeNumbers) #3. remving numbers from the text
stop <- c(stopwords('en'),'will','nike','shoe') # Creating a vector of common English stopwords and project related stop words
corpus_Stopwords <- tm::tm_map(corpus_Numbers, tm::removeWords, stop) #4 removing the stop words , command order is a key here.
corpus_RemoveSpace <- tm::tm_map(corpus_Stopwords, tm::stripWhitespace) #5 remove the space between words

######################################################################## 

### analysis
## starting the analyses the corpus
dtm <- tm::DocumentTermMatrix(corpus_RemoveSpace) #6 DTM counts the frequency of each word (min 3 characters)
tm::inspect(dtm) #examine the frequency of the words used in the document and disperseness

thewords_used <- dtm$dimnames$Terms #7 dtm: terms are here already in the system as dtm is used
wordsIndoc <- thewords_used[dtm$j] #8 two documents index numerator

selectDoc <- (dtm$i) #9 determine whether the word belongs to doc 1 here or 2
wordsInSelectedDoc <- wordsIndoc[selectDoc] #10 name and enumerate the words from the 1 st doc
# print(wordsInSelectedDoc) # print the results

freqConcepts <- dtm$v[selectDoc]  #11 determine the frequency of words in doc 1
lookInside <- data.frame(Term = wordsInSelectedDoc,freqConcepts = freqConcepts) #12 133 put the previous result into the table view
# print(lookInside)

######################################################################## 

### Stemmisation
corpus.stem <- tm::tm_map(corpus_RemoveSpace, tm::stemDocument, lang = "English") #13 get the stems of the words
# print(corpus.stem[[24]]$content) #Testing for random tweet24

dtm <- tm::DocumentTermMatrix(corpus.stem) # get the frequency of themmed doc twit 24 as an example

#  selectDoc <- (dtm$i)
# print(data.frame(Term = dtm$dimnames$Terms[dtm$j[selectDoc]],Freq = dtm$v[selectDoc]) ) #print the results as mentioned in 12
# ### dimentionality has been reduced from 23 words to 20

freq <- colSums(as.matrix(dtm)) # get the freq of words for the entire doc i.e. all tweets
# print(freq)

ordered <- freq[order(freq,decreasing = TRUE)] # get the freq of 10 most popular words
print(head(ordered,n = 10))  

### plot the results  xlab-ais names...  las-words in x axis, csx.names-fot size, las 2 vertical y axis, labeled x axis
# wordcloud(names(freq),freq,min.freq = 1) #word cloud with min freq of 1 word used
set.seed(423)
suppressWarnings(wordcloud(names(freq), freq, min.freq = 3,
          max.words = 200,random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2")))

# Plot word frequencies
barplot(ordered,cex.names = 1.0, las = 1, 
        main = "Most Frequent Words",
        ylab = "Frequency",xlab = "Terms")

## Explore frequent terms and their associations
# Printing terms appearing at least 30 times in the selected tweets
print(tm::findFreqTerms(dtm,lowfreq =30))

######################################################################## 

###posneg: Finding out the Positive and Negative words in the tweets
## Impoting positive and negative word sets by Prof. Minqing Hu and Bing Liu
pos.words <- scan('positive-words.txt', what='character', comment.char=';')
neg.words <- scan('negative-words.txt', what='character', comment.char=';')

#Adding project specific positive words to the word sets by Prof. Minqing Hu and Bing Liu
pos.words<-c(pos.words, 'good', 'best', 'love', 'loved', 'thnx', 'Grt', 
             'gr8', 'thank','thanks', 'trendy', 'awesome', 'nice', 'light','lightweight','nyc1','wonderful','comfortable','comfy','cool')
neg.words <- c(neg.words, 'shit', 'shitty', 'heavy', 'damn', 'no', 'not','bleh','boo')

######################################################################## 

##sentimentAnalysis
scSentiment <- function(sentences, pos.words, neg.words, .progress='none') #tweets prameterisd as a sentence
  {
  list<-lapply(sentences, function(sentence, pos.words, neg.words)
  {#Regular expressions to ensure that the received corpora is clear
    ##useful if Sentiment Analysis is run separately.
    sentence <- gsub('[[:punct:]]',' ',sentence)
    sentence <- gsub('[[:cntrl:]]','',sentence)
    sentence <- gsub('\\d+','',sentence)
    sentence <- gsub('\n','',sentence)
    
    sentence <- tolower(sentence) #pre processing for safety
    #bringing all tweets to a single list
    list_words <- str_split(sentence, '\\s+') # generating a word 'list' from the tweet
    #changing to vector
    unlist_word <- unlist(list_words) # unlisting the list for match action
    #Matching words with the positive and negative lists, which generates a binary result
    pmatch <- match(unlist_word, pos.words) 
    nmatch <- match(unlist_word, neg.words)
    # getting rid of the 0s (or non-matches in both variables)
    pmatch <- !is.na(pmatch) 
    nmatch <- !is.na(nmatch)
    # Count of postive and negative words in a tweet
    pp<-sum(pmatch)
    nn <- sum(nmatch)
    score <- sum(pmatch) - sum(nmatch) # Score of a tweet = (No. of Positive Words - No. of Negative Words)
    #Storing all three params in a list and returning from function
    list1<-c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  # Attaching the list elements to separate variables, and making separate dataframes of these
  score_new<-lapply(list, `[[`, 1)
  pp1=score=lapply(list, `[[`, 2)
  nn1=score=lapply(list, `[[`, 3)
  #Generating separate DFs for the threeech sentiment
  scores.df <- data.frame(score=score_new, text=sentences)
  positive.df <- data.frame(Positive=pp1, text=sentences)
  negative.df <- data.frame(Negative=nn1, text=sentences)
  
  # Returning all dataframes from the function call
  list_df<-list(scores.df, positive.df, negative.df)
  return(list_df)
}

result <- scSentiment(nike_eText, pos.words, neg.words)

######################################################################## 

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1<-result[[1]]
test1$text<-NULL
test2<-result[[2]]
test2$text<-NULL
test3<-result[[3]]
test3$text<-NULL

# Taking the sentiment scores in variable sc
ss1<-test1[1,]
ss2<-test2[1,]
ss3<-test3[1,]   #q1---ss1......qq1--ssc qq2--ssp...qq3--ssn
ssc<-melt(ss1, var='Score')
ssp<-melt(ss2, var='Positive')
ssn<-melt(ss3, var='Negative') 
ssc['Score'] <- NULL # the score
ssp['Positive'] <- NULL # the postive sentment
ssn['Negative'] <- NULL # the negative sentiment
# For Visualisation taking it into a data frame (with the scores)
table1 <- data.frame(Text=result[[1]]$text, Score=ssc)
table2 <- data.frame(Text=result[[2]]$text, Score=ssp)
table3 <- data.frame(Text=result[[3]]$text, Score=ssn)

#Merging all the three tabes into 1
combined<-data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

#Histogram showing the Positive and Negative words in the Tweets, and the overall score of Tweets on the Pos-Neg Scale
hist(combined$Positive, col=rainbow(10), xlab = 'Positives')
hist(combined$Negative, col=rainbow(10), xlab = 'Negatives')
hist(combined$Score, col=rainbow(10), xlab = 'Score of Tweets')

#Pie Chart of Positives-Negatives in the Tweets
pie <- c(sum(combined$Positive), sum(combined$Negative))
label <- c("Positive", "Negative")
suppressWarnings(pie(pie, labels = label, col=rainbow(length(label)),explode=0.00, main="Sentiment Analysis"))

######################################################################## 

#Positive Percentage

#Taking the sentiment out in separate variable
posSc<-combined$Positive #with +ive
negSc<-combined$Negative #with -ive

# Calc +ive %age
combined$PosPercent <- posSc/ (posSc+negSc)

# Removing Non-Numbers
pp <- combined$PosPercent
pp[is.nan(pp)] <- 0
combined$PosPercent <- pp

#Negative Percentage

# Calc -ive %age
combined$NegPercent <- negSc/ (posSc+negSc)

# Removing Non-Numbers
nn <- combined$NegPercent
nn[is.nan(nn)] <- 0
combined$NegPercent <- nn

##Finding out the scores for each level of Sentiment

#Good

Sc <- combined$Score

#Output of following is FALSE or TRUE
good <- sapply(Sc, function(Sc) Sc <= 3 && Sc > 0)
#Converts to actual value
# Sc[good]
list_good <- Sc[good]
value_good <- length(list_good)

#Very good

vgood <- sapply(Sc, function(Sc) Sc > 3)
#Converts to actual value
# Sc[vgood]
list_vgood <- Sc[vgood]
value_vgood <- length(list_vgood)

#Bad : Unsatisfactory

#Output of following is FALSE or TRUE
bad <- sapply(Sc, function(Sc) Sc >= -3 && Sc < 0)
#Converts to actual value
# Sc[bad]
list_bad <- Sc[bad]
value_bad <- length(list_bad)

#Very bad : Poor

#Output of following is FALSE or TRUE
vbad <- sapply(Sc, function(Sc) Sc < -3)
#Converts to actual value
# Sc[vbad]
list_vbad <- Sc[vbad]
value_vbad <- length(list_vbad)

#Neutral
neutral <- sapply(Sc, function(Sc) Sc == 0) 
list_neutral <- Sc[neutral]
value_neutral <- length(list_neutral)

slices1 <- c(value_good, value_bad , value_vgood , value_neutral , value_vbad )
lbls1 <- c("Good", "Poor", "Outstanding", "Neutral", "Awful")
pct <- round(slices1/sum(slices1)*100) #Percentage
lbls1 <- paste(lbls1, pct) # display pecent
lbls1 <- paste(lbls1,"%",sep="") # display pecent
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="Percentage of Tweets with Particular Sentiment")


