# Loading/Installing required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tm, SnowballC, wordcloud, DT, syuzhet, dplyr, ggplot2, data.table)

########## Obatining Financial Data on Top NASDAQ and NYSE conpanies ##########

top_nasdaq = read.csv('/Users/huanghaotian/Desktop/Top NASDAQ.csv')
str(top_nasdaq)
top_nasdaq$Symbol = as.character(top_nasdaq$Symbol)
nasdaq_ticker_list = top_nasdaq$Symbol

summary_nasdaq = read.csv('/Users/huanghaotian/Desktop/Web Scraping Project/summary.csv')


# colnames(x = summary_nasdaq) = c('previous_close','bid','volume','52_wk_range','EPS','PE_ratio','earnings_date',
#                                  'beta', 'ex_div_date','avg_volumn','market_cap','open','div_yield','1yr_target',
#                                  'day_range','ask')

summary_nasdaq$ticker_name = nasdaq_ticker_list[1:300]
summary_nasdaq$company_name = top_nasdaq$Name[1:300]
summary_nasdaq$sector = top_nasdaq$Sector[1:300]
summary_nasdaq$industry = top_nasdaq$Industry[1:300]
# summary_nasdaq$latest_price = top_nasdaq$LastSale[1:105]
summary_nasdaq$EPS = as.character(summary_nasdaq$EPS)
summary_nasdaq$previous_close = as.character(summary_nasdaq$previous_close)
summary_nasdaq$PE_ratio = as.character(summary_nasdaq$PE_ratio)
summary_nasdaq$open_price = as.character(summary_nasdaq$open_price)


summary_nasdaq = summary_nasdaq[, c(17,18,19,20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

# Testing: use dplyr to fetch data needed
TSLA = summary_stocks %>% filter(ticker_name == 'BABA') %>% select(earnings_date)




###########################################################################################################

top_nyse = read.csv('/Users/huanghaotian/Desktop/Top NYSE.csv')
str(top_nyse)
top_nyse$Symbol = as.character(top_nyse$Symbol)
nyse_ticker_list = top_nyse$Symbol

summary_nyse = read.csv('/Users/huanghaotian/Desktop/Web Scraping Project/summary1.csv')

# colnames(x = summary_nyse) = c('previous_close','bid','volume','52_wk_range','EPS','PE_ratio','earnings_date',
#                                'beta', 'ex_div_date','avg_volumn','market_cap','open','div_yield','1yr_target',
#                                'day_range','ask')

summary_nyse$ticker_name = nyse_ticker_list[1:300]
summary_nyse$company_name = top_nyse$Name[1:300]
summary_nyse$sector = top_nyse$Sector[1:300]
summary_nyse$industry = top_nyse$Industry[1:300]
# summary_nyse$latest_price = top_nyse$LastSale[1:105]
summary_nyse$EPS = as.character(summary_nyse$EPS)
summary_nyse$previous_close = as.character(summary_nyse$previous_close)
summary_nyse$PE_ratio = as.character(summary_nyse$PE_ratio)
summary_nyse$open_price = as.character(summary_nyse$open_price)


summary_nyse = summary_nyse[, c(17,18,19,20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]


#################### JOIN NYSE AND NASDAQ ####################

summary_stocks = rbind(summary_nasdaq, summary_nyse)


###########################################################################################################

# Reviews on Stock
stock_review = read.csv('/Users/huanghaotian/Desktop/Web Scraping Project/selenium/reviews.csv')

stock_review = stock_review[!duplicated(stock_review), ]

colnames(x = stock_review) = c('thumbsup#','thumbsdown#','time','username','content')

stock_review = stock_review[, c(4,3,1,2,5)]

# aapl_review = aapl_review[, c(2,3,4,5,1)]

str(stock_review)
stock_review$content = as.character(stock_review$content)


# General Cleaning Work
stock_review$content = gsub("[[:punct:]]", "", stock_review$content) # remove punctuations
stock_review$content = gsub("<p>", "", stock_review$content) # remove junk charactors
stock_review$content = gsub("\n", " ", stock_review$content)
stock_review$content

############ Step 1 ############
# Make tm source - corpus
# The main structure for managing documents in tm is a so-called Corpus, representing a collection of text
# documents. A corpus is an abstract concept, and there can exist several implementations in parallel. The
# default implementation is the so-called VCorpus (short for Volatile Corpus) which realizes a semantics as known
# from most R objects: corpora are R objects held fully in memory.

step1 <- Corpus(VectorSource(stock_review$content))
step1$content[1:5]

############ Step 2 ############
# Change all to lower case

step2 <- tm_map(step1, content_transformer(tolower))
step2$content[1:5]

############ Step 3 ############
# Delete "stop words" from the document
# Stop word can be read from existed list stopwords("en")
length(stopwords("en"))
head(stopwords("en"))

step3 <- tm_map(step2, removeWords, stopwords("en"))
step3$content[1:5]

# Stemming words
# It will transfer word into its root. 
# add_step <- tm_map(step3, stemDocument, language = "english") 
# add_step$content[1:5]

############ Step 4 ############
# Get the result of word & its frequency

step4 = TermDocumentMatrix(step3)
step4 = as.matrix(step4)
step4 = as.data.frame(step4)
step4$freq = rowSums(step4)
step4[, 1:(dim(step4)[2]-1)] = NULL # only keeping the 'freq' column
head(step4)

wordcloud(words = rownames(step4), freq = step4$freq, min.freq = 4, colors=brewer.pal(8, "Dark2"))

get_sentiment(stock_review$content)
mean(get_sentiment(stock_review$content))
sentiment_data = get_nrc_sentiment(stock_review$content)

sentiment_sum = as.data.frame(colSums(prop.table(sentiment_data[, 1:8])))

setDT(sentiment_sum, keep.rownames = TRUE)[]
colnames(x = sentiment_sum) = c('emotion', 'percentage')
sentiment_sum$percentage = sentiment_sum$percentage * 100

ggplot(data = sentiment_sum, mapping = aes(sentiment_sum$emotion, sentiment_sum$percentage)) + 
              geom_bar(stat = "identity")+
              xlab('Emotion') +
              ylab('Percentage %') + ggtitle('AAPL NRC Sentiment')

#qplot(data = sentiment_sum, x = sentiment_data$anger)

# barplot(
#   sort(colSums(prop.table(get_nrc_sentiment(stock_review$content)[, 1:8]))),
#   horiz = FALSE,
#   cex.names = 0.2,
#   las = 0.5,
#   main = "Emotions in Sample text", xlab="Percentage"
# )

dim(get_sentiment_dictionary(dictionary = 'syuzhet'))

weighted.mean(stock_review$`thumbsup#`, get_sentiment(stock_review$content))

##########################################################################################

# Some Calculation
class(top_nasdaq$MarketCap)
sum(top_nasdaq$MarketCap)
sum(top_nasdaq$MarketCap[1:400])/sum(top_nasdaq$MarketCap)


class(top_nyse$MarketCap)
sum(top_nyse$MarketCap)
sum(top_nyse$MarketCap[1:400])/sum(top_nyse$MarketCap)
