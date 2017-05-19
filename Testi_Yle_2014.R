library(tm)
library(slam)

a <- Corpus( DirSource( "/Users/tlaurone/Documents/Digital_methods/Yle_kulttuuri/Tiedostot_v/2014_lemmatut", encoding = "UTF-8" ) )

stop <- scan('/Users/tlaurone/Documents/Digital_methods/LAS_testit/stopwordsyle-fi.txt', what = list(""), sep = '\n' )
stop <- c( stopwords("finnish") , stop , recursive=T )

## bunch of cleanup and transformations
a <- tm_map(a, removeNumbers )
a <- tm_map(a, stripWhitespace )
a <- tm_map(a, removePunctuation )
a <- tm_map(a, content_transformer(tolower) )
a <- tm_map(a, removeWords, stop )

## compute word frequencies
dtm <-DocumentTermMatrix(a)

dtmfrequency <- col_sums( dtm , na.rm = T )
frequency <- sort(frequency, decreasing=TRUE)

## choose removal boundaries for further data analysis

upper = Inf ## floor( length( frequency ) * .005 )
lower = floor( length( frequency) * .95 )
## upper = frequency[ upper ]
lower = frequency[ lower ]
## upper = as.integer( upper )
lower = as.integer( lower ) + 1

dtm2 = DocumentTermMatrix( a , control = list( bounds = list( global = c( lower, upper ) ) ) )

## throw away columns with 0 indicators
dtm3 <- dtm2[ row_sums( dtm2 ) > 0, ]

library(topicmodels)

burnin = 1000
iter = 1000
keep = 50

## after some googling:
raw.sum=apply(table,1,FUN=sum) #sum by raw each raw of the table
table=table[raw.sum!=0,] # delete 0-raws

model <- LDA( dtm , k = k, method = "Gibbs", control =  list(burnin = burnin, iter = iter, keep = keep) )

return( model )

