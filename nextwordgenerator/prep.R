if(FALSE){

# NOTE: we are commenting out this entire script with: if(FALSE){}
# because the downloadable items are heavy and other processing features are time consuming
# this script has been run offline to generate the final n-gram database 
# and to write it into a .csv file (n2to5gram.csv) that will be read into the actual app

# to run the script, just comment out the if(FALSE){}

########################################################################################################
########################################################################################################
# FUNCTIONS
########################################################################################################

#********************************************************************
cleancorpus <- function(x){

# remove non-ASCII characters such as ×¤ö¼
x$content = iconv(x$content, from = "UTF-8", to = "ASCII", sub = "")

# convert all text to lower case, remove numbers

x = tm_map(x, content_transformer(tolower))
x = tm_map(x, removeNumbers)

#remove twitter handles, hashtags and URLs (http, https, ftp) 

x$content = gsub(" @\\S*", " ", x$content) 
x$content = gsub(" #\\S*", " ", x$content)  
x$content = gsub("(f|ht)(tp)(s?)(://)(\\S*)", " ", x$content) 

# remove punctuations (preserving apostrophes and intra-word hyphens), profanity, extra white spaces
# we do not intend to stem the words or remove stopwords

x = tm_map(x, removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE)
x = tm_map(x, removeWords, profanity)
x = tm_map(x, stripWhitespace)

x
}
#**********************************************************************

#**********************************************************************
processgram <- function(x){

# split each string into component words
if(ncol(x)==2){x <- data.frame(x$Freq, do.call(rbind, strsplit(as.character(x$Var1), split = " ")))}

# clean n-gram to remove "words" that are:
# only single letters such as "s", "m" (except "a" and "i", which are words in themselves)
# starting with hyphen
# repetition of single letters such as "aaaa", "bb"
# "ve"

for(i in 2:ncol(x)){
x <- x[-c(which(x[,i] %in% letters[-c(1,9)]), grep("^-|^([a-z])\\1+$|^ve$", x[,i])),]
}

# paste together all columns except the last - which will form the "next word"
x = data.frame(V1 = x[,1], V2 = apply(as.data.frame(x[,-c(1,ncol(x))]), 1, paste, collapse = " "), V3 = x[,ncol(x)])

# convert non-frequency columns to character
for(i in 2:3){x[,i] = as.character(x[,i])}

# sort
x = x[order(x$V2, -x$V1),]

# return
x
}
#**********************************************************************

#**********************************************************************
prunegram <- function(x){

# prune n-gram to lighten memory requirement by: 
# retaining only top-3 "next word" for any unique value of search,
# removing entries with frequency of just 1, and dropping frequency column

x = x[ave(x$V1, x$V2, FUN = seq_along) <= 3, ]
x[which(x$V1>1),c(2,3)]
}
#**********************************************************************

########################################################################################################
options(java.parameters = "- Xmx3000m")
library(tm)
library(RWeka)
library(openNLP)

# download data
fileURL = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(fileURL, "projectdata.zip")

# unzip data 
unzip("projectdata.zip")

# read data 
blogdata = readLines("final/en_US/en_US.blogs.txt")
newsdata = readLines("final/en_US/en_US.news.txt")
twitterdata = readLines("final/en_US/en_US.twitter.txt")

combineddata = c(blogdata, newsdata, twitterdata)

# release memory
rm("blogdata"); rm("newsdata"); rm("twitterdata")

# random sampling of 1% data
# we are unable to work with more due to computational limitations
set.seed(321321)
combineddata = sample(combineddata)

p = 0.01
rowtrain = which(rbinom(n = length(combineddata), size = 1, prob = p)==1)
trainset = combineddata[rowtrain]
rm(combineddata); rm(rowtrain)

# convert text into Corpus form
traincorpus = Corpus(VectorSource(trainset))
rm("trainset")

fileURL = "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
download.file(fileURL, "profanity.txt")
profanity = readLines("profanity.txt")

traincorpus = cleancorpus(traincorpus)

rm("profanity")

# 2-gram frequency
n2gramfreq = data.frame(table(NGramTokenizer(traincorpus$content, Weka_control(min = 2, max = 2))))
n2gramfreq = processgram(n2gramfreq)
n2gramfreq = prunegram(n2gramfreq)

# 3-gram frequency

n3gramfreq = data.frame(table(NGramTokenizer(traincorpus$content, Weka_control(min = 3, max = 3))))
n3gramfreq = processgram(n3gramfreq)
n3gramfreq = prunegram(n3gramfreq)

# 4-gram frequency

n4gramfreq = data.frame(table(NGramTokenizer(traincorpus$content, Weka_control(min = 4, max = 4))))
n4gramfreq = processgram(n4gramfreq)
n4gramfreq = prunegram(n4gramfreq)

# 5-gram frequency

n5gramfreq = data.frame(table(NGramTokenizer(traincorpus$content, Weka_control(min = 5, max = 5))))
n5gramfreq = processgram(n5gramfreq)
n5gramfreq = prunegram(n5gramfreq)

# n-gram database

n2to5gram = rbind(n2gramfreq, n3gramfreq, n4gramfreq, n5gramfreq)
rm("n2gramfreq"); rm("n3gramfreq"); rm("n4gramfreq"); rm("n5gramfreq"); rm("traincorpus")

# expanding our dictionary with Corpus of Contemporary American English / COCA's 
# 2-gram for better performance in accuracy and time-taken 

fileURL = "https://www.ngrams.info/coca/download/w2_.zip"
download.file(fileURL, "ext2gram.zip")
unzip("ext2gram.zip")
ext2gram = read.delim("w2_.txt", sep = "\t", header = FALSE)

ext2gram = processgram(ext2gram)
ext2gram = prunegram(ext2gram)
n2to5gram = rbind(n2to5gram, ext2gram)
n2to5gram = unique(n2to5gram)

# write n2to5gram database into csv to load at time of app launch
write.csv(n2to5gram, "n2to5gram.csv", row.names = FALSE)

# remove all files and folders now no longer required
itemsremove = c("projectdata.zip", "final", "profanity.txt", "ext2gram.zip", "w2_.txt")
unlink(itemsremove, recursive = TRUE)

rm(list = ls())

########################################################################################################
########################################################################################################

# below is the closure of the if(FALSE){}
}