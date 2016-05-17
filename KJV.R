library(readr)
library(stringr)
rawKJV <- read_lines("./pg30.txt", skip = 94, n_max = 114473-371) 
#Skip first 94 lines of commentary at the top
#used nmax=-1 to read to the bottom and manually found that 371 lines below are 
#commentary so we remove from bottom

#remove all "" marks and remove all those lines
noquotesKJV <- rawKJV[nchar(rawKJV)!=0]

#create levels for OT and NT - find where they being and end
which(grepl("Matthew", noquotesKJV)) #find where the Books start - NT starts at line 63600

#create levels for chapter names - find where they being and end
which(grepl("Book", noquotesKJV)) #find where each Book start
#create chunks of 10??

test <- rawKJV[1:20]

#remove verse numbers
nonumKJV <- sub("([0-9]+)\\:([0-9]+)\\:([0-9]+)","",noquotesKJV)

#remove leading spaces
nospaceKJV <- c(sub("^\\s+", "", nonumKJV), rep("",6))
nospaceKJV[1:20]
bookLocations<- grep("Book", nospaceKJV)

#Make chunks of 10 
chunksKJV <- c(" ")
for (i in seq_along(nospaceKJV)){
  if (i%in% bookLocations){
    chunksKJV[(length(chunksKJV))] <- nospaceKJV[i]
  }
  else{
    if ((i%%10 == 1)&(!(i%in% bookLocations))){
      idx <- length(chunksKJV)+1
      chunksKJV[idx] <- str_c(nospaceKJV[i], 
                                                nospaceKJV[i+1],
                                                nospaceKJV[i+2],
                                                nospaceKJV[i+3],
                                                nospaceKJV[i+4],
                                                nospaceKJV[i+5],
                                                nospaceKJV[i+6],
                                                nospaceKJV[i+7],
                                                nospaceKJV[i+8],
                                                nospaceKJV[i+9], sep = " ")
    }
  }
}
tail(chunksKJV)
head(chunksKJV)


#How to use  NRC Word-Emotion Association Lexicon -test 
library(syuzhet)
# (sentimentTotalsKJV <- colSums(get_nrc_sentiment(chunksKJV)))
# KJVnrc <- cbind(linenumber = seq_along(chunksKJV), get_nrc_sentiment(chunksKJV))
# a <- cbind(names(KJVnrc),sentimentTotalsKJV)

BookStart <- c()
for (i in 1:66){
  if (i<10){
    BookStart <- c(BookStart, grep(paste("Book 0", i, sep = ""), chunksKJV))
  }
  else{
    BookStart <- c(BookStart, grep(paste("Book ", i, sep = ""), chunksKJV))
  }
}
BookStart 

#Extract ith book (BookNum must be input by user)
#Call extractbook(n = booknumber)
extractbook <- function(BookNum){
  if (!BookNum %in% 1:66){
    stop("Enter a number between 1 and 66 inclusive")
  }
  else if (BookNum<=65){
    chunksKJV[BookStart[BookNum]:BookStart[BookNum+1]-1]
  }
  else
    chunksKJV[BookStart[BookNum]:length(chunksKJV)]
} 

#Book 1 to 39 is the Old Testament
#Book 40 to 66 is the New Testament
# Let’s make a Testament factor for the dataframe 
KJVnrc <- cbind(linenumber = seq_along(chunksKJV), get_nrc_sentiment(chunksKJV))

KJVnrc$testament <- "Old Testament"
KJVnrc[BookStart[40]:length(chunksKJV),'testament'] <- "New Testament"
KJVnrc$testament <- as.factor(KJVnrc$testament)


#Make a Book name factor for each book and restart a line number for each 
#new book

books <- c()
booknum <- c()
for(i in 1:66){
  books <- c(books,strsplit(chunksKJV[BookStart],"    ")[[i]][[3]])
  booknum <- c(booknum,strsplit(chunksKJV[BookStart],"    ")[[i]][[1]])
}

KJVnrc$book <- " "
for(j in BookStart){
  KJVnrc[j:length(chunksKJV),'book'] <- unlist(strsplit(chunksKJV[j],"    "))[3]
  KJVnrc[j:length(chunksKJV),'booknum'] <- unlist(strsplit(chunksKJV[j],"    "))[1]
}
KJVnrc$testament <- as.factor(KJVnrc$testament)
KJVnrc$book <- as.factor(KJVnrc$book)
KJVnrc$booknum <- as.factor(KJVnrc$booknum)

for(k in books){
  Booklength <- length(KJVnrc$linenumber[KJVnrc$book == k])
  KJVnrc$linenumber[KJVnrc$book == k] <- 1:Booklength
}

View(KJVnrc)

#Positive and Negative Sentiment
#Overall postive vs. negative sentiment in the text of KJV

library(dplyr)
library(reshape2)
KJVnrc$negative <- -KJVnrc$negative
# #By book
# KJVposneg <- KJVnrc %>% select(linenumber, book, positive, negative) %>% 
#   melt(id = c("linenumber", "book"))
# names(KJVposneg) <- c("linenumber", "book", "sentiment", "value")
# View(KJVposneg)

#By testament
KJVposneg <- KJVnrc %>% select(linenumber, testament, positive, negative) %>%
  melt(id = c("linenumber", "testament"))
names(KJVposneg) <- c("linenumber", "testament", "sentiment", "value")
View(KJVposneg)

library(ggplot2)
library(ggthemes)

ggplot(data = KJVposneg, aes(x = linenumber, y = value, color = sentiment)) +
  facet_wrap(~testament, nrow = 3) +
  geom_point(size = 4, alpha = 0.5) + theme_minimal() +
  ylab("Sentiment") + 
  ggtitle(expression(paste("Positive and Negative Sentiment in ", 
                           italic("King James Bible")))) +
  theme(legend.title=element_blank()) + 
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
    scale_color_manual(values = c("aquamarine3", "midnightblue"))

#As barchart
ggplot(data = KJVposneg, aes(x = linenumber, y = value, color = sentiment, fill = sentiment)) +
  facet_wrap(~testament, nrow = 3) +
  geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Positive and Negative Sentiment in ", 
                           italic("King James Bible")))) +
  theme(legend.title=element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(legend.justification=c(1,1), legend.position=c(1, 0.71)) 
  scale_fill_manual(values = c("aquamarine3", "midnightblue")) +
  scale_color_manual(values = c("aquamarine3", "midnightblue"))


# Make a new data frame to calculate the fourier transforms
#Uses get_sentiment() which returns numeric vector of sentiment values, one value for each input sentence.
KJVsentiment <- data.frame(cbind(linenumber = seq_along(chunksKJV), sentiment=get_sentiment(chunksKJV, method = "nrc")))

KJVsentiment$testament <- "Old Testament"
KJVsentiment[BookStart[40]:length(chunksKJV),'testament'] <- "New Testament"
KJVsentiment$testament <- as.factor(KJVsentiment$testament)

#get_transformed_values() -
# Converts input values into a standardized set of filtered and reverse transformed 
# values for easy plotting and/or comparison.
KJVft <- as.numeric(get_transformed_values(KJVsentiment$sentiment, 
                                             low_pass_size = 3,
                                             scale_vals = TRUE,
                                             scale_range = FALSE))
KJVft <- data.frame(cbind(linenumber = seq_along(KJVft), ft = KJVft))
View(KJVft)


#Plot the fourier transforms
ggplot(data = KJVft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in ", italic("King James Bible")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) #+
