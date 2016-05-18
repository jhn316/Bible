library(readr)
library(stringr)

#skip all the comments at the top and bottom
rawKALEVALA <- read_lines("./pg7000.txt", skip = 87, n_max = 23482-375) 
noquotesKALEVALA <- gsub("\"","", rawKALEVALA[nchar(rawKALEVALA)!=0])
nospacesKALEVALA <- sub("^\\s+", "", noquotesKALEVALA)

#Make chunks of 10 
chunksKALEVALA <- c(" ")
for (i in seq_along(nospacesKALEVALA)) {
  if (i%%10 == 1) chunksKALEVALA[ceiling(i/10)] <- str_c(nospacesKALEVALA[i], 
                                                nospacesKALEVALA[i+1],
                                                nospacesKALEVALA[i+2],
                                                nospacesKALEVALA[i+3],
                                                nospacesKALEVALA[i+4],
                                                nospacesKALEVALA[i+5],
                                                nospacesKALEVALA[i+6],
                                                nospacesKALEVALA[i+7],
                                                nospacesKALEVALA[i+8],
                                                nospacesKALEVALA[i+9], sep = " ")
}

tail(chunksKALEVALA)
head(chunksKALEVALA)

library(SnowballC)

test3 <- wordStem(unlist(strsplit(test2," ")), language = "finnish")

#do.call(merge.xts, c( lapply(list.of.tickers, Yahoo.Fetch), all=FALSE ))

test4 <- do.call(function(x) wordStem(x, language = "finnish"),  strsplit(test2," "))

stemsKALEVALA <- data.frame(wordStem(unlist(strsplit(chunksKALEVALA[1]," ")), language = "finnish"))
for (idx in 2:length(chunksKALEVALA)){
  rbind(stemsKALEVALA, wordStem(unlist(strsplit(chunksKALEVALA[idx]," ")), language = "finnish"))
}
