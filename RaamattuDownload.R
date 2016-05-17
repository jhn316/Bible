#install packages
#install.packages("RCurl")
#install.packages("XML")
install.packages("RDSTK")


#load packages
library(RCurl)
library(XML)
library(RDSTK)

# assign input (could be a html file, a URL, html text, or some combination of all three is the form of a vector)
input <- getURL("http://www.evl.fi/raamattu/1933,38/1Moos.1.html")

# evaluate input and convert to text
txt <- html2text(input)
str(txt)
