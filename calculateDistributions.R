# script which calcualtes distributions from the govdocs dataset 

library(ggplot2)
source('utils.R')

tsvPath <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/metadata.tsv"
fileMetadata <- read.table(tsvPath, header=TRUE, sep="\t", stringsAsFactors = FALSE)

fileMetadata$numPage <- as.numeric(fileMetadata$numPage)
fileMetadata$numParag <- as.numeric(fileMetadata$numParag)
fileMetadata$numTables <- as.numeric(fileMetadata$numTables)
fileMetadata$numWords <- as.numeric(fileMetadata$numWords)

paragPageScatter <- ggplot(fileMetadata, aes(x=numPage, y=numParag)) + geom_point() +
  scale_x_continuous(limits = c(0,5000)) + 
  scale_y_continuous(limits = c(0,5000))
paragPageScatter

wordParagScatter <- ggplot(fileMetadata, aes(x=numWords, y=numParag)) + geom_point() +
  scale_x_continuous(limits = c(0,50000)) + 
  scale_y_continuous(limits = c(0,5000))
wordParagScatter


paragHist <- ggplot(fileMetadata, aes(x=numParag)) + geom_histogram() 
paragHist

pageHist <- ggplot(fileMetadata, aes(x=numPage)) + geom_histogram()
pageHist




