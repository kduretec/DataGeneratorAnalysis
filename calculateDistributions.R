# script which calcualtes distributions from the govdocs dataset 

library(ggplot2)
source('utils.R')

tsvPath <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/metadata.tsv"

distFolder <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/distributions/"

fileMetadata <- read.table(tsvPath, header=TRUE, sep="\t", stringsAsFactors = FALSE)

fileMetadata$numPage <- as.numeric(fileMetadata$numPage)
fileMetadata$numParag <- as.numeric(fileMetadata$numParag)
fileMetadata$numTables <- as.numeric(fileMetadata$numTables)
fileMetadata$numWords <- as.numeric(fileMetadata$numWords)

paragPageScatter <- ggplot(fileMetadata, aes(x=numPage, y=numParag)) + geom_point() +
  scale_x_continuous(limits = c(0,50)) + 
  scale_y_continuous(limits = c(0,100))
path <- paste(distFolder, "paragPageScatter.png", sep="")
png(path, width=640, heigh=480)
  print(paragPageScatter)
dev.off()

wordParagScatter <- ggplot(fileMetadata, aes(x=numWords, y=numParag)) + geom_point() +
  scale_x_continuous(limits = c(0,50000)) + 
  scale_y_continuous(limits = c(0,5000))
path <- paste(distFolder, "wordParagScatter.png", sep="")
png(path, width=640, heigh=480)
  print(wordParagScatter)
dev.off()


#page distributions
binPath <- "input/pageBins.txt"
binFrame <- valueFrequencies(binPath, fileMetadata, "numPage")
saveBins(distFolder, "pageDist", binFrame)

#word distributions
binPath <- "input/wordBins.txt"
binFrame <- valueFrequencies(binPath, fileMetadata, "numWords")
saveBins(distFolder, "wordDist", binFrame)

#paragraph distributions
binPath <- "input/paragraphBins.txt"
binFrame <- valueFrequencies(binPath, fileMetadata, "numParag")
saveBins(distFolder, "paragDist", binFrame)

#table distributions
binPath <- "input/tableBins.txt"
binFrame <- valueFrequencies(binPath, fileMetadata, "numTables")
saveBins(distFolder, "tableDist", binFrame)
