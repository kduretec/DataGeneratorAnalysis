# script for analysing govdocs dataset 

library(ggplot2)
source('utils.R')

folderIN <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/vbsproperties/"

listFiles <- list.files(folderIN)

fileMetadata <- data.frame(fileName=character(), numPage=character(), numCh=character(), numWords=character(), 
                           numLines=character(), numParag=character(), numTables=character())

for (f in listFiles) {
  filNameList <- unlist(strsplit(f, ".", fixed=TRUE))
  name <- filNameList[1]
  extension <- filNameList[2]
  fileResult <- paste(folderIN, f, sep="")
  if (file.exists(fileResult)) {
    print(fileResult)
    tmp <- readVBSMetadata(fileResult, name)
    if (is.null(tmp)) next
    fileMetadata <- rbind(fileMetadata, tmp) 
  }
}

fileMetadata$numPage <- as.numeric(fileMetadata$numPage)
fileMetadata$numParag <- as.numeric(fileMetadata$numParag)
fileMetadata$numTables <- as.numeric(fileMetadata$numTables)
fileMetadata$numWords <- as.numeric(fileMetadata$numWords)

paragPageScatter <- ggplot(fileMetadata, aes(x=numPage, y=numParag)) + geom_point() +
  scale_x_continuous(limits = c(0,1000)) + 
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