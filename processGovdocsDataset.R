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


assignClass <- function(bins, value, nameV) {
  return (bins[value[[nameV]] >= bins$start & value[[nameV]] <= bins$end,]$code)
}

valueFrequencies <- function(fileBins, met, nameV) {
  bins <- read.table(fileBins, header=TRUE, sep="\t", stringsAsFactors = FALSE)
  bins$start <- as.numeric(bins$start)
  bins$end <- as.numeric(bins$end)
  bins$code <- as.numeric(bins$code)
  met$code <- apply(met, 1, function(x) assignClass(bins, x, nameV))
  met$code <- as.character(met$code)
  newFrame <- aggregate(met$code, list(code=met$code), FUN=length)
  
  newFrame <- merge(bins, newFrame, by="code")
  
  barPlot <- ggplot(newFrame, aes(x=title, y=x)) + geom_bar(stat="identity")
  barPlot
}





