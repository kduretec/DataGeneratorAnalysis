# script which calcualtes distributions from the govdocs dataset 

library(ggplot2)
source('utils.R')

tsvPath <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/metadata.tsv"

distFolder <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/distributions/"

samplesPath <- paste(distFolder, "samples.tsv", sep="")
  
fileMetadata <- read.table(tsvPath, header=TRUE, sep="\t", stringsAsFactors = FALSE)

fileMetadata$numPage <- as.numeric(fileMetadata$numPage)
fileMetadata$numParag <- as.numeric(fileMetadata$numParag)
fileMetadata$numTables <- as.numeric(fileMetadata$numTables)
fileMetadata$numWords <- as.numeric(fileMetadata$numWords)

paragPageScatter <- ggplot(fileMetadata, aes(x=numPage, y=numParag)) + geom_point(alpha=0.3, color="#3182bd") +
  scale_x_continuous(limits = c(0,750)) + 
  scale_y_continuous(limits = c(0,30000)) + theme_bw()
path <- paste(distFolder, "paragPageScatter.png", sep="")
png(path, width=640, heigh=480)
  print(paragPageScatter)
dev.off()

wordParagScatter <- ggplot(fileMetadata, aes(x=numWords, y=numParag)) + geom_point(alpha = 0.3, color="#3182bd") +
#wordParagScatter <- ggplot(fileMetadata, aes(x=numWords, y=numParag)) + geom_bin2d() +
  scale_x_continuous(limits = c(0,50000)) + 
  scale_y_continuous(limits = c(0,5000)) + theme_bw()
path <- paste(distFolder, "wordParagScatter.png", sep="")
png(path, width=640, heigh=480)
  print(wordParagScatter)
dev.off()


#page distributions
binPath <- "input/pageBins.txt"
binFrameNPag <- valueFrequencies(binPath, fileMetadata, "numPage")
saveBins(distFolder, "pageDist", binFrameNPag)
binFrameNPag$feature <- "numPage"

#word distributions
binPath <- "input/wordBins.txt"
binFrameNWo <- valueFrequencies(binPath, fileMetadata, "numWords")
saveBins(distFolder, "wordDist", binFrameNWo)
binFrameNWo$feature <- "numWords"

#paragraph distributions
binPath <- "input/paragraphBins.txt"
binFrameNPar <- valueFrequencies(binPath, fileMetadata, "numParag")
saveBins(distFolder, "paragDist", binFrameNPar)
binFrameNPar$feature <- "numParag"

#table distributions
binPath <- "input/tableBins.txt"
binFrameNTab <- valueFrequencies(binPath, fileMetadata, "numTables")
saveBins(distFolder, "tableDist", binFrameNTab)
binFrameNTab$feature <- "numTables"

allBinsFrame <- rbind(binFrameNPag, binFrameNPar, binFrameNTab, binFrameNWo) 
# calculate representative samples 
combinationFrame <- expand.grid(numParag=binFrameNPar$title, numPage=binFrameNPag$title, 
                                numWords=binFrameNWo$title, numTables=binFrameNTab$title)

numProp <- c("numPage", "numWords", "numParag", "numTables")
samples <- apply(combinationFrame, 1, function(x) calculateSamples(x, allBinsFrame, fileMetadata, numProp, samplesPath))
#sampleFrame <- rbind(samples)
samples <- samples[!is.na(samples)]
calculateSamples <- function(row, bins, metadata , names, path) {
  #print(row)
  met <- metadata
  for (prop in names) {
    #print(prop)
    start <- bins[bins$feature==prop & bins$title==row[[prop]], ]$start 
    end <- bins[bins$feature==prop & bins$title==row[[prop]], ]$end
    met <- met[met[[prop]] >= start & met[[prop]] <= end,]
  }
  num <- nrow(met)
  #print(num)
  if (num==0) {return (NA)}
  s <- met[sample(1:num,1), ]
  #print(s[1,]$fileName)
  #s$numSamples <- num
  return (s[1,]$fileName)
}



