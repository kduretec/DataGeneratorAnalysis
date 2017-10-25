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
  scale_x_continuous(limits = c(0,2500)) + 
  scale_y_continuous(limits = c(0,100000)) + theme_bw()
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

combinationFrame <- expand.grid(numParag=binFrameNPar$title, numPage=binFrameNPag$title)


#numProp <- c("numPage", "numWords", "numParag", "numTables")
numProp <- c("numPage", "numParag")
samples <- apply(combinationFrame, 1, function(x) calculateSamples(x, allBinsFrame, fileMetadata, numProp, samplesPath))
#sampleFrame <- rbind(samples)
samples <- samples[!is.na(samples)]

samplesMetadata <- fileMetadata[fileMetadata$fileName %in% samples, ]

samplesScatter <- paragPageScatter + geom_point(data=samplesMetadata, aes(x=numPage, y=numParag), size=3, shape=18)
path <- paste(distFolder, "paragPageScatterSamples.png", sep="")
png(path, width=640, heigh=480)
print(samplesScatter)
dev.off()

samplesMetadata <- samplesMetadata[,colnames(samplesMetadata) %in% c(numProp, "fileName")]
samplesMetadata$fileName <- paste("sample_",samplesMetadata$fileName, sep="")
for (nam in numProp) {
  samplesMetadata[[nam]] <- paste(nam, ":", samplesMetadata[[nam]], sep="")
}
write.table(samplesMetadata, samplesPath, row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
