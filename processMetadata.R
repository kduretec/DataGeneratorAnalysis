library(ggplot2)
source('utils.R')

experiment <- "ExperimentTest"
pathDocuments <- paste(basePath, "Generated/", experiment, "/Documents/", sep="")
pathMetadata <- paste(basePath, "Generated/", experiment, "/GroundTruth/Metadata/", sep="")


listFiles <- list.files(pathDocuments)

dfHolder <- data.frame(docName=character(), name=character(), source=character(), value=character())

for (f in listFiles) {
  name <- unlist(strsplit(f, ".", fixed=TRUE))[1]
  
  fileMetadata <- paste(pathMetadata, name, ".xml", sep="")
  print(fileMetadata)
  tmp <- readTestMetadata(fileMetadata)
  dfHolder <- rbind(dfHolder, tmp)
}

dfHolder$measureName <- paste(dfHolder$name, ".", dfHolder$source, sep="")

testMetadata <- data.frame(testCaseName=unique(dfHolder$testCaseName))

uniqMeasures <- unique(dfHolder$measureName)
for (meas in uniqMeasures) {
  dfTmp <- dfHolder[dfHolder$measureName==meas,]
  dfTmp <- dfTmp[,names(dfTmp) %in% c("testCaseName", "value")]
  testMetadata <- merge(x=testMetadata, y=dfTmp, by=c("testCaseName"), all.x = TRUE)
    
}

colnames(testMetadata) <- c("testCaseName", uniqMeasures)
testMetadata$pagecount.GenerationProcess <- as.numeric(as.character(testMetadata$pagecount.GenerationProcess))
testMetadata$paragraphcount.DataGenerator <- as.numeric(as.character(testMetadata$paragraphcount.DataGenerator))

sizePlot <- ggplot(testMetadata, aes(x=as.numeric(size.Fits))) + geom_histogram()
sizePlot

#pageCountHist <- ggplot(testMetadata, aes(x=pagecount.GenerationProcess)) + geom_bar()
pageCountHist <- ggplot(testMetadata, aes(x=pagecount.GenerationProcess)) + geom_histogram(aes(y=..count..), colour = "black", fill = "grey") + 
  labs(x="Page Count", y="Count")
pageCountHist

paragraphCountHist <- ggplot(testMetadata, aes(x=paragraphcount.DataGenerator)) + geom_histogram(aes(y=..count..), colour = "black", fill = "grey") +
  labs(x="Paragraph Count", y="Count")
paragraphCountHist


pageparagScater <- ggplot(testMetadata, aes(x=pagecount.GenerationProcess, y=paragraphcount.DataGenerator)) + geom_point() + 
  labs(x="Page Count", y="Paragraph Count") + theme_bw() +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))
pageparagScater

