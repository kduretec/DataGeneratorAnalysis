library(ggplot2)

source('utils.R')

pathDocuments <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/Documents/"
pathToolOutput <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/ToolOutput/"

tools <- c("ApacheTika1_1", "ApacheTika1_2", "ApacheTika1_13", "DocToText", 
           "Xpdf")
  
listFiles <- list.files(pathDocuments)

dfHolder <- data.frame(testCaseName=character(), toolName=character(), measure=character(), value=character(), format=character())

for (tool in tools) {
  pathToolResult <- paste(pathToolOutput, tool, "/results/", sep="")
  
  for (f in listFiles) {
    filNameList <- unlist(strsplit(f, ".", fixed=TRUE))
    name <- filNameList[1]
    extension <- filNameList[2]
    fileResult <- paste(pathToolResult, name, ".xml", sep="")
    if (file.exists(fileResult)) {
      print(fileResult)
      tmp <- readDocumentMeasures(fileResult, extension)
      dfHolder <- rbind(dfHolder, tmp) 
    }
  }
}

dfHolder$value <- as.numeric(as.character(dfHolder$value))
dfAll <- dfHolder[!is.nan(dfHolder$value),]
dfAll <- dfAll[dfAll$measure=="percCorrect",]
barPlot <- ggplot(dfAll, aes(x=factor(testCaseName), y=value, fill=toolName)) + 
  geom_bar(stat="identity", position="dodge") +
  coord_flip()
#barPlot

totalResults <- dfAll[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("ApacheTika_11", "ApacheTika_12", "ApacheTika_113", "DocToText"),]
  
barPlot2 <- ggplot(totalResults, aes(x=factor(toolName), y=value)) + 
  geom_bar(stat="identity", position="dodge") + labs(x="Tool Name", y="Percentage of correctly extracted text")
barPlot2


dfPdf <- dfHolder[!is.nan(dfHolder$value),]
dfPdf <- dfPdf[dfPdf$measure=="percCorrect",]
dfPdf <- dfPdf[dfPdf$format=="pdf",]
totalResults <- dfPdf[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Xpdf", "ApacheTika_113", "DocToText"),]

barPlot2 <- ggplot(totalResults, aes(x=factor(toolName), y=value)) + 
  geom_bar(stat="identity", position="dodge") + labs(x="Tool Name", y="Percentage of correctly extracted text")
barPlot2




