library(ggplot2)

source('utils.R')

pathDocuments <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/Documents/"
pathToolOutput <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/ToolOutput/"

tools <- c("ApacheTika1_1", "ApacheTika1_2", "ApacheTika1_13", "TextUtil")
  
listFiles <- list.files(pathDocuments)

dfHolder <- data.frame(testCaseName=character(), toolName=character(), measure=character(), value=character())

for (tool in tools) {
  pathToolResult <- paste(pathToolOutput, tool, "/results/", sep="")
  
  for (f in listFiles) {
    name <- unlist(strsplit(f, ".", fixed=TRUE))[1]
    
    fileResult <- paste(pathToolResult, name, ".xml", sep="")
    print(fileResult)
    tmp <- readDocumentMeasures(fileResult)
    dfHolder <- rbind(dfHolder, tmp)
  }
}

dfHolder$value <- as.numeric(as.character(dfHolder$value))
dfHolder <- dfHolder[!is.nan(dfHolder$value),]

barPlot <- ggplot(dfHolder, aes(x=factor(testCaseName), y=value, fill=toolName)) + 
  geom_bar(stat="identity", position="dodge") +
  coord_flip()
barPlot

