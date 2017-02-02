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
dfHolder[dfHolder$toolName=="ApacheTika_11",]$toolName <- "Apache Tika v1.1"
dfHolder[dfHolder$toolName=="ApacheTika_12",]$toolName <- "Apache Tika v1.2"
dfHolder[dfHolder$toolName=="ApacheTika_113",]$toolName <- "Apache Tika v1.13"

#barPlot <- ggplot(dfAvgCorrect, aes(x=factor(testCaseName), y=value, fill=toolName)) + 
#  geom_bar(stat="identity", position="dodge") +
#  coord_flip()
#barPlot



############
# ALL FORMATS 
###########


#avgCorrect 
dfAvgCorrect <- dfHolder[dfHolder$measure=="percCorrect",]
dfAvgCorrect <- dfAvgCorrect[!is.nan(dfAvgCorrect$value),]
dfAvgCorrect$value <- as.numeric(as.character(dfAvgCorrect$value))


totalResults <- dfAvgCorrect[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Apache Tika v1.1", "Apache Tika v1.2", "Apache Tika v1.13", "DocToText"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))
  
barPlotAvgCorrect <- ggplot(totalResults, aes(x=toolName, y=value)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) + 
  geom_text(aes(label=round(value, digits = 3)), position = position_dodge(width=0.9),vjust=-0.25) +
  scale_y_continuous(limits = c(0,1.00)) +
  labs(x="Tool Name", y="Percentage of correctly extracted text (avgCorrect)")
barPlotAvgCorrect

#avgOrder 
dfAvgOrder <- dfHolder[dfHolder$measure=="orderPreserved",]
dfAvgOrder <- dfAvgOrder[!is.nan(dfAvgOrder$value),]
dfAvgOrder$value <- as.logical(as.character(dfAvgOrder$value))
dfAvgOrder[dfAvgOrder$value==TRUE,]$value <- 1
dfAvgOrder[dfAvgOrder$value==FALSE,]$value <- 0

totalResults <- dfAvgOrder[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Apache Tika v1.1", "Apache Tika v1.2", "Apache Tika v1.13", "DocToText"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))

barPlotAvgOrder <- ggplot(totalResults, aes(x=toolName, y=value)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) + 
  geom_text(aes(label=round(value, digits = 3)), position = position_dodge(width=0.9),vjust=-0.25) +
  scale_y_continuous(limits = c(0,1.00)) +
  labs(x="Tool Name", y="Percentage of files with correct order (avgOrder)")
barPlotAvgOrder

#avgLayout
dfAvgLayout <- dfHolder[dfHolder$measure=="percLayoutPreserved",]
dfAvgLayout <- dfAvgLayout[!is.nan(dfAvgLayout$value),]
dfAvgLayout$value <- as.numeric(as.character(dfAvgLayout$value))

totalResults <- dfAvgLayout[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Apache Tika v1.1", "Apache Tika v1.2", "Apache Tika v1.13", "DocToText"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))

barPlotAvgLayout <- ggplot(totalResults, aes(x=toolName, y=value)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) + 
  geom_text(aes(label=round(value, digits = 3)), position = position_dodge(width=0.9),vjust=-0.25) +
  scale_y_continuous(limits = c(0,1.00)) +
  labs(x="Tool Name", y="Average percentage of snippets with preserved layout (avgLayout)")
barPlotAvgLayout






############
# ONLY PDF
###########


#avgCorrect 
dfAvgCorrect <- dfHolder[dfHolder$measure=="percCorrect",]
dfAvgCorrect <- dfAvgCorrect[dfAvgCorrect$format=="pdf",]
dfAvgCorrect <- dfAvgCorrect[!is.nan(dfAvgCorrect$value),]
dfAvgCorrect$value <- as.numeric(as.character(dfAvgCorrect$value))


totalResults <- dfAvgCorrect[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Xpdf", "Apache Tika v1.13", "DocToText"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))

barPlotAvgCorrect <- ggplot(totalResults, aes(x=toolName, y=value)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) + 
  geom_text(aes(label=round(value, digits = 3)), position = position_dodge(width=0.9),vjust=-0.25) +
  scale_y_continuous(limits = c(0,1.00)) +
  labs(x="Tool Name", y="Percentage of correctly extracted text (avgCorrect)")
barPlotAvgCorrect

#avgOrder 
dfAvgOrder <- dfHolder[dfHolder$measure=="orderPreserved",]
dfAvgOrder <- dfAvgOrder[dfAvgOrder$format=="pdf",]
dfAvgOrder <- dfAvgOrder[!is.nan(dfAvgOrder$value),]
dfAvgOrder$value <- as.logical(as.character(dfAvgOrder$value))
dfAvgOrder[dfAvgOrder$value==TRUE,]$value <- 1
dfAvgOrder[dfAvgOrder$value==FALSE,]$value <- 0

totalResults <- dfAvgOrder[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Xpdf", "Apache Tika v1.13", "DocToText"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))

barPlotAvgOrder <- ggplot(totalResults, aes(x=toolName, y=value)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) + 
  geom_text(aes(label=round(value, digits = 3)), position = position_dodge(width=0.9),vjust=-0.25) +
  scale_y_continuous(limits = c(0,1.00)) +
  labs(x="Tool Name", y="Percentage of files with correct order (avgOrder)")
barPlotAvgOrder

#avgLayout
dfAvgLayout <- dfHolder[dfHolder$measure=="percLayoutPreserved",]
dfAvgLayout <- dfAvgLayout[dfAvgLayout$format=="pdf",]
dfAvgLayout <- dfAvgLayout[!is.nan(dfAvgLayout$value),]
dfAvgLayout$value <- as.numeric(as.character(dfAvgLayout$value))

totalResults <- dfAvgLayout[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Xpdf", "Apache Tika v1.13", "DocToText"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))

barPlotAvgLayout <- ggplot(totalResults, aes(x=toolName, y=value)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) + 
  geom_text(aes(label=round(value, digits = 3)), position = position_dodge(width=0.9),vjust=-0.25) +
  scale_y_continuous(limits = c(0,1.00)) +
  labs(x="Tool Name", y="Average percentage of snippets with preserved layout (avgLayout)")
barPlotAvgLayout
