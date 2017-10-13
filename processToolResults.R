library(ggplot2)
library(ggsci)
source('utils.R')

experimentName <- "ExperimentTest"

pathDocuments <- paste("/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/Generated/", experimentName, "/Documents/", sep="")
pathResults <- paste("/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/Generated/", experimentName, "/Results/", sep="")
pathToolResults <- paste(pathResults, "Tools/", sep="")

#tools <- c("ApacheTika1_1", "ApacheTika1_2", "ApacheTika1_13", "DocToText", "Xpdf", "icecite", "AbiWord")
 
listTools <- list.files(pathToolResults)
print(listTools)
listFiles <- list.files(pathDocuments)

dfHolder <- data.frame(testCaseName=character(), toolName=character(), measure=character(), value=character(), format=character())

for (tool in listTools) {
  pTR <- paste(pathToolResults, tool, "/results/", sep="")
  
  for (f in listFiles) {
    filNameList <- unlist(strsplit(f, ".", fixed=TRUE))
    name <- filNameList[1]
    extension <- filNameList[2]
    fileResult <- paste(pTR, name, ".xml", sep="")
    if (file.exists(fileResult)) {
      print(fileResult)
      tmp <- readDocumentMeasures(fileResult, extension)
      dfHolder <- rbind(dfHolder, tmp) 
    }
  }
}

#dfHolder[dfHolder$toolName=="ApacheTika_11",]$toolName <- "Apache Tika v1.1"
#dfHolder[dfHolder$toolName=="ApacheTika_12",]$toolName <- "Apache Tika v1.2"
#dfHolder[dfHolder$toolName=="ApacheTika_113",]$toolName <- "Apache Tika v1.13"

#barPlot <- ggplot(dfAvgCorrect, aes(x=factor(testCaseName), y=value, fill=toolName)) + 
#  geom_bar(stat="identity", position="dodge") +
#  coord_flip()
#barPlot



############
# ALL FORMATS 
###########
allTotalResults <- data.frame(toolName=character(), measureName=character(), value=numeric())

#avgCorrect 
dfAvgCorrect <- dfHolder[dfHolder$measure=="percCorrect",]
dfAvgCorrect <- dfAvgCorrect[!is.nan(dfAvgCorrect$value),]
dfAvgCorrect$value <- as.numeric(as.character(dfAvgCorrect$value))


totalResults <- dfAvgCorrect[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Apache Tika v1.1", "Apache Tika v1.2", "Apache Tika v1.13", "DocToText", "AbiWord"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))
totalResults$measureName <- "avgCorrect"
allTotalResults <-   totalResults
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
totalResults <- totalResults[totalResults$toolName %in% c("Apache Tika v1.1", "Apache Tika v1.2", "Apache Tika v1.13", "DocToText", "AbiWord"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))
totalResults$measureName <- "avgOrder"
allTotalResults <-  rbind(allTotalResults, totalResults)

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
totalResults <- totalResults[totalResults$toolName %in% c("Apache Tika v1.1", "Apache Tika v1.2", "Apache Tika v1.13", "DocToText", "AbiWord"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))
totalResults$measureName <- "avgLayout"
allTotalResults <-  rbind(allTotalResults, totalResults)

barPlotAvgLayout <- ggplot(totalResults, aes(x=toolName, y=value)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) + 
  geom_text(aes(label=round(value, digits = 3)), position = position_dodge(width=0.9),vjust=-0.25) +
  scale_y_continuous(limits = c(0,1.00)) +
  labs(x="Tool Name", y="Average percentage of snippets with preserved layout (avgLayout)")
barPlotAvgLayout


allTotalResults$measureName <- factor(allTotalResults$measureName, levels=c("avgCorrect", "avgOrder", "avgLayout"))
barPlotAllResults <- ggplot(allTotalResults, aes(x=toolName, y=value, fill=measureName)) + 
  geom_bar(stat="identity", position="dodge", width = 0.75, colour="gray25") + 
  geom_text(aes(label=round(value, digits = 2)), position = position_dodge(width=0.8), vjust=-0.5, size=5) +
  scale_y_continuous(limits = c(0,1.00)) +
  scale_fill_brewer() + theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y=element_blank(), 
        axis.text = element_text(size=17), axis.title = element_text(size=20),
        legend.position="bottom", legend.title=element_blank(), 
        legend.text=element_text(size=15))
barPlotAllResults




############
# ONLY PDF
###########

allTotalResults <- data.frame(toolName=character(), measureName=character(), value=numeric())

#avgCorrect 
dfAvgCorrect <- dfHolder[dfHolder$measure=="percCorrect",]
dfAvgCorrect <- dfAvgCorrect[dfAvgCorrect$format=="pdf",]
dfAvgCorrect <- dfAvgCorrect[!is.nan(dfAvgCorrect$value),]
dfAvgCorrect$value <- as.numeric(as.character(dfAvgCorrect$value))


totalResults <- dfAvgCorrect[,c(FALSE,TRUE,FALSE,TRUE)]
totalResults <- aggregate(value~toolName, totalResults, FUN = mean)
totalResults <- totalResults[totalResults$toolName %in% c("Xpdf", "Apache Tika v1.13", "DocToText", "icecite", "AbiWord"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))
totalResults$measureName <- "avgCorrect"
allTotalResults <-   totalResults

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
totalResults <- totalResults[totalResults$toolName %in% c("Xpdf", "Apache Tika v1.13", "DocToText", "icecite", "AbiWord"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))
totalResults$measureName <- "avgOrder"
allTotalResults <-  rbind(allTotalResults, totalResults)

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
totalResults <- totalResults[totalResults$toolName %in% c("Xpdf", "Apache Tika v1.13", "DocToText", "icecite", "AbiWord"),]
totalResults <- transform(totalResults, toolName = reorder(toolName, -value))
totalResults$measureName <- "avgLayout"
allTotalResults <-  rbind(allTotalResults, totalResults)

barPlotAvgLayout <- ggplot(totalResults, aes(x=toolName, y=value)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) + 
  geom_text(aes(label=round(value, digits = 3)), position = position_dodge(width=0.9),vjust=-0.25) +
  scale_y_continuous(limits = c(0,1.00)) +
  labs(x="Tool Name", y="Average percentage of snippets with preserved layout (avgLayout)")
barPlotAvgLayout

allTotalResults$measureName <- factor(allTotalResults$measureName, levels=c("avgCorrect", "avgOrder", "avgLayout"))
barPlotAllResults <- ggplot(allTotalResults, aes(x=toolName, y=value, fill=measureName)) + 
  geom_bar(stat="identity", position="dodge", width = 0.75, colour="gray25") + 
  geom_text(aes(label=round(value, digits = 2)), position = position_dodge(width=0.8), vjust=-0.5, size=5) +
  scale_y_continuous(limits = c(0,1.00)) +
  scale_fill_brewer() + theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y=element_blank(), 
        axis.text = element_text(size=17), axis.title = element_text(size=20),
        legend.position="bottom", legend.title=element_blank(), 
        legend.text=element_text(size=15))
barPlotAllResults








##########################
# calculating the worst  #
# 10 files for each tool #
##########################

tools <- c("Apache Tika v1.1", "Apache Tika v1.2", "Apache Tika v1.13", "DocToText", "AbiWord")
for (tool in tools) {
  
  currentToolRes <- dfHolder[dfHolder$toolName==tool & dfHolder$measure=="percCorrect",]
  currentToolRes$value <- as.numeric(as.character(currentToolRes$value))
  currentToolRes <- currentToolRes[order(currentToolRes$value),]
  write.table(currentToolRes[1:10,], paste(pathResults,tool,".tsv", sep=""), sep="\t",row.names=FALSE, 
              col.names = TRUE)
  
}








