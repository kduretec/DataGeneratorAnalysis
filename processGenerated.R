####

OBSOLETE

####

library(ggplot2)
library(plotly)

pathResults <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/Results/" 
results <- read.table(paste(pathResults, "rawResults.tsv", sep=""), header = TRUE, sep = "\t", 
                      colClasses = c("character","character","character"), stringsAsFactors = FALSE)

# select unique test cases and define features for each test case
uniqDocNames <- unique(results$Name)
featuresElements <- c("size", "GT_format", "GT_platform", "VB_pagecount", "GT_wordcount", 
                      "GT_numCol", "GT_paragraphcount", "GT_textboxcount", "GT_controlboxcount")

# generate a data frame with all the features for each test case 
generatedData <- data.frame(TestCase=uniqDocNames)
for (feat in featuresElements) {
  generatedData[[feat]] <- rep(NA,length(uniqDocNames))
}
for (nam in uniqDocNames) {
  resTemp <- results[results$Name==nam,]
  for (feat in featuresElements) {
    if (nrow(results[resTemp$Name==nam & resTemp$Element==feat,]) > 0) {
      generatedData[generatedData$TestCase==nam,][[feat]] <- 
        resTemp[resTemp$Name==nam & resTemp$Element==feat,]$Value  
    } else {
      generatedData[generatedData$TestCase==nam,][[feat]] <- NA
    }
  }
}

write.table(generatedData, file=paste(pathResults,"testFeatures.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)

generatedData$size <- as.numeric(generatedData$size)
generatedData$size <- generatedData$size/1024
sizeHistogram <- ggplot(generatedData, aes(x=size)) + geom_histogram(binwidth = 4000)
ggplotly(sizeHistogram)

#formatHistogram <- ggplot(generatedData, aes(x=GT-format)) + geom_histogram()
#gplotly(formatHistogram)



