library(ggplot2)

pathPlots <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/ToolOutput/plots/" 
results <- 
  read.table(
    "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/ToolOutput/results.tsv",
    header = TRUE, sep = "\t", colClasses = c("character","character","character"), stringsAsFactors = FALSE 
    )



sD <- results[results$Element=="size",]
sD$Value <- as.numeric(sD$Value)
sD$Value <- sD$Value / 1024 
sizeHist <- ggplot(sD, aes(x=Value)) + geom_histogram()
png(filename = paste(pathPlots, "sizeDist.png", sep=""), width = 1800, height = 1200, res=300)
print(sizeHist)
dev.off()

uniqDocName <- unique(results$Name)
featuresElements <- c("GT-format", "GT-platform", "GTVB-pagecount", "GT-paragraphcount", "GT-wordcount", "GT-numCol")
swSC <- data.frame(TestCase=uniqDocName)
for (feat in featuresElements) {
  swSC[[feat]] <- rep(NA,length(uniqDocName))
}

for (nam in uniqDocName) {
  for (feat in featuresElements) {
    swSC[swSC$TestCase==nam,][[feat]] <- results[results$Name==nam & results$Element==feat,]$Value
  }
  
}

# swSC <- swSC[swSC$Format=="doc",]
# wsScat <- ggplot(swSC, aes(x=Word, y=Size)) + geom_point()
# png(filename = paste(pathPlots, "wordSizeSCP.png", sep=""), width = 1800, height = 1200, res=300)
# print(wsScat)
# dev.off()




