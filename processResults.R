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
sizeHist <- ggplot(sD, aes(x=Value)) + geom_histogram(binwidth = 200)
png(filename = paste(pathPlots, "sizeDist.png", sep=""), width = 1800, height = 1200, res=300)
print(sizeHist)
dev.off()

uniqDocName <- unique(results$Name)
swSC <- data.frame(Name=uniqDocName, Size=rep(0,length(uniqDocName)), Word=rep(0,length(uniqDocName)), Format=rep(0,length(uniqDocName)))
for (nam in unique(results$Name)) {
  swSC[swSC$Name==nam,]$Size <- results[results$Element=="size" & results$Name==nam,]$Value
  swSC[swSC$Name==nam,]$Word <- results[results$Element=="GT-wordcount" & results$Name==nam,]$Value
  swSC[swSC$Name==nam,]$Format <- results[results$Element=="GT-format" & results$Name==nam,]$Value
}

swSC <- swSC[swSC$Format=="doc",]
wsScat <- ggplot(swSC, aes(x=Word, y=Size)) + geom_point()
png(filename = paste(pathPlots, "wordSizeSCP.png", sep=""), width = 1800, height = 1200, res=300)
print(wsScat)
dev.off()




