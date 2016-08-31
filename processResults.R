library(ggplot2)


### process samples and govdocs 
###

pathGov <-
  "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Paper/Govdocs/"
results <-
  read.table(
    paste(pathGov,"govdocs-mime.tsv", sep = ""),
    header = TRUE, sep = "\t", stringsAsFactors = FALSE
  )



### mime type histogram 
mimeHist <-
  ggplot(results, aes(x = reorder(mime,-amount), y = amount)) + geom_bar(stat = "identity") +
  labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
png(
  filename = paste(pathGov, "govMime.png", sep = ""), width = 1800, height = 1200, res =
    300
)
print(mimeHist)
dev.off()


ptTable <- read.table(
  paste(pathGov,"PTtable.csv", sep = ""),
  header = TRUE, sep = ",", stringsAsFactors = FALSE
)

pLine <- ggplot(ptTable, aes(x=sample_size)) + geom_line(aes(y=pcoverage)) + geom_line(aes(y=tcoverage))
png(
  filename = paste(pathGov, "ptcoverage-line.png", sep = ""), width = 1800, height = 1200, res =
    300
)
print(pLine)
dev.off()

ptScater <- ggplot(ptTable, aes(x=pcoverage, y=tcoverage, size=sample_size)) + geom_point()
png(
  filename = paste(pathGov, "ptcoverage-bubble.png", sep = ""), width = 1800, height = 1200, res =
    300
)
print(ptScater)
dev.off()



samples <-  read.table(
  paste(pathGov,"FeatureDistributions.tsv", sep = ""),
  header = TRUE, sep = "\t", stringsAsFactors = FALSE
)
samples$size <- samples$size / 1024 
sizeDistSamples <- data.frame(box=c("1-10", "10-50", "50-100", "100-500", "500-1000", "1000-5000", "5000-10000", "10000-50000"), countGenerated=rep(0,8))
for (i in 1:nrow(samples)) {
  pos <- "1-10"
  if (samples[i,]$size < 10) {
    pos <- "1-10"
  } else if (samples[i,]$size < 50) {
    pos <- "10-50"
  } else if (samples[i,]$size < 100) {
    pos <- "50-100"
  } else if (samples[i,]$size < 500) {
    pos <- "100-500"
  } else if (samples[i,]$size < 1000) {
    pos <- "500-1000"
  } else if (samples[i,]$size < 5000) {
    pos <- "1000-5000"
  } else if (samples[i,]$size < 10000) {
    pos <- "5000-10000"
  } else if (samples[i,]$size < 50000) {
    pos <- "10000-50000"
  } 
  sizeDistSamples[sizeDistSamples$box==pos,]$countGenerated <- sizeDistSamples[sizeDistSamples$box==pos,]$countGenerated + 1
}

sizeDistSamples$box <- factor(sizeDistSamples$box, levels = sizeDistSamples$box )
sizeHistSamples <- ggplot(sizeDistSamples, aes(x=as.factor(box), y=countGenerated)) + geom_bar(stat="identity") +
  labs(x="File size (kB)", y="number of objects") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

png(filename = paste(pathGov, "samplesSizeDist.png", sep=""), width = 1800, height = 1200, res=300)
print(sizeHistSamples)
dev.off()












pathPlots <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/ToolOutput/results/" 
results <- 
  read.table(paste(pathPlots, "rawResults.tsv", sep=""),
    header = TRUE, sep = "\t", colClasses = c("character","character","character"), stringsAsFactors = FALSE 
    )

uniqDocName <- unique(results$Name)
featuresElements <- c("size", "GT-format", "GT-platform", "GTVB-pagecount", "GT-wordcount", "GT-numCol", "GT-paragraphcount", 
                    "GT-textboxcount", "GT-controlboxcount", 
                    "TIKA_1_1_PARAGRAPH-present", "TIKA_1_1_TXTBX-present", "TIKA_1_1_CTBX-present",  
                    "TIKA_1_2_PARAGRAPH-present", "TIKA_1_2_TXTBX-present", "TIKA_1_2_CTBX-present",
                    "TIKA_1_13_PARAGRAPH-present", "TIKA_1_13_TXTBX-present", "TIKA_1_13_CTBX-present",
                    "TEXTUTIL_PARAGRAPH-present", "TEXTUTIL_TXTBX-present", "TEXTUTIL_CTBX-present")
swSC <- data.frame(TestCase=uniqDocName)
for (feat in featuresElements) {
  swSC[[feat]] <- rep(NA,length(uniqDocName))
}

for (nam in uniqDocName) {
  for (feat in featuresElements) {
    if (nrow(results[results$Name==nam & results$Element==feat,]) > 0) {
      swSC[swSC$TestCase==nam,][[feat]] <- results[results$Name==nam & results$Element==feat,]$Value  
    } else {
      swSC[swSC$TestCase==nam,][[feat]] <- NA
    }
    
  }
  
}



##calculating bugs 
tools <- c("TIKA_1_1", "TIKA_1_2", "TIKA_1_13", "TEXTUTIL")
textElements <- c("PARAGRAPH-present", "TXTBX-present", "CTBX-present")

resultsBugs <- data.frame(ToolName = c("ApacheTika 1.1", "ApacheTika 1.2", "ApacheTika 1.13", "TextUtil"), NumPar = c(0,0,0,0), NumTB = c(0,0,0,0), NumCB = c(0,0,0,0))

for (tool in tools) {
  for (textEl in textElements) {
    whC <- paste(tool,"_",textEl, sep="")
    print(whC)
    swBugs <- swSC[!is.na(swSC[[whC]]),]
    swBugs <- swBugs[swBugs[["GT-format"]] %in% c("doc", "docx"),]  
    swBugs <- swBugs[as.numeric(swBugs[[whC]]) < 1.0,]
    numBugs <- nrow(swBugs)
    write.table(swBugs, file=paste(pathPlots, whC, ".tsv", sep=""), 
                quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
    posX <- match(tool,tools)
    posY <- match(textEl, textElements) + 1
    resultsBugs[posX,posY] <- numBugs 
  } 
}
write.table(resultsBugs, file=paste(pathPlots,"allBugs2.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)







# files <- results[results$Element=="GT-format" & results$Value=="doc",]
# files <- file[file$Element=="GT-platform" & file$Value=="Win7-Office2007",]$Name
sD <- swSC[swSC[["GT-format"]]=="doc" & swSC[["GT-platform"]]=="Win7-Office2007",]
name <- "generatedSizeDistDOCWin7Office2007.png"
#sD <- swSC
#name <- "generatedSizeDistAll.png"

sD$size <- as.numeric(sD$size)
sD$size <- sD$size / 1024 
sizeDist <- data.frame(box=c("1-10", "10-50", "50-100", "100-500", "500-1000", "1000-5000", "5000-10000", "10000-50000"), countGenerated=rep(0,8))
for (i in 1:nrow(sD)) {
  pos <- "1-10"
 if (sD[i,]$size < 10) {
    pos <- "1-10"
  } else if (sD[i,]$size < 50) {
    pos <- "10-50"
  } else if (sD[i,]$size < 100) {
    pos <- "50-100"
  } else if (sD[i,]$size < 500) {
    pos <- "100-500"
  } else if (sD[i,]$size < 1000) {
    pos <- "500-1000"
  } else if (sD[i,]$size < 5000) {
    pos <- "1000-5000"
  } else if (sD[i,]$size < 10000) {
    pos <- "5000-10000"
  } else if (sD[i,]$size < 50000) {
    pos <- "10000-50000"
  } 
  sizeDist[sizeDist$box==pos,]$countGenerated <- sizeDist[sizeDist$box==pos,]$countGenerated + 1
}
sizeDist$box <- factor(sizeDist$box, levels = sizeDist$box )
sizeHist <- ggplot(sizeDist, aes(x=factor(box), y=countGenerated)) + geom_bar(stat="identity") +
  labs(x="File size (kB)", y="number of objects") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
png(filename = paste(pathPlots, name, sep=""), width = 1800, height = 1200, res=300)
print(sizeHist)
dev.off()


# swSC <- swSC[swSC$Format=="doc",]
# wsScat <- ggplot(swSC, aes(x=Word, y=Size)) + geom_point()
# png(filename = paste(pathPlots, "wordSizeSCP.png", sep=""), width = 1800, height = 1200, res=300)
# print(wsScat)
# dev.off()




