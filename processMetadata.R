source('utils.R')

pathDocuments <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/Documents/"
pathMetadata <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/GroundTruth/Metadata/"


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
