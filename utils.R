library(XML)

file <- "/Users/kresimir/Projects/TestDataGenerator/ToolEvaluator/src/test/resources/ApacheTika/results/testCase1.xml"


readDocumentMeasures <- function(file, format, toolN) {
  data <- xmlParse(file)
  measuresPath <- "//documentResults/measures/measure"
  df <- data.frame(measure=sapply(data[measuresPath], function(x) xmlGetAttr(x, "name")),
                   value=sapply(data[measuresPath], xmlValue)
  )
  docName <- xmlValue(data["//evaluationResults/input"][[1]])
  softName <- xmlValue(data["//evaluationResults/tool"][[1]])
  df$testCaseName <- docName
  df$toolName <- softName
  df$format <- format
  return(df)
}

file <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Generated/GroundTruth/Metadata/000964_PSMDoc_doc_Win7-Office2010.xml"

readTestMetadata <- function(file) {
  data <- xmlParse(file)
  metadataPath <- "//metadata/metadataEntries/entry"
  df <- data.frame(name=sapply(data[metadataPath], function(x) xmlGetAttr(x,"name")), 
                   source=sapply(data[metadataPath], function(x) xmlGetAttr(x,"source")),
                   value=sapply(data[metadataPath], xmlValue)
                   )
  docName <- xmlValue(data["//metadata/document"][[1]])
  df$testCaseName <- docName
  return(df)
}


readVBSMetadata <- function(inFolder) {
  
  listFiles <- list.files(inFolder)
  numFiles <- length(listFiles)
  metadataFrame <- data.frame(fileName=rep(NA,numFiles), numPage=rep(NA,numFiles), numCh=rep(NA,numFiles), 
                              numWords=rep(NA,numFiles), numLines=rep(NA,numFiles), numParag=rep(NA,numFiles), 
                              numTables=rep(NA,numFiles))
  i <- 0
  for (f in listFiles) {
    i <- i + 1
    filNameList <- unlist(strsplit(f, ".", fixed=TRUE))
    name <- filNameList[1]
    filePath <- paste(inFolder, "/", f, sep="")
    if (file.size(filePath)==0) 
      next
    df <- read.table(filePath, header=FALSE, sep=":", stringsAsFactors = FALSE)
    if (nrow(df)==0) 
      next
    # tf <- data.frame(fileName=name, numPage=df[1,2], numCh=df[2,2], numWords=df[3,2], 
    #                  numLines=df[4,2], numParag=df[5,2], numTables=df[6,2])
    metadataFrame[i,]$fileName <- name
    metadataFrame[i,]$numPage <- df[1,2]
    metadataFrame[i,]$numCh <- df[2,2]
    metadataFrame[i,]$numWords <- df[3,2]
    metadataFrame[i,]$numLines <- df[4,2]
    metadataFrame[i,]$numParag <- df[5,2]
    metadataFrame[i,]$numTables <- df[6,2]
    # if (is.null(metadataFrame)) {
    #   metadataFrame <- tf
    # } else {
    #   metadataFrame <- rbind(metadataFrame, tf)
    # }  
  }
  
  return (metadataFrame)
  
}