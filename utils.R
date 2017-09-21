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


readVBSMetadata <- function(file, n) {
  
  if (file.size(file)==0) 
    return(NULL)
  df <- read.table(file, header=FALSE, sep=":", stringsAsFactors = FALSE)
  if (nrow(df)==0) 
    return(NULL)
  metadataFrame <- data.frame(fileName=n, numPage=df[1,2], numCh=df[2,2], numWords=df[3,2], 
                              numLines=df[4,2], numParag=df[5,2], numTables=df[6,2])
  
  
  
  return (metadataFrame)
  
}