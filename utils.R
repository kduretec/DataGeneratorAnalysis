library(XML)
library(ggplot2)
file <- "/Users/kresimir/Projects/TestDataGenerator/ToolEvaluator/src/test/resources/ApacheTika/results/testCase1.xml"

basePath <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/"

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
                              numTables=rep(NA,numFiles),numWordTable=rep(NA,numFiles),numParagTable=rep(NA,numFiles))
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
    metadataFrame[i,]$numWordTable <- df[7,2]
    metadataFrame[i,]$numParagTable <- df[8,2]
    # if (is.null(metadataFrame)) {
    #   metadataFrame <- tf
    # } else {
    #   metadataFrame <- rbind(metadataFrame, tf)
    # }  
  }
  
  return (metadataFrame)
  
}

assignClass <- function(bins, value, nameV) {
  return (bins[value[[nameV]] >= bins$start & value[[nameV]] <= bins$end,]$code)
}

valueFrequencies <- function(fileBins, met, nameV) {
  bins <- read.table(fileBins, header=TRUE, sep="\t", stringsAsFactors = FALSE)
  #print(bins)
  bins$start <- as.numeric(bins$start)
  bins$end <- as.numeric(bins$end)
  bins$code <- as.numeric(bins$code)
  met$code <- apply(met, 1, function(x) assignClass(bins, x, nameV))
  met$code <- as.character(met$code)
  newFrame <- aggregate(met$code, list(code=met$code), FUN=length)
  
  newFrame <- merge(bins, newFrame, by="code")
  sumTotal <- sum(newFrame$x)
  newFrame$perc <- newFrame$x/sumTotal
  return (newFrame)
  #barPlot <- ggplot(newFrame, aes(x=title, y=x)) + geom_bar(stat="identity")
  #barPlot
}

saveBins <- function(folder, name, bins) {
  bins <- transform(bins, title=reorder(title, -perc) )
  barPlot <- ggplot(bins, aes(x=title, y=perc)) + geom_bar(stat="identity", position="dodge", color="#3182bd", fill="#3182bd") +
    geom_text(aes(label=round(perc, digits = 3)), position = position_dodge(width=0.8), vjust=-0.5, size=5) +
    scale_fill_brewer() + theme_bw() +
    theme(axis.title.x = element_blank(), axis.title.y=element_blank(), axis.text = element_text(size=17))
  path <- paste(folder, name,".png", sep="")
  print(path)
  png(filename=path, width=640, height = 480)
    print(barPlot)
  dev.off()
  path <- paste(folder, name,".tsv", sep="")
  write.table(bins, path, sep="\t", row.names = FALSE, col.names = TRUE)
}

calculateSamples <- function(row, bins, metadata , names, path) {
  #print(row)
  met <- metadata
  for (prop in names) {
    #print(prop)
    start <- bins[bins$feature==prop & bins$title==row[[prop]], ]$start 
    end <- bins[bins$feature==prop & bins$title==row[[prop]], ]$end
    met <- met[met[[prop]] >= start & met[[prop]] <= end,]
  }
  num <- nrow(met)
  #print(num)
  if (num==0) {return (NA)}
  s <- met[sample(1:num,1), ]
  #print(s[1,]$fileName)
  #s$numSamples <- num
  return (s[1,]$fileName)
}



