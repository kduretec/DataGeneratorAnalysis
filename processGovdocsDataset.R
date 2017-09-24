# script for processing govdocs metadata and 
# saving that metadata into a data frame for easier 
# future processing 
source('utils.R')

folderIN <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/vbsproperties/"

listFiles <- list.files(folderIN)

fileMetadata <- data.frame(fileName=character(), numPage=character(), numCh=character(), numWords=character(), 
                           numLines=character(), numParag=character(), numTables=character())

numFiles <- length(listFiles)
count <- 0
for (f in listFiles) {
  if (count %% 1000 == 0) 
    print (paste("Processed ", count, "/", numFiles, sep=""))
  count <- count + 1
  filNameList <- unlist(strsplit(f, ".", fixed=TRUE))
  name <- filNameList[1]
  extension <- filNameList[2]
  fileResult <- paste(folderIN, f, sep="")
  if (file.exists(fileResult)) {
    #print(fileResult)
    tmp <- readVBSMetadata(fileResult, name)
    if (is.null(tmp)) next
    fileMetadata <- rbind(fileMetadata, tmp) 
  }
  if (count == 2000) {
    break 
  }
}

print(paste("Processed ", count, " files", sep=""))

fileOut <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/metadata.tsv"
write.table(fileMetadata, fileOut, sep="\t", col.names = TRUE, row.names = FALSE)



