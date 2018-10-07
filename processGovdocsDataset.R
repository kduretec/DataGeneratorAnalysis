# script for processing govdocs metadata and 
# saving that metadata into a data frame for easier 
# future processing 

# this script on bigger number of files is not efficient as 
# it uses rbind function

source('utils.R')

folderIN <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/vbsproperties2/"

listFiles <- list.files(folderIN)

fileMetadata <- data.frame(fileName=character(), numPage=character(), numCh=character(), numWords=character(), 
                           numLines=character(), numParag=character(), numTables=character(), numWordTable=character(), numParagTable=character())

tUZFolder<- "temp"

dir.create(tUZFolder)


numFiles <- length(listFiles)
count <- 0
for (f in listFiles) {
  
  if (count %% 10 == 0) 
    print (paste("Processed ", count, "/", numFiles, sep=""))
  count <- count + 1
  filNameList <- unlist(strsplit(f, ".", fixed=TRUE))
  name <- filNameList[1]
  extension <- filNameList[2]
  fileResult <- paste(folderIN, f, sep="")
   
  unzip(fileResult, exdir=tUZFolder)
  tempFolder <- paste(tUZFolder, "/", name, sep="")
  tmp <- readVBSMetadata(tempFolder)
  fileMetadata <- rbind(fileMetadata, tmp)
  # if (count==1) {
  #   break
  # }
}

 print(paste("Processed ", count, " files", sep=""))
# 
fileMetadata<- fileMetadata[complete.cases(fileMetadata),]
fileOut <- "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/publications/INFSOF/experiments/real world dataset/metadata.tsv"
write.table(fileMetadata, fileOut, sep="\t", col.names = TRUE, row.names = FALSE)
# 
unlink(tUZFolder, recursive=TRUE)

