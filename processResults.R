#####

OBSOLETE

######



library(ggplot2)
library(plotly)

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

swSC <- swSC[!grepl("000964",swSC[["TestCase"]]),]

##calculating bugs 
tools <- c("TIKA_1_1", "TIKA_1_2", "TIKA_1_13", "TEXTUTIL")
textElements <- c("PARAGRAPH-present", "TXTBX-present", "CTBX-present")

resultsBugs <- data.frame(ToolName = c("ApacheTika 1.1", "ApacheTika 1.2", "ApacheTika 1.13", "TextUtil"), 
                          NumPar = c(0,0,0,0), NumTB = c(0,0,0,0), NumCB = c(0,0,0,0))

for (tool in tools) {
  for (textEl in textElements) {
    whC <- paste(tool,"_",textEl, sep="")
    print(whC)
    swBugs <- swSC[!is.na(swSC[[whC]]),]
    swBugs <- swBugs[swBugs[["GT-format"]] %in% c("doc", "docx"),]
    total <- nrow(swBugs)
    write.table(swBugs, file=paste(pathPlots, whC, "-all.tsv", sep=""), 
                quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
    swBugs <- swBugs[as.numeric(as.numeric(swBugs[[whC]])) < 1.0,]
    numBugs <- nrow(swBugs)
    write.table(swBugs, file=paste(pathPlots, whC, ".tsv", sep=""), 
                quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
    posX <- match(tool,tools)
    posY <- match(textEl, textElements) + 1
    resultsBugs[posX,posY] <- numBugs / total 
  } 
}
write.table(resultsBugs, file=paste(pathPlots,"allBugs2.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)

platform <- c("Win7-Office2007", "Win7-Office2010")
format <- c("doc", "docx", "rtf", "pdf")

resultsBugs2 <- data.frame(platform = c("Win7-Office2007", "Win7-Office2007", "Win7-Office2007", "Win7-Office2007", "Win7-Office2010",
                                       "Win7-Office2010","Win7-Office2010","Win7-Office2010"),
                          format = c("doc", "docx", "rtf", "pdf", "doc", "docx", "rtf", "pdf"), 
                          tika11 = c(0,0,0,0,0,0,0,0), 
                          tika12 = c(0,0,0,0,0,0,0,0), 
                          tika113 = c(0,0,0,0,0,0,0,0), 
                          textutil = c(0,0,0,0,0,0,0,0))

for (plat in platform) {
  for (fm in format) {
    curVal <- swSC[swSC[["GT-platform"]] ==plat,]
    curVal <- curVal[curVal[["GT-format"]] ==fm,]
    num <- nrow(curVal)
    tika11Count <- 0
    tika12Count <- 0
    tika113Count <- 0
    textUtilCount <- 0
    for (i in 1:num) {
            if (as.numeric(curVal[i,][["TIKA_1_1_PARAGRAPH-present"]])<1.0) {
              tika11Count <- tika11Count + 1
            } else if (as.numeric(curVal[i,][["GT-textboxcount"]])>0) {
              if (as.numeric(curVal[i,][["TIKA_1_1_TXTBX-present"]])<1.0) {
                tika11Count <- tika11Count + 1
              }
            } else if (!is.na(curVal[i,][["GT-controlboxcount"]])) {
              if (as.numeric(curVal[i,][["TIKA_1_1_CTBX-present"]])<1.0)
                tika11Count <- tika11Count + 1
            }
      
            if (as.numeric(curVal[i,][["TIKA_1_2_PARAGRAPH-present"]])<1.0) {
              tika12Count <- tika12Count + 1
            } else if (as.numeric(curVal[i,][["GT-textboxcount"]])>0) {
              if (as.numeric(curVal[i,][["TIKA_1_2_TXTBX-present"]])<1.0) {
                tika12Count <- tika12Count + 1
              }
            } else if (!is.na(curVal[i,][["GT-controlboxcount"]])) {
              if (as.numeric(curVal[i,][["TIKA_1_2_CTBX-present"]])<1.0)
            tika12Count <- tika12Count + 1
            }
      
          if (as.numeric(curVal[i,][["TIKA_1_13_PARAGRAPH-present"]])<1.0) {
            tika113Count <- tika113Count + 1
          } else if (as.numeric(curVal[i,][["GT-textboxcount"]])>0) {
            if (as.numeric(curVal[i,][["TIKA_1_13_TXTBX-present"]])<1.0) {
              tika113Count <- tika113Count + 1
            }
          } else if (!is.na(curVal[i,][["GT-controlboxcount"]])) {
            if (as.numeric(curVal[i,][["TIKA_1_13_CTBX-present"]])<1.0)
              tika113Count <- tika113Count + 1
          }
      
      
          if (as.numeric(curVal[i,][["TEXTUTIL_PARAGRAPH-present"]])<1.0) {
            textUtilCount <- textUtilCount + 1
          } else if (as.numeric(curVal[i,][["GT-textboxcount"]])>0) {
            if (as.numeric(curVal[i,][["TEXTUTIL_TXTBX-present"]])<1.0) {
              textUtilCount <- textUtilCount + 1
            }
          } else if (!is.na(curVal[i,][["GT-controlboxcount"]])) {
            if (as.numeric(curVal[i,][["TEXTUTIL_CTBX-present"]])<1.0)
              textUtilCount <- textUtilCount + 1
          }
      
    }
    resultsBugs2[resultsBugs2$platform==plat & resultsBugs2$format==fm, ]$tika11 <- tika11Count / num
    resultsBugs2[resultsBugs2$platform==plat & resultsBugs2$format==fm, ]$tika12 <-1- tika12Count / num
    resultsBugs2[resultsBugs2$platform==plat & resultsBugs2$format==fm, ]$tika113 <-1- tika113Count / num
    resultsBugs2[resultsBugs2$platform==plat & resultsBugs2$format==fm, ]$textutil <-1- textUtilCount / num
    
    
    
#     tika11 <- curVal[as.numeric(curVal[["TIKA_1_1_PARAGRAPH-present"]])==1.0,]
#     tika11 <- tika11[as.numeric(tika11[["TIKA_1_1_TXTBX-present"]])==1.0,]
#     tika11 <- tika11[as.numeric(tika11[["TIKA_1_1_CTBX-present"]])==1.0,]
#     
#     #tika11Par <- as.numeric(curVal[["TIKA_1_1_PARAGRAPH-present"]])
#     #tika11Par <- tika11Par[!is.na(tika11Par)]
#     
#     #tika11TXTBX <-  as.numeric(curVal[["TIKA_1_1_TXTBX-present"]])
#     #tika11TXTBX <- tika11TXTBX[!is.na(tika11TXTBX)]
#     #tika11CTBX <-  as.numeric(curVal[["TIKA_1_1_CTBX-present"]])
#     #tika11CTBX <- tika11CTBX[!is.na(tika11CTBX)]
#     tika11Res <- 1 - nrow(tika11) / num;
# #     if (fm == "doc") {
# #        
# #        
# #        #tika11Res <- (mean(tika11Par)  + mean(tika11TXTBX)) / 2
# #     } else {
# #       tika11Res <- (mean(tika11Par) + mean(tika11CTBX) + mean(tika11TXTBX)) / 3
# #     }
#     resultsBugs2[resultsBugs2$platform==plat & resultsBugs2$format==fm, ]$tika11 <- tika11Res
#     
#     tika12 <- curVal[as.numeric(curVal[["TIKA_1_2_PARAGRAPH-present"]])==1.0,]
#     tika12 <- tika12[as.numeric(tika12[["TIKA_1_2_TXTBX-present"]])==1.0,]
#     tika12 <- tika12[as.numeric(tika12[["TIKA_1_2_CTBX-present"]])==1.0,]
#     tika12Res <- 1 - nrow(tika12) / num;
#     
# #     tika12Par <- as.numeric(curVal[["TIKA_1_2_PARAGRAPH-present"]])
# #     tika12Par <- tika12Par[!is.na(tika12Par)]
# #     tika12TXTBX <-  as.numeric(curVal[["TIKA_1_2_TXTBX-present"]])
# #     tika12TXTBX <- tika12TXTBX[!is.na(tika12TXTBX)]
# #     tika12CTBX <-  as.numeric(curVal[["TIKA_1_2_CTBX-present"]])
# #     tika12CTBX <- tika12CTBX[!is.na(tika12CTBX)]
# #     tika12Res <- 0
# #     if (fm == "doc") {
# #       tika12Res <- (mean(tika12Par)  + mean(tika12TXTBX)) / 2
# #     } else {
# #       tika12Res <- (mean(tika12Par) + mean(tika12CTBX) + mean(tika12TXTBX)) / 3
# #     }
#     resultsBugs2[resultsBugs2$platform==plat & resultsBugs2$format==fm, ]$tika12 <- tika12Res
#     
#     
#     tika113 <- curVal[as.numeric(curVal[["TIKA_1_13_PARAGRAPH-present"]])==1.0,]
#     tika113 <- tika113[as.numeric(tika113[["TIKA_1_13_TXTBX-present"]])==1.0,]
#     tika113 <- tika113[as.numeric(tika113[["TIKA_1_13_CTBX-present"]])==1.0,]
#     tika113Res <- 1 - nrow(tika113) / num;
#     
# #     tika113Par <- as.numeric(curVal[["TIKA_1_13_PARAGRAPH-present"]])
# #     tika113Par <- tika113Par[!is.na(tika113Par)]
# #     tika113TXTBX <-  as.numeric(curVal[["TIKA_1_13_TXTBX-present"]])
# #     tika113TXTBX <- tika113TXTBX[!is.na(tika113TXTBX)]
# #     tika113CTBX <-  as.numeric(curVal[["TIKA_1_13_CTBX-present"]])
# #     tika113CTBX <- tika113CTBX[!is.na(tika113CTBX)]
# #     tika113Res <- (mean(tika113Par) + mean(tika113CTBX) + mean(tika113TXTBX)) / 3
# #     tika113Res <- 0
# #     if (fm == "doc") {
# #       tika113Res <- (mean(tika113Par)  + mean(tika113TXTBX)) / 2
# #     } else {
# #       tika113Res <- (mean(tika113Par) + mean(tika113CTBX) + mean(tika113TXTBX)) / 3
# #     }
#     resultsBugs2[resultsBugs2$platform==plat & resultsBugs2$format==fm, ]$tika113 <- tika113Res
#     
#     textUtil <- curVal[as.numeric(curVal[["TEXTUTIL_PARAGRAPH-present"]])==1.0,]
#     textUtil <- textUtil[as.numeric(textUtil[["TEXTUTIL_TXTBX-present"]])==1.0,]
#     textUtil <- textUtil[as.numeric(textUtil[["TEXTUTIL_CTBX-present"]])==1.0,]
#     textUtilRes <- 1 - nrow(textUtil) / num;
#     
# #     textUtilPar <- as.numeric(curVal[["TEXTUTIL_PARAGRAPH-present"]])
# #     print(textUtilPar)
# #     textUtilPar <- textUtilPar[!is.na(textUtilPar)]
# #     textUtilTXTBX <- as.numeric(curVal[["TEXTUTIL_TXTBX-present"]])
# #     textUtilTXTBX <- textUtilTXTBX[!is.na(textUtilTXTBX)]
# #     textUtilCTBX <- as.numeric(curVal[["TEXTUTIL_CTBX-present"]])
# #     textUtilCTBX <- textUtilCTBX[!is.na(textUtilCTBX)]
# #     textUtilRes <- 0
# #     if (fm == "doc") {
# #       textUtilRes <- (mean(textUtilPar)  + mean(textUtilTXTBX)) / 2
# #     } else {
# #       textUtilRes <- (mean(textUtilPar) + mean(textUtilCTBX) + mean(textUtilTXTBX)) / 3
# #     }
#     resultsBugs2[resultsBugs2$platform==plat & resultsBugs2$format==fm, ]$textutil <- textUtilRes
#     
  }
}

write.table(resultsBugs2, file=paste(pathPlots,"overallRes.tsv", sep=""), 
            quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)


resultsBugs3 <- data.frame(platform = c("Win7-Office2007", "Win7-Office2007", "Win7-Office2007", "Win7-Office2007", "Win7-Office2010",
                                        "Win7-Office2010","Win7-Office2010","Win7-Office2010"),
                           format = c("doc", "docx", "rtf", "pdf", "doc", "docx", "rtf", "pdf"), 
                           PT = c(0,0,0,0,0,0,0,0), 
                           TB = c(0,0,0,0,0,0,0,0), 
                           CB = c(0,0,0,0,0,0,0,0), 
                           TBCB = c(0,0,0,0,0,0,0,0))


for (plat in platform) {
  for (fm in format) {
    curVal <- swSC[swSC[["GT-platform"]] ==plat,]
    curVal <- curVal[curVal[["GT-format"]] ==fm,]
    
    num <- nrow(curVal)
    
    curValPT <- curVal[curVal[["GT-textboxcount"]]==0 | is.na(curVal[["GT-textboxcount"]]),]
    curValPT <- curValPT[curValPT[["GT-controlboxcount"]]==0 | is.na(curValPT[["GT-controlboxcount"]]),]
    numPT <- nrow(curValPT)
    curValPT <- curValPT[as.numeric(curValPT[["TIKA_1_1_PARAGRAPH-present"]]) < 1.0,]
    numPTFail <- nrow(curValPT)
    resultsBugs3[resultsBugs3$platform==plat & resultsBugs3$format==fm, ]$PT <- 1- numPTFail / numPT
    
    curValTB <- curVal[curVal[["GT-textboxcount"]]>0 & !is.na(curVal[["GT-textboxcount"]]),]
    curValTB <- curValTB[curValTB[["GT-controlboxcount"]]==0 | is.na(curValTB[["GT-controlboxcount"]]),]
    numTB <- nrow(curValTB)
    curValTB <- curValTB[as.numeric(curValTB[["TIKA_1_1_PARAGRAPH-present"]]) <1.0 | 
                           as.numeric(curValTB[["TIKA_1_1_TXTBX-present"]]) <1.0,]
    numTBFail <- nrow(curValTB)
    resultsBugs3[resultsBugs3$platform==plat & resultsBugs3$format==fm, ]$TB <- 1- numTBFail / numTB
    
    
    curValCB <- curVal[curVal[["GT-controlboxcount"]]>0 & !is.na(curVal[["GT-controlboxcount"]]),]
    curValCB <- curValCB[curValCB[["GT-textboxcount"]]==0 | is.na(curValCB[["GT-textboxcount"]]),]
    numCB <- nrow(curValCB)
    curValCB <- curValCB[as.numeric(curValCB[["TIKA_1_1_PARAGRAPH-present"]]) <1.0 | 
                           as.numeric(curValCB[["TIKA_1_1_CTBX-present"]]) <1.0,]
    numCBFail <- nrow(curValCB)
    resultsBugs3[resultsBugs3$platform==plat & resultsBugs3$format==fm, ]$CB <- 1- numCBFail / numCB
    
    curValTBCB <- curVal[curVal[["GT-textboxcount"]]>0 & !is.na(curVal[["GT-textboxcount"]]),]
    curValTBCB <- curValTBCB[curValTBCB[["GT-controlboxcount"]]>0 & !is.na(curValTBCB[["GT-controlboxcount"]]),]
    numTBCB <- nrow(curValTBCB)
    curValTBCB <- curValTBCB[as.numeric(curValTBCB[["TIKA_1_1_PARAGRAPH-present"]]) <1.0 | 
                             as.numeric(curValTBCB[["TIKA_1_1_CTBX-present"]]) <1.0 |
                             as.numeric(curValTBCB[["TIKA_1_1_TXTBX-present"]]) <1.0 ,]
    numTBCBFail <- nrow(curValTBCB)
    resultsBugs3[resultsBugs3$platform==plat & resultsBugs3$format==fm, ]$TBCB <- 1- numTBCBFail / numTBCB
    
    #     
    
    
#     pt <- mean(as.numeric(curValPT[[c]]))
#     resultsBugs3[resultsBugs3$platform==plat & resultsBugs3$format==fm, ]$PT <- pt
#     
#     curValTB <- curVal[curVal[["GT-textboxcount"]]>0 & !is.na(curVal[["GT-textboxcount"]]),]
#     curValTB <- curValTB[curValTB[["GT-controlboxcount"]]==0 | is.na(curValTB[["GT-controlboxcount"]]),]
#     
#     tb <- mean(  ((as.numeric(curValTB[["GT-paragraphcount"]])-as.numeric(curValTB[["GT-textboxcount"]]))*as.numeric(curValTB[["TIKA_1_1_PARAGRAPH-present"]]) + 
#           as.numeric(curValTB[["GT-textboxcount"]])*as.numeric(curValTB[["TIKA_1_1_TXTBX-present"]])) / as.numeric(curValTB[["GT-paragraphcount"]]) )
#     resultsBugs3[resultsBugs3$platform==plat & resultsBugs3$format==fm, ]$TB <- tb  
#     
#     curValCB <- curVal[curVal[["GT-controlboxcount"]]>0 & !is.na(curVal[["GT-controlboxcount"]]),]
#     curValCB <- curValCB[curValCB[["GT-textboxcount"]]==0 | is.na(curValCB[["GT-textboxcount"]]),]
#     
#     cb <- mean(  ((as.numeric(curValCB[["GT-paragraphcount"]])-as.numeric(curValCB[["GT-controlboxcount"]]))*as.numeric(curValCB[["TIKA_1_1_PARAGRAPH-present"]]) + 
#                     as.numeric(curValCB[["GT-controlboxcount"]])*as.numeric(curValCB[["TIKA_1_1_CTBX-present"]])) / as.numeric(curValCB[["GT-paragraphcount"]]) )
#     resultsBugs3[resultsBugs3$platform==plat & resultsBugs3$format==fm, ]$CB <- cb  
#     
#     curValTBCB <- curVal[curVal[["GT-textboxcount"]]>0 & !is.na(curVal[["GT-textboxcount"]]),]
#     curValTBCB <- curValTBCB[curValTBCB[["GT-controlboxcount"]]>0 & !is.na(curValTBCB[["GT-controlboxcount"]]),]
#     print(curValTBCB)
#     tbcb <- mean(  ((as.numeric(curValTBCB[["GT-paragraphcount"]])-as.numeric(curValTBCB[["GT-controlboxcount"]])-as.numeric(curValTBCB[["GT-textboxcount"]])) *
#                       as.numeric(curValTBCB[["TIKA_1_1_PARAGRAPH-present"]]) + 
#                       as.numeric(curValTBCB[["GT-controlboxcount"]]) * as.numeric(curValTBCB[["TIKA_1_1_CTBX-present"]]) + 
#                       as.numeric(curValTBCB[["GT-textboxcount"]]) * as.numeric(curValTBCB[["TIKA_1_1_TXTBX-present"]]) ) / 
#                       as.numeric(curValTBCB[["GT-paragraphcount"]]) )
#     resultsBugs3[resultsBugs3$platform==plat & resultsBugs3$format==fm, ]$TBCB <- tbcb  
    
    }
  
  
}

write.table(resultsBugs3, file=paste(pathPlots,"overallResTika11.tsv", sep=""), 
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




