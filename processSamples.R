#####

OBSOLETE

####


library(ggplot2)
library(plotly)

#Govdocs data
pathGov <-
  "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Paper/Govdocs/"
govMime <- read.table(paste(pathGov,"govdocs-mime.tsv", sep = ""),
                      header = TRUE, sep = "\t", stringsAsFactors = FALSE)



mimeHist <-
  ggplot(govMime, aes(x = reorder(mime,-amount), y = amount)) + geom_bar(stat = "identity") + 
  labs(x=NULL, y=NULL) + theme(axis.text.x = element_text(angle = 20, hjust = 1))

png(filename = paste(pathGov, "govMime.png", sep = ""), width = 1800, height = 1200, res = 300)
print(mimeHist)
dev.off()
ggplotly(mimeHist)






ptTable <- read.table(paste(pathGov,"PTtable.csv", sep = ""), header = TRUE, sep = ",", 
                      stringsAsFactors = FALSE )
ptTable$sample_size <- as.numeric(ptTable$sample_size)
ptTable$pcoverage <- as.numeric(ptTable$pcoverage)
ptTable$tcoverage <- as.numeric(ptTable$tcoverage)
pLine <- ggplot(ptTable, aes(x=sample_size, y=pcoverage)) + geom_line()
png(filename = paste(pathGov, "pcoverage.png", sep = ""), width = 1800, height = 1200, res = 300)
print(pLine)
dev.off()
ggplotly(pLine)
