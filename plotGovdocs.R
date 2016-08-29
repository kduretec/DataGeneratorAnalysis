library(ggplot2)

pathGov <-
  "/Users/kresimir/Dropbox/Work/Projects/BenchmarkDP/benchmarking/publications/JSS/Paper/Govdocs/"
results <-
  read.table(
    paste(pathGov,"govdocs-mime.tsv", sep = ""),
    header = TRUE, sep = "\t", stringsAsFactors = FALSE
  )




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
