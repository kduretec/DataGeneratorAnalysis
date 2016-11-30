library(XML)

file <- "/Users/kresimir/Projects/TestDataGenerator/ToolEvaluator/src/test/resources/ApacheTika/results/testCase1.xml"


readDocumentMeasures <- function(file) {

  data <- xmlParse(file)
  measuresPath <- "//documentResults/measures/measure"
  measuresNamePath <- "//documentResults/measures/measure/@name"
  df <- data.frame(measure=sapply(data[measuresPath], function(x) xmlGetAttr(x, "name")),
                   value=sapply(data[measuresPath], xmlValue)
  )
  
}