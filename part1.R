library(dplyr)

# synthetic depression data
depressionData = data.frame( # do not change "depressionData"
  pregnant = c(1,0,1,1),
  depressed = c("yes","yes","no","no") %>% as.factor(),
  hospitalized = c(1, 0, 0, 0) %>% as.logical()
) %>% tbl_df()

# tree: a model that outputs the odds of hospitalization from inputs of data (datums)
tree = data.frame( # do not change "tree"
  splitVariable = c("depressed", "pregnant", NA, NA, NA),
  split = c("yes", 1, NA, NA, NA),
  trueChild = c(2, 4, NA, NA, NA),
  falseChild = c(3, 5, NA, NA, NA),
  odds = c(NA, NA, 0.1, 2, 3)
)


predictOddsOnDataSet = function(tree, data, active = 1) {
  apply(data, 1, (function(x) {predictedOdds(tree=tree, x, active=1)})  )
}

predictedOdds = function(tree, datum, active = 1) {
  
  if(is.na(tree[active,"splitVariable"])) { # leaf of tree, so output value
    
    return(tree$odds[active])
    
  } else {  # internal node of tree, so continue down tree to true/false child
    
    if((datum[[tree[active,"splitVariable"] %>% as.character]] %>% as.character) == tree[active,"split"]) {
      
      return(predictedOdds(tree, datum, active = tree[active,"trueChild"]))
    
    } else {
      
      return(predictedOdds(tree, datum, active = tree[active,"falseChild"]))
      
    }
  }
}

#added quotes to truchild and falsechild when setting the new active node
odds = predictOddsOnDataSet(tree, depressionData)
prob = odds/(odds+1)

data <- cbind(depressionData,prob) %>% tbl_df()
data <- data %>% mutate(pred = prob > 0.5)

data <- data[,c(3,5)]

tp = filter(data,hospitalized == TRUE & pred == TRUE) %>% nrow()
fp = filter(data,hospitalized == FALSE & pred == TRUE) %>% nrow()
fn = filter(data,hospitalized == TRUE & pred == FALSE) %>% nrow()
tn = filter(data,hospitalized == FALSE & pred == FALSE) %>% nrow()

acc <- (tp + tn)/(tp+fp+fn+tn)
sens <- tp/(tp+fn)
spec <- fp/(fp+tn)
prec <- tp/(tp+fp)
recall <- sens

data.frame(acc,sens,spec,prec,recall)

library("ggplot2")

MLE = function(pVector) {
  p <- c()
  y <- c()
  for (i in pVector) {
    result <- (i^5)*(1-i)^5
    p <- c(p,i)
    y <- c(y,result)
  }
  plotData <- data.frame(p,y)
  maxY <- max(plotData$y)
  maxP <- plotData %>% filter(y == maxY)
  print(maxP)
  ggplot(plotData,aes(x = p,y = y)) + geom_point()
}

pVector <- seq(0,1,0.01)

MLE(pVector)

MAP = function(pVector,alpha,beta) {
  p <- c()
  y <- c()
  a <- 5 + (alpha - 1)
  b <- 5 + (beta - 1)
  for (i in pVector) {
    result <- (i^a)*(1-i)^b
    p <- c(p,i)
    y <- c(y,result)
  }
  plotData <- data.frame(p,y)
  maxY <- max(plotData$y)
  maxP <- plotData %>% filter(y == maxY)
  print(maxP)
  ggplot(plotData,aes(x = p,y = y)) + geom_point()
}

MAP(pVector,11,21)
