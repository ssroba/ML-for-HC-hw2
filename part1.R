## Part 1: Concept questions (6 points)

The code that follows introduces a toy data set, decision tree model, and two prediction functions.
```{r eval=T, message=F}
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
    
  } else {                                  # internal node of tree, so continue down tree to true/false child
    
    if( (datum[[tree[active,"splitVariable"] %>% as.character]] %>% as.character) == tree[active,"split"])
      return(predictedOdds(tree, datum, active = tree[active,trueChild]))
    
    else
      return(predictedOdds(tree, datum, active = tree[active,falseChild]))
    
  }
  
}

# goal: run predictOddsOnDataSet(tree, depressionData)
```

First, verify to yourself that, for the fourth patient in ```depressionData```, the tree should have output an odds of 0.1.

Fix the function ```predictedOdds``` so that ```predictedOddsOnDataSet``` outputs the odds for each patient in data. Use the debugger functions like ```debugOnce(predictedOdds)``` or ```browser()``` to inspect the code. 

What did you change?
[response required]

Add a column of the predicted probabilities of hospitalization to depressionData. Display it.
[response required]

Using a threshold probability of 0.5, what is:
  
  - the accuracy of the model?
- the sensitivity of the model?
- the specificity of the model?
- the precision of the model?
- the recall of the model?

[responses required]  

Suppose you want to know the prevalence of diabetes in Pittsburgh. If you randomly survey 10 Pittsburghers and 5 of them state they have diabetes:
  
  - what is the maximum likelihood estimate for the prevalence of diabetes?
- given your strong belief specified by a beta prior of $\alpha = 11, \beta = 21$, what is the maximum a posteriori estimate for the prevalence of diabetes?
[responses required]  