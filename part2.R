## Part 2: Analysis (9 points)

#### Preliminaries
- **Y:** What was the definition of the primary outcome in this study?
  "Death or dependency at 6 months"
  
- What is (are) the variable name(s) for the outcome?
  "OCCODE"

- **U:** what is (are) the variable name(s) for the intervention, and what is (are) their possible values?
    "RXASP:Y/N" 
    "RXHEP: M/L/N"
    
- **V, W:** describe the covariates included and the population being studied.

v = "delay between stroke and randomization", "conscious state at randomization", "sex", "age", 
      "symptoms noted on waking", "atrial fibrillation", "CT before randomization", "infarct visible on CT",
      "Heparin within 24 hours prior to randomization", "aspirin within 3 days prior to randomization",
      "systolic blood pressure at randomization", "body defecits", "mental defecits", "stroke subtype", "treatments
      in hospital"

w = "Patients were eligible if there was evidence of an acute stroke (irrespective of severity) with onset less than 48 hours
      previously, and if there was no evidence ofintracranial haemorrhage, and no clear indications for, or contraindications to, 
      heparin or aspirin. The fundamental criterion for eligibility was simply that the physician was uncertain whether or not 
      to administer either or both of the trial treatments to that particular patient. Possible reasons not to include a patient 
      were either only a small likelihood of worthwhile benefit or a high risk of adverse effects. At randomisation, 61% were aged 
      over 70, 23% had impaired consciousness and 16% were known to be in atrial fibrillation. 67% had had a CT scan before 
      randomisation (49% of these scans showed an infarction, no infarct lesion being visible in the remainder); a further 29% 
      were scanned after randomisation; and 4% never had a CT scan."

- Construct a so-called Table 1 for groups of {aspirin, no aspirin} use, including information on age, gender, systolic blood pressure, and conscious state.

library(dplyr)

data = read.csv("IST_csv.csv")

table1 <- data %>%
  select(RXASP,AGE,RSBP,SEX,RCONSC) %>%
  group_by(RXASP) %>%
  summarize(count = n(), male = sum(SEX=="M"),female = sum(SEX=="F"), age = round(mean(AGE),2), 
            SBP = round(mean(RSBP),2),alert = sum(RCONSC=="F"),unconscious = sum(RCONSC=="U"),
            drowsy = sum(RCONSC=="D"))

colnames(table1) = c("Aspirin", "N", "Males", "Females", "Avg. Age", "Avg. Sys. BP",
                     "Alert", "Unconscoius","Drowsy")

#### Machine learning analysis
Note: for this analysis, use a simple 50-50 train-test split.

train_set <- data %>% sample_frac(0.5, replace = FALSE)
test_set <- data %>% setdiff(train_set)

Let our outcome of interest be "dead or dependent at 6 months", i.e. so that we have a binary classification problem. What percent of patients are dead or dependent at 6 months in your train set and test set?

trainDorD <- train_set %>% 
  count(OCCODE) %>%
  filter(OCCODE==1 | OCCODE==2) %>%
  select(n) %>%
  sum()

trainDorD <- trainDorD/nrow(train_set)

testDorD <- test_set %>% 
  count(OCCODE) %>%
  filter(OCCODE==1 | OCCODE==2) %>%
  select(n) %>%
  sum()

testDorD <- testDorD/nrow(test_set)

Choose which variables to include in your model. For example, remove variables for outcomes at 14 days (because if you are dead at 14 days you are certainly dead at 6 months). Moreover, you should remove all features measured after baseline if you want to make a prediction based on baseline data. Similarly, specific indicators of the outcome should also be removed, since those are measurements past the baseline that are not our outcome of interest. For these reasons, you will need to remove clusters of variables. Justify your approach.

vars <- colnames(data)
varsRemoved <- c(1,22,23,24,25,28:94,96:112)

predVars <- vars[-varsRemoved]

train_set <- train_set[,predVars]
test_set <- test_set[,predVars]

Of the remaining variables, decide whether to exclude variables with missing data, impute them, and/or use indicator variables. (Note that if you choose multiple imputation for some variables, you would need to pool the results when evaluating performance, however for homework you may just use the first imputed data set). Justify your approach.
#RXHEP: M coded as H (high) in pilot
train_set$RXHEP <- as.character(train_set$RXHEP)
train_set$RXHEP[train_set$RXHEP == "H"] <- "M"
train_set$RXHEP <- as.factor(train_set$RXHEP)

test_set$RXHEP <- as.character(test_set$RXHEP)
test_set$RXHEP[test_set$RXHEP == "H"] <- "M"
test_set$RXHEP <- as.factor(test_set$RXHEP)

#MAR = impute, MNAR = indicators
#RATRIAL, RASP3, and RHEP24 MNAR, so indicator variables added
#OCCODE = 0 and 9 are unknown

train_set = replace(train_set, train_set[,] == "", NA)
test_set = replace(test_set, test_set[,] == "", NA)

train_set$OCCODE[train_set$OCCODE == 0|train_set$OCCODE == 9] <- NA
test_set$OCCODE[test_set$OCCODE == 0|test_set$OCCODE == 9] <- NA

train_set <- as.data.frame(lapply(train_set, function (x) if (is.factor(x)) factor(x) else x))
test_set <- as.data.frame(lapply(test_set, function (x) if (is.factor(x)) factor(x) else x))

train_set$OCCODE <- as.factor(train_set$OCCODE)
test_set$OCCODE <- as.factor(test_set$OCCODE)

train_set <- train_set %>% 
  mutate(RATIND = (is.na(RATRIAL))) %>% 
  mutate(RASIND = (is.na(RASP3))) %>%
  mutate(RHEPIND = (is.na(RHEP24))) %>%
  mutate(OCCIND = (is.na(OCCODE)))

test_set <- test_set %>% 
  mutate(RATIND = (is.na(RATRIAL))) %>% 
  mutate(RASIND = (is.na(RASP3))) %>%
  mutate(RHEPIND = (is.na(RHEP24))) %>%
  mutate(OCCIND = (is.na(OCCODE)))

train_set[,c("RATIND","RASIND","RHEPIND","OCCIND")] <- lapply(train_set[,c("RATIND","RASIND","RHEPIND","OCCIND")],factor)
test_set[,c("RATIND","RASIND","RHEPIND","OCCIND")] <- lapply(test_set[,c("RATIND","RASIND","RHEPIND","OCCIND")],factor)

levels(train_set$OCCODE) <- c(1,1,0,0)
levels(test_set$OCCODE) <- c(1,1,0,0)

library(missForest)

rfTrain <- missForest(train_set,maxiter=10,ntree=100,variablewise=TRUE)
rfTest <- missForest(test_set,maxiter=10,ntree=100,variablewise=TRUE)

trainData <- rfTrain$ximp
testData <- rfTest$ximp

Use the following machine learning algorithms: logistic regression, naive Bayes, Tree Augmented Naive Bayes, and decision tree (specify any parameters you set that are not the default). The packages that you may find useful here are: "glm", "bnlearn", and "rpart", but you may use others if desired. In a table, report the accuracy with 95% confidence intervals for each algorithm.

##########################################################################################
library(stringr)
library(caret)
library(ROCR)

vars <- colnames(test_set)
feats <- vars[-23]

lrFunc <- function(vars,train,maxIt) {
  
  maxIt = maxIt - 1
  
  lr = glm(reformulate(termlabels = vars, response = train$OCCODE==1),
           family=binomial(link="logit"),
           data = train)
  
  sig <- coef(summary(lr))[-1,"Pr(>|z|)"]
  not_sig <- sig[sig>0.01] %>% length()
  
  if(not_sig==0 | maxIt <= 0) {
    return(lr)
  }
  
  index <- match(max(sig),sig)
  name <- names(sig)[index]
  if(grepl("TRUE",name)) {
    name <- substr(name,1,nchar(name)-4)
  } else {
    if(name!="RDELAY" & name!="AGE" & name!="RSBP"){
      if(grepl("STYPE",name)) {
        name <- "STYPE"
      } else {
        name <- substr(name,1,nchar(name)-1)
      }
    }
  }
  
  vars <- vars[-match(name,vars)]
  lrFunc(vars,train,maxIt)
}

convertToPreds <- function(pred) {
  
  pred = ifelse(pred > 0.5, 1, 0)
  pred = pred %>% as.factor()
  
  return(pred)
}

lr <- lrFunc(feats,trainData,length(feats))

lrPred <- predict.glm(lr,testData,type="response")

lrPrediction <- prediction(lrPred, testData$OCCODE)
lrPerf <- performance(lrPrediction, measure = "tpr", x.measure = "fpr")
plot(lrPerf)

lrPred <- convertToPreds(lrPred)
lrCM <- confusionMatrix(lrPred,testData$OCCODE,positive = "1")
lrAcc <- lrCM$overall[1]

#############################################################
library(bnlearn)
library(klaR)

discretizeFunc <- function(train,test) {
  
  data <- bind_rows(train,test)
  data[,sapply(data, is.integer)] = lapply(data[,sapply(data, is.integer)], as.numeric)
  data = discretize(data)
  
  return(data)
}

nbFunc <- function(train,test) {
  
  data = discretizeFunc(train,test)
  
  trainData = data[1:nrow(train),]
  testData = data[-(1:nrow(train)),]
  
  trainControl = trainControl(method = "cv",number=10)
  fitted <- train(trainData[,feats],trainData$OCCODE, method = "nb", trControl = trainControl)
  
  nbPred <- predict(fitted,testData, type = "response")
  
  return(nbPred)
}

nb <- nbFunc(trainData, testData)

nbPrediction <- prediction(nb[,1], testData$OCCODE)
nbPerf <- performance(nbPrediction, measure = "tpr", x.measure = "fpr")
plot(nbPerf)

nbPred <- convertToPreds(nb[,1])
nbCM <- confusionMatrix(nbPred,testData$OCCODE,positive = "1")
nbAcc <- nbCM$overall[1]

##############################################################

tanFunc <- function(train,test) {
  
  data = discretizeFunc(train,test)
  
  trainData = data[1:nrow(train),]
  testData = data[-(1:nrow(train)),]
  
  tan = tree.bayes(trainData, "OCCODE")
  fitted = bn.fit(tan, trainData)
  tanPred <- predict(fitted, testData, prob = T) %>% attr("prob")
  
  return(tanPred)
}

tan <- tanFunc(trainData, testData)

tanPrediction <- prediction(tan[1,], testData$OCCODE)
tanPerf <- performance(tanPrediction, measure = "tpr", x.measure = "fpr")
plot(tanPerf)

tanPred <- convertToPreds(tan[1,])
tanCM = confusionMatrix(tanPred,testData$OCCODE,positive = "1")
tanAcc <- tanCM$overall[1]

##############################################################
library(rpart)

treeFunc <- function(train,test) {
  
  tree <- rpart(OCCODE~.,data = train,method = "class")
  treePred <- predict(tree,test)
  
  return(treePred)
}

treePred <- treeFunc(trainData,testData)

treePrediction <- prediction(treePred[,1], testData$OCCODE)
treePerf <- performance(treePrediction, measure = "tpr", x.measure = "fpr")
plot(treePerf)

treePred <- convertToPreds(treePred[,1])
treeCM <- confusionMatrix(treePred,testData$OCCODE,positive = "1")
treeAcc <- treeCM$overall[1]

Construct an ROC (receiver operating characteristic) curve for each model and overlay them on a graph using ggplot. Include a legend. Hint: you will find the package "ROCR" helpful (or you might try the package "precrec", but I have not tested it).
[response required]

Construct a PR (precision recall) curve for each model. Include a legend.
[response required]

#### Conclusions
Let's draw conclusions from this study. Specifically,

- how well are we able to predict death or dependence at 6 months? [response required]
- what is the average treatment effect of aspirin on death or dependence at 6 months? Is aspirin significantly better than the alternative? [response required]
- of the algorithms tested, which algorithms perform the best? Justify your statement.
[response required]

Congratulations, you've conducted a comparison of machine learning algorithms for mortality prediction! Commit your solutions to your git repository with an informative comment. ```git push``` will help you upload it to the cloud service you choose to use (github, bitbucket, etc).




