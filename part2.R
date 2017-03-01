## Part 2: Analysis (9 points)

#### Preliminaries
- **Y:** What was the definition of the primary outcome in this study?
- What is (are) the variable name(s) for the outcome?

- **U:** what is (are) the variable name(s) for the intervention, and what is (are) their possible values?

- **V, W:** describe the covariates included and the population being studied.

[responses required]

- Construct a so-called Table 1 for groups of {aspirin, no aspirin} use, including information on age, gender, systolic blood pressure, and conscious state.

[response required]

#### Machine learning analysis
Note: for this analysis, use a simple 50-50 train-test split.

Let our outcome of interest be "dead or dependent at 6 months", i.e. so that we have a binary classification problem. What percent of patients are dead or dependent at 6 months in your train set and test set?
[response required]

Choose which variables to include in your model. For example, remove variables for outcomes at 14 days (because if you are dead at 14 days you are certainly dead at 6 months). Moreover, you should remove all features measured after baseline if you want to make a prediction based on baseline data. Similarly, specific indicators of the outcome should also be removed, since those are measurements past the baseline that are not our outcome of interest. For these reasons, you will need to remove clusters of variables. Justify your approach.
[response required]

Of the remaining variables, decide whether to exclude variables with missing data, impute them, and/or use indicator variables. (Note that if you choose multiple imputation for some variables, you would need to pool the results when evaluating performance, however for homework you may just use the first imputed data set). Justify your approach.
[response required]

Use the following machine learning algorithms: logistic regression, naive Bayes, Tree Augmented Naive Bayes, and decision tree (specify any parameters you set that are not the default). The packages that you may find useful here are: "glm", "bnlearn", and "rpart", but you may use others if desired. In a table, report the accuracy with 95% confidence intervals for each algorithm.
[response required]

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