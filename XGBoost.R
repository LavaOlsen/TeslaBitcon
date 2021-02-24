####################################################################################################################
####################################################################################################################
#                                                ****                                                              #
#                                 Advanced topics in tree-based models:                                            #
#                                                                                                                  #
#                                   eXtreme Gradient Boosting                                                      #
#                                             (xgboost)                                                            #
#                                                ****                                                              #  
####################################################################################################################
####################################################################################################################


# 1. Introduction
##################################################################################################################################################
#   * Extreme Gradient boosting (xgboost) generates models using the same general boosting learning process discussed before
#   * It is similar to Gradient Boosting, but even more efficient 
#   * It is very popular in data science currently -->
#   * It is consistently used to win machine learning competitions on Kaggle 👀
#   * You may google for this new algorithm and take it as an assignment or implement it into you Master thesis project 
#   * https://en.wikipedia.org/wiki/XGBoost
#   * xgboost is a very effective implementation of an old idea coming from Adaboost and gradient boosted trees
#   * handles both factor and numeric variables


# * The idea and the evolution of xgboost shortly explained:
#   * Boosting (sequentially building new classifiers using weights basing on outcomes of the previous ones)
#   * Adaboost (ARCing, improving a predictor using iterative reweighting of incorrectly classified cases) -->
#   * Boosted trees (each model in a chain of predictor corrects mistakes of the previous one) -->
#   * Gradient Boosting (+ greedy optimization with gradient descent) -->
#   * eXtreme Gradient Boosting (+ regularization to prevent overfitting + an efficient implementation)
#  


#   * xgboost can be used for classification and regression using function ´xgboost´ in R 
  # * xgboost - the tuning parameters are:
  #     - Number of Boosting Iterations (nrounds, numeric) - length of chain
  #     - Max Tree Depth (max_depth, numeric) - depth of a tree (default: 6, typical: 3-10)
  #     - Shrinkage (eta, numeric) - reducing contribution of subsequent models by shrinking the weights (default: 0.3, typical: 0.01-0.2)
  #     - Minimum Loss Reduction (gamma, numeric) - spliting nodes only when the split gives positive reduction in the loss function (default: 0)
  #     - Subsample Ratio of Columns (colsample_bytree, numeric) - fraction of columns to be sampled randomly for each of the trees (default: 1, typical: 0.5-1)
  #     - Minimum Sum of Instance Weight (min_child_weight, numeric) - minimum sum of weight for a child node; protects overfit (default: 1)
  #     - Subsample Percentage (subsample, numeric) - fraction of observations to be sampled randomly for each of the trees (default: 1, typical: 0.5-1)


#   * To implement xgboost you can use it with package `caret` , or without it
#   * Below I show an example using package `caret`
#   *`caret` contains a set of functions that attempt to streamline (make more efficient 👌) the process for many creating predictive models

# *   More about `caret`
#       * A unified interface 👌 for many of the models we have seen so far
#       * 'caret' can do:
#         * Dataset splits (e.g. learning / test)
#         * Data preprocessing:
#           - dummy variables
#           - finding zero- and near-zero predictors
#           - finding and removing correlated predictors
#           - centering and scaling
#         * Building / learning models (e.g. with cross-validation).
#         * Automatic parameter tunning  


# 2. Application
##################################################################################################################################################

#   * we use OJ dataset (ISL textbook) 
#   * see description of data in the package ISLR 
#   * the objective in this case is classification: predict which brand of juice the individual prefer and will purchase
#   * the dependent variable has two levels: brand CH and brand MM

library(caret)
library(xgboost)
library(ISLR)

attach(OJ)
View(OJ)

set.seed(1013)

train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]


#  2.1 Fiting and comparing models
##################################################################################################################################################

# model fitting 
#  *  to fit models we are using the funcion 'train' from caret 
#  *  basic syntax of 'train' function in 'caret´ has three parameters: train (formula, data, method) 
#  *  'method' allows us to handle many kinds of models including:
#         * tree-based models (ctree2, rpart)
#         * boosting models (gbm, gamboost)
#         * bagged models (treebag, bagEarth)
#         * random forest (rf, cforest, qrf)
#         * linear regression models (enet, pcr, glmnet)
#         * neural networks (nnet, pcaNNet, neuralnet). See https://www.rdocumentation.org/packages/caret/versions/4.20/topics/train

#  *  below I used ctree2, a basic-tree algorithm, which has only 2 hyperparametrs: max depth and min criterion
#  *  by default, ´train´ function determines the distinct number of values of the parameters 👀 (but, we can also set them or tune them ourselves - see last part below) 
#  *  by default ´train´ applies bootstraping but, if we want to implement cross-validation we can do so 
#  *  below I implemented three repeats of 5-fold cv --> 
#  *  to do so,I first defined train.param using the ´trainControl´ function, which basically handles some of the optional arguments of function 'train'.


# training parameters
train.param <- trainControl(method = "cv", number = 5) #She did some modifications to the traincontrol, since it is classifications

# we try several models:

# 1) basic decision tree
model.tree <- train(Purchase ~ ., OJ.train,
                    method = "ctree2",         
                    trControl = train.param) #For classifications she did some iterations to the model to defeine the best tree


# now check: 
model.tree # fit and additional info

  #' We see that we have accuracy and kappa, by changing step 1 to have metrix = "ROC"
  #' we are able to present sensitivity and specificity

model.tree$finalModel # the best model description - trained on the full dataset (re-fitted)
plot(model.tree$finalModel)

model.tree$metric # model performance metrics used; other metrics can be used if explicitly mentioned in ´train´ function
model.tree$results # coefficients of the model (if showing them makes any sense) + performance criteria



# 2) random forest 1
model.rf.1 <- train(Purchase ~ ., OJ.train,
                    method = "rf", ntree = 10,
                    trControl = train.param)
model.rf.1

# 3) random forest 2
model.rf.2 <- train(Purchase ~ ., OJ.train,
                    method = "rf", ntree = 150,
                    trControl = train.param)
model.rf.2

# 4) xgboost
model.xgboost <- train(Purchase ~ ., OJ.train,
                       method = "xgbTree",
                       tuneGrid = data.frame(.nrounds=300, .max_depth=3,
                                             .eta=0.03,.gamma=0,
                                             .subsample=0.5, .colsample_bytree=0.1,
                                             .min_child_weight = 1),
                       trControl = train.param)
model.xgboost


# gathering of the results
results <- resamples(list(tree = model.tree,
                          rf.1  = model.rf.1,
                          rf.2  = model.rf.2,
                          xboost = model.xgboost))

# summary
summary(results)
bwplot(results)
dotplot(results)

# In the output we can see tables of Accuracy and (Cohen´s) Kappa for each model evaluated -->
# these are the default metrics used to evaluate algorithms on binary and multi-class classification datasets in 'caret' package
# Both accurary and kappa are taken over the population of cross validation folds and trials.
# For our data, we notice xboost model is performing the best in terms of accuracy and kappa.

#   * About Cohen´s Kappa: 
#     * accuracy is the percentage of correctly classified instances out of all instances. 
#     * Cohen’s Kappa is like accuracy, except that it is normalized at the baseline of random chance. 
#     * e.g. in our data, given 60-40 split for classes 1 (CH) and 0 (MM) in our sample, we can achieve 60% accuracy by predicting all instances are for class 1
#     * Kappa = (O-E)/(1-E) where O is the observed accuracy and E is the expected accuracy.
#     * Kappa is considered by some data scientists as a more useful measure to use on problems that have an imbalance in the classes.

#     * Cohen suggested the Kappa result be interpreted as follows: 
#         * values ≤ 0 as indicating no agreement and 0.01–0.20 as none to slight, 
#         * 0.21–0.40 as fair, 0.41– 0.60 as moderate, 
#         * 0.61–0.80 as substantial
#         * 0.81–1.00 as almost perfect agreement.
#         * Reference: McHugh, M. 2012 "Interrater reliability: the kappa statistic" for more details. 



# 2.2. Assessing predictive power using test data (unseen data) 
####################################################################################################################

# 1) tree
# confusion matrix 
class.real.tree <- OJ.test$Purchase # actual class
class.pred.tree <- predict(model.tree, OJ.test, type = "raw") # predicted class
scoring.tree    <- predict(model.tree, OJ.test, type = "prob") [, "CH"] # predicted ["yes"] class probability

confusionMatrix(data = class.pred.tree, reference = class.real.tree, positive = "CH") # we can point the positive class "CH" (the "1")

# ROC and AUC
library(caTools) 
colAUC(scoring.tree , class.real.tree, plotROC = TRUE) 


# 2) random forest 1
# confusion matrix 
class.real.rf.1 <- OJ.test$Purchase 
class.pred.rf.1 <- predict(model.rf.1, OJ.test, type = "raw") 
scoring.rf.1    <- predict(model.rf.1, OJ.test, type = "prob") [, "CH"] 

confusionMatrix(data = class.pred.rf.1, reference = class.real.rf.1, positive = "CH") 

# ROC and AUC
library(caTools) 
colAUC(scoring.rf.1 , class.real.rf.1, plotROC = TRUE) 


# 3) random forest 2
# confusion matrix 
class.real.rf.2 <- OJ.test$Purchase 
class.pred.rf.2 <- predict(model.rf.2, OJ.test, type = "raw") 
scoring.rf.2    <- predict(model.rf.2, OJ.test, type = "prob") [, "CH"] 

confusionMatrix(data = class.pred.rf.2, reference = class.real.rf.2, positive = "CH") 

# ROC and AUC
library(caTools) 
colAUC(scoring.rf.2 , class.real.rf.2, plotROC = TRUE) 



# 4) xgboost
# confusion matrix 
class.real.xgb <- OJ.test$Purchase 
class.pred.xgb <- predict(model.xgboost, OJ.test, type = "raw")
scoring.xgb    <- predict(model.xgboost, OJ.test, type = "prob") [, "CH"] 

confusionMatrix(data = class.pred.xgb, reference = class.real.xgb, positive = "CH")  

# ROC and AUC
library(caTools) 
colAUC(scoring.xgb , class.real.xgb, plotROC = TRUE) # the winner ✌️ ! the winner �✌️



# variable importance
####################################################################################################################
# var.imp <- varImp(model.xgboost, scale = FALSE)
# plot(var.imp)
# var.imp$importance


# notes: 
#######################################################################################################################
# * In the past, many classification models were used including Logistic Regression and Discriminant analysis.
# * Nowadays, it is trendy to use xgboost
# * But sometimes xgbbost is too complicated, or there is no need to use it




# 2.3. Tuning (hyper)parameters
####################################################################################################################

#  * As you may observed, each model has its parameters and their choice influences greatly the model performance
#  * ´train' function allows to conduct resampling over a grid of tuning values of the parameters


# to get reproducible results
set.seed(123)

# let us define a space of feasible parameters 
# to do so, we create a data frame with one row per tuning parameter combination and the parameters start with periods.

#  example for a decision tree
   # ctree2 has two main parameters maxdepth and mincriterion, so we define some values in a data.frame (the selection of this values may be tricky..)
tune.grid <- data.frame(.maxdepth = 3:6, .mincriterion = c(.1, .2, .3, .4)) # no `expand.grid` yet

# train the model by adding tune.grid in our function
train.res <- train(Purchase ~ ., OJ.train, 
                   method = "ctree2",
                   trControl = train.param,
                   tuneGrid = tune.grid)
train.res
plot(train.res)
# the best model - trained on the full dataset
train.res$finalModel

# an alternative to data.frame is  
  #   * ´expand.grid´: all combinations are explored
  #   * ´seq´ : even more values can be considered
  #   * E.g.:  tune.grid <- expand.grid(maxdepth = seq(3,6,1), mincriterion = seq(.1,.4, 0.1)) 



#  example for xgboost
    #  notice there are quite a few parameters to play with in this case; processing time will be very long..

set.seed(123)

tune.grid.xgboost <- expand.grid(max_depth=3:6, gamma=c(0, 1, 2, 3, 5), eta=c(0.03, 0.06, 0.1, 0.2), nrounds=300,
  subsample=0.5, colsample_bytree=0.1, min_child_weight = 1)


model.xgboost <- train(Purchase ~ ., OJ.train,
                       method = "xgbTree",
                       tuneGrid = tune.grid.xgboost,
                       trControl = train.param)

model.xgboost
plot(model.xgboost)
var.imp <- varImp(model.xgboost, scale = FALSE)
plot(var.imp)
var.imp$importance



#  example for gradient boosting
set.seed(123)
Grid <- expand.grid(n.trees = seq(50,1000,50), interaction.depth = c(30), shrinkage = c(0.1), n.minobsinnode=10)

fit.gbm <- train(Purchase ~ ., OJ.train,
                 method = 'gbm',
                 tuneGrid=Grid,
                 trControl=train.param)
fit.gbm
plot(fit.gbm)


#  Finally, we can simply set random so that the computer selects randomply the parameters 
#   example for gradient boosting with ´search = random´ selection of parameters

train.param <- trainControl(method = "cv", number = 5, search="random")

fit.gbm <- train(Purchase ~ ., OJ.train,
                 method = 'gbm',
                 trControl=train.param, tuneLength=30)



####################################################################################################################
# exercises

# 1. Compare performance of previous classifiers with different sets of parameters (including default values). 
# 2. Select another dataset (e.g. choose a classier from the proposed exercises in ISL) and try to build a possibly good model using ´xgboost´ 





# 2.4. Exporting the models for sharing and deployment of data mining results between applications and across data management systems
#####################################################################################################################################

# Predictive Modeling Markup Language (PMML) is an XML-based standard for representing data mining results. 
# PMML is designed to enable the sharing and deployment of data mining results between applications and across data management systems. 
# We can import PMML models that are produced below by using other applications supporing PMML like SAS and Python.


library(pmml)

# ordered factor not supported by pmml
OJ.train$Purchase <- factor(OJ.train$Purchase, ordered = FALSE)

m.rf <- randomForest(Purchase ~ ., data = OJ.train, ntree = 20)

saveXML(pmml(m.rf, data = OJ.train), "random-forest-model.pmml") # you need to use formula interface to be able to export the model


# End
#####################################################################################################################################



