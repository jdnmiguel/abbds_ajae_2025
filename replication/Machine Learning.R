############ Objective : Apply RF on wheat growers data 
############ Author: Jeremy Do Nascimento Miguel - jeremy.dnmiguel@gmail.com
############ Date: April 2021; Update: June 2024

## UPDATE PATH ROW 27


# Code source: http://uc-r.github.io/gbm_regression
 install.packages("randomForest") 
 install.packages("lares")
 
library(randomForest)
library(haven)
library(corrplot)
library(xgboost)
library(caTools)
library(dplyr)
library(caret)
library(boot)
library(data.table)
library(lares)
require(gbm)
require(pdp)
require(scales)


data_switch <- read_dta("C:/Users/mient/Dropbox/Doctorat/Wheat Experiment 2021/replication/Data/clean/data_ML_switch.dta")


attach(data) # allow to call directly the variable through their name



summary(data)
sapply(data, class)


#drop data from game test 
data <- data[!(data$cup_gap %in% c(NA)),]

#Keep subset of variables for correlation graph
data_corr <-  subset(data, select = -c(game, gametest, id, cup_sum, cup, region, zone, market, volume_avg, entrepreneurship5_rev ))

#recode missing extraction to 0
data_corr$extraction[is.na(data_corr$extraction)] <- 0


corr_var(data_corr, # name of dataset
         cup_gap, # name of variable to focus on
           top = 15, # display top 15 correlations,
         max_pvalue=0.10, 
         tag=TRUE
)



#drop variables
data <- subset(data, select = -c(game, gametest, volume_bought, id,cup, cup_sum, region, zone, market, volume_avg, z_loc, entrepreneurship5_rev))

# Check whether we have missing data
colSums(is.na(data))

#Normalize data excepting treatment (binary)
data_scaled <- subset(data, select = -c( treatment, cup_gap, cup))
data_scaled <-  as.data.frame(scale(data_scaled))
data <- subset(data, select = c(treatment, cup_gap)) 
data <- data.frame(data, data_scaled) 

# Keep only selected variable


#Correlation matrix 
mcor <- cor(data[sapply(data, function(x) !is.factor(x))])
corrplot(mcor, method="number")


# Training and test data set
set.seed(2811)

index <- createDataPartition(data$cup_gap, p = .7, 
                             list = FALSE, 
                             times = 1)
index <- as.vector(index)
train_data <- data[index,]
test_data  <- data[-index, ]

# randomize data
set.seed(2811)
random_index <- sample(1:nrow(train_data), nrow(train_data))
random_train <- train_data[random_index, ]




## XGBOOST
# Need to one hot encode the training and testing data sets
require(vtreat)
# variable names
features <- setdiff(names(train_data), "cup_gap")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(train_data, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)     

# Prepare the training data
features_train <- vtreat::prepare(treatplan, train_data, varRestriction = new_vars) %>% as.matrix()
response_train <- train_data$cup

# Prepare the test data
features_test <- vtreat::prepare(treatplan, test_data, varRestriction = new_vars) %>% as.matrix()
response_test <- test_data$cup_gap


# dimensions of one-hot encoded data
dim(features_train)
dim(features_test)

set.seed(2811)

# create parameter list
# eta: controls the learning rate
#max_depth: tree depth
#min_child_weight: minimum number of obs required in each terminal node
#subsample: % of training data to sample for each tree
#colsample: % of columns to sample from each tree
params <- list(
  eta = .2,
  max_depth = 5 ,
  min_child_weight = 7,
  subsample = .65,
  colsample_bytree = .9
)


# create hyperparameter grid
hyper_grid <- expand.grid(
  eta = c(.2, .3, .4),
  max_depth = c(3, 5, 10),
  min_child_weight = c(3, 5, 7),
  subsample = c(.65), 
  colsample_bytree = c(.8, .9),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(2811)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 500,
    nfold = 5,
    objective = "binary:logistic",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 5 # stop if no improvement for 5 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)

## fill it up with best parameters
params <- list(
  eta = .1,
  max_depth = 5 ,
  min_child_weight = 7,
  subsample = .7,
  colsample_bytree = 0.8
)

## OPTIMAL 
xgb.fit.final <- xgboost(
  params = params,
  data = features_train,
  label = response_train,
  nrounds = 1000,
  objective = "reg:squarederror",  # for regression models
  verbose = 0               # silent,
)

# Visualizing
#1- create the importance matrix; 3 different importance measures
importance_matrix <- xgb.importance(model = xgb.fit.final)

#2- Feed this matrix 
#Gain: the relative contribution of the corresponding feature to the model
# calculated by taking each features contribution for each tree in the model.
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain", xlab = "Gain") 






require(SHAPforxgboost)
# shap.prep() returns the long-format SHAP data from either model
shap_long_data <- shap.prep(xgb_model = xgb.fit.final, X_train=features_train)

shap.importance(shap_long_data, names_only = FALSE, top_n = Inf)

# SHAP SUMMARY PLOT: from the xgboost model
shap.plot.summary.wrap1(model = xgb.fit.final, X = features_train, top_n=15)

shap_values <- shap.values(xgb_model = xgb.fit.final,  X = features_train)

shap_mean <- shap_values$mean_shap_score



cor_shape_pred <- cor(shap_values$shap_score, features_train)
cor_shape_pred <- diag(cor_shape_pred)
cor_shape_pred <- as.data.frame(cor_shape_pred)
cor_shape_pred <- setDT(cor_shape_pred, keep.rownames = TRUE)[]
cor_shape_pred <- cor_shape_pred[order(rn),] 


shap_mean <- as.data.frame(shap_mean)
shap_mean <- setDT(shap_mean, keep.rownames = TRUE)[]
shap_mean <- shap_mean[order(rn),] 

shap_mean <- subset(shap_mean, select = -c(rn))

xy <-data.frame(cor_shape_pred, shap_mean)
xy = subset(xy, select =-c(rn.1))

xy$rn <- factor(xy$rn, levels = xy$rn[order(xy$shap_mean)])
xy$rn  # notice the changed order of factor levels

#select only top15 features
xy1 <- xy %>% top_n(10, xy$shap_mean)

xy1 <- xy1 %>%  arrange(desc(shap_mean))

top15lab <- data.frame(nom = c("Quantity Sold", "# Traders known out district", "Experience",
                               "# Traders known in District", "Favorable to change", 
                               "# Employees", "Aspirations", "Risk Averse", "# Storage facilities",
                               "Present Biased"))
xy1 <-data.frame(xy1,top15lab)

xy1$nom <- factor(xy1$nom,                                    # Factor levels in decreasing order
                  levels =xy1$nom[order(xy1$shap_mean, decreasing = FALSE)])


top15xgb <- ggplot(xy1, aes(x=shap_mean, y=nom, fill=cor_shape_pred)) +
  scale_fill_gradient2(low ="blue", mid = "grey", high = "red")+
  geom_bar(width=0.7, stat = "identity") + 
  labs(fill = "Correlation", x= "Mean |Shapley|", y="Predictors") +
  theme_bw() + 
  theme ( panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Modifier le trait des axes
          axis.line = element_line(colour = "black"))

top15xgb

xgb.plot.shap(features_train, model = xgb.fit.final, top_n = 10, n_col = 3)


##### MACHINE LEARNING ON SWITCHING ROUND 
# here Y = n_left which is the first draw at which they switch 

sapply(data_switch, class)


#drop data from game test 
data <- data_switch[!(data_switch$n_left %in% c(NA)),]

#Keep subset of variables for correlation graph
data_corr <-  subset(data_switch, select = -c(game, id, cup_sum, cup, region, zone, market, volume_avg, entrepreneurship5_rev ))

#recode missing extraction to 0
data_corr$extraction[is.na(data_corr$extraction)] <- 0


corr_var(data_corr, # name of dataset
         n_left, # name of variable to focus on
         top = 15, # display top 15 correlations,
         max_pvalue=0.10, 
         tag=TRUE
)



#drop variables
data <- subset(data_switch, select = -c(game,  volume_bought, id,cup, cup_sum, region, zone, market, volume_avg, z_loc, entrepreneurship5_rev))

# Check whether we have missing data
colSums(is.na(data))

#Normalize data excepting treatment (binary)
data_scaled <- subset(data, select = -c( treatment, n_left, cup_gap))
data_scaled <-  as.data.frame(scale(data_scaled))
data <- subset(data, select = c(treatment, n_left, cup_gap)) 
data <- data.frame(data, data_scaled) 

# Keep only selected variable


#Correlation matrix 
mcor <- cor(data[sapply(data, function(x) !is.factor(x))])
corrplot(mcor, method="number")


# Training and test data set
set.seed(2811)

index <- createDataPartition(data$cup_gap, p = .7, 
                             list = FALSE, 
                             times = 1)
index <- as.vector(index)
train_data <- data[index,]
test_data  <- data[-index, ]

# randomize data
set.seed(2811)
random_index <- sample(1:nrow(train_data), nrow(train_data))
random_train <- train_data[random_index, ]




## XGBOOST
# Need to one hot encode the training and testing data sets
require(vtreat)
# variable names
features <- setdiff(names(train_data), "n_left")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(train_data, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)     

# Prepare the training data
features_train <- vtreat::prepare(treatplan, train_data, varRestriction = new_vars) %>% as.matrix()
response_train <- train_data$n_left

# Prepare the test data
features_test <- vtreat::prepare(treatplan, test_data, varRestriction = new_vars) %>% as.matrix()
response_test <- test_data$n_left


# dimensions of one-hot encoded data
dim(features_train)
dim(features_test)

set.seed(2811)

# create parameter list
# eta: controls the learning rate
#max_depth: tree depth
#min_child_weight: minimum number of obs required in each terminal node
#subsample: % of training data to sample for each tree
#colsample: % of columns to sample from each tree
params <- list(
  eta = .2,
  max_depth = 5 ,
  min_child_weight = 7,
  subsample = .65,
  colsample_bytree = .9
)


# create hyperparameter grid
hyper_grid <- expand.grid(
  eta = c(.2, .3, .4),
  max_depth = c(3, 5, 10),
  min_child_weight = c(3, 5, 7),
  subsample = c(.65), 
  colsample_bytree = c(.8, .9),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(2811)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 500,
    nfold = 5,
    objective = "binary:logistic",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 5 # stop if no improvement for 5 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)

## fill it up with best parameters
params <- list(
  eta = .1,
  max_depth = 5 ,
  min_child_weight = 7,
  subsample = .7,
  colsample_bytree = 0.8
)

## OPTIMAL 
xgb.fit.final <- xgboost(
  params = params,
  data = features_train,
  label = response_train,
  nrounds = 1000,
  objective = "reg:squarederror",  # for regression models
  verbose = 0               # silent,
)

# Visualizing
#1- create the importance matrix; 3 different importance measures
importance_matrix <- xgb.importance(model = xgb.fit.final)

#2- Feed this matrix 
#Gain: the relative contribution of the corresponding feature to the model
# calculated by taking each features contribution for each tree in the model.
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain", xlab = "Gain") 






require(SHAPforxgboost)
# shap.prep() returns the long-format SHAP data from either model
shap_long_data <- shap.prep(xgb_model = xgb.fit.final, X_train=features_train)

shap.importance(shap_long_data, names_only = FALSE, top_n = Inf)

# SHAP SUMMARY PLOT: from the xgboost model
shap.plot.summary.wrap1(model = xgb.fit.final, X = features_train, top_n=15)

shap_values <- shap.values(xgb_model = xgb.fit.final,  X = features_train)

shap_mean <- shap_values$mean_shap_score



cor_shape_pred <- cor(shap_values$shap_score, features_train)
cor_shape_pred <- diag(cor_shape_pred)
cor_shape_pred <- as.data.frame(cor_shape_pred)
cor_shape_pred <- setDT(cor_shape_pred, keep.rownames = TRUE)[]
cor_shape_pred <- cor_shape_pred[order(rn),] 


shap_mean <- as.data.frame(shap_mean)
shap_mean <- setDT(shap_mean, keep.rownames = TRUE)[]
shap_mean <- shap_mean[order(rn),] 

shap_mean <- subset(shap_mean, select = -c(rn))

xy <-data.frame(cor_shape_pred, shap_mean)
xy = subset(xy, select =-c(rn))

xy$rn <- factor(xy$rn, levels = xy$rn[order(xy$shap_mean)])
xy$rn  # notice the changed order of factor levels

#select only top15 features
xy1 <- xy %>% top_n(10, xy$shap_mean)

xy1 <- xy1 %>%  arrange(desc(shap_mean))

top15lab <- data.frame(nom = c("Favorable to change", "# Traders known out District", "Age",
                               "Aspiration", "# Traders known in District", 
                               "Experience", "Volume Sold", "Optimal Fixed cost", "# Employees",
                               "# Storage facilities"))
xy1 <-data.frame(xy1,top15lab)

xy1$nom <- factor(xy1$nom,                                    # Factor levels in decreasing order
                  levels =xy1$nom[order(xy1$shap_mean, decreasing = FALSE)])


top15xgb <- ggplot(xy1, aes(x=shap_mean, y=nom, fill=cor_shape_pred)) +
  scale_fill_gradient2(low ="blue", mid = "grey", high = "red")+
  geom_bar(width=0.7, stat = "identity") + 
  labs(fill = "Correlation", x= "Mean |Shapley|", y="Predictors") +
  theme_bw() + 
  theme ( panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Modifier le trait des axes
          axis.line = element_line(colour = "black"))

top15xgb



