# Immune-based machine-learning models predict oncologic outcomes in resected biliary tract cancer
# Gradient boosting machines (GBM)
library(survival)
library(gbm)
library(caret)
library(survcomp)
library(Hmisc)
library(readr)

GBM_dss <- read_csv("C:/Users/user/Desktop/GBM_dss.csv",
                    col_types = cols(lymphnode = col_factor(levels = c("0", "1")), 
                                     pgrade = col_factor(levels = c("0", "1")), 
                                     signature = col_factor(levels = c("1", "2", "3")), 
                                     tmargin = col_factor(levels = c("0", "1")), 
                                     tumortype = col_factor(levels = c("0", "1", "2"))))
GBM_rfs <- read_csv("C:/Users/user/Desktop/GBM_rfs.csv",
                    col_types = cols(lymphnode = col_factor(levels = c("0", "1")), 
                                     pgrade = col_factor(levels = c("0", "1")), 
                                     signature = col_factor(levels = c("1", "2", "3")), 
                                     tmargin = col_factor(levels = c("0", "1")), 
                                     tumortype = col_factor(levels = c("0", "1", "2"))))
# Note.
# lymphnode_levels = c(0,1),labels = c("Negative","Positive")
# pgrade_levels = c(0,1),labels = c("Moderate to well","Poor")
# signature_levels = c(1,2,3),labels = c("Cluster 1","Cluster 2","Cluster 3")
# tmargin_levels = c(0,1),labels = c("Negative","Positive")
# tumortype_levels = c(0,1,2),labels = c("Intrahepatic","Extrahepatic","Gallbladder")

train_dss_data = data.frame(GBM_dss)
train_rfs_data = data.frame(GBM_rfs)

gbm_dss_model <- gbm(Surv(time, status) ~ ., data = train_dss_data, distribution = "coxph",n.trees = 3000,
                 interaction.depth = 3, n.minobsinnode = 5, shrinkage = 0.001, cv.folds = 10)
gbm_rfs_model <- gbm(Surv(time, status) ~ ., data = train_rfs_data, distribution = "coxph",n.trees = 3000,
                     interaction.depth = 3, n.minobsinnode = 5, shrinkage = 0.001, cv.folds = 10)

train_dss_predict <- predict(gbm_dss_model, newdata = train_dss_data , n.trees = 3000, type = "link")
train_rfs_predict <- predict(gbm_rfs_model, newdata = train_rfs_data , n.trees = 3000, type = "link")
write.csv(train_dss_predict,file = "C:/Users/user/Desktop/train_dss_Y.csv",quote=F)
write.csv(train_rfs_predict,file = "C:/Users/user/Desktop/train_rfs_Y.csv",quote=F)

# Independent external validation

validation_data <- read_csv("C:/Users/user/Desktop/",
                    col_types = cols(lymphnode = col_factor(levels = c("0", "1")), 
                                     pgrade = col_factor(levels = c("0", "1")), 
                                     signature = col_factor(levels = c("1", "2", "3")), 
                                     tmargin = col_factor(levels = c("0", "1")), 
                                     tumortype = col_factor(levels = c("0", "1", "2"))))
validation_data = data.frame(validation_data)

validation_dss_predict <- predict(gbm_dss_model, newdata = validation_data, n.trees = 3000, type = "link")
validation_rfs_predict <- predict(gbm_rfs_model, newdata = validation_data, n.trees = 3000, type = "link")
write.csv(validation_dss_predict,file = "C:/Users/user/Desktop/validation_dss_Y.csv",quote=F)
write.csv(validation_rfs_predict,file = "C:/Users/user/Desktop/validation_rfs_Y.csv",quote=F)