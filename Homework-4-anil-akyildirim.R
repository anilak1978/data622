
library(VGAM)
library(caret)
library(caret)
library(MASS) 
library(pROC)
library(rpart.plot)
library(randomForest)
library(rpart)
library(RColorBrewer)
library(rattle)
library(rpart.plot)
library(ipred)
library(gbm)
library(randomForest)
library(xgboost)
library(Matrix)
library(datasets)
library(tidyverse)
library(nnet)
library(xgboost)

#set training and test data
set.seed(123)

training.samples <- iris$Species %>% 
  createDataPartition(p = 0.8, list = FALSE)

trdata  <- iris[training.samples, ]
tstdata <- iris[-training.samples, ]

# Multinomial Model
multinomialModel <- multinom(Species ~ ., data=trdata)
predicted_score_multinomial <- predict(multinomialModel, tstdata, "probs")
predicted_class_multinomial <- predict(multinomialModel, tstdata)
multinomialModel_cmx <- confusionMatrix(table(predicted_class_multinomial, tstdata$Species))
multinomialModel_mtab <- table(predicted_class_multinomial, tstdata$Species)


#lda model

#first normilize the data.

# Estimate preprocessing parameters
preproc.param <- trdata %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
trdata.transformed <- preproc.param %>% predict(trdata)
tstdata.transformed <- preproc.param %>% predict(tstdata)


#lda model 
lda_model<-lda(Species ~ ., data=trdata.transformed)

lda_class_probabilities<-predict(lda_model,tstdata.transformed[,1:4],type="response")

(lda_cmx<-table(lda_class_probabilities$class,tstdata.transformed[[5]]))
lda_mtab<-table(lda_class_probabilities$class,tstdata.transformed[[5]])
(lda_accuracy<-sum(diag(lda_cmx))/sum(lda_cmx))
lda_cmx<-confusionMatrix(table(lda_class_probabilities$class,tstdata.transformed[[5]]))
lda_cmx

#rpart model
rpart_model <- rpart(Species ~ ., data=tstdata, method="class")

rpart_class_probabilities<-predict(rpart_model,tstdata[,1:4],type="class")

(rpart_mtab<-table(rpart_class_probabilities,tstdata[[5]]))
rpart_cmx<-confusionMatrix(rpart_mtab)
(rpart_accuracy<-sum(diag(rpart_mtab))/sum(rpart_mtab))


#bagging

bag_model<-bagging(Species ~ ., data=trdata)

bag_class_probabilities<-predict(bag_model,tstdata[,1:4])#,type="response")

(bag_mtab<-table(bag_class_probabilities,tstdata[[5]]))
(bag_cmx<-confusionMatrix(bag_mtab))

(bag_accuracy<-sum(diag(bag_mtab))/sum(bag_mtab))

#gbm
nlev<-3 # number of classes
if(!require(gbm)) library(gbm)
gbm_model<-gbm(Species~., 	
               data=trdata,n.trees=5000,interaction.depth=nlev,
               shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",verbose=FALSE,n.cores=4)
gbm_class_probabilities<-predict(gbm_model,tstdata[,1:4],n.trees=5000,type="response")
gbm_pred<-apply(gbm_class_probabilities,1,which.max)

gbm_predicted_class<-unlist(lapply(gbm_pred,FUN=function(x)levels(tstdata[[5]])[[x]]))

(gbm_mtab<-table(gbm_predicted_class,tstdata[[5]]))
(gbm_accuracy<-sum(diag(gbm_mtab))/sum(gbm_mtab))
(gbm_cmx <- confusionMatrix(gbm_mtab))


#gbm 2
gbm_model2<-gbm(Species~.,data=trdata,n.trees=5000,interaction.depth=nlev,
                shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",
                verbose=FALSE,n.cores=4)
gbm_class_probabilities2<-predict(gbm_model2,tstdata[,1:4],n.trees=5000,type="response")
gbm_pred2<-apply(gbm_class_probabilities2,1,which.max)
gbm_pred2[which(gbm_pred2=="1")]<-levels(tstdata[[5]])[1]
gbm_pred2[which(gbm_pred2=="2")]<-levels(tstdata[[5]])[2]
gbm_pred2[which(gbm_pred2=="3")]<-levels(tstdata[[5]])[3]
gbm_pred2<-as.factor(gbm_pred2)
l<-union(gbm_pred2,tstdata[[5]])
(gbm_mtab2<-table(factor(gbm_pred2,l),factor(tstdata[[5]],l)))
(gbm_accuracy2<-sum(diag(gbm_mtab2))/sum(gbm_mtab2))
(gbm_cmx_2<-confusionMatrix(gbm_mtab2))

nlev<-4 # number of classes+1
gbm_model3<-gbm(Species~., 	
                data=trdata,n.trees=5000,interaction.depth=nlev,
                shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",verbose=FALSE,n.cores=4)
gbm_class_probabilities3<-predict(gbm_model3,tstdata[,1:4],n.trees=5000,type="response")
gbm_pred3<-apply(gbm_class_probabilities3,1,which.max)
##############
gbm_pred3[which(gbm_pred3=="1")]<-levels(tstdata[[5]])[1]
gbm_pred3[which(gbm_pred3=="2")]<-levels(tstdata[[5]])[2]
gbm_pred3[which(gbm_pred3=="3")]<-levels(tstdata[[5]])[3]
gbm_pred3<-as.factor(gbm_pred3)
l<-union(gbm_pred3,tstdata[[5]])
(gbm_mtab3<-table(factor(gbm_pred3,l),factor(tstdata[[5]],l)))
(gbm_accuracy3<-sum(diag(gbm_mtab3))/sum(gbm_mtab3))
(gbm_cmx3<-confusionMatrix(gbm_mtab3))

# random Forest
rf_model<-randomForest(Species~., data=trdata)
rf_pred<-predict(rf_model,tstdata[,1:4])
rf_mtab<-table(rf_pred,tstdata[[5]])
rf_cmx<-confusionMatrix(rf_mtab)
rf_cmx$overall
rf_cmx$byClass

#XGBoost only works with numeric vectors. 
#need to convert all other forms of data into numeric vectors.
# we use Matrix sparse.model.matrix for that
#Model 1

trdatamx<-sparse.model.matrix(Species~.-1,data=trdata)
tstdatamx<-sparse.model.matrix(Species~.-1,data=tstdata)

xgb_model<-xgboost(data=trdatamx,label=trdata$Species,max_depth = 2, 
                   eta = 1, nrounds = 2,nthread = 2, objective = "multi:softmax",num_class=5)

xgb_pred <- predict(xgb_model,tstdatamx)
xgb_tab<-table( xgb_pred)
xgb_pred[which(xgb_pred=="1")]<-levels(tstdata$Species)[1]
xgb_pred[which(xgb_pred=="2")]<-levels(tstdata$Species)[2]
xgb_pred[which(xgb_pred=="3")]<-levels(tstdata$Species)[3]
xgb_mtab<-table(xgb_pred,tstdata[[5]])
xgb_cmx<-confusionMatrix(xgb_mtab)
xgb_cmx$overall
xgb_cmx$byClass


#XGBoost Model 2

xgb_model4<-xgboost(data=trdatamx,label=trdata$Species,max_depth = 4, 
                    eta = 1, nrounds = 3,nthread = 2, objective = "multi:softmax",num_class=5)
xgb_pred4 <- predict(xgb_model4,tstdatamx)
xgb_tab4<-table( xgb_pred4)
temp_xgb_tab4<-xgb_tab4

xgb_pred4[which(xgb_pred4=="1")]<-levels(tstdata$Species)[1]
xgb_pred4[which(xgb_pred4=="2")]<-levels(tstdata$Species)[2]
xgb_pred4[which(xgb_pred4=="3")]<-levels(tstdata$Species)[3]
xgb_mtab4<-table(xgb_pred4,tstdata[[5]])
xgb_cmx4<-confusionMatrix(xgb_mtab4)
xgb_cmx4$overall
xgb_cmx4$byClass

# Xgboost model 3
xgb_model5<-xgboost(data=trdatamx,label=trdata$Species,max_depth = 5, 
                    eta = 1, nrounds = 4,nthread = 2, objective = "multi:softmax",num_class=5)
xgb_pred5 <- predict(xgb_model5,tstdatamx)
table( xgb_pred5)

xgb_tab5<-table( xgb_pred5)
temp_xgb_tab5<-xgb_tab5

xgb_pred5[which(xgb_pred5=="1")]<-levels(tstdata$Species)[1]
xgb_pred5[which(xgb_pred5=="2")]<-levels(tstdata$Species)[2]
xgb_pred5[which(xgb_pred5=="3")]<-levels(tstdata$Species)[3]
xgb_mtab5<-table(xgb_pred5,tstdata[[5]])
xgb_cmx5<-confusionMatrix(xgb_mtab5)
xgb_cmx5$overall
xgb_cmx5$byClass

txt<-capture.output({lapply(ls()[grep("cmx",ls())],FUN=function(x)eval(parse(text=x)))})

writeLines(txt,"confusionMxOutput_anil_akyildirim.txt")


mtabtxt<-capture.output({lapply(ls()[grep("mtab",ls())],FUN=function(x)eval(parse(text=x)))})
writeLines(mtabtxt,"mtabOutput_anil_akyildirim.txt") 