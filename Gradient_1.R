library(car)
library(MASS)
library(Metrics)
library(h2o)

#Reading the data files
purchase<-read.csv('train.csv',stringsAsFactors = FALSE,header = TRUE)
View(purchase)

##Data Cleaning
##checking on the missing values and empty values

sum(is.na(purchase))
sapply(purchase,function(x) sum(is.na(x)))

##replacing the missing values with zeroes

purchase[is.na(purchase)]<-0


h2o.server <- h2o.init( nthreads= -1)

#Converting all columns to factors

selCols = names(purchase)[1:11]
purchase[selCols]<-lapply(purchase[selCols],factor)
str(purchase)

purchase.h2o<-as.h2o(purchase)
View(purchase.h2o)

features=c("User_ID","Product_ID")


gbmF_model_1 = h2o.gbm( x=features,
                        y = "Purchase",
                        training_frame =purchase.h2o ,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =500,
                        learn_rate = 0.05,
                        nbins_cats = 5891
)

gbmF_model_2 = h2o.gbm( x=features,
                        y = "Purchase",
                        training_frame = purchase.h2o,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =430,
                        learn_rate = 0.04,
                        nbins_cats = 5891
)

dl_model_1 = h2o.deeplearning( x=features,
                               y = "Purchase",
                               training_frame =purchase.h2o,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=60,
                               adaptive_rate =F
)

##making the test dataset ready

#Reading the data files
purchase_test<-read.csv('test.csv',stringsAsFactors = FALSE,header = TRUE)
View(purchase)

##Data Cleaning
##checking on the missing values and empty values

sum(is.na(purchase_test))
sapply(purchase_test,function(x) sum(is.na(x)))

##replacing the missing values with zeroes

purchase_test[is.na(purchase_test)]<-0


h2o.server <- h2o.init( nthreads= -1)

#Converting all columns to factors

selCols_test = names(purchase_test)[1:11]
purchase_test[selCols_test]<-lapply(purchase_test[selCols_test],factor)
str(purchase)

purchase.h2o_test<-as.h2o(purchase_test)
View(purchase.h2o)


#Making the predictions for model_1
testPurchase_gbm_1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = purchase.h2o_test) )

testPurchase_gbm_1$predict=ifelse(testPurchase_gbm_1$predict<0,0,testPurchase_gbm_1$predict)
testPurchase_gbm_1$predict

#Making the predictions for model_2
testPurchase_gbm_2 = as.data.frame(h2o.predict(gbmF_model_2, newdata = purchase.h2o_test) )

testPurchase_gbm_2$predict=ifelse(testPurchase_gbm_2$predict<0,0,testPurchase_gbm_2$predict)
testPurchase_gbm_2$predict

#Making the predictions for dl_model_1
dl_modeltest_1= as.data.frame(h2o.predict(dl_model_1, newdata = purchase.h2o_test) )

dl_modeltest_1$predict=ifelse(dl_modeltest_1$predict<0,0,dl_modeltest_1$predict)

final<-0.4*(testPurchase_gbm_1$predict)+
       0.4*(testPurchase_gbm_2$predict)+
       0.2*(dl_modeltest_1$predict)      
submission<-as.data.frame(cbind(purchase_test$User_ID,purchase_test$Product_ID,final))
purchase_test$User_ID

rmse<-rmse(purchase$Purchase,final)
rmse

colnames(submission)<-c("User_ID","Product_ID","Purchase")
View(submission)
write.csv(submission,"solution_prediction.csv",row.names = F)

