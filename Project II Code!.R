#number of observations (rows) in the dataframe
obs_count<-dim(nl2_TIDY_DATA)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(.7 * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

train <- nl2_TIDY_DATA[train_ind, ] 
test<- nl2_TIDY_DATA[-train_ind, ] 

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(train)
dim(test)
View(nl2_TIDY_DATA)

M1 <- lm(RBI ~ Hits, train)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, train) 
View(PRED_1_IN)

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, test) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-train$RBI)^2)/length(PRED_1_IN))  
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-test$RBI)^2)/length(PRED_1_OUT)) 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,81,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(Hits=x_grid))
plot(train$RBI ~ train$Hits, col='blue')
lines(x_grid, predictions, col='black', lwd=3)
points(test$RBI ~ test$Hits, col='red', pch=3)
train$Hits2<- train$Hits^2
test$Hits2<- test$Hits^2
#MODEL2
M2<- lm(RBI ~ Hits + Hits2, train)
summary(M2)
#Predictions on Training Data (IN SAMPLE)
PRED_2_IN<- predict(M2, train)
View(PRED_2_IN)
#Predictions on Testing Data for Benchmarks (OUT of SAMPLE)
PRED_2_OUT <- predict(M2, test)
View(PRED_2_OUT)
#Calculate Errors 
RMSE_2_IN<- sqrt(sum((PRED_2_IN-train$RBI)^2)/length(PRED_1_IN))
RMSE_2_OUT<- sqrt(sum((PRED_2_OUT-test$RBI)^2)/length(PRED_1_OUT))
RMSE_2_IN 
RMSE_2_OUT 

#Plotting the Model 
x_grid <- seq(0,81,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(Hits=x_grid, Hits2=x_grid^2))
plot(train$RBI ~ train$Hits, col='blue')
lines(x_grid, predictions, col='black', lwd=3)
points(test$RBI ~ test$Hits, col='red', pch=3)

#MODEL3
M3 <- lm(RBI ~ Hits + Walks, train)
summary(M3)
#Predictions on Training Data (IN SAMPLE)
PRED_3_IN<- predict(M3, train)
View(PRED_3_IN)
#Predictions on Testing Data for Benchmarks (OUT of SAMPLE)
PRED_3_OUT <- predict(M3, test)
View(PRED_3_OUT)
#Calculate Errors 
RMSE_3_IN<- sqrt(sum((PRED_3_IN-train$RBI)^2)/length(PRED_1_IN))
RMSE_3_OUT<- sqrt(sum((PRED_3_OUT-test$RBI)^2)/length(PRED_1_OUT))
RMSE_3_IN 
RMSE_3_OUT
#Plotting the Model   
x_grid <- seq(0,81,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(Hits=x_grid, Walks=x_grid^2))
plot(train$RBI ~ train$Hits, col='blue')
lines(x_grid, predictions, col='black', lwd=3)
points(test$RBI ~ test$Hits, col='red', pch=3)

#MODEL4
M4<- lm(RBI ~ Runs, train)
summary(M4)
#Predictions on Training Data (IN SAMPLE)
PRED_4_IN <- predict(M4, train) 
View(PRED_4_IN)
#Predictions on Testing Data for Benchmarks (OUT of SAMPLE)
PRED_4_OUT <- predict(M4, test) 
View(PRED_4_OUT)
#Calculate Errors 
RMSE_4_IN<-sqrt(sum((PRED_4_IN-train$RBI)^2)/length(PRED_4_IN))  
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-test$RBI)^2)/length(PRED_4_OUT)) 
RMSE_4_IN
RMSE_4_OUT
#Plotting the Model   
x_grid <- seq(0,81,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M4, list(Runs=log(x_grid)))
plot(train$RBI ~ train$Hits, col='blue', ylim=c(0, 50))
lines(x_grid, predictions, col='black', lwd=3)
points(test$RBI ~ test$Hits, col='red', pch=3)

#MODEL COMPARISON 
RMSE_1_IN 
RMSE_2_IN 
RMSE_3_IN 
RMSE_4_IN 

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT 
RMSE_2_OUT 
RMSE_3_OUT 
RMSE_4_OUT 

###PLOTTING THE REGRESSION MODELS AGAINST ONE ANOTHER###
x_grid <- seq(0,81,.1) #CREATES GRID OF X-AXIS VALUES
plot(train$RBI ~ train$Hits, col='blue')
predictions_1 <- predict(M1, list(Hits=x_grid))
predictions_2 <- predict(M2, list(Hits=x_grid, Hits2=x_grid^2))
predictions_3 <- predict(M3, list(Hits=x_grid, Walks=x_grid^2))
predictions_4 <- predict(M4, list(Runs=log(x_grid)))

lines(x_grid, predictions_1, col='yellow', lwd=3) 
lines(x_grid, predictions_2, col='black', lwd=3) 
lines(x_grid, predictions_3, col='purple', lwd=3) 
lines(x_grid, predictions_4, col='orange', lwd=3) 
points(test$RBI ~ test$Hits, col='red', pch=3)

