require(dplyr)
require(corrplot)
require(ggplot2)
require(quantregForest)
require(gridExtra)
require(gtools)
setwd("D:/UMD_Krishnesh/Project/Analytics/Correlation_One")
data=read.csv(file="stock_returns_base150.csv", header=TRUE)
str(data)
table(is.na(data))
summary(data)

# Split into Train and test dataset
train_data = dplyr::filter(data, !is.na(S1))
test_data = dplyr::filter(data, is.na(S1))

# get date
train_data$date<-as.character(train_data$date)
Dates <- format(as.POSIXct(strptime(train_data$date,"%m/%d/%Y %H:%M",tz="")) ,format = "%m-%d-%Y")
train_data$date<- Dates
train_data$date<-as.Date(train_data$date, "%m-%d-%Y")

#get date
test_data$date<-as.character(test_data$date)
Dates <- format(as.POSIXct(strptime(test_data$date,"%m/%d/%Y %H:%M",tz="")) ,format = "%m-%d-%Y")
test_data$date<- Dates
test_data$date<-as.Date(test_data$date, "%m-%d-%Y")

# Correlation Matrix

corr_data<-dplyr::select(train_data, -date)

cap_cor<- cor(corr_data)
corrplot(cap_cor, method = "square", tl.cex=0.7, tl.srt = 45, tl.col = "black")


#EDA of the Stock changes over time series
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Plot individual time series graphs
p1<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S2), colour = "grey") + 
  ylab(label="S1 vs S2") + 
  xlab("Date")

p2<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S3), colour = "grey") + 
  ylab(label="S1 vs S3") + 
  xlab("Date")
p3<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S4), colour = "grey") + 
  ylab(label="S1 vs S4") + 
  xlab("Date")
p4<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S5), colour = "grey") + 
  ylab(label="S1 vs S5") + 
  xlab("Date")
p5<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S6), colour = "grey") + 
  ylab(label="S1 vs S6") + 
  xlab("Date")
p6<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S7), colour = "grey") + 
  ylab(label="S1 vs S7") + 
  xlab("Date")
p7<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S8), colour = "grey") + 
  ylab(label="S1 vs S8") + 
  xlab("Date")
p8<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S9), colour = "grey") + 
  ylab(label="S1 vs S9") + 
  xlab("Date")
p9<-ggplot(train_data, aes(x = as.Date(train_data$date, "%m-%d-%Y"))) + 
  geom_line(aes(y = S1), colour="blue") + 
  geom_line(aes(y = S10), colour = "grey") + 
  ylab(label="S1 vs S10") + 
  xlab("Date")
multiplot(p1, p2, p3, p4, p5, p6,p7,p8,p9, cols=3)



#---------------------------------------------------------------------------------------------
#Random Forest
library(randomForest)



k = 10 #Folds

# sample from 1 to k, nrow times (the number of observations in the data)
x <- NULL
for(i in 1:(nrow(train_data)/k))
{
  x <-rbind(x, data.frame(id = sample(1:k, 10, replace = FALSE)))
  
  
}

train_data <- cbind(train_data,x)
 
  

list <- 1:k

# prediction and testset data frames that we add to with each iteration over
# the folds

rf_prediction <- data.frame()
rf_testsetCopy <- data.frame()

#Creating a progress bar to know the status of CV
library(plyr)
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  rf_trainingset <- subset(train_data, id %in% list[-i])
  rf_testset <- subset(train_data, id %in% c(i))
  
  # run a random forest model
  rf_model <- quantregForest(x=rf_trainingset[,-c(1,2,12)], y=rf_trainingset$S1, data = rf_trainingset, ntree = 100)
  
  # remove response column 1, 
  rf_temp <- as.data.frame(predict(rf_model, rf_testset[,-c(1,2,12)], what=mean))
  # append this iteration's predictions to the end of the prediction data frame
  rf_prediction <- rbind(rf_prediction, rf_temp)
  
  # append this iteration's test set to the test set copy data frame

  rf_testsetCopy <- rbind(rf_testsetCopy, as.data.frame(rf_testset[,2]))
  
  progress.bar$step()
}

# add predictions
rf_result <- cbind(rf_prediction, rf_testsetCopy[, 1])
names(rf_result) <- c("Predicted", "Actual")
rf_result$Difference <- abs(rf_result$Actual - rf_result$Predicted)

# As an example use Mean Absolute Error as Evalution 
summary(rf_result$Difference)
#----------------------------------------------------------------------------
#
library(glmnet)

gm_prediction <- data.frame()
gm_testsetCopy <- data.frame()

for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  gm_trainingset <- subset(train_data, id %in% list[-i])
  gm_testset <- subset(train_data, id %in% c(i))
  
  test<-gm_trainingset[,-c(1,2,12)]
  test1<-gm_trainingset[,2]
  
  # run a elastic net regression model
  fit<-glmnet(x=as.matrix(test), y=as.matrix(test1), family="gaussian", alpha=0.5, lambda=0.001)
  
  gm_temp<-as.data.frame(predict(fit, as.matrix(gm_testset[,-c(1,2,12)]), s= 0.001))
  
  # append this iteration's predictions to the end of the prediction data frame
  gm_prediction <- rbind(gm_prediction, gm_temp)
  
  # append this iteration's test set to the test set copy data frame
 
  gm_testsetCopy <- rbind(gm_testsetCopy, as.data.frame(gm_testset[,2]))
  
  progress.bar$step()
}

# add predictions 
gm_result <- cbind(gm_prediction, gm_testsetCopy[, 1])
names(gm_result) <- c("Predicted", "Actual")
gm_result$Difference <- abs(gm_result$Actual - gm_result$Predicted)

# As an example use Mean Absolute Error as Evalution 
summary(gm_result$Difference)

#----------------------------------------------------------------------------
final_prediction<-data.frame()

# Apply mdel on test data to generate predicted values
RF_stock<- quantregForest(x=train_data[,-c(1,2,12)], y=train_data$S1, importance = TRUE, ntree = 501, data=train_data)
RF_stock


RF_stock_pred <- data.table(pred = predict(RF_stock, test_data[,-c(2,12)],what=mean))

sum(RF_stock_pred)
count(RF_stock_pred)
# Values predicted for Low confidence iterval
RF_stock_pred[,pred_low := predict(RF_stock, test_data[,-c(2,12)],what=0.025)]
# Values predicted for High Confidence Interval
RF_stock_pred[,pred_high := predict(RF_stock, test_data[,-c(2,12)],what=0.975)]
RF_stock_pred[,pred_se := predict(RF_stock, test_data[,-c(2,12)],what=sd)]
write.csv(RF_stock_pred, "Overall_Predict.csv", row.names = F)
# alpha <- 0.95
# RF_stock_sim <- data.table(sim = 1:100, melt(predict(RF_stock, test_data[,-c(2,12)],what=0.01*(1:100)), value = predS1))
# RF_High_Low <- RF_stock_sim[order(Var2)][,cumS1 := cumsum(value), by = "Var2"][,.(cumS1_low = quantile(cumS1, (1 - alpha)/2),
#                 cumS1_high = quantile(cumS1, 1 - (1 - alpha)/2)), by = "Var1"]

final_prediction<-as.data.table(cbind(Date=test_data[1],RF_stock_pred[,c("pred"),with = FALSE]))
setnames(final_prediction,"pred","Value")
setnames(final_prediction,"date","Date")

write.csv(final_prediction,"predictions.csv",row.names = F)

rf_importances=importance(RF_stock, scale=FALSE)
varImpPlot(RF_stock, main="Variable Importance")

summary(RF_stock)
