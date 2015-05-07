library(randomForest)

setwd("C:/Users/VIVEK/Desktop/kaggle/Restaurant Revenue Prediction")

train<-read.csv("train.csv")
test  <- read.csv("test.csv")

n.train <- nrow(train)

# Add revenue column in test data set
test$revenue <- 1
myData <- rbind(train, test)
rm(train, test)

#Tranform date to number of open days
myData$Open.Date <- as.POSIXlt("01/01/2015", format="%m/%d/%Y") - as.POSIXlt(myData$Open.Date, format="%m/%d/%Y")
myData$Open.Date <- as.numeric(myData$Open.Date / 1000) #Scale for factors

# Consider value '0' as NA & then impute using na.roughfix function from RandomForest
for(i in 1:37){
  myData[which(myData[,paste0('P',i)]==0),paste0('P',i)] = NA
  myData[,paste0('P',i)] <- na.roughfix(myData[,paste0('P',i)])
}
rm(i)

# Transform City classes into "Other" & rest of the classes
myData$City                                      <- as.character(myData$City)
myData$City[myData$City.Group == "Other"]        <- "Other"
myData$City[myData$City == unique(myData$City)[4]] <- unique(myData$City)[2]
myData$City                                      <- as.factor(myData$City)
myData$City.Group                                <- NULL

#revenue outlier replacement 
box.plot1 <- boxplot(myData[1:n.train,]$revenue,plot=F)
myData[which(myData[1:n.train,]$revenue>box.plot1$stats[5]),'revenue'] = box.plot1$stats[5]
boxplot(myData[1:n.train,]$revenue,plot=T)

#Model & Prediction
rf_model2 <- randomForest(revenue~Open.Date+City+P29+P28+P22+P8+P19+P1+P20+P12+P6+P21+P23+P2+P11+P13+P10+P17+P5+P3+Type+P26+P4+P25+P9+P27+P32+P15+P14+P18,data=myData[1:n.train,])
prediction <- predict(rf_model2, myData[-c(1:n.train),])
head(prediction)

#Make Submission
submit<-as.data.frame(cbind(seq(0, length(prediction) - 1, by=1), prediction))
colnames(submit)<-c("Id","Prediction")
write.csv(submit,"submission.csv",row.names=FALSE,quote=FALSE)
