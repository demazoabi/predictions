set.seed(123)
setwd("C:/Users/MY/Downloads")
getwd()
actualdata=read.csv("CLV_8637rows.csv")
setwd("C:/Users/MY/Downloads")
getwd()
testdata=read.csv("CLV_NoTargetVar_PREDICT.csv")
install.packages("Metrics")
library(Metrics)
install.packages("class")
library(class)
##regression 
reg=lm(actualdata[1:497,]$CLV ~ actualdata[1:497,]$Income+ actualdata[1:497,]$Gender
       +actualdata[1:497,]$EmploymentStatus+ actualdata[1:497,]$Education
       + actualdata[1:497,]$MonthsSinceLastClaim+ actualdata[1:497,]$Coverage
       + actualdata[1:497,]$NumberOfPolicies+ actualdata[1:497,]$NumberOfOpenComplaints
       + actualdata[1:497,]$VehicleClass)
reg_data=testdata
reg_data$CLV=predict(reg, newdata=testdata)
reg_mae=mae(actualdata$CLV,reg_data$CLV)
summary(reg_mae)

##knn 
k_data=testdata
pre_data=actualdata
pre_data$Income <- as.numeric(factor(pre_data$Income))
pre_data$Gender <-  as.numeric(factor(pre_data$Gender))
pre_data$EmploymentStatus <- as.numeric(factor(pre_data$EmploymentStatus))
pre_data$Education <- as.numeric(factor(pre_data$Education))
pre_data$MonthsSinceLastClaim <- as.numeric(factor(pre_data$MonthsSinceLastClaim))
pre_data$Coverage <- as.numeric(factor(pre_data$Coverage))
pre_data$NumberOfPolicies <- as.numeric(factor(pre_data$NumberOfPolicies))
pre_data$NumberOfOpenComplaints <- as.numeric(factor(pre_data$NumberOfOpenComplaints))
pre_data$VehicleClass <- as.numeric(factor(pre_data$VehicleClass))
train_data <- pre_data[, c("Income", "Gender", "EmploymentStatus", "Education",
                           "MonthsSinceLastClaim", "Coverage", "NumberOfPolicies",
                           "NumberOfOpenComplaints", "VehicleClass")]
train_class <- pre_data[, "CLV"]
k_data$Income <- as.numeric(factor(k_data$Income))
k_data$Gender <-  as.numeric(factor(k_data$Gender))
k_data$EmploymentStatus <- as.numeric(factor(k_data$EmploymentStatus))
k_data$Education <- as.numeric(factor(k_data$Education))
k_data$MonthsSinceLastClaim <- as.numeric(factor(k_data$MonthsSinceLastClaim))
k_data$Coverage <- as.numeric(factor(k_data$Coverage))
k_data$NumberOfPolicies <- as.numeric(factor(k_data$NumberOfPolicies))
k_data$NumberOfOpenComplaints <- as.numeric(factor(k_data$NumberOfOpenComplaints))
k_data$VehicleClass <- as.numeric(factor(k_data$VehicleClass))
test= k_data[, c("Income", "Gender", "EmploymentStatus", "Education",
                 "MonthsSinceLastClaim", "Coverage", "NumberOfPolicies",
                 "NumberOfOpenComplaints", "VehicleClass")]
k_data$CLV <- knn(train_data,test,train_class,11)
knn_mae=mae(as.numeric(factor(actualdata$CLV)), as.numeric(factor(k_data$CLV)))
summary(knn_mae)

##decision tree
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
datawithoutID=actualdata[,2:22]
actualtree =rpart(CLV~., data = datawithoutID)
plot(actualtree)
text(actualtree)
rpart.plot(actualtree)
tree_data=testdata[2:21]
tree_data$CLV=predict(actualtree,tree_data)
pre_tree=rpart(CLV~.,tree_data )
plot(pre_tree)
text(pre_tree)
rpart.plot(pre_tree)
tree_mae=mae(actualdata$CLV,tree_data$CLV)
summary(tree_mae)

# i choose knn -final
final_predictions = data.frame("CustomerID" = testdata$CustomerID, "CLV" = k_data$CLV)
write.csv(final_predictions, "Predictions211407291.csv", row.names = F)

final_mae = data.frame("MAE" = 3664)
write.csv(final_mae, "MAE211407291.csv", row.names = F)
