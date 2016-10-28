#load train and test file
BMS.Train <- read.csv("C:/Users/Nishit/Downloads/AV/Big Mart Sales/BMS-Train.csv")
View(BMS.Train)
BMS.Test <- read.csv("C:/Users/Nishit/Downloads/AV/Big Mart Sales/BMS-Test.csv")
View(BMS.Test)

#add a column
BMS.Test$Item_Outlet_Sales <- 1

#combine the data set
BMS.Combi <- rbind(BMS.Train,BMS.Test)

#impute missing values with median
BMS.Combi$Item_Weight[is.na(BMS.Combi$Item_Weight)] <- median(BMS.Combi$Item_Weight, na.rm = TRUE)

#impute 0 with median
BMS.Combi$Item_Visibility <- ifelse(BMS.Combi$Item_Visibility == 0, median(BMS.Combi$Item_Visibility),BMS.Combi$Item_Visibility)

#find mode and impute
table(BMS.Combi$Outlet_Size, BMS.Combi$Outlet_Type)
levels(BMS.Combi$Outlet_Size)[1] <- "Other"

#remove the dependent and identifier variables
Data_PCA <- subset(BMS.Combi, select = -c(Item_Outlet_Sales, Item_Identifier, Outlet_Identifier))

#check available variables' class
str(Data_PCA)

#load library
library(dummies)

#create a dummy data frame
new_Data_PCA <- dummy.data.frame(Data_PCA, names = c("Item_Fat_Content","Item_Type",
                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                   "Outlet_Location_Type","Outlet_Type"))

#check available variables' class
str(new_Data_PCA)

#divide the new data
Train_PCA <- new_Data_PCA[1:nrow(BMS.Train),]
Test_PCA <- new_Data_PCA[-(1:nrow(BMS.Train)),]

#principal component analysis
#scale. = T, to normalize the variables to have standard deviation = 1
prin_comp <- prcomp(Train_PCA, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

#pricipal component loadings of 44 PCAs
prin_comp$rotation

#first 4 principal components and first 5 rows
prin_comp$rotation[1:5,1:4]

#dimensions of matrix x with principal component score vectors
dim(prin_comp$x)

#plot the resultant principal components
biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2
pr_var

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained by first 20 principal components
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot, type=b (both points and lines)
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot, type=l (lines)
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "l")

#addind a training set with principal components
BMS_Training <- data.frame(Item_Outlet_Sales = BMS.Train$Item_Outlet_Sales, prin_comp$x)

#Considering the first 30 PCAs which account for around 98% of the total variance
BMS_Training <- BMS_Training[,1:31]

#running a decision tree model on training set
library(rpart)
rpart.model <- rpart(Item_Outlet_Sales ~ .,data = BMS_Training, method = "anova")
rpart.model

#transforming test into PCA
BMS_Testing <- predict(prin_comp, newdata = Test_PCA)
BMS_Testing <- as.data.frame(BMS_Testing)

#Considering the first 30 PCAs 
BMS_Testing <- BMS_Testing[,1:31]

#making prediction on test data using decision tree
rpart.prediction <- predict(rpart.model, BMS_Testing)


Output <- read.csv("C:/Users/Nishit/Downloads/AV/Big Mart Sales/Submission.csv")
Final_Submission <- data.frame(Item_Identifier = Output$Item_Identifier, 
                               Outlet_Identifier = Output$Outlet_Identifier, 
                               Item_Outlet_Sales = rpart.prediction)

write.csv(Final_Submission, "Submission1.csv",row.names = F)

cor(BMS_Training)

#Applying Regression
LinearRegression=lm(BMS_Training$Item_Outlet_Sales~., data = BMS_Training)
summary(LinearRegression)

#removing variables with high p-value
#BMS_Training1=subset(BMS_Training,select = -c(PC2, PC8, PC17, PC22, PC26))
#LinearRegression1=lm(BMS_Training1$Item_Outlet_Sales~., data = BMS_Training1)
#summary(LinearRegression1)

#making prediction on test data using regression
lr.prediction=predict(LinearRegression,BMS_Testing)

Output1 <- read.csv("C:/Users/Nishit/Downloads/AV/Big Mart Sales/Submission.csv")
Final_Submission <- data.frame(Item_Identifier = Output1$Item_Identifier, 
                               Outlet_Identifier = Output1$Outlet_Identifier, 
                               Item_Outlet_Sales = lr.prediction)

write.csv(Final_Submission, "Submission2.csv",row.names = F)

#Applying Random Forests
install.packages("randomForest")
library(randomForest)

#Building and Applying the Model
#set random seed
set.seed(1234)

rf.model <- randomForest(BMS_Training$Item_Outlet_Sales~.,data=BMS_Training, n.trees=250,interaction.depth=8, importance = T)


