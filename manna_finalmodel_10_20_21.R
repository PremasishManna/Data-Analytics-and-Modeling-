getwd()
rm(list=ls()) # clearing the existing stored memory 
df1= read.csv("Felony_Data.csv")
table(is.na(df1)) # check for missing value 
str(df1) #check for data type 
ncol(df1) # check for number of columns
dim(df1) # checking the dimension of the matrix 
#**************************
#df_miss= index(df1[,2:ncol(df1)][is.na(df1[,2:ncol(df1)])])
#****************************
index_miss= which(is.na(df1), arr.ind = TRUE) # determining the index of the missing values 
missing_value_per= (7/(558*21))*100  #missing value percent is below 1 percent of the total observation so it can be ignored
df2= na.omit(df1)  #removing the missing value form the data set 
table(is.na(df2))  # checek for missing value in the new data set 
df3= df2[,2:ncol(df2)] # data containing only continuous variable 
summary(df1)
summary(df2)
df3$Year = factor(df3$Year)  # converting year to factor 
summary(df3$Year)
########## Analyzing the data graphically  ###############
library(Hmisc)
library(dplyr)
library(stats)
library(car)
library(tidyr)
library(fitdistrplus)
summary(df3)
##### Fitting outcome variable ##########
fit_felony= fitdist(df3$Felony.rate.10000, 'norm') ### fitting the variable to a normal distribution 
plot(fit_felony)
summary(fit_felony)

df3$Year= numeric(df3$Year)
##### corr plot  ######
library(corrplot)
m_df= cor(df3)
corrplot(m_df, method= 'number',title = "Correlation plot", bg= 'white')
#### hist######
hist(df3$Year, col = "magenta")
######box plot########
par(mfrow= c(5,4))
color_set= c(rep("red",4) ,rep("blue", 4), rep("green",4),rep("yellow",4), rep("magenta",3))
for(i in 2:ncol(df3))
    {
      boxplot(df3[,i], horizontal= FALSE, col= color_set[i-1], xlab= colnames(df3[i]), title(main="Predictor Spread",outer = 'True'))
     
}
class(m_df) # checking the type of variable 
######## Determining the Predictors#####
corr_year_outcome= m_df[2,1] # getting the correlation factor on outcome 
corr_year_mean= (sum(abs(m_df[,1]))-1)/19 # mean correlation index over all the factor 
df4= df3[,2:ncol(df3)]
##### scaling all the data#######
normalization = function(X){   # function to normalize the predictors// scaling the data to same scale 
  (X-min(X))/(max(X)-min(X))
}
df5_norm= as.data.frame(lapply(df4[1:19], normalization)) # normalized  x train set except the outcome  including the correlated data 
#felony_norm= as.data.frame(lapply(df4[1], normalization)) # normalized y train  set except only felony 
m_df_norm= cor(df5_norm, method = "pearson") #calculating the correlation of the normalized data 
df6_norm= subset(df5_norm, select= c(-4,-5,-7,-8,-10,-12))# data frame after removing highly correlated data 

##### Function to perform random flag value allocation####

randomsplit = function(split, dlen)
{
  s= c()
  for(i in 1:dlen)
  {
    if(runif(1)<= split){    # allocating random values between 0 to 1
      s[i] = 1}
    else { 
      s[i]=0
    }
  }
  return(s)   # returning the randomly allocated vector 
}
##### Function to perform data splitting#######3
data_split= function(x,f){
  c = randomsplit(x, nrow(f))
  t.gr1=  cbind(f,c)   # binding with the column 
  train = subset(t.gr1, t.gr1$c== 1, na.rm= TRUE)[,1:length(f)]
  test = subset(t.gr1,t.gr1$c== 0, na.rm= TRUE)[,1:length(f)]
  x.train = subset(train, select= c(-1))
  y.train= train$Felony.rate.10000
  x.test = subset(test, select = c(-1))
  y.test= test$Felony.rate.10000

  split_list= list(train,test,x.train, y.train,x.test,y.test)
  return(split_list)}
#x_y_split= data_split(0.75,df5_norm)


##########model fitting #######
library(glmnet)
library(earth)
lm_train_model= function(train,test,x.train, y.train, x.test,y.test){
  lm.train= lm(Felony.rate.10000~., data= train)
  summary(lm.train)
  lm.train.predict= predict(lm.train, newdata = x.train, type = 'response')
  lm.test.predict= predict(lm.train, newdata = x.test, type = 'response')
  summary(lm.train.predict)
  lm.rsme.train= sqrt(mean((y.train-lm.train.predict)^2))
  lm.rsme.test= sqrt(mean((y.test-lm.test.predict)^2))
  lm.mae.train= mean(abs(y.train-lm.train.predict))
  lm.mae.test= mean(abs(y.test-lm.test.predict))
  a= sum((y.train-lm.train.predict)^2)
  b= sum((y.train-mean(y.train))^2)
  lm.r2.train= (1- (a/b))
  lm.r2.test= 1-(((sum((y.test-lm.test.predict)^2))/(sum((y.test-mean(y.test))^2))))
  lm.train.residual=(y.train-lm.train.predict)
  lm.test.residual=(y.test-lm.test.predict)
  lm_model_info= list(lm.r2.test,lm.r2.train,lm.mae.test,lm.mae.train,lm.rsme.test,lm.rsme.train,lm.test.residual,lm.train.residual)
  return(lm_model_info)
  }
#lm_model_fit= lm_train_model(train1,test1,x.train1,y.train1,x.test1,y.test1)
glm_train_model= function(train,test,x.train, y.train, x.test,y.test){
    glm.train= glm(Felony.rate.10000~., data= train, family = gaussian())
    summary(glm.train)
    glm.train.predict= predict(glm.train, newdata = x.train, type = 'response')
    glm.test.predict= predict(glm.train, newdata = x.test, type = 'response')
    summary(glm.train.predict)
    glm.train.best= step(glm.train, direction = 'both',k=3.84,family= gaussian())
    glm.rsme.train= sqrt(mean((y.train-glm.train.predict)^2))
    glm.rsme.test= sqrt(mean((y.test-glm.test.predict)^2))
    glm.mae.train= mean(abs(y.train-glm.train.predict))
    glm.mae.test= mean(abs(y.test-glm.test.predict))
    a= sum((y.train-glm.train.predict)^2)
    b= sum((y.train-mean(y.train))^2)
    glm.r2.train= (1- (a/b))
    glm.r2.test= 1-(((sum((y.test-glm.test.predict)^2))/(sum((y.test-mean(y.test))^2))))
    glm.train.residual=(y.train-glm.train.predict)
    glm.test.residual=(y.test-glm.test.predict)
    glm_model_info= list(glm.r2.test,glm.r2.train,glm.mae.test,glm.mae.train,glm.rsme.test,glm.rsme.train,glm.test.residual,glm.train.residual)
    return(glm_model_info)
  }
#glm_model_fit= glm_train_model(train1,test1,x.train1,y.train1,x.test1,y.test1)
ridge_train_model = function(train,test,x.train, y.train, x.test,y.test)
{
  r_train_x= data.matrix(x.train)
  r_test_x = data.matrix(x.test)
  r_train_y= data.matrix(y.train)
  l_r= cv.glmnet(r_train_x,r_train_y,alpha=1)
  lamda= l_r$lambda.min
  ridg.lambda=l_r$lambda
  ridg.train= glmnet(r_train_x,r_train_y,alpha = 1,lambda = lamda)
  summary(ridg.train)
  plot(ridg.train)
  ridg.train.predict= predict(ridg.train,s=lamda, newx= r_train_x)
  ridg.test.predict= predict(ridg.train,s=lamda, newx= r_test_x)
  ridg.rsme.train= sqrt(mean((y.train-ridg.train.predict)^2))
  ridg.rsme.test= sqrt(mean((y.test-ridg.test.predict)^2))
  ridg.mae.train= mean(abs(y.train-ridg.train.predict))
  ridg.mae.test= mean(abs(y.test-ridg.test.predict))
  a= sum((y.train-ridg.train.predict)^2)
  b= sum((y.train-mean(y.train))^2)
  ridg.r2.train= (1- (a/b))
  ridg.r2.test= 1-(((sum((y.test-ridg.test.predict)^2))/(sum((y.test-mean(y.test))^2))))
  ridg.train.residual=(y.train-ridg.train.predict)
  ridg.test.residual=(y.test-ridg.test.predict)
  ridg_model_info= list(ridg.r2.test,ridg.r2.train,ridg.mae.test,ridg.mae.train,ridg.rsme.train,ridg.rsme.train,ridg.train.residual,ridg.test.residual,ridg.lambda)
  return(ridg_model_info)
}

lasso_train_model = function(train,test,x.train, y.train, x.test,y.test)
{
  l_train_x= data.matrix(x.train)
  l_test_x = data.matrix(x.test)
  l_train_y= data.matrix(y.train)
  l_r= cv.glmnet(l_train_x,l_train_y,alpha=0)
  lamda= l_r$lambda.min
  lambda_full= l_r$lambda
  laso.train= glmnet(l_train_x,l_train_y,alpha = 0,lambda = lamda)
  laso.train.predict= predict(laso.train,s=lamda, newx= l_train_x)
  laso.test.predict= predict(laso.train,s=lamda, newx= l_test_x)
  laso.rsme.train= sqrt(mean((y.train-laso.train.predict)^2))
  laso.rsme.test= sqrt(mean((y.test-laso.test.predict)^2))
  laso.mae.train= mean(abs(y.train-laso.train.predict))
  laso.mae.test= mean(abs(y.test-laso.test.predict))
  a= sum((y.train-laso.train.predict)^2)
  b= sum((y.train-mean(y.train))^2)
  laso.r2.train= (1- (a/b))
  laso.r2.test= 1-(((sum((y.test-laso.test.predict)^2))/(sum((y.test-mean(y.test))^2))))
  laso.train.residual=(y.train-laso.train.predict)
  laso.test.residual=(y.test-laso.test.predict)
  laso_model_info= list(laso.r2.test,laso.r2.train,laso.mae.test,laso.mae.train,laso.rsme.train,laso.rsme.train,laso.train.residual,laso.test.residual,lambda_full)
  return(laso_model_info)
}
best_model_fit = function(){
  x_y_split= data_split(0.70,df5_norm)
  train1=x_y_split[[1]]
  test1=x_y_split[[2]]
  x.train1=x_y_split[[3]]
  y.train1=x_y_split[[4]]
  x.test1=x_y_split[[5]]
  y.test1=x_y_split[[6]]
  lm_model_fit= lm_train_model(train1,test1,x.train1,y.train1,x.test1,y.test1)
  glm_model_fit= glm_train_model(train1,test1,x.train1,y.train1,x.test1,y.test1)
  ridge_model_fit= ridge_train_model(train1,test1,x.train1,y.train1,x.test1,y.test1)
  laso_model_fit= lasso_train_model(train1,test1,x.train1,y.train1,x.test1,y.test1)
  model_fit_data= list(lm_model_fit,glm_model_fit,ridge_model_fit,laso_model_fit)
  return(model_fit_data)}

##### Model Fitting and Tuning ########
data=list()  # initializing a list in which model diagnostics will be stored 
r2.train.model=c()  ##### initializing vectors to store data of each model 
rsme.train.model= c()
mae.train.model= c()
r2.test.model=c()
rsme.test.model= c()
mae.test.model= c()


##### Fitting each model several times################
for(k  in 1:30){
  data[[k]]= best_model_fit()
}


for(i in 1:4){
  
  for (k in 1:30){
    r2.test.model= c(r2.test.model, data[[k]][[i]][[1]])
    r2.train.model= c(r2.train.model, data[[k]][[i]][[2]])
    mae.test.model=  c(mae.test.model,data[[k]][[i]][[3]])
    mae.train.model= c(mae.train.model, data[[k]][[i]][[4]])
    rsme.test.model= c(rsme.test.model,data[[k]][[i]][[5]])
    rsme.train.model=c(rsme.train.model, data[[k]][[i]][[6]])
  }}
 model_data= cbind(r2.test.model,r2.train.model,mae.test.model,mae.train.model,rsme.train.model,rsme.test.model)



  
####### plotting the model stat ########
mat.r2.model.test= matrix(r2.test.model, nrow= 30,ncol= 4, byrow= FALSE)
mat.r2.model.train= matrix(r2.train.model, nrow= 30,ncol= 4, byrow= FALSE)
mat.mae.model.train= matrix(mae.train.model, nrow= 30,ncol= 4, byrow= FALSE)
mat.mae.model.test= matrix(mae.test.model, nrow= 30,ncol= 4, byrow= FALSE)
mat.rsme.model.train= matrix(rsme.train.model, nrow= 30,ncol= 4, byrow= FALSE)
mat.rsme.model.test= matrix(rsme.test.model, nrow= 30,ncol= 4, byrow= FALSE)

par(mfrow= c(2,2))
label= c('lm R2 fit','Glm R2 fit',' Ridge R2 fit','Lasso R2 fit ')
for(i in 1:4)
{
plot(mat.r2.model.test[,i],type = 'b', col= 'red', lwd = 2, main = label[i], xlab = 'Iteration', ylab = "R2 fit value " )
lines(mat.r2.model.train[,i], type = 'b', col = 'green',lwd =2)
legend("topleft",legend = c("test","train"),col = c("red","green"),lty= 1)}

par(mfrow= c(2,2))
label1= c('lm Mae Error','Glm Mae Error',' Ridge Mae Error','Lasso Mae Error ')
for(i in 1:4){
  plot(mat.mae.model.test[,i], type = 'b', col= 'blue', lwd = 2, main = label1[i],xlab = 'Iteration', ylab = 'MAE Error')
  lines(mat.mae.model.train[,i], type = 'b', col = 'yellow',lwd =2)
  legend("topleft",legend = c("test","train"),col = c("blue","yellow"),lty= 1)}

par(mfrow= c(2,2))
label2= c('lm RSME Error','Glm RSME Error',' Ridge RSME Error','Lasso RSME Error ')
for(i in 1:4){
  plot(mat.rsme.model.test[,i], type = 'b', col= 'magenta', lwd = 2 , main = label2[i],xlab = 'Iteration', ylab= 'RSME Error')
  lines(mat.rsme.model.train[,i],type = 'b',col= 'green', lwd =2)
  legend("topleft",legend = c("test","train"),col = c("magenta","green"),lty= 1)}
##### Best Model Fit calculation ###########
lm.model.fit.train= (mean(mat.r2.model.train[,1])*100)
lm.model.fit.test= (mean(mat.r2.model.test[,1])*100)

glm.model.fit.train= (mean(mat.r2.model.train[,2])*100)
glm.model.fit.test= (mean(mat.r2.model.test[,2])*100)

ridg.model.fit.train= (mean(mat.r2.model.train[,3])*100)
ridg.model.fit.test= (mean(mat.r2.model.test[,3])*100)

laso.model.fit.train= (mean(mat.r2.model.train[,3])*100)
laso.model.fit.test= (mean(mat.r2.model.test[,3])*100)


lm.model.mae.train= (mean(mat.mae.model.train[,1]))
lm.model.mae.test= (mean(mat.mae.model.test[,1]))

glm.model.mae.train= (mean(mat.mae.model.train[,2]))
glm.model.mae.test= (mean(mat.mae.model.test[,2]))

ridg.model.mae.train= (mean(mat.mae.model.train[,3]))
ridg.model.mae.test= (mean(mat.mae.model.test[,3]))

laso.model.mae.train= (mean(mat.mae.model.train[,3]))
laso.model.mae.test= (mean(mat.mae.model.test[,3]))

####### Linear model best model############

x_y_split= data_split(0.75,df5_norm) # data splitting into 75% train data  and 25 test data
train=x_y_split[[1]]        # assigning data from split list to variable 
test=x_y_split[[2]]
x.train=x_y_split[[3]]
y.train=x_y_split[[4]]
x.test=x_y_split[[5]]
y.test=x_y_split[[6]]

lm.train= lm(Felony.rate.10000~., data= train)  # training lm model with all the predictors 
summary(lm.train)
lm.train.predict= predict(lm.train, newdata = x.train, type = 'response') # predicting the output based on the linear model obtained on train data 
lm.test.predict= predict(lm.train, newdata = x.test, type = 'response') # predicting the output based on the linear model obtained on test data
summary(lm.train.predict)
plot(lm.train)  # ploting the trained output 
lm.rsme.train= sqrt(mean((y.train-lm.train.predict)^2))   # calculating the root mean square error for train data 
lm.rsme.test= sqrt(mean((y.test-lm.test.predict)^2))    # calculating the root mean square error for the test data 
lm.mae.train= mean(abs(y.train-lm.train.predict))   # mean absolute error train data 
lm.mae.test= mean(abs(y.test-lm.test.predict))  # mean absolute error test data
a= sum((y.train-lm.train.predict)^2)      
b= sum((y.train-mean(y.train))^2)
lm.r2.train= (1- (a/b))   # r2 value of the train data 
lm.r2.test= 1-(((sum((y.test-lm.test.predict)^2))/(sum((y.test-mean(y.test))^2))))   # r2 value of the test data 
lm.train.residual=(y.train-lm.train.predict)  # residual of the train data
lm.test.residual=(y.test-lm.test.predict)   # residual of the test data 
test_accuracy= lm.r2.test*100
train_accuracy = lm.r2.train*100   
############## Final output #############
print(paste("The Test accuracy",test_accuracy))
print (paste("The Train Accuracy", train_accuracy))


