setwd("~/Desktop/womens-machine-learning-competition-2019/WDataFiles") 
data <- read.table("WNCAATourneyDetailedResults.csv", header=TRUE,sep=",")

# regression tree
install.packages("tree")  #, lib="/my/own/R-packages/")
library(tree)

length <- 252*2
x <- data.frame("win" = c(length),"FGM" = c(length),"FGA" = c(length),"FGM3" = c(length),"FGA3" = c(length),"FTM" = c(length),"FTA" = c(length),"OR" = c(length),"DR" = c(length),"Ast" = c(length),"TO" = c(length),"Stl" = c(length),"Blk" = c(length),"PF" = c(length),"Home" = c(length))
for (i in 1:252){
  x[2*i-1,1]<-1
  x[2*i-1,2]=data[i,9]
  x[2*i-1,3]=data[i,10]
  x[2*i-1,4]=data[i,11]
  x[2*i-1,5]=data[i,12]
  x[2*i-1,6]=data[i,13]
  x[2*i-1,7]=data[i,14]
  x[2*i-1,8]=data[i,15]
  x[2*i-1,9]=data[i,16]
  x[2*i-1,10]=data[i,17]
  x[2*i-1,11]=data[i,18]
  x[2*i-1,12]=data[i,19]
  x[2*i-1,13]=data[i,20]
  x[2*i-1,14]=data[i,21]
  if (data[i,7]=='H'){
    x[2*i-1,15]='H';
  }else{
    x[2*i-1,15]='A';
  }
  x[2*i,1]=0
  x[2*i,2]=data[i,22]
  x[2*i,3]=data[i,23]
  x[2*i,4]=data[i,24]
  x[2*i,5]=data[i,25]
  x[2*i,6]=data[i,26]
  x[2*i,7]=data[i,27]
  x[2*i,8]=data[i,28]
  x[2*i,9]=data[i,29]
  x[2*i,10]=data[i,30]
  x[2*i,11]=data[i,31]
  x[2*i,12]=data[i,32]
  x[2*i,13]=data[i,33]
  x[2*i,14]=data[i,34]
  if (data[i,7]=='A'){
    x[2*i,15]='H';
  }else{
    x[2*i,15]='A';
  }
}
x[1,15]='A';

x_A <- subset(x, Home=='A')
x_H <- subset(x, Home=='H')

data.tree <- tree(win~FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+Ast+TO+Stl+Blk+PF, data=x_H) 
plot(data.tree, type="uniform")
text(data.tree)


# random forest
install.packages("randomForest")
library(randomForest)

suppressWarnings(output.forest <- randomForest(win~FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+Ast+TO+Stl+Blk+PF, data = x))
print(output.forest)
plot(output.forest)
varImpPlot(output.forest)

#test set
y <- data.frame("win" = c(315*2),"FGM" = c(315*2),"FGA" = c(315*2),"FGM3" = c(315*2),"FGA3" = c(315*2),"FTM" = c(315*2),"FTA" = c(315*2),"OR" = c(315*2),"DR" = c(315*2),"Ast" = c(315*2),"TO" = c(315*2),"Stl" = c(315*2),"Blk" = c(315*2),"PF" = c(315*2),"Home" = c(315*2))
for (i in 1:315){
  y[2*i-1,1]<-1
  y[2*i-1,2]=data[i+252,9]
  y[2*i-1,3]=data[i+252,10]
  y[2*i-1,4]=data[i+252,11]
  y[2*i-1,5]=data[i+252,12]
  y[2*i-1,6]=data[i+252,13]
  y[2*i-1,7]=data[i+252,14]
  y[2*i-1,8]=data[i+252,15]
  y[2*i-1,9]=data[i+252,16]
  y[2*i-1,10]=data[i+252,17]
  y[2*i-1,11]=data[i+252,18]
  y[2*i-1,12]=data[i+252,19]
  y[2*i-1,13]=data[i+252,20]
  y[2*i-1,14]=data[i+252,21]
  if (data[i+252,7]=='H'){
    y[2*i-1,15]='H';
  }else{
    y[2*i-1,15]='A';
  }
  y[2*i,1]=0
  y[2*i,2]=data[i+252,22]
  y[2*i,3]=data[i+252,23]
  y[2*i,4]=data[i+252,24]
  y[2*i,5]=data[i+252,25]
  y[2*i,6]=data[i+252,26]
  y[2*i,7]=data[i+252,27]
  y[2*i,8]=data[i+252,28]
  y[2*i,9]=data[i+252,29]
  y[2*i,10]=data[i+252,30]
  y[2*i,11]=data[i+252,31]
  y[2*i,12]=data[i+252,32]
  y[2*i,13]=data[i+252,33]
  y[2*i,14]=data[i+252,34]
  if (data[i,7]=='A'){
    y[2*i,15]='H';
  }else{
    y[2*i,15]='A';
  }
}



oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(win~FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+Ast+TO+Stl+Blk+PF,data=x,mtry=mtry) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,y) #Predictions on Test Set for each Tree
  test.err[mtry]= with(y, mean( (win - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
}

#error table
error <- data.frame("number of predictor"=c(1:13),"out of bag error"=oob.err, "test error"=test.err)
library(gridExtra)
grid.table(round(error,digits=3))

#error plot
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))





# scatterplot
pairs(x)
plot(x)
pairs(~FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+Ast+TO+Stl+Blk+PF,data=x, main="Simple Scatterplot Matrix")

mydata.cor = cor(x[,2:14])
install.packages("Hmisc")
library("Hmisc")
mydata.rcorr = rcorr(as.matrix(x[,2:14]))
mydata.rcorr
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
library(gridExtra)
grid.table(round(mydata.coeff,digits=3))

install.packages("corrplot")
library(corrplot)
corrplot(mydata.cor)


z <- data.frame("FGM" = c(length),"FGA" = c(length),"FGM3" = c(length),"FGA3" = c(length),"FTM" = c(length),"FTA" = c(length),"OR" = c(length),"DR" = c(length),"Ast" = c(length),"TO" = c(length),"Stl" = c(length),"Blk" = c(length),"PF" = c(length))
for (i in 1:252){
  for (j in 1:13){
    z[i,j]=x[i*2-1,j+1]-x[i*2,j+1]
  }
}








#linear regression using difference between RPI
data2 <- read.table("WNCAATourneyCompactResults.csv", header=TRUE,sep=",")
rpi <- read.table("rpi.csv", header=TRUE,sep=",")
rpi_regression1 <- data.frame("team_rpi"=c(1),"opponent_rpi"=c(2),"win"=c(1))
for (i in 1:nrow(data2)){
  rpi_regression1[2*i-1,1]=data2[i,3]
  rpi_regression1[2*i-1,2]=data2[i,5]
  rpi_regression1[2*i-1,3]=1
  rpi_regression1[2*i,1]=data2[i,5]
  rpi_regression1[2*i,2]=data2[i,3]
  rpi_regression1[2*i,3]=0
}

rpi_regression2 <- data.frame("rpi_difference"=c(1),"win"=c(1))
for (i in 1:nrow(data2)){
  rpi1=0
  rpi2=0
  for (j in 1:nrow(rpi)){
    if (rpi[j,1]==data2[i,3]){
      rpi1=rpi[j,2]
    }
    if (rpi[j,1]==data2[i,5]){
      rpi2=rpi[j,2]
    }
  }
  rpi_regression2[2*i-1,1]=rpi1-rpi2
  rpi_regression2[2*i-1,2]=1
  rpi_regression2[2*i,1]=rpi2-rpi1
  rpi_regression2[2*i,2]=0
}

#scatterplot
scatter.smooth(x=rpi_regression2$rpi_difference, y=rpi_regression2$win, main="Dist ~ win")

#correlation between x and y
cor(rpi_regression2$rpi_difference, rpi_regression2$win) #0.637

#linear regression model
linearMod <- glm(win ~ rpi_difference, data=rpi_regression2)  # build linear regression model on full data
print(linearMod)
# y' = 0.5 + 1.942*x(rpi_difference,team_RPI-opponent_RPI)
# y(winning probability) = 1/(1 + e^(-1*y'))




# second linear regression model
rpi_regression3 <- data.frame("rpi_difference_square"=c(1),"rpi_difference"=c(1),"win"=c(1))
for (i in 1:nrow(rpi_regression2)){
  rpi_regression3[i,1]=rpi_regression2[i,1]^2
  rpi_regression3[i,2]=rpi_regression2[i,1]
  rpi_regression3[i,3]=rpi_regression2[i,2]
}

#scatterplot
#scatter.smooth(x=(rpi_regression3$rpi_difference_square,rpi_regression3$rpi_difference], y=rpi_regression3$win, main="Dist ~ win")

install.packages("scatterplot3d") # Install
library("scatterplot3d") # load               
scatterplot3d(rpi_regression3[1:3], angle = 55)
               
4#correlation between x and y
cor(rpi_regression3$rpi_difference_square, rpi_regression2$win) #0
cor(rpi_regression3$rpi_difference, rpi_regression2$win) #0.637

#linear regression model
linearMod <- glm(win ~ rpi_difference_square+rpi_difference, data=rpi_regression3)  # build linear regression model on full data
print(linearMod)
# y' = 0.5 + 1.223*e^(-14)*x(rpi_difference,team_RPI-opponent_RPI)^2 + 1.942*x
# y(winning probability) = 1/(1 + e^(-1*y'))







