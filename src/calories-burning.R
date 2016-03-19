#####################
## Loading dataset ##
#####################

# make sure you are at the root of the folder containing the dataset (use set.wd())
muscle <- read.table(file = "muscle-incomplete.txt", header = TRUE)
attach(muscle)

##########################
## Exploratory analysis ##
##########################

# Explore pairs of scatterplots (two covariates at a time)
pairs(muscle)

# Correlation
cor(weight, calories, use="complete")
cor(calhour, calories, use="complete")
cor(calhour,weight)
cor.test(calhour, calories,alternative="two.sided",method="pearson")

# 3D rotable scatterplot
library(rgl)
colors.scatterplot <- c("blue", "red", "green", "black", "yellow", "purple", "brown")
plot3d(weight, calhour, calories, col=colors.scatterplot[as.factor(weight)],
       type="s", size=2)

# Multiple models of calories organised by weights
cf <- sapply(muscle$weight,
             function(x) coef(lm(calories ~ calhour, data=subset(muscle, weight==x))))
Sx <- reorder(muscle$weight, cf[1,])

# Plotting the models in treillis 
library(lattice)
xyplot(calories ~ calhour|Sx, groups=weight, data=muscle, type=c('p','r'), auto.key=TRUE , aspect="xy",par.settings=list(axis.text=list(cex=0.6), fontsize=list(text=8, points=10)))

# Summary statistics for each variables
library(pastecs)
options(digits=2)
description.muscle <- stat.desc(muscle[,c("weight", "calhour", "calories")], basic=TRUE, desc=TRUE)
description.muscle

# missing data exploration
library(VIM)
aggr(muscle)
barMiss(muscle[,c("weight","calories")])
marginplot(muscle[c(3,2)],col=c("blue", "red", "orange"))

# Check for clustering using a profile plot
muscle.c <- as.data.frame(scale(muscle, center = TRUE, scale = TRUE))
plot(c(0,4),c(min(apply(na.omit(muscle.c[1:24,]),2,min)),max(apply(na.omit(muscle.c[1:24,]),2,max))),type="n", xlab="",ylab="Values",xlim=c(0.9, 3.1),xaxt='n') #xaxt='n' #removes the labels from the x-axis
for (k in (1:nrow(muscle.c))) {
  points(1:3,muscle.c[k,],type="l")
}
axis(1, at=1:3, labels=c("weight","calhour","calories"))

############################
## Complete case analysis ##
############################

# First linear model, containing only the weight
lm1<-lm(calories~weight,data=muscle)
summary(lm1)
# Second linear model, containing only the calhour
lm2<-lm(calories~calhour,data=muscle)
summary(lm2)
# Third linear model, containing the weight and calhour
lm3<-lm(calories~weight+calhour,data=muscle)
summary(lm3)
# Fourth linear model, containing the weight, the calhour and an interaction term
lm4<-lm(calories~weight+calhour+I(weight*calhour),data=muscle)
summary(lm4)


##########################################################
## Missing data analysis - Multiple imputation analysis ##
##########################################################

library(mice)

##Different imputation methods used
imp1 <- mice(muscle,method =c("","","pmm"))
imp2 <- mice(muscle,method =c("","","norm.nob"),m=100)
imp3 <- mice(muscle,method =c("","","norm"))
imp4 <- mice(muscle,method =c("","","sample"))
imp2

##Diagnostic checking of the imputed data sets
imp2$imp$calories[1:8,1:5]
complete(imp2,1)[1:10,]
com <- complete(imp2, "long", inc=TRUE)
col <- rep(c("blue","red")[1+as.numeric(is.na(imp2$data$calories))],5)
stripplot(calories ~ .imp, data=com,jit=TRUE, fac=0.8,col=col, pch=20, 
          cex=1.4,xlab="Imputation number",xlim=seq(1,100,10))

##Analyzing the imputed data sets
fit1 <- with(imp2,glm(calories ~ weight+calhour+I(weight*calhour)))
pool(fit1)
summary(pool(fit1))
fit2 <- with(imp3,glm(calories ~ weight+calhour+I(weight*calhour)))
pool(fit2)
summary(pool(fit2))


##########################################
## Missing data analysis - IPW analysis ##
##########################################

muscle.missing<-read.table(file=file.choose(),header=TRUE)

###Muscle IPW
# Creating the missing data indicator variable r
muscle.missing$r<-as.numeric(!is.na(muscle.missing$calories))
head(muscle.missing,24)

# Fitting the the logistic regression model to calculate the probabilities of being complete
muscle.ipw.glm<-glm(r~weight+calhour,data=muscle.missing,control = list(maxit = 500),family=binomial)
summary(muscle.ipw.glm)

# calculate the weights 
muscle.missing$w<-1/fitted(muscle.ipw.glm)
head(muscle.missing,24)

# fitting the final multiple linear regression
muscle.results.ipw<-lm(calories ~ weight+calhour+I(weight*calhour),weights=muscle.missing$w,data=muscle.missing)
summary(muscle.results.ipw)

