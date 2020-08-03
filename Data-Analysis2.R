library(knitr) #kable
library(MASS) #boxcox
library(leaps) #stepwise

test <- read.csv("C:/Users/ryans/Desktop/AMS/Project2/P203869.csv", header=TRUE)
head(test)
View(test)

# choice to find the correlations between the independent variables and the dependent variable
ifelse(round(cor(test),2)>.3,round(cor(test),2),'')  ## E1,E3,G7,G17 only have correlation with Y 
round(cor(test),2)

# modeling the environment variables
model_env <- lm(Y ~ E1+E2+E3+E4, test)
summary(model_env)  # adj-r2 = 0.2424
plot(resid(model_env)~fitted(model_env),main='Residual Plot')  # seems to be 'adequate'
library(MASS)
boxcox(model_env)
par(mfrow=c(2,2))
plot(model_env)
dev.off()

# modeling the additional contribution of the genetic variables
#model_env_gen <- lm(Y ~ (.)^2, data=df)                 # assuming 2nd order interaction
model_env_gen <- lm(Y ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data=test )
plot(resid(model_env_gen)~fitted(model_env_gen),main='Residual Plot')
library(MASS)
boxcox(model_env_gen)

par(mfrow=c(2,2))
plot(model_env_gen)
dev.off()
summary(model_env_gen)
summary(model_env_gen)$adj.r.square     # .4745

model_trans <- lm(I(Y^0.5) ~ (.)^2, test)
plot(resid(model_trans)~fitted(model_trans),main='New Residual Plot')
summary(model_trans)$adj.r.square       # .4750 / not significant improvement

# stepwise 
Ya=test$Y
m<- regsubsets(x=model.matrix(model_trans)[,-1],y=I((df$Y^0.5)),nbest=1,nvmax=5,method = 'forward', intercept = T)
#m <- regsubsets( model.matrix(model_trans)[,-1], I(Y^0.5), nbest = 1 , nvmax=5, method = 'forward', intercept = TRUE )
temp <- summary(m)
var <- colnames(model.matrix(model_trans))
m_select <- apply(temp$which,1,function(x) paste0(var[x],collapse='+'))
kable(data.frame(cbind(model=m_select,adjr2=temp$adjr2,BIC=temp$bic)),
      caption='Model Summary')
  ## i would choose the 2nd model. (no g x e interaction)

m_main <- lm((Ya^0.5) ~ ., test)
temp <- summary(m_main)
kable(temp$coefficients[abs(temp$coefficients[,4]) <= 0.001, ],
      caption='Sig Coefficients')
  ## E2 ~ E4 were significant.

m_2nd <- lm((Ya^0.5) ~ (.)^2, test)
temp <- summary(m_2nd)
kable(temp$coefficients[abs(temp$coefficients[,4]) <= 0.001, ],
      caption='2nd Interaction')
  ## no significant 2nd order interaction

m_final <- lm(Y ~ E1+E3+G7+G17, test)
summary(m_final)

