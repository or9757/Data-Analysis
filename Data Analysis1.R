setwd('C:/Users/ryans/Desktop/AMS/315/Project1')

a1 <- read.csv('P1A1 3869.csv', header = F)
a2 <- read.csv('P1A2 3869.csv', header = F)

## sort the files by subject ID and merge them
names(a1) <- c('id','x')  # key column designated
names(a2) <- c('id','y')  # key column designated

# 1. file sort
a1 <- a1[order(a1$id),]
a2 <- a2[order(a2$id),]

# 2. merge
a <- merge(a1,a2,by='id')
a
## deal with missing data
n1 <- nrow(subset(a,!is.na(y)))  # rows that have dependent variable
n1
n2 <- nrow(subset(a,!is.na(x)))  # rows that have independent variable
n2
n3 <- nrow(subset(a,!is.na(y)&!is.na(x)))  # rows that have both the dependent and independent variable
n4 <- nrow(subset(a,!(is.na(y)&is.na(x))))  # rows that have at least one variable

d <- data.frame(y=n1,x=n2,y_and_x=n3,y_or_x=n4)
d

## impute missing values
install.packages('Amelia')
library(Amelia)
set.seed(1234)

imputed <- amelia(a, m=5, parallel = "multicore")

imputed_a <- imputed$imputations[[1]]
plot(imputed_a[,2:3])

l <- lm(y ~ x, imputed_a)
summary(l)
anova(l)
install.packages('alr3')
library(alr3)
confint(l)

####### PART B

b <- read.csv('P1B 3869.csv', header = T)
sum(complete.cases(b))
head(b)
plot(b[,2:3])

install.packages('tidyverse')
library(tidyverse)

b1 <- b %>% mutate(x1=round(x,1)) %>% group_by(x1) %>% mutate(x_bin = round(mean(x),2))
plot(b1)

plot(b1[,c(5,3)])

b2 <- b1[,c(3,5,2)]

fit <- lm(y~x_bin, b2)
pureErrorAnova(fit)

b3 <- b2 %>% 
  mutate(y_sq=y^2,
         y_sqrt=sqrt(y),
         x_sq=x^2,
         x_sqrt=sqrt(x),
         x_inv=1/x,
         x_ln=log(x))

l1 <- lm(y ~ x, b3)
summary(l1)
l1 <- lm(y ~ x_sq, b3)
summary(l1)
l1 <- lm(y ~ x_sqrt, b3)
summary(l1)
l1 <- lm(y ~ x_inv, b3)
summary(l1)
l1 <- lm(y ~ x_ln, b3)
summary(l1)
l1 <- lm(y_sq ~ x, b3)
summary(l1)
l1 <- lm(y_sq ~ x_sq, b3)
summary(l1)
l1 <- lm(y_sq ~ x_sqrt, b3)
summary(l1)
l1 <- lm(y_sq ~ x_inv, b3)
summary(l1)
l1 <- lm(y_sq ~ x_ln, b3)
summary(l1)
l1 <- lm(y_sqrt ~ x, b3)
summary(l1)
l1 <- lm(y_sqrt ~ x_sq, b3)
summary(l1)
l1 <- lm(y_sqrt ~ x_sqrt, b3)
summary(l1)
l1 <- lm(y_sqrt ~ x_inv, b3)
summary(l1)
l1 <- lm(y_sqrt ~ x_ln, b3)
summary(l1)

#l3 <- lm(y_sq ~ x + x_sq + x_sqrt + x_inv + x_ln, b3)
l3 <- lm(y_sqrt ~ x + x_sq + x_sqrt + x_inv + x_ln, b3)
#l3 <- lm(y ~ x + x_sq + x_sqrt + x_inv + x_ln, b3)
summary(step(l3,direction = 'both'))
anova(step(l3,direction = 'both'))
anova(step(l1,direction = 'both'))
confint(step(l3,direction = 'both'))
confint(step(l1,direction = 'both'))
