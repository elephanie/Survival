library(MASS)
library(survival)
library(survMisc)
library("survminer")


##---------------------
# GOODNESS OF FIT
##--------------------

##-- log(-log(suvival)) plot
data(gehan)
gehansurv= Surv(gehan$time,gehan$cens)
plot(survfit(gehansurv ~ gehan$treat), col = c("black", "red"), fun = "cloglog")


#-- goodness of fit, risk groups
#-- Method and example are from: May S, Hosmer DW 1998. 
#-- A simplified method of calculating an overall goodness-of-fit test 
#-- for the Cox proportional hazards model. Lifetime Data Analysis 
data("pbc", package = "survival")
str(pbc)
head(pbc)

pbc <- pbc[!is.na(pbc$trt), ]
pbc[pbc$id==253, "age"] <-  54.4
pbc[pbc$id==107, "protime"] <-  10.7
c1 <- coxph(Surv(time, status==2) ~
              age + log(albumin) + bili + edema + protime,
            data=pbc)

gof(c1, G=10)
gof(c1)

#--time varying covariates

ls("package:survival")
