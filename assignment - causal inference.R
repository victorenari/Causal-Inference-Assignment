rm(list = ls())
library(Matching)
library(rgenoud)
set.seed(2)

### question 2 ----
data <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- data[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]
foo <- foo[c(-19, -47), ]
which(is.na(foo) == TRUE)

variates <- foo[c('pbs2s3','wartype', 'logcost', 'wardur', 'factnum', 'factnum2', 'trnsfcap', 'develop', 'exp', 'decade', 'treaty', 'untype4')]
for (a in names(variates)){
  variates[[a]] <- mean(variates[[a]], na.rm = TRUE)
}
variates <- variates[1,]

# Adding interaction term to the interaction term data
interact <- variates[1,]
interact['wardur*untype4'] <- interact['wardur']*interact['untype4']

glm1 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + 
            decade + treaty + untype4, data=foo, family = 'binomial')
glm2 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + 
            decade + treaty + untype4 + wardur*untype4, data=foo, family = 'binomial')

trt <- data.frame(variates)
trt[['untype4']] <- 1

control <- data.frame(variates)
control[['untype4']] <- 0

trt_interact <- data.frame(interact)
trt_interact[['untype4']] <- 1

control_interact <- data.frame(interact)
control_interact[['untype4']] <- 0

effect <- matrix()
effect_interact <- matrix()

i = 0
for(year in 1:315){
  trt['wardur'] = year
  control['wardur'] = year
  trt_interact['wardur'] = year
  control_interact['wardur'] = year
  i <- i+1
  effect[i] <- predict(glm1, trt, 'response') - predict(glm1, control, type = 'response')
  effect_interact[i] <- predict(glm2, trt_interact, 'response') - predict(glm2, control_interact, type = 'response')
}

# Plotting the replication of figure 8
plot(effect,xlim=c(5,315),ylim=c(0,0.8),type='l',lty = 'dotted', xlab = "Duration of war in months", 
     ylab="Marginal effects of UN peacebuilding operations",xaxs="i",yaxs="i",xaxt="n",yaxt="n", sub="FIG. 8. Causal Effect of Multidimensional UN Peacekeeping Operations")
lines(effect_interact,xaxt="n",yaxt="n",xaxs="i",yaxs="i")
axis(1, xaxp=c(0,315,63))
axis(2, yaxp=c(0.0,0.8,8))


### question 4 ----

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- foo[c(-4, -16, -19, -47, -84, -93, - 98), ]

Tr <- rep(0, length(foo$untype))
Tr[which(foo$untype != "None")] <- 1
Tr

#Logistic Regression Estimates

#Logit 5 years
logit_5years <- glm(pbs5l ~ Tr + wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade,
             family = binomial, data = foo)
summary(logit_5years)
#Coefficients:
#             Estimate   Std. Error  z value  Pr(>|z|)    
#Tr           0.8233143  0.6172817   1.334    0.182278 
MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap  + foo$treaty + foo$develop + foo$exp +foo$decade, nboots=500)
#Before Matching Minimum p.value: 0.00010717 


#Logit 2 years
logit_2years <- glm(pbs2l ~ Tr + wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade,
             family = binomial, data = foo)
summary(logit_2years)
#Coefficients:
#             Estimate   Std. Error  z value Pr(>|z|)    
#(Intercept)  7.0449263  1.8755670   3.756   0.000173 ***
#Tr           0.5996839  0.6184427   0.970   0.332212
MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap  + foo$treaty + foo$develop + foo$exp +foo$decade, nboots=500)


#Propensity score matching
propensity_score <- glm(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade,
                         family = binomial, data = foo)
#Before Matching Minimum p.value: 0.00010717 

#Match for 2 years
mout_2years <- Match(Tr = Tr, X = propensity_score$fitted, Y = foo$pbs2l, BiasAdjust = TRUE)
mb  <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap  + foo$treaty + foo$develop + foo$exp + foo$decade, match.out = mout_2years ,nboots=500)
#After Matching Minimum p.value: 0.008
mout_2years$est #0.3635877
mout_2years$est.noadj #0.3636364
summary(mout_2years)

#Match for 5 years
mout_5years <- Match(Tr = Tr, X = propensity_score$fitted, Y = foo$pbs5l, BiasAdjust = TRUE)
mb  <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap + foo$treaty + foo$develop + foo$exp +foo$decade, match.out = mout_5years ,nboots=500)
#After Matching Minimum p.value: 0.012
mout_5years$est #0.3938908
mout_5years$est.noadj #0.3939394
summary(mout_5years)

#Genetic Matching

X <- cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap, foo$treaty, foo$develop, foo$exp, foo$decade)

#GenMatch for 2 years
genMatch2 <- GenMatch(Tr = Tr, X = X, M=1, pop.size=200, max.generations=25, wait.generations=10)
Match_gen2 <- Match(Tr = Tr, X = X,  Y = foo$pbs2l, BiasAdjust = TRUE, Weight.matrix = genMatch2, M = 1)
summary(Match_gen2)
mb  <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap + foo$treaty + foo$develop + foo$exp + foo$decade, match.out = Match_gen2 ,nboots=500)
#After Matching Minimum p.value: 0.3
Match_gen2$est #0.2335326
Match_gen2$est.noadj #0.1515152

#GenMatch for 5 years
genMatch5 <- GenMatch(Tr = Tr, X = X, M=1, pop.size=200, max.generations=25, wait.generations=10)
Match_gen5 <- Match(Tr = Tr, X = X, Y = foo$pbs5l, BiasAdjust = TRUE, Weight.matrix = genMatch5, M = 1)
summary(Match_gen5)
mb  <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap + foo$treaty + foo$develop + foo$exp + foo$decade, match.out = Match_gen5,nboots=500)
#After Matching Minimum p.value: 0.262
Match_gen5$est #0.2828753
Match_gen5$est.noadj #0.2121212
