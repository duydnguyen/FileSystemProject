## This file has been moved to Google Drive
rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#### Initialize Header File ####
FilePath <- '../Google Drive/projects/filesystem_Spring2014/Data'
setwd(FilePath)

source("fncs.R")
########### Model fitting ######
# model.int3.lm <- lm(log2(dspan) ~ size + chunk.order + fsync + sync
#                     + size:chunk.order + size:fsync + size:sync
#                     + chunk.order:fsync + chunk.order:sync + fsync:sync +
#                       + size:chunk.order:fsync + size:chunk.order:sync + size:fsync:sync
#                     + chunk.order:fsync:sync,
#       contrasts = list(size = contr.sum, chunk.order = contr.sum, fsync = contr.sum,
#                   sync = contr.sum),
#                     data = data.sys)

#### size = 12K with 2-interactions
data.12 <- subset(data.sys, size == "12")
model.12 <- lm(dspan ~ fsync + sync + chunk.order  
               +fsync:sync + fsync:chunk.order + sync:chunk.order
               ,contrasts = list(chunk.order = contr.sum, fsync = contr.sum, sync = contr.sum),
               data = data.12)
#summary(model.12)
#qqline(model.12$coefficients)
#aggregate(data.12$dspan, list(chunk =data.12$chunk.order), mean)
# Comments: This model has a clear pattern in residual plot --> Model inadequacy -> fix: Box-Cox transform, glm?

### Decompose factors: Drop sync3 (1 level). Cannot decompose chunk.order (Discuss!)
# Set up 
factor.fsync <- EvalFac(f = data.12$fsync, "fsync")
factor.sync <- EvalFac(f = data.12$sync, "sync")
##factor.c.order <- EvalFac(f = data.12$chunk.order, "c.order")
data.12f <- data.frame(data.12$dspan, data.12$chunk.order, factor.fsync, factor.sync[,-3])
names(data.12f)[1] <- c("ndspan")
names(data.12f)[2] <- c("c.order")
data.12f[,3:dim(data.12f)[2]] <- lapply(data.12f[,3:dim(data.12f)[2]], factor)

## code effects
# main effect
lm.str <- "ndspan ~ "
fac.names <- names(data.12f)[2:length(data.12f)]
lm.str <- paste(lm.str,fac.names[1],sep='')
for (i in 3:length(data.12f)){
    lm.str <- paste(lm.str, fac.names[i-1],sep=" + ")   
}

## Fit model with 2 & 3-interactions: Resid plot shows inadequate model, hist(resid) looks normal
model.12f <- lm(ndspan ~ fsync1 + fsync2 + fsync3 + sync1 + sync2 + c.order + .^2. + .^3.
                ,data = data.12f)
anova(model.12f)


# All main effects, 2-, 3- interactions are significant. Moving from full -> reduced 
#(2880 para -> 1410 para) R^2 only reduces to 0.9487
## Diagnotic Plots:
layout(matrix(c(1,2), nrow=1, ncol=2))
plot(model.int3.lm$fitted, model.int3.lm$res)
qqnorm(model.int3.lm$res)
qqline(model.int3.lm$res)
## Significant effects using qqplot
coef.int3 <- model.int3.lm$coefficients[2:length(model.int3.lm$coefficients)]
library(faraway)
qqline(coef.int3, lty=2, col="red")
identify(Q <- qqnorm(coef.int3, pch=21, bg="green3", cex=1.5) )
# Pick the first 114 most significant effects
store.int3 <- model.int3.lm$coef[ abs(model.int3.lm$coef) >5 ]
View(store.int3)
View(store)
#### Significant effects using Lenth's method
# Compute the median of |effect_i|
med <- median(abs(coef.int3))
# Compute s0
s0 <- 1.5*med
# trimming constant = 2.5 * s0
# compute pseudo standard error PSE
PSE <- 1.5 * median(abs(coef.int3[which(abs(coef.int3) < 2.5*s0)]))
# Compute t.PSE
t.PSE <- coef.int3 / PSE
# IER =???for I=1409 at level 0.01 (Appendix H in Wu&Hamada) : No available value
## Using BsMD package
library(BsMD)
Lenth <- LenthPlot(coef.int3, alpha = 0.01, plt = TRUE, limits = TRUE, 
          xlab = "factors", ylab = "effects")

effect.sig <- coef.int3[which(abs(t.PSE) > Lenth[4])]
length(effect.sig) #885
View(effect.sig)
############# Orthogonal Array Designs ##################
install.packages("DoE.base")
library(DoE.base)
OA <- oa.design(nfactors = 4, nlevels=c(15,6,8,4), nruns=100)
dim(OA)
View(OA)
oa.design(nfactors = 3, nlevels = 2)


