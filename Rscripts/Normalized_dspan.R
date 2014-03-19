## This file has been moved to Google Drive
rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#### Initialize Header File ####
FilePath <- '../Google Drive/projects/filesystem_Spring2014/Data'
setwd(FilePath)

source("fncs.R")
########### Model fitting with 12KB ######
#### size = 12K with 2-interactions
data.12 <- subset(data.sys, size == "12")
model.12 <- lm(dspan ~ fsync + sync + chunk.order  
               +fsync:sync + fsync:chunk.order + sync:chunk.order
               ,contrasts = list(chunk.order = contr.sum, fsync = contr.sum, sync = contr.sum),
               data = data.12)
#aggregate(data.12$dspan, list(chunk =data.12$chunk.order), mean)
# Comments: This model has a clear pattern in residual plot --> Model inadequacy -> fix: Box-Cox transform, glm?

### Decompose factors: Drop sync3 (1 level). Cannot decompose chunk.order (Discuss!)
# Set up 
factor.fsync <- EvalFac(f = data.12$fsync, "fsync")
factor.sync <- EvalFac(f = data.12$sync, "sync")
##factor.c.order <- EvalFac(f = data.12$chunk.order, "c.order")
data.12f <- data.frame(data.12$dspan, data.12$chunk.order, factor.fsync, factor.sync[,-dim(factor.sync)[2]])
names(data.12f)[1] <- c("ndspan")
names(data.12f)[2] <- c("c.order")
data.12f[,3:dim(data.12f)[2]] <- lapply(data.12f[,3:dim(data.12f)[2]], factor)

## code effects
# main effect
lm.str <- "log2(ndspan) ~ "
fac.names <- names(data.12f)[2:length(data.12f)]
for (i in 3:length(data.12f)){
    lm.str <- paste(lm.str, fac.names[i-1],sep=" ")   
    lm.str <- paste(lm.str, " +", sep="")
}
lm.str <- paste(lm.str,fac.names[1], "+ .^2. + .^3. +.^4. + .^5. + .^6.",sep=' ')
## Fit model with 2 & 3-interactions: Resid plot shows inadequate model, hist(resid) looks normal
model.12f <- lm(lm.str,
                contrasts = list(c.order = contr.sum, fsync1 = contr.sum, fsync2 = contr.sum, fsync3 = contr.sum,
                                 sync1 = contr.sum, sync2 = contr.sum)
                ,data = data.12f)
summary(model.12f)
plot(model.12f)
plot(model.12f$residuals)

## red vs. full model
red <- lm(log2(ndspan) ~  fsync1 + fsync2 + fsync3 + sync1 + sync2 + c.order + .^2. + .^3. +.^4. + .^5.,
          contrasts = list(c.order = contr.sum, fsync1 = contr.sum, fsync2 = contr.sum, fsync3 = contr.sum,
                           sync1 = contr.sum, sync2 = contr.sum)
          ,data = data.12f)

anova(red, model.12f)
# Backward Stepwise BIC
null <- lm(ndspan ~ 1 ,data = data.12f)
smodel.12f <- step(null, scope= list(lower=null, upper=model.12f), direction="both", 
                   #k=log(dim(data.12)[1]))
                   k=2)
summary(smodel.12f)
anova(smodel.12f)

##### Function: input of size, output model based on AIC stepwise
smodel <- MSelect(180)
summary(smodel)
######## Sensitivity Analysis ########
model.SA<- lm(log2(dspan) ~ size + fsync + sync + chunk.order + 
                      + size:chunk.order + size:fsync + size:sync
                    + chunk.order:fsync + chunk.order:sync + fsync:sync +
                      + size:chunk.order:fsync + size:chunk.order:sync + size:fsync:sync
                    + chunk.order:fsync:sync , 
              contrasts = list(size = contr.sum, fsync = contr.sum, sync = contr.sum, chunk.order = contr.sum)
              ,data = data.sys)


a <- anova(model.SA) 

vec <- vec[1:4]/sum(vec)

data.frame(cbind(c("size","c.order","fsync","sync"),vec))

######### Follow-up runs ##############
######### Peter's paper ##############


