## This file has been moved to Google Drive
rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))



#### Initialize Header File ####
FilePath <- '../Google Drive/projects/filesystem_Spring2014/Data'
################################

################ Input data #################
setwd(FilePath)
Filename <- '3chunks.txt'

data.sys <- read.table(file=Filename, header=TRUE,sep=' ',
                         colClasses=c("character","numeric",NA,"factor","factor","factor","character"))
names(data.sys) <- c("runs","size", "dspan", "chunk.order", "fsync", "sync", "chunk.number" )

########### ANOVA with 2 & 3 interactions; size as blocks ######
model.int3.lm <- lm(log2(dspan) ~ size + chunk.order + fsync + sync
                     + size:chunk.order + size:fsync + size:sync
                     + chunk.order:fsync + chunk.order:sync + fsync:sync +
                     + size:chunk.order:fsync + size:chunk.order:sync + size:fsync:sync
                     + chunk.order:fsync:sync 
                     ,data = data.sys)
## Effect coding
model.int3.lm <- lm(log2(dspan) ~ size + chunk.order + fsync + sync
                    + size:chunk.order + size:fsync + size:sync
                    + chunk.order:fsync + chunk.order:sync + fsync:sync +
                      + size:chunk.order:fsync + size:chunk.order:sync + size:fsync:sync
                    + chunk.order:fsync:sync,
      contrasts = list(size = contr.sum, chunk.order = contr.sum, fsync = contr.sum,
                  sync = contr.sum),
                    data = data.sys)

## Normalized dspan
model.int3.lm <- lm(log2(dspan/size) ~ as.factor(size) + chunk.order + fsync + sync
                    + as.factor(size):chunk.order + as.factor(size):fsync + as.factor(size):sync
                    + chunk.order:fsync + chunk.order:sync + fsync:sync +
                      + as.factor(size):chunk.order:fsync + as.factor(size):chunk.order:sync + size:fsync:sync
                    + chunk.order:fsync:sync ,data = data.sys.n)

summary(model.int3.lm) 
coef3 <- model.int3.lm$coef

 


out <- capture.output(summary(model.int3.lm))
cat(out,file="out_3chunks.txt",sep="\n",append=TRUE)
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


