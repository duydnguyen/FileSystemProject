################ Input data #################
Filename <- '3chunks.txt'
data.sys <- read.table(file=Filename, header=TRUE,sep=' ',
                         colClasses=c("character","numeric",NA,"factor","factor","factor","character"))
names(data.sys) <- c("runs","size", "dspan", "chunk.order", "fsync", "sync", "chunk.number" )
## normalize dspan
data.sys$dspan <- data.sys$dspan/data.sys$size
data.sys$size <- as.factor(data.sys$size/1024)
data.sys <- as.data.frame(data.sys)

############## Functions ###################
EvalFac <- function(f, name){
    str <- as.matrix(f)
    n.chunk <- nchar(str[1])
    n.run <- length(str) 
    f.eval <- matrix(c(rep(0,n.chunk*n.run)), nrow = n.run, ncol = n.chunk)
    
    for (i in 1:n.run)
        for (j in 1:n.chunk)
            f.eval[i,j] <- as.numeric(substr(str[i],j,j)) 
    
    
    f.eval <- as.data.frame(f.eval)
    
    for (j in 1:n.chunk) names(f.eval)[j] <- paste(name,j,sep='')
    return(f.eval)
}
#factor <- EvalFac(f = data.12$fsync, "fsync")



