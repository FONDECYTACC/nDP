pacman::p_load(IrregLong, MEMSS, survival, geepack, data.table, install=T)

data(Phenobarb)
Phenobarb$event <- 1-as.numeric(is.na(Phenobarb$conc))
data <- Phenobarb
data <- data[data$event==1,]
data$id <- as.numeric(data$Subject)
data <- data[data$time<16*24,]
data <- data[order(data$id,data$time),]
head(data)

#https://cran.r-project.org/web/packages/IrregLong/vignettes/Irreglong-vignette.html