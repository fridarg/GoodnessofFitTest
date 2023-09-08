#FridaRangel
#A01651385
library(fitdistrplus)
data <- read.csv("/Users/frida/tarea3datascience/Algerian_forest_fires_dataset_UPDATE.csv")

temp = data$Temperature
hist(temp, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "lnorm", "gamma", "exp")
resultstemp <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultstemp[[i]] <- fitdist(data$Temperature, distr = distribution_list[i])
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultstemp[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

rh = data$RH
hist(rh, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "lnorm", "gamma", "exp")
resultsrh <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsrh[[i]] <- fitdist(data$RH, distr = distribution_list[i])
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsrh[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}


ws = data$Ws
hist(ws, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "lnorm", "gamma", "exp")
resultsws <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsws[[i]] <- fitdist(data$Ws, distr = distribution_list[i])
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsws[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

rain = data$Rain
hist(rain, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "gamma", "exp")
resultsrain <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsrain[[i]] <- fitdist(data$Rain, distr = distribution_list[i], method = "mge")
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsrain[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

ffmc = data$FFMC
hist(ffmc, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "lnorm", "gamma", "exp")
resultsffmc <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsffmc[[i]] <- fitdist(data$FFMC, distr = distribution_list[i])
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsffmc[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

dmc = data$DMC
hist(ws, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "lnorm", "gamma", "exp")
resultsdmc <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsdmc[[i]] <- fitdist(data$DMC, distr = distribution_list[i])
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsdmc[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

dc = data$DC
hist(dc, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "lnorm", "gamma", "exp")
resultsdc <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsdc[[i]] <- fitdist(data$DC, distr = distribution_list[i])
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsdc[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

isi = data$ISI
hist(isi, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "gamma", "exp")
resultsisi <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsisi[[i]] <- fitdist(data$ISI, distr = distribution_list[i], method = "mge")
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsisi[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

bui = data$BUI
hist(bui, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "lnorm", "gamma","exp")
resultsbui <- vector("list", length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsbui[[i]] <- fitdist(data$BUI, distr = distribution_list[i])
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsbui[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

fwi = data$FWI
hist(fwi, col=rgb(1,0,0,0.5), probability=TRUE)
distribution_list <- c("norm", "gamma", "exp")
resultsfwi <- vector("list",length(distribution_list))

for (i in 1:length(distribution_list)) {
  resultsfwi[[i]] <- fitdist(data$FWI, distr = distribution_list[i], method = "mge")
}

for (i in 1:length(distribution_list)) {
  gof_stats <- gofstat(resultsfwi[[i]])
  print(paste("Distribution:", distribution_list[i]))
  print(as.numeric(gof_stats$ks))
  print(gof_stats$kstest)
}

classes = data$Classes
newvalue <- c()
for (i in 1:length(classes)) {
  if (startsWith( classes[i], "n")) newvalue[i] <- 0 else newvalue[i] <- 1
}
hist(newvalue, col=rgb(1,0,0,0.5), probability=TRUE)

