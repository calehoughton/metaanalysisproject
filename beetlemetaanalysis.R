install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")

dataset <- read_excel("~/Win7/Desktop/beetledata3.xlsx")
plot(dataset$z_slope~dataset$z_se, xlab = "Z-Transformed Standard Error", ylab = "Z-Transformed Slope")

store<-data.frame(dataset$z_slope, dataset$z_se, dataset$N)
names(store)<-c("slope","standard.error", "n")

par(mfrow=c(1,2))
plot(store$slope,store$n,xlab="Z-Transformed Slope",ylab="Sample size")
plot(store$slope,(1/store$standard.error),xlab="Z-Transformed Slope",ylab="Precision, (1/se)")

library(metafor)
meta<-rma(yi=slope,sei=standard.error,data=store)
meta

par(mfrow=c(1,1))
funnel(meta, ylab = "Z-Transformed Standard Error", xlab = "Z-Transformed Slope")
forest(meta,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra", xlab = "Z-Transformed Slope")


