# Entomoscelis americana
temp <- c(24.5,21.2,25.0,21.6,21.0,22.8,24.5,21.2,25.0,21.6,21.0,22.8)
mass <- c(28.92, 27.44, 30.92, 30.08, 27.12, 29.76, 21.60, 20.88, 23.28, 22.32, 20.84,22.36)
dataframe <- data.frame(temp,mass)
model <- lm(mass~temp, data = dataframe)
summary(model)
zmodel <- lm(scale(mass)~temp, data = dataframe)
summary(zmodel)
cor.test(mass,temp)

# Anomala expansa expansa
library(readxl)
dataframe2 <- read_excel("~/Win7/Desktop/beetledata.xlsx", 
                         sheet = "Table001 (Page 1)")
model2 <- lm(Elytra_length~Av_yearly_temp, data = dataframe2)
summary(model2)
zmodel2 <- lm(scale(Elytra_length)~Av_yearly_temp, data = dataframe2)
summary(zmodel2)
Elytra_length <- (dataframe2$Elytra_length)
Av_yearly_temp <- (dataframe2$Av_yearly_temp)
cor.test(Elytra_length, Av_yearly_temp)

# Notiophilis biguttatus
temp3 <- c(5.5,6.0,7.0,9.0,10.0,11.0,11.5,17.5,17.5,18.5,19.0,19.0,20.5)
mass3 <- c(67.50,67.40,63.00,64.80,61.50,60.00,59.90,57.00,54.00,55.00,55.50,55.00, 52.50)
dataframe3 <- data.frame(temp3,mass3)
model3 <- lm(mass3~temp3, data = dataframe3)
summary(model3)
zmodel3 <- lm(scale(mass3)~temp3, data = dataframe3)
summary(zmodel3)
cor.test(mass3,temp3)

#Gyretes sinuatus
temp4 <- c(16.1, 17.2,17.8, 19.0,20.5,21.0,21.5)
mass4 <- c(5.56,5.44,5.38,5.26,5.11,5.05,5.00)
dataframe4 <- data.frame(temp4,mass4)
model4 <- lm(mass4~temp4, data = dataframe4)
summary(model4)
zmodel4 <- lm(scale(mass4)~temp4, data = dataframe4)
summary(zmodel4)
cor.test(mass4,temp4)

#Enaphalodes rufulus
temp5 <- c(21.1,26.7,32.2,21.1,26.7,32.2)
mass5 <- c(0.90,1.28,1.33,0.68,1.04,1.21)
dataframe5 <- data.frame(temp5,mass5)
model5 <- lm(mass5~temp5, data = dataframe5)
summary(model5)
zmodel5 <- lm(scale(mass5)~temp5, data = dataframe5)
summary(zmodel5)
cor.test(mass5,temp5)

#Chrysomela populi
temp6 <- c(18.0,20.0,22.0,24.0,26.0,18.0,20.0, 22.0,24.0,26.0)
mass6 <- c(81.00,77.00,91.00,64.00,55.00,72.00,70.00, 79.00,60.00,77.00)
dataframe6 <- data.frame(temp6,mass6)
model6 <- lm(mass6~temp6, data = dataframe6)
summary(model6)
zmodel6 <- lm(scale(mass6)~temp6, data = dataframe6)
summary(zmodel6)
cor.test(mass6,temp6)


#Entomoscelis americana
temp7 <- c(25.0,21.6,21.0,22.8,24.5,21.2,25.0,21.6,21.0,22.8,24.5,21.2)
mass7 <- c(77.30,75.20, 67.80,74.40,72.30,68.60,58.20,55.80,52.10,55.90,54.00,  52.20)
dataframe7 <- data.frame(temp7,mass7)
model7 <- lm(mass7~temp7, data = dataframe7)
summary(model7)
zmodel7 <- lm(scale(mass7)~temp7, data = dataframe7)
summary(zmodel7)
cor.test(mass7,temp7)

#Paropsis atomaria
temp8 <- c(16.0, 20.0, 24.0, 27.0, 16.0, 20.0, 24.0, 27.0, 16.0, 20.0, 24.0, 27.0, 16.0, 20.0, 24.0, 27.0)
mass8 <- c(5.45, 5.40, 5.36, 5.30, 5.92, 5.92, 5.80, 5.78, 5.28, 5.15, 5.21, 5.05, 5.66, 5.65, 5.53, 5.44)
dataframe8 <- data.frame(temp8,mass8)
model8 <- lm(mass8~temp8, data = dataframe8)
summary(model8)
zmodel8 <- lm(scale(mass8)~temp8, data = dataframe8)
summary(zmodel8)
cor.test(mass8,temp8)


#Pterohelaeus darlingensis

temp9 <- c(33.1, 30.2, 28.6, 24.4, 20.6, 33.1, 30.2, 28.6, 24.4, 20.6)
mass9 <- c(169.00, 207.90, 196.90, 195.30, 251.80, 191.20, 235.00, 214.40, 216.30, 265.70)
dataframe9 <- data.frame(temp9, mass9)
model9 <- lm(mass9 ~ temp9, data = dataframe9)
summary(model9)
zmodel9 <- lm(scale(mass9) ~ temp9, data = dataframe9)
summary(zmodel9)
cor.test(mass9, temp9)

#Pterohelaeus alternatus

temp10 <- c(33.1, 30.2, 28.6, 24.4, 20.6, 33.1, 30.2, 28.6, 24.4, 20.6)
mass10 <- c(218.10, 253.10, 239.20, 255.90, 392.90, 249.10, 270.10, 261.90, 276.10, 432.30)
dataframe10 <- data.frame(temp10, mass10)
model10 <- lm(mass10 ~ temp10, data = dataframe10)
summary(model10)
zmodel10 <- lm(scale(mass10) ~ temp10, data = dataframe10)
summary(zmodel10)
cor.test(mass10, temp10)

#Callosobruchus maculatus
temp11 <- c(20.0, 25.0, 30.0, 35.0, 20.0, 25.0, 30.0, 35.0, 20.0, 25.0, 30.0, 35.0, 20.0, 25.0, 30.0, 35.0)
mass11 <- c(7.76, 6.96, 6.08, 5.33, 6.51, 4.94, 4.12, 3.78, 6.46, 5.96, 5.46, 4.98, 5.58, 4.52, 3.88, 3.64)
dataframe11 <- data.frame(temp11, mass11)
model11 <- lm(mass11 ~ temp11, data = dataframe11)
summary(model11)
zmodel11 <- lm(scale(mass11) ~ temp11, data = dataframe11)
summary(zmodel11)
cor.test(mass11, temp11)

#Haptoncu ocularis
temp12 <- c(15.0, 20.0, 25.0, 30.0, 15.0, 20.0, 25.0, 30.0)
mass12 <- c(0.71, 0.76, 0.74, 0.71, 0.68, 0.75, 0.73, 0.68)
dataframe12 <- data.frame(temp12, mass12)
model12 <- lm(mass12 ~ temp12, data = dataframe12)
summary(model12)
zmodel12 <- lm(scale(mass12) ~ temp12, data = dataframe12)
summary(zmodel12)
cor.test(mass12, temp12)

#Carpophilus marginellus
temp13  <- c(18.0, 20.0, 25.0, 30.0, 18.0, 20.0, 25.0, 30.0)
mass13 <- c(0.76, 0.76, 0.74, 0.71, 0.76, 0.75, 0.73, 0.72)
dataframe13 <- data.frame(temp13, mass13)
model13 <- lm(mass13 ~ temp13, data = dataframe13)
summary(model13)
zmodel13 <- lm(scale(mass13) ~ temp13, data = dataframe13)
summary(zmodel13)
cor.test(mass13, temp13)

#Attagenus megatoma
temp14 <- c(20.0, 25.0, 28.0, 30.0, 35.0, 20.0, 25.0, 28.0, 30.0, 35.0)
mass14 <- c(5.40, 8.20, 8.70, 9.20, 9.30, 8.10, 13.20, 15.40, 15.40, 15.20)
dataframe14 <- data.frame(temp14, mass14)
model14 <- lm(mass14 ~ temp14, data = dataframe14)
summary(model14)
zmodel14 <- lm(scale(mass14) ~ temp14, data = dataframe14)
summary(zmodel14)
cor.test(mass14, temp14)


#Adalia bipunctata
temp15 <- c(19.0, 23.0, 27.0, 19.0, 23.0, 27.0, 19.0, 23.0, 27.0, 19.0, 23.0, 27.0, 19.0, 23.0, 27.0, 19.0, 23.0, 27.0)
mass15 <- c(4.86, 4.89, 4.48, 3.92, 3.65, 3.54, 5.26, 5.34, 4.70, 4.00, 4.16, 3.50, 5.26, 5.34, 4.70, 4.01, 4.08, 3.56)
dataframe15 <- data.frame(temp15, mass15)
model15 <- lm(mass15 ~ temp15, data = dataframe15)
summary(model15)
zmodel15 <- lm(scale(mass15) ~ temp15, data = dataframe15)
summary(zmodel15)
cor.test(mass15, temp15)

#Stator limbatus
temp16 <- c(24.0, 30.0, 36.0, 24.0, 30.0, 36.0, 24.0, 30.0, 36.0, 24.0, 30.0, 36.0)
mass16 <- c(1.41, 1.33, 1.28, 1.52, 1.42, 1.33, 1.49, 1.36, 1.24, 1.55, 1.44, 1.30)
dataframe16 <- data.frame(temp16, mass16)
model16 <- lm(mass16 ~ temp16, data = dataframe16)
summary(model16)
zmodel16 <- lm(scale(mass16) ~ temp16, data = dataframe16)
summary(zmodel16)
cor.test(mass16, temp16)

#Notiophilus rufipes
temp17 <- c(12.0, 16.0, 21.0, 12.0, 16.0, 21.0, 12.0, 16.0, 21.0, 12.0, 16.0, 21.0)
mass17 <- c(2.91, 3.17, 2.66, 2.46, 2.67, 2.25, 2.49, 2.64, 2.08, 2.30, 2.44, 2.14)
dataframe17 <- data.frame(temp17, mass17)
model17 <- lm(mass17 ~ temp17, data = dataframe17)
summary(model17)
zmodel17 <- lm(scale(mass17) ~ temp17, data = dataframe17)
summary(zmodel17)
cor.test(mass17, temp17)

#Hylobius abietis
temp18 <- c(12.5, 15.0, 17.5, 20.0, 25.0, 12.5, 15.0, 17.5, 20.0, 25.0)
mass18 <- c(49.23, 51.02, 52.14, 56.62, 45.20, 43.86, 45.87, 49.23, 49.68, 45.87)
dataframe18 <- data.frame(temp18, mass18)
model18 <- lm(mass18 ~ temp18, data = dataframe18)
summary(model18)
zmodel18 <- lm(scale(mass18) ~ temp18, data = dataframe18)
summary(zmodel18)
cor.test(mass18, temp18)


#Scymnus subvillosus
temp19 <- c(20.0, 25.0, 30.0, 20.0, 25.0, 30.0)
mass19 <- c(0.24, 0.24, 0.24, 0.2, 0.24, 0.24)
dataframe19 <- data.frame(temp19, mass19)
model19 <- lm(mass19 ~ temp19, data = dataframe19)
summary(model19)
zmodel19 <- lm(scale(mass19) ~ temp19, data = dataframe19)
summary(zmodel19)
cor.test(mass19, temp19)








