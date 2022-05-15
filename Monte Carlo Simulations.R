library(ggplot2)
library(readr)
library(readr)
library(data.table)
library(ggplot2)
library(dplyr)

# A function to get mode of the data SOURCE: www.tutorialspoint.com

getmode <- function(vec) {
  uniqvec <- unique(vec)
  uniqvec[which.max(tabulate(match(vec, uniqvec)))]
}

main <- read.csv("mainn.csv")

#Kharif dataset
main_kharif<-subset(main,season=="Kharif")

#Removing Outliers
mean_k<-mean(main_kharif$v46,na.rm=TRUE)
sd_k<-sd(main_kharif$v46,na.rm=TRUE)
main_kharif<-subset(main_kharif,v46<=9.7854)


#Running Monte Carlo Simulations
kharif_model<-lm(v46~index,main_kharif)

B0_k_true<-summary(kharif_model)$coefficients[1, 1]
B1_k_true<-summary(kharif_model)$coefficients[2, 1]
i<-100

sumB0<-0
sumB1<-0



for(x in 1: i){
  main_sample_kharif<-main_kharif[sample(1:nrow(main_kharif),80*(nrow(main_kharif))/100),]

  sample_model_k<-lm(v46~index,main_sample_kharif)
  
  B0<-summary(sample_model_k)$coefficients[1, 1]
  B1<-summary(sample_model_k)$coefficients[2, 1]
  
  sumB0<-B0+sumB0
  sumB1<-B1+sumB1
}









AvgB0<-sumB0/i
AvgB1<-sumB1/i

ansB0<-paste("Kharif B0- True: ",B0_k_true," Avg: ",AvgB0)
ansB1<-paste("Kharif B1- True: ",B1_k_true," Avg: ",AvgB1)

print(ansB0)
print(ansB1)



#Rabi dataset
main_rabi<-subset(main,season=="Rabi")

#Removing Outliers
mean_r<-mean(main_rabi$v46,na.rm=TRUE)
sd_r<-sd(main_rabi$v46,na.rm=TRUE)

main_rabi<-subset(main_rabi,v46<=9.4874)

#Running Monte Carlo Simmulations
rabi_model<-lm(v46~index,main_rabi)

B0_r_true<-summary(rabi_model)$coefficients[1, 1]
B1_r_true<-summary(rabi_model)$coefficients[2, 1]

count<-100

sumB0<-0
sumB1<-0



for(x in 1: count){
  main_sample_rabi<-main_rabi[sample(1:nrow(main_rabi),80*(nrow(main_rabi))/100),]
  
  sample_model_r<-lm(v46~index,main_sample_rabi)
  
  B0<-summary(sample_model_r)$coefficients[1, 1]
  B1<-summary(sample_model_r)$coefficients[2, 1]
  
  sumB0<-B0+sumB0
  sumB1<-B1+sumB1
}

AvgB0<-sumB0/count
AvgB1<-sumB1/count

ansB0<-paste("Rabi B0- True: ",B0_r_true," Avg: ",AvgB0)
ansB1<-paste("Rabi B1- True: ",B1_r_true," Avg: ",AvgB1)

print(ansB0)
print(ansB1)







