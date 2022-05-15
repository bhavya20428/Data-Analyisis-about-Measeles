theData<- mainn
curData <- data.frame(theData$v46, theData$gdp, theData$beds, theData$tap, theData$index, theData$v11, theData$v16, theData$v21, theData$v28, theData$v34)

south <- c('Andhra Pradesh', 'Telangana', 'Karnataka', 'Kerela', 'Tamil Nadu')
west <- c('Rajasthan', 'Gujarat', 'Goa','Maharashtra')
east <- c('Bihar', 'Orissa', 'Jharkhand', 'West Bengal')
north <- c('Himachal Pradesh', 'Punjab', 'Uttrakhand', 'Uttar Pradesh', 'Haryana')
central <- c('Madhya Pradesh', 'Chhattisgarh')

theData$North <- ifelse(theData$state %in% north, 1, 0)
theData$West <- ifelse(theData$state %in% west, 1, 0)
theData$East <- ifelse(theData$state %in% east, 1, 0)
theData$Central <- ifelse(theData$state %in% central, 1, 0)

mean_d<-mean(theData$v46,na.rm=TRUE)
sd_d<-sd(theData$v46,na.rm=TRUE)
theData<-subset(theData,v46<=9.60493)


kharif <- subset(theData, season == "Kharif")
kharif <- na.omit(kharif)
rabi <- subset(theData, season == "Rabi")
rabi <- na.omit(rabi)




k_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + South , data = kharif )

summary(k_dummy_linear_model)

sse_kd <- sum((fitted(k_dummy_linear_model) - kharif$v46)^2)
print(sse_kd)

k_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45  , data = kharif )
summary(k_linear_model)
sse_ku <- sum((fitted(k_linear_model) - kharif$v46)^2)
print(sse_ku)

rabi_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + South , data = rabi )
summary(rabi_dummy_linear_model)
sse_rd <- sum((fitted(rabi_dummy_linear_model) - rabi$v46)^2)
print(sse_rd)

rabi_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45, data = rabi )
summary(rabi_linear_model)
sse_ru <- sum((fitted(rabi_linear_model) - rabi$v46)^2)
print(sse_ru)






k_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + North , data = kharif )

summary(k_dummy_linear_model)

sse_kd <- sum((fitted(k_dummy_linear_model) - kharif$v46)^2)
print(sse_kd)

k_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45  , data = kharif )
summary(k_linear_model)
sse_ku <- sum((fitted(k_linear_model) - kharif$v46)^2)
print(sse_ku)

rabi_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + North , data = rabi )
summary(rabi_dummy_linear_model)
sse_rd <- sum((fitted(rabi_dummy_linear_model) - rabi$v46)^2)
print(sse_rd)

rabi_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45, data = rabi )
summary(rabi_linear_model)
sse_ru <- sum((fitted(rabi_linear_model) - rabi$v46)^2)
print(sse_ru)





k_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + West , data = kharif )

summary(k_dummy_linear_model)

sse_kd <- sum((fitted(k_dummy_linear_model) - kharif$v46)^2)
print(sse_kd)

k_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45  , data = kharif )
summary(k_linear_model)
sse_ku <- sum((fitted(k_linear_model) - kharif$v46)^2)
print(sse_ku)

rabi_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + West , data = rabi )
summary(rabi_dummy_linear_model)
sse_rd <- sum((fitted(rabi_dummy_linear_model) - rabi$v46)^2)
print(sse_rd)

rabi_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45, data = rabi )
summary(rabi_linear_model)
sse_ru <- sum((fitted(rabi_linear_model) - rabi$v46)^2)
print(sse_ru)





k_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + East , data = kharif )

summary(k_dummy_linear_model)

sse_kd <- sum((fitted(k_dummy_linear_model) - kharif$v46)^2)
print(sse_kd)

k_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45  , data = kharif )
summary(k_linear_model)
sse_ku <- sum((fitted(k_linear_model) - kharif$v46)^2)
print(sse_ku)

rabi_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + East , data = rabi )
summary(rabi_dummy_linear_model)
sse_rd <- sum((fitted(rabi_dummy_linear_model) - rabi$v46)^2)
print(sse_rd)

rabi_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45, data = rabi )
summary(rabi_linear_model)
sse_ru <- sum((fitted(rabi_linear_model) - rabi$v46)^2)
print(sse_ru)





k_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + Central , data = kharif )

summary(k_dummy_linear_model)

sse_kd <- sum((fitted(k_dummy_linear_model) - kharif$v46)^2)
print(sse_kd)

k_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45  , data = kharif )
summary(k_linear_model)
sse_ku <- sum((fitted(k_linear_model) - kharif$v46)^2)
print(sse_ku)

rabi_dummy_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 + Central , data = rabi )
summary(rabi_dummy_linear_model)
sse_rd <- sum((fitted(rabi_dummy_linear_model) - rabi$v46)^2)
print(sse_rd)

rabi_linear_model <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45, data = rabi )
summary(rabi_linear_model)
sse_ru <- sum((fitted(rabi_linear_model) - rabi$v46)^2)
print(sse_ru)
#ggplot(theData,aes(y=v46,x=index ))+geom_point()+geom_smooth(method="lm")
library(ggplot2)
non_south=function(x){coef(linear_model)[2]*x+coef(linear_model)[1]}
south=function(x){coef(linear_model)[2]*x+coef(linear_model)[1]+coef(linear_model)[3]}

ggplot(theData,aes(y=v46,x=gdp,color=South))+geom_point()+
  stat_function(fun=non_south,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=south,geom="line",color=scales::hue_pal()(2)[2])

s <- subset(theData, South==1,
            select=c(v46))
ns <- subset(theData, South==0,
            select=c(v46))

s_list = unlist(s)
ns_list = unlist(ns)

write.csv(theData,"/Users/hitesh/Downloads/Ques3_1.csv", row.names = FALSE)


var.test(s_list, ns_list, alternative = "two.sided")