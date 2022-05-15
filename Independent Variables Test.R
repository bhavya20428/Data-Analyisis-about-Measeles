
library(readr)
library(data.table)
library(ggplot2)
library(dplyr)

# A function to get mode of the data SOURCE: www.tutorialspoint.com

getmode <- function(vec) {
  uniqvec <- unique(vec)
  uniqvec[which.max(tabulate(match(vec, uniqvec)))]
}

theData <- read.csv("mainn.csv")

mean_d<-mean(theData$v46,na.rm=TRUE)
sd_d<-sd(theData$v46,na.rm=TRUE)
theData<-subset(theData,v46<=9.60493)


View(theData)


theData_kh <- subset(theData, theData$season=="Kharif")
# mean_k<-mean(main_kharif$v46,na.rm=TRUE)
# sd_k<-sd(main_kharif$v46,na.rm=TRUE)
# main_kharif<-subset(main_kharif,v46<=9.7854)

listOfvars <- c('gdp', 'beds', 'tap', 'index','v1',  'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8', 'v9', 'v10', 'v11', 'v12', 'v13', 'v14', 'v15', 'v16', 'v17', 'v18', 'v19', 'v20', 'v21', 'v22', 'v23', 'v24', 'v25', 'v26', 'v27', 'v28', 'v29', 'v30', 'v31', 'v32', 'v33', 'v34', 'v35', 'v36', 'v37', 'v38', 'v39', 'v40', 'v41', 'v42', 'v43', 'v44', 'v45','v46', 'v47' )
curData <- subset(theData_kh, TRUE, select = listOfvars)
newData_kh <- na.omit(curData)
View(newData)

# curData <- data.frame(theData$v46, theData$gdp, theData$beds, theData$tap, theData$v18, theData$v19, theData$index, theData$v11, theData$v16, theData$v21, theData$v17, theData$v28, theData$v34)
# curData <- na.omit(curDat)
# View(curData)

# LR <- lm(formula = v46 ~ gdp + beds + index + v2 + v3 + v8 + v11 + v12 + v13 + v17 + v18 + v25  +  v29  + v34 + v42 + v45  , data = newData_kh )
# Multiple R-squared:  0.01333
# LR <- lm(formula = v46 ~ gdp + beds + tap + index + v40 + v41 + v42 + v43 + v44 + v45 + v47, data = theData )
# Multiple R-squared:  0.9218
# LR <- lm(formula = v46 ~ gdp + beds + tap + index  + v2 + v3 + v11 + v12 + v13 + v17 + v18 + v25  +  v29  + v34 + v40 + v41 + v42 + v43 + v44 + v45 + v47 , data = newData_kh )
# Multiple R-squared:  0.9222
# LR <- lm(formula = v46 ~ gdp + beds + tap + index  + v2 + v3 + v11 + vC12 + v13 + v17 + v18 + v25  +  v29  + v34 + v40 +  v47 + v45 , data = newData_kh )

# FAULTY LR MODEL
LR <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v40  + v41 + v42 + v43 + v44 + v45 + v47 , data = newData_kh )
summary(LR)
LR <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 , data = newData_kh )

# 
# LRE <- lm(formula = v46 ~ v47, data = newData_kh )
# summary(LRE)

print(LR)
summary(LR)

LR$coefficients


final_data_kh <- rbind(newData_kh, standard_res_kh)
res_ind_Data_kh<-data.frame(newData_kh$index, standard_res_kh)
print(final_data_kh)

y_kh <- data.frame(newData_kh$index, newData_kh$v46)


# require(scales) 
# ggplot(res_ind_Data_kh ,aes(newData_kh$index, standard_res_kh))+
#   geom_bar(stat='identity',fill=colors()[128])+
#   scale_y_continuous(labels = comma)+labs(title = "Kharif Measles Linear Model Plot ", y= "Residuals", x = "Yield Index")
# 

# ggplot(res_ind_Data_kh,aes(newData_kh.index, fill = standard_res_kh)) + geom_histogram(alpha=0.5, aes(y=..density..), position='identity' ) +
#   stat_bin(bins=45) +labs(title = "Plot 2 Kharif", y= "Residual", x = "Index")


# dataff <- data.frame(newData_kh$gdp, newData_kh$beds, newData_kh$index, newData_kh$v2, newData_kh$v3, newData_kh$v6, newData_kh$v13, newData_kh$v18, newData_kh$v25,  newData_kh$v29, newData_kh$v34, newData_kh$v42, newData_kh$v45)


ypredkh <- c(3.178 - 0.0008724*newData_kh$index)
y_pred_kh <- data.frame(newData_kh$index, ypredkh)
print(y_pred_kh)

plot(res_ind_Data_kh$newData_kh.index, res_ind_Data_kh$standard_res_kh, main = "Kharif Plot Residuals VS Yield Index",
     ylab = "Residual",xlab = "Yield Index")
plot( y_kh$newData_kh.index, y_kh$newData_kh.v46, main = "Kharif Plot True Variable Death By Measles VS Yield Index",
      ylab = "True Variable Death By Measles",xlab = "Yield Index")
# plot(x, y1, type='n', ann=F, xlim=c(1,2),ylim=c(1,6))
# abline(LR, col = 'blue')
coeff<-LR$coefficients

y_hat_kh <- fitted(LR)
print(y_hat_kh)
plot( y_kh$newData_kh.index, y_hat_kh, main = "Predicted Plot True Variable Death By Measles VS Yield Index",
      ylab = "Predicted Variable Death By Measles",xlab = "Yield Index")
# 
# plot( y_kh$newData_kh.index, y_pred_kh, main = "Kharif Plot Predicted Variable Death By Measles VS Yield Index",
#       ylab = "Predicted Variable Death By Measles",xlab = "Yield Index")
# 


# _+__________________________________________________
#  PART C

standard_res_kh <- resid(LR)
print(standard_res_kh)
print(sum(standard_res_kh))
hist(standard_res_kh,main = "Residuals")
# _+__________________________________________________
#  PART D

ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$gdp)
print(sum(ux))
hist(ux,main = "gdp")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$beds)
print(sum(ux))
hist(ux,main = "beds")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$index)
print(sum(ux))
hist(ux,main = "index")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v2)
print(sum(ux))
hist(ux,main = "Pregnant women registered for ANC within first trimester")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v3)
print(sum(ux))
hist(ux,main = "Pregnant women recieved 3 ANC check-ups")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v6)
print(sum(ux))
hist(ux,main = "Tested moderately anaemic")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v13)
print(sum(ux))
hist(ux,main = "Instititional Deliveries")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v18)
print(sum(ux))
hist(ux,main = "Women recieved Post-Partem check-up within 48 hrs of delivery")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v25)
print(sum(ux))
hist(ux,main = "%age of reported live births")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v29)
print(sum(ux))
hist(ux,main = "New borns breastfed within One Hour")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v34)
print(sum(ux))
hist(ux,main = "Fully immunized children in age group of 9 to 11 months")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v42)
print(sum(ux))
hist(ux,main = "%age of LBW deaths in infants")
ux <- c(res_ind_Data_kh$standard_res_kh*newData_kh$v45)
print(sum(ux))
hist(ux,main = "%age of Infant deaths due to fever")

# require(scales)
# ggplot(data.frame(ux) ,aes(ux, ux))+
#   geom_bar(stat='identity',fill=colors()[128])+
#   scale_y_continuous(labels = comma)+labs(title = "Rabi Measles Linear Model Plot ", y= "Residuals", x = "Yield Index")
# 
# 














theData_rb <- subset(theData, theData$season=="Rabi")
# mean_k<-mean(main_kharif$v46,na.rm=TRUE)
# sd_k<-sd(main_kharif$v46,na.rm=TRUE)
# main_kharif<-subset(main_kharif,v46<=9.7854)


curData <- subset(theData_rb, TRUE, select = listOfvars)
newData_rb <- na.omit(curData)
View(newData)


# curData <- data.frame(theData$v46, theData$gdp, theData$beds, theData$tap, theData$v18, theData$v19, theData$index, theData$v11, theData$v16, theData$v21, theData$v17, theData$v28, theData$v34)
# curData <- na.omit(curDat)
# View(curData)


# LR <- lm(formula = v46 ~ gdp + beds + tap + index  + v2 + v3 + v11 + v12 + v13 + v17 + v18 + v25  +  v29  + v34   , data = theData_rb )
# Multiple R-squared:  0.01333
# LR <- lm(formula = v46 ~ gdp + beds + tap + index + v40 + v41 + v42 + v43 + v44 + v45 + v47, data = theData )
# Multiple R-squared:  0.9218
# LR <- lm(formula = v46 ~ gdp + beds + tap + index  + v2 + v3 + v11 + v12 + v13 + v17 + v18 + v25  +  v29  + v34 + v40 + v41 + v42 + v43 + v44 + v45 + v47 , data = newData_rb )
# Multiple R-squared:  0.9222
LR <- lm(formula = v46 ~ gdp +beds+ index + v2 + v3  + v6 + v13 + v18 + v25  +  v29  + v34 + v42 + v45 , data = newData_rb )


print(LR)
summary(LR)

LR$coefficients

standard_res_rb <- resid(LR)
print(standard_res_rb)
print(mean(standard_res_rb))
final_data_rb <- rbind(newData_rb, standard_res_rb)
res_ind_Data_rb<-data.frame(newData_rb$index, standard_res_rb)

y_rb <- data.frame(newData_rb$index, newData_rb$v46)


plot(res_ind_Data_rb$newData_rb.index, res_ind_Data_rb$standard_res_rb, main = "Rabi Plot Residuals VS Yield Index",
     ylab = "Residual",xlab = "Yield Index")

plot( y_rb$newData_rb.index, y_rb$newData_rb.v46, main = "Rabi Plot True Variable Death By Measles VS Yield Index",
      ylab = "Residual",xlab = "Yield Index")


y_hat_rb <- fitted(LR)
print(y_hat_rb)
plot( y_rb$newData_rb.index, y_hat_rb, main = "Predicted Plot True Variable Death By Measles VS Yield Index",
      ylab = "Predicted Variable Death By Measles",xlab = "Yield Index")


# require(scales) 
# ggplot(res_ind_Data_rb ,aes(newData_rb$index, standard_res_rb))+
#   geom_bar(stat='identity',fill=colors()[128])+
#   scale_y_continuous(labels = comma)+labs(title = "Rabi Measles Linear Model Plot ", y= "Residuals", x = "Yield Index")
#  
 

# plot(newData_rb$index, standard_res_rb, 
#      +     ylab="Residuals", xlab="Yield Index", 
#      +     main="Old Faithful Eruptions") 

# _+__________________________________________________
#  PART C

standard_res_rb <- resid(LR)
print(standard_res_rb)
print(sum(standard_res_rb))
hist(standard_res_rb,main = "Residuals")
# _+__________________________________________________
#  PART D

ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$gdp)
print(sum(ux))
hist(ux,main = "gdp")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$beds)
print(sum(ux))
hist(ux,main = "beds")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$index)
print(sum(ux))
hist(ux,main = "index")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v2)
print(sum(ux))
hist(ux,main = "Pregnant women registered for ANC within first trimester")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v3)
print(sum(ux))
hist(ux,main = "Pregnant women recieved 3 ANC check-ups")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v6)
print(sum(ux))
hist(ux,main = "Tested moderately anaemic")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v13)
print(sum(ux))
hist(ux,main = "Instititional Deliveries")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v18)
print(sum(ux))
hist(ux,main = "Women recieved Post-Partem check-up within 48 hrs of delivery")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v25)
print(sum(ux))
hist(ux,main = "%age of reported live births")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v29)
print(sum(ux))
hist(ux,main = "New borns breastfed within One Hour")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v34)
print(sum(ux))
hist(ux,main = "Fully immunized children in age group of 9 to 11 months")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v42)
print(sum(ux))
hist(ux,main = "%age of LBW deaths in infants")
ux <- c(res_ind_Data_rb$standard_res_rb*newData_rb$v45)
print(sum(ux))
hist(ux,main = "%age of Infant deaths due to fever")
