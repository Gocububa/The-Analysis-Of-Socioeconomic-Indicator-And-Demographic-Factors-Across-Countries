project<-read.csv('countries-of-the-world.csv',sep=',')
summary(project)
str(project)
data<-subset(project,select=-c(4,13,14,15))
library(dplyr)
library(stringr)
data1<-subset(data,select=c(1,2))
data2<-subset(data,select=-c(1,2))
data1
data2 <- data2 %>% mutate_all(~str_replace(., ",", "."))

# Tüm sütunları numeric yap
data2 <- data2 %>% mutate_all(as.numeric)
head(data2)
data<-cbind(data1,data2)
data

data$Phones..per.1000.[6]
mean(data$Phones..per.1000.,na.rm=T)

library(vtable)
st(data)
#Q1 net migration range -20,99 ile 23,06 ise mean 2 midir?
mean(data$Net.migration,na.rm =T)
max(data$Net.migration,na.rm=T)
min(data$Net.migration,na.rm=T)
ggplot(data,aes(x=Net.migration))+
  geom_density(col='blue')+
  labs(title = 'Distribution of Net Migration')+
  theme_minimal()
result1<-t.test(data$Net.migration,mu=2)
if(result1$p.value < 0.05) {
  cat("There is a significant difference between the means of the two variables.")
} else {
  cat("There is no significant difference between the means of the two variables.")
}
equation <- 9.05e-05<0.001
boxplot(data$Net.migration, main = "Box Plot", ylab = "Values")
abline(h = mean(data$Net.migration,na.rm = T), col = "red")
abline(h = 2, col = "blue", lwd = 2, lty = "dashed")
# Check if the equation is true
if (equation) {
  cat("The equation is true.")
} else {
  cat("The equation is false.")
}
#Q2 phones per 1000 200'den düşükle 200'den büyüklerin literacy mean karşılaştırması
library(ggplot2)
filtered.data<-filter(data,Phones..per.1000.>200)
mean(filtered.data$Literacy....,na.rm = T)
ggplot(filtered.data, aes(x = Literacy....)) +
  geom_density() +
  labs(title = "Density Plot", x = "Literacy") +
  theme_minimal()
filtered.data2<-filter(data,Phones..per.1000.<200)
mean(filtered.data2$Literacy....,na.rm=T)
ggplot(filtered.data2, aes(x = Literacy....)) +
  geom_density() +
  labs(title = "Density Plot", x = "Literacy") +
  theme_minimal()
result <- t.test(filtered.data$Literacy...., filtered.data2$Literacy...., alternative = "two.sided")
result$p.value
# Interpret the results
if(result$p.value < 0.05) {
  cat("There is a significant difference between the means of the two variables.")
} else {
  cat("There is no significant difference between the means of the two variables.")
}
#Q3 arable 20'den büyük olanların genele proportionları %20 mi?
sumarable<-data$Arable....[data$Arable....>20]
sumarable2<-na.omit(sumarable)
count<-length(sumarable2)
total<-length(na.omit(data$Arable....))
55/225
prop.test(count,total,p=0.2)
#Q4 gdp'si en düşük olanla gdp'si en yüksek olan bölgelerin service oranları karşılaştırması
gdp_per_capita <- data$GDP....per.capita.
regions <- factor(data$Region)
mean_gdp_per_capita <- tapply(gdp_per_capita, regions, mean, na.rm = TRUE)
mean_gdp_per_capita
subset_data <- subset(data, Region == 'WESTERN EUROPE')
region_sum <- sum(subset_data$Industry, subset_data$Service, subset_data$Agriculture,na.rm=T)
service_proportion <- subset_data$Service / region_sum
prop.test()
#Q5 hangi varible'lar net immigration'ı etkiliyor ?
linearreg<-lm(data$Coastline..coast.area.ratio.~data$Net.migration,data=data)
summary(linearreg)
ggplot(data,aes(x=data$Coastline..coast.area.ratio.,y=data$Net.migration,color=Region))+
  geom_point()+
  theme_minimal()
ggplot(data,aes(x=data$Coastline..coast.area.ratio.,y=data$Net.migration,color=Region))+
  geom_point()+
  theme_minimal()+
  facet_wrap(~Region,labeller = label_both)
multiplereg<-glm(data$Net.migration~data$Coastline..coast.area.ratio.+data$Service+data$Industry+data$Agriculture+data$Pop..Density..per.sq..mi..+data$GDP....per.capita.+data$Literacy....+data$Phones..per.1000.+data$Arable....)
summary(multiplereg)
h<-lm(data$Net.migration~data$GDP....per.capita.)
summary(h)
library(knitr)
install.packages("broom")  # Install the 'broom' package (if not already installed)
install.packages("knitr")  # Install the 'knitr' package (if not already installed)

library(broom)  # Load the 'broom' package
library(knitr)
multiplereg2<-lm(data$Net.migration~data$Coastline..coast.area.ratio.+data$Service+data$Industry+data$Agriculture+data$Pop..Density..per.sq..mi..+data$GDP....per.capita.+data$Literacy....+data$Phones..per.1000.+data$Arable....)
model_summary <- tidy(multiplereg)
table_summary <- kable(model_summary, format = "simple", digits = 4)
table_summary
plot(multiplereg, which = 1)
plot(fitted(multiplereg), residuals(multiplereg))
#Q6 infant mortality ve birthrate'in deathrate'e etkisi var mı
mymodel<-glm(data$Deathrate~data$Infant.mortality..per.1000.births.+data$Birthrate)
plot(mymodel,which=1)
model <- lm(data$Deathrate ~ data$Birthrate + data$Infant.mortality..per.1000.births., data = data)
residuals <- rstandard(model)
qqnorm(residuals)
qqline(residuals)
birth_rate <- data$Birthrate
infant_mortality <- data$Infant.mortality
death_rate <- data$Deathrate
df <- data.frame(Birthrate = birth_rate, InfantMortality = infant_mortality, Deathrate = death_rate)
model <- aov(Deathrate ~ Birthrate * InfantMortality, data = df)
summary(model)
