#Import the database containning all the variables for all the countries
library(readxl)
Dat <- read_excel("C:/Users/hadar/Desktop/Projet R/Datacov.xlsx")
View(Dat)
names(Datacov)
#Taking out the variables for only France
country_names = unique(Datacov$CountryName)
covid_france=Datacov[Datacov$CountryName=="France",]
#Exporting to Excel 
write.table(datacov , "Dat.xlsx" , row.names = FALSE , sep = ";")
getwd()
#Import the first base containning independant variables
library(readxl)
Dat <- read_excel("C:/Users/hadar/Desktop/Projet R/Dat.xlsx")
View(Dat)
#Import the first base containning dependant variable
library(readxl)
defaillance_entreprises <- read_excel("C:/Users/hadar/Desktop/Projet R/defaillance entreprises.xlsx")
View(defaillance_entreprises)
#Binding the base xith the dependant variable
base = cbind(Dat, defaillance_entreprises$Nbre_def)
#Summary
summary(base)
#Correlation 
cor(base)
#Histogram
hist(base$`C3_Cancel public events`)
hist(base$StringencyIndex)
#Plot
plot(base$StringencyIndex, t="l",
     xlab="Number of days since January, 1st, 2020",
     ylab="Cumulated number of cases")

plot(base$`defaillance_entreprises$Nbre_def`, t="l",
     xlab="Number of days since January, 1st, 2020",
     ylab="defaillances")
# Categorisation
dich_event_2=c(1:67)
for(i in 1:67) {
  if (base$`C3_Cancel public events`[i]==2) dich_event_2[i] = 1
  else dich_event_2[i] = 0
}
#Regression
reg=lm(base$`defaillance_entreprises$Nbre_def`~base$StringencyIndex+base$EconomicSupportIndexForDisplay+base$`C3_Cancel public events`,base)
summary(reg)
