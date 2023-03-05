---
title: "R Notebook"
output: html_notebook
---


  
#libraries


library(readxl)
dane <- read_excel("dane/gospodarstwa.xlsx", sheet=1)
dane
install.packages('skimr')
library(skimr)
skim(dane)
 

  

zmienne<- read_xlsx("dane/gospodarstwa.xlsx", sheet=2, range='A2:B18') 

etykiety <- read_xlsx("dane/gospodarstwa.xlsx", sheet=3, range='A2:C70')
 


  
library(dplyr)
#Create a collection that includes households from the villages.

zad2 <- dane[dane$klm==6,]
z2dplyr <-dane %>% filter(klm==6) 
z2dplyr
identical(zad2, z2dplyr)
 
  
#Create a set in which there are households with incomes > 2000 PLN
zad3 <- dane[dane$dochg>2000,] #with empty values
zad3
zad3dplyr <- dane %>% filter(dochg>2000 | is.na(dochg)) #without empty values
zad3dplyr



 


  
#Create a set which contains households from the Wielkopolska voivodship which live in a rural area and have an income of more than 3000 PLN.

dane %>% filter(woj==30 & klm==6 & dochg>3000)
 
  
#Select households from the Dolnośląskie and Mazowieckie Voivodships from cities with more than 500,000 inhabitants.
dane %>% filter(woj %in% c('02','14') & klm==1)
 

  
#Randomly select a set of 30% of households.

set.seed(1)
zad6 <- dane %>% slice_sample(prop=0.3,replace=FALSE)
zad6
 

  
#play the lottery.
toto <- as.data.frame(1:49)
toto
losy <- toto %>% slice_sample(n=6,replace=FALSE)
losy

 

  
#Randomly select 100 farms.
zad7 <- dane %>% slice_sample(n=100,replace=FALSE)
zad7

 


  
#Select households with data only for the voivodship and wydg column. but with reference only to households from Wielkopolskie voivodship.


zad9 <- dane %>% filter(woj=='30') %>% select(woj,wydg)
zad9
 

  
#Select all households whose income is between 3000-4000 and leave only this variable in the set.

zad10 <- dane %>% filter(between(dochg,3000,4000)) %>% select(dochg)
zad10

 


  
#Display the names of all the variables in the gospodarstwa set
names(dane)

#Select all columns that begin with the letter d..
dane %>% select(starts_with('d'))
 

  
# Create a new variable difference=dochg-wydg. Leave the variables in the set dochg, wydg and difference


zad14 <- dane %>% mutate(roznica=dochg-wydg) %>% select(dochg,wydg,roznica)
zad14
 

  
#Create new variables x=ln(dochg) and y=ln(wydg). Remember that ln is calculated for positive values.
dane %>% filter(wydg>0, dochg>0) %>% mutate(x= log(dochg),y=log(wydg))

 
  
#select households from kuj-pom voivodship living in rural areas. Create a variable doch5 that is 5% greater than dochg - take income positive. Leave the variables woj, dochg, doch5.t


dane %>% filter(woj=='04'&klm==6&dochg>0) %>% mutate(doch5=1.05*dochg) %>% select(woj,dochg,doch5)
 



  
#Change the names of the variables dochg to dochod and wydg to wydatki


zad16 <- dane %>% rename(dochod=dochg,wydatki=wydg)
zad16
 


  
# Calculate how many households there were due to the different variants of the variable klm

table(dane$klm)

#with dplyr

zad17 <- dane %>% group_by(klm) %>% count()
zad17


zad17b<- dane %>% count(klm)
zad17b
install.packages('xtable')
library(xtable)
xtable(zad17)
 



  
# Calculate the average expenditure of all households

dane %>% summarise(srednia=mean(wydg,na.rm=TRUE))

 


  
# Calculate the average expenditure and income of all households.

dane %>% filter(dochg>0) %>%  summarise(srednia_wyd=mean(wydg,na.rm=TRUE), srednia_doch= mean(dochg, na.rm=TRUE))
 

  
# Calculate the mean, min, max, standard deviation and median of expenditure - wydg.

summary(dane$wydg)
dane %>% summarise(mean= mean(wydg, na.rm=TRUE),min = min(wydg, na.rm=TRUE),max=max(wydg, na.rm=TRUE), std=sd(wydg, na.rm = TRUE), median=median(wydg, na.rm=TRUE))
 

  
#Calculate the average expenditure-wydg within a class of places- klm.

dane %>% group_by(klm) %>% summarise(mean= round(mean(wydg, na.rm=TRUE),1))


 

  
# Sort the set of households in ascending order by expenditure
dane %>% arrange(wydg)
 

  
#Sort the set of households in descending order by income.

dane %>% arrange(desc(dochg) )

dane %>% arrange(-dochg)
 

  
#Sort the klm in descending order and the expenditure-wydg within that class in ascending order.


dane %>% arrange(-klm,wydg)
 

  
 # Create a new variable 'class' which takes two values: "wieś", when the farm is from a village and "miasto" when the farm is from a city.


summary(dane)

jo <-dane %>% mutate(klasa=case_when(klm==6~"wies",TRUE~ "miasto"))
tab <-table(jo$klasa)
barplot(tab)
 

  
#Create a new variable 'pow' which takes three variations: "up to 60", "(60-100>" and "above 100" depending on the value of the variable d36 (area of apartments).
summary(dane$d36)


zad33 <- dane %>% mutate(pow=case_when(d36<=60~"up to 60",d36<=100~"  (60;100>",TRUE~" above 100"))
tabtab <-table(zad33$pow)
tabtab
barplot(tabtab)
 

  

sum(is.na(dane$dochg)) #36 housholds did not reply
zad34<- dane %>% mutate(dochod=case_when(is.na(dochg)~"no data", dochg<=2000 ~"low", dochg <=5000~"middle", TRUE~"high"))
zad34
table(zad34$dochod)
 
  

#Create a set wlkp. This set should contain households from the Wielkopolska voivodship whose income is in the range 2500-3000 PLN. The income should be sorted in descending order. In addition to income, there should be 4 other variables: klm, woj, zut and wydg, dochg. On this set, for the variables dochg and wydg, create a correlation scatter diagram.

wlkp <- dane %>% filter(woj=="30" & between(dochg,2500,3000)) %>% arrange(desc(dochg)) %>% select(klm,woj,zut,wydg,dochg)

wlkp
plot(wlkp$dochg,wlkp$wydg, xlab="dochody",ylab="wydatki",col='blue',pch="~" )
 





  
#Calculate how many households there were in each province due to variants of variable d61 (Household material situation).
zad19a <-dane %>% group_by(woj) %>% count(d61)
zad19a
zad19b <-dane %>% count(woj,d61)
zad19b

 


  
# Create two new variables difference=dochg-wydg and income os=dochg/los and leave them as the only ones in the dataset.
#los is the number of persons in the household  
zad31<- dane %>% mutate(difference=abs(dochg-wydg), income_person=dochg/los) %>% select(difference,income_person)
zad31

 

