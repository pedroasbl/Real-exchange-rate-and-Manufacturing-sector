setwd('D:/Material - Mestrado FEA-RP/Disciplinas/2º Semestre/Econometria II/Trabalho')

library(haven)
library(tidyverse)
library(gplots)
library(plm)
library(stargazer)
library(dummies)
library(sampleSelection)
# Reading the dataset and preparing for the merge 

d1 = read_dta('teste.dta')
d2 = read_dta('heritage.dta')
dcol = read_dta('coldata110.dta')

#Working on the Colony type data:
dcol = rename(dcol,country=name) 



#Renaming the variable of the second dataframe for the merge
#d2 = rename(d2,country=countryname)

#Creating a common country vector to drop certain variables of the dataframe
countrynames1 = pull(d1, var =2)
countrynames1 = unique(countrynames1)
countrynames2 = pull(d2,var=1)
countrynames2 = unique(countrynames2)

#Doing the Loop..
country1 = c()
for (i in countrynames1){
  country_final = c()
  for (j in countrynames2){
    if (i == j){
      country_final = c(j)
      country1 = c(country_final,country1)
      country1=sort(country1)   
    }
  }
}

#Filtering the data by our variable 'country1'

d3 = filter(d2,country %in% country1)

dL = read.csv("latitude.csv")
dL = rename(dL,i=country)
dL = rename(dL,country=name)


dL = filter(dL,country %in% country1)

dcol = filter(dcol,country %in% country1)

#Mergin the Data: there is two ways that we can do this. First, use only the intersection between the dfs
#or keeping all obs of the big one. 
d_final = merge(d1,d3, by=c("country","year"))

#PREPARING THE DATA FOR THE HECKIT


d_final2 = merge(d_final,dL ,by = "country")
d_final2 = merge(d_final2,dcol, by="country",all.x = TRUE)

d_final2 = d_final2 %>% 
  mutate(distance_equator = abs(latitude)/90)



d_final2 = d_final2[!(d_final$year< 2000),]

d_final2 = d_final2 %>% mutate(Quality = as.numeric(d_final2$overallscore>quantile(d_final$overallscore,0.5,na.rm = TRUE)))


d_final2=d_final2 %>%
  mutate(pop = tcgdp*1000000/cgdp)%>%
  mutate(outputpercapita = output /pop)%>%
  mutate(loutputpercapita = log(outputpercapita)) %>%
  mutate(lkc = log(kc)) %>%
  mutate(lki = log(ki)) %>%
  mutate(lopenk = log(openk))




d_final2$growth = (d_final2$outputpercapita - lag(d_final2$outputpercapita))




#primarly we will try to make this exercise using the ballaced panel.

#preparing the data for the estimation :
d_final =d_final[!(d_final$year< 2000),]
#Saving DF
#saveRDS(d_final, file = "d_final.rds")

d_final=d_final %>%
  mutate(pop = tcgdp*1000000/cgdp)%>%
  mutate(outputpercapita = output /pop)%>%
  mutate(loutputpercapita = log(outputpercapita)) %>%
  mutate(lkc = log(kc)) %>%
  mutate(lki = log(ki)) %>%
  mutate(lopenk = log(openk))


d_final=d_final %>%
  mutate(Quality1 = as.numeric(d_final$overallscore <= quantile(d_final$overallscore,0.1)),
         Quality2 = as.numeric(d_final$overallscore>quantile(d_final$overallscore,0.1) & d_final$overallscore<=quantile(d_final$overallscore,0.5)), 
         Quality3 = as.numeric(d_final$overallscore>quantile(d_final$overallscore,0.5)& d_final$overallscore<=quantile(d_final$overallscore,0.9)))


d_final = d_final %>% mutate(Quality4 = as.numeric(d_final$overallscore>quantile(d_final$overallscore,0.9)))



d_final=d_final %>%
  mutate(Quality1Tr = as.numeric(d_final$tradefreedom <= quantile(d_final$tradefreedom,0.1)),
         Quality2Tr = as.numeric(d_final$tradefreedom>quantile(d_final$tradefreedom,0.1) & d_final$tradefreedom<=quantile(d_final$tradefreedom,0.5)), 
         Quality3Tr = as.numeric(d_final$tradefreedom>quantile(d_final$tradefreedom,0.5)& d_final$tradefreedom<=quantile(d_final$tradefreedom,0.9)))


d_final = d_final %>% mutate(Quality4Tr = as.numeric(d_final$tradefreedom>quantile(d_final$tradefreedom,0.9)))





d_final = d_final %>%
  mutate(id = paste(isic,country,sep=";")) %>%
  pdata.frame(index = c("id","year"))

d_final$growth = (d_final$outputpercapita - lag(d_final$outputpercapita))/lag(d_final$outputpercapita)



# Estimation :
#first try ge,rq,rl
#pca =princomp(d_final[,c(86,92,98)],center = TRUE, scale. = TRUE)


##First Estmation
options(scipen=999)

x = plm(growth ~ lag(outputpercapita) + lunderval + lrgdpch,data= d_final,model ="within")
y = plm(growth ~ lag(outputpercapita) + lunderval + lrgdpch + lopenk + lkc + lki,data= d_final,model ="within")
z = plm(growth ~ lag(outputpercapita) + lrgdpch + lopenk + lkc + lki   + I(lunderval*Quality1) +I(lunderval*Quality2)+ 
          I(lunderval*Quality3) +I(lunderval*Quality4) ,data= d_final,model ="within")

##Second Estimation


fit1 = plm(growth ~ lag(outputpercapita) + lunderval + lrgdpch,data= d_final,model ="within")
fit2 = plm(growth ~ lag(outputpercapita) + lunderval + lrgdpch + lopenk + lkc + lki,data= d_final,model ="within")
fit3 = plm(growth ~ lag(outputpercapita) + lrgdpch + lopenk + lkc + lki   + I(lunderval*Quality1Tr) +I(lunderval*Quality2Tr)+ 
          I(lunderval*Quality3Tr) +I(lunderval*Quality4Tr) ,data= d_final,model ="within")
summary(z)
stargazer(x,y,z)
stargazer(fit1,fit2,fit3)


stargazer(summary)

##Heckit Estimation

ols1 = lm(outputpercapita ~ lrgdpch +lopenk + lkc +lki , data=subset(d_final2, Quality==1))

heck1 = heckit( Quality ~ distance_equator + indtype + indviol, outputpercapita  ~ lrgdpch +lopenk + lkc +lki  , data=d_final2 )


  stargazer(ols1,heck1)
