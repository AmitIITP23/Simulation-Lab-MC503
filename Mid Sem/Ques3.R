covid_data<-read.csv("C:/Users/notso/Desktop/R Mid Sem/Latest Covid-19 India Status.csv")
sum(covid_data$Active)
cat(paste("Total Active Cases :-",sum(covid_data$Active) ,"\n", sep=" "))
sum(covid_data$Discharged)
cat(paste("Total Discharged :-",sum(covid_data$Discharged) ,"\n", sep=" "))
sum(covid_data$Deaths)
cat(paste("Total Deaths :-",sum(covid_data$Deaths) ,"\n", sep=" "))

state_subset0<- subset(covid_data, covid_data$Total.Cases == max(covid_data$Total.Cases))
state_subset0
state_subset0$State.UTs
cat(paste("State with maximum total cases :-",state_subset0$State.UTs ,"\n", sep=" "))

state_subset1<- subset(covid_data, covid_data$Active == max(covid_data$Active))
state_subset1
state_subset1$State.UTs
cat(paste("State with maximum active cases :-",state_subset1$State.UTs ,"\n", sep=" "))



state_subset2<- subset(covid_data, covid_data$Deaths == max(covid_data$Deaths))
state_subset2
state_subset2$State.UTs
cat(paste("State with maximum deaths :-",state_subset2$State.UTs ,"\n", sep=" "))


activeCases <- covid_data$Active
discharged <- covid_data$Discharged
activeCases
discharged

correlation <- cor(activeCases, discharged, method = "pearson")
correlation

covariance <- cov(activeCases,discharged, method = 'pearson') 
covariance
cat(paste("Co-Relation and Co-Variance are :-", correlation, covariance,"\n", sep=" "))

firstTen = covid_data[c(1:10),]
print(firstTen)

tenStates <- firstTen$State.UTs

deaths <- firstTen$Deaths
barplot(deaths,names.arg=tenStates,xlab="State",ylab="Death", main="State V/S Deaths")