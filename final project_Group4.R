# IST719 Information Visualization (M801)
# Final project R code
# Made by group no.4: Sophie Teitel, Yifan Wang and Kobi Wiseman

cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment
# Set working directory 
# Change to the folder containing the data file in your own computer:
setwd("C:/Users/User/Desktop/IST719")
data <- read.csv("C:/Users/User/Desktop/IST719/datasets_18389_24039_Accidental_Drug_Related_Deaths__2012-2017 (1).csv", header = T, na.strings = c("","NA"))
# the file above can be downloaded from the following url:
# https://www.kaggle.com/adarsh4u/drug-related-deaths
View(data)

# uploading relevant packages
library(usmap) # for drawing the CT map
library(VennDiagram) # Venn diagram of mixing drugs in substance abuse cases


# since substance abuse is recorded only from 2016 in this dataset, a subset of the data must be made:

# extracting the year from date
data$Year <- as.numeric(substr(data$Date,7,10))


data20162017 <- data[data$Year>2015,] # keeping only rows with a year 2016 and up
View(data20162017)
dim(data20162017) # this dataframe has 1955 rows and 33 columns.

# working on this file, we noticed that many of the variables details have been recorded manually, as free text.
# therefore, grouping many values together is needed:

# setting new variables
# race
data20162017$Race_New[data20162017$Race=="White"] <- "White"
data20162017$Race_New[data20162017$Race=="Black"] <- "Black"
data20162017$Race_New[data20162017$Race=="Hispanic, White"] <- "Hispanic-White"
data20162017$Race_New[data20162017$Race=="Hispanic, Black"] <- "Other"
data20162017$Race_New[data20162017$Race=="Unknown"] <- "Other"
data20162017$Race_New[data20162017$Race=="Asian, Other"] <- "Other"
data20162017$Race_New[data20162017$Race=="Asian Indian"] <- "Other"
data20162017$Race_New[data20162017$Race=="Chinese"] <- "Other"
data20162017$Race_New[data20162017$Race=="Hawaiian"] <- "Other"
data20162017$Race_New[data20162017$Race=="Native American"] <- "Other"
data20162017$Race_New[data20162017$Race=="Native American, Other"] <- "Other"

# grouping death cause
data20162017$InjuryCause <- 'Other'
data20162017$InjuryCause[is.na(data20162017$DescriptionofInjury)] <- 'Uknown'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='SUBSTANCE  ABUSE'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='substance abuse'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Substance abuse'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Substance Abuse'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='SUBSTANCE ABUSE'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='substance abuse, injection'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='SUBSTANCE  ABUSE'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='substance abuse (injection)'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Substance abuse)'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Abuse'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Abuse of Medication'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Abuse of Medications'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Abused prescription medications'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Took Medications'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='prescription medicine abuse'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Ingested medications'] <- 'Substance Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury=='Ingested medication'] <- 'Substance Abuse'

data20162017$InjuryCause[data20162017$DescriptionofInjury== 'drug use'] <- 'Drug Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Drug abuse'] <- 'Drug Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Ethanol and drug abuse'] <- 'Drug Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Drug and ethanol abuse'] <- 'Drug Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Took fentanyl and ethanol'] <- 'Drug Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Drug and alcohol abuse'] <- 'Drug Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Took cocaine'] <- 'Drug Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'USED COCAINE'] <- 'Drug Abuse'

data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Alcohol Medications'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Combined Medication Alcohol'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Combined Medication with Alcohol'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Combined Medications with Alcohol'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Drank Alcohol and Took Prescription Medications'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Ingested medications, alcohol'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'alcohol abuse'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Alcohol and Medication, alcohol'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Alcohol and substance abuse'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'ALCOHOL MEDICATION ABUSE'] <- 'Alcohol Abuse'
data20162017$InjuryCause[data20162017$DescriptionofInjury== 'Drank Alcohol to Excess While on Prescription Medications'] <- 'Alcohol Abuse'



# creating and indication variable for Substance Abuse for multi-dimensional plotting
# with this variable as one the levels:
data20162017$Death <- 1 # creating a variable for counting each row as a single case using sum function

# creating an indication variable for substance abuse case or not
data20162017$IndSubsAbuse <- 0 
data20162017$IndSubsAbuse[data20162017$InjuryCause=='Substance Abuse'] <- 1



# Binning Age
summary(data20162017$Age)
data20162017$Age_GRP <- 0

data20162017$Age_GRP[data20162017$Age>=70] <- '70+'
data20162017$Age_GRP[data20162017$Age<=69] <- '60-69'
data20162017$Age_GRP[data20162017$Age<=59] <- '50-59'
data20162017$Age_GRP[data20162017$Age<=49] <- '40-49'
data20162017$Age_GRP[data20162017$Age<=39] <- '30-39'
data20162017$Age_GRP[data20162017$Age<=29] <- '20-29'
data20162017$Age_GRP[data20162017$Age<=19] <- '10-19'
data20162017$Age_GRP[is.na(data20162017$Age)] <- '40-49' # replacing na's with the most frequent category


# drugs used
# top drugs used
data20162017$Heorin_New <- 'N'
data20162017$Heorin_New[data20162017$Heroin=="Y"] <- 'Y'
data20162017$Cocaine_New <- 'N'
data20162017$Cocaine_New[data20162017$Cocaine=="Y"] <- 'Y'
data20162017$Fentanyl_New <- 'N'
data20162017$Fentanyl_New[data20162017$Fentanyl=="Y"] <- 'Y'
data20162017$Benzodiazepine_New <- 'N'
data20162017$Benzodiazepine_New[data20162017$Benzodiazepine=="Y"] <- 'Y'
data20162017$EtOH_New <- 'N'
data20162017$EtOH_New[data20162017$EtOH=="Y"] <- 'Y'
data20162017$Oxymorphone_New <- 'N'
data20162017$Oxymorphone_New[data20162017$Oxymorphone=="Y"] <- 'Y'
data20162017$Oxycodone_New <- 'N'
data20162017$Oxycodone_New[data20162017$Oxycodone=="Y"] <- 'Y'


## charts

# over time
barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Year), FUN = sum), margin=2), beside = T, col = c("grey","yellow"), ylim = c(0,0.8), xlab = 'Year', ylab = 'Proportion of Cases' )
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext("Portion of Substance Abuse Cases Over Entire Drug Related Deaths By Year", line = 1)

# Gender
barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Sex), FUN = sum), margin = 2),beside = T, col = c("grey","yellow"), ylab = 'Proportion of Cases', xlab = 'Gender' ,ylim = c(0,0.6))
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext("Portion of Substance Abuse Cases Over Entire Drug Related Deaths By Gender", line = 1)

# we examined using a pie chart, but decided to used bar charts instead.
#pie(c(51,49), col = c('grey','yellow'))
#legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
#mtext("Female", line = -37)

#pie(c(44.2,55.7), col = c('grey','yellow'))
#legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
#mtext("Male", line = -37)

# Race
barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Race_New), FUN = sum), margin = 2),beside = T , col = c("grey","yellow"), ylim = c(0,1), xlab = 'Race', ylab = 'Proportion of Cases')
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext("Portion of Substance Abuse Cases Over Entire Drug Related Deaths By Race", line = 1)

# Age
barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Age_GRP), FUN = sum), margin = 2), beside = T, names.arg = c("10-19","20-29","30-39","40-49","50-59","60-69","70+"), col = c("grey","yellow"), ylim = c(0,1), xlab = 'Age Group', ylab = 'Proportion of Cases') 
mtext('Proportion of Substance Abuse Cases Out of Overall Drug Related Deaths By Age', line=1)
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)

# Drugs Used
barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Fentanyl_New), FUN = sum), margin = 2), col = c("grey","yellow"), ylim = c(0,0.8), beside = T, names.arg = c('Used Other Drug','Used Fentanyl'), xlab = 'Substance Abuse', ylab = 'Proportion of Cases') 
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext('Fentanyl', line=1)


barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Cocaine_New), FUN = sum), margin = 2), col = c("grey","yellow"), ylim = c(0,0.8), beside = T, names.arg = c('Used Other Drug','Used Cocaine'), xlab = 'Substance Abuse', ylab = 'Proportion of Cases') 
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext('Cocaine', line=1)


barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Heorin_New), FUN = sum), margin = 2), col = c("grey","yellow"), ylim = c(0,0.8), beside = T, names.arg = c('Used Other Drug','Used Heroin'), xlab = 'Substance Abuse', ylab = 'Proportion of Cases') 
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext('Heroin', line=1)


barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Benzodiazepine_New), FUN = sum), margin = 2), col = c("grey","yellow"), ylim = c(0,0.8), beside = T, names.arg = c('Used Other Drug','Used Heroine'), xlab = 'Substance Abuse', ylab = 'Proportion of Cases') 
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext('Benzodiazepine', line=1) # not a target audience, therefore won't be on the poster


barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$EtOH_New), FUN = sum), margin = 2), col = c("grey","yellow"), ylim = c(0,0.8), beside = T, names.arg = c('Used Other Drug','Used Heroine'), xlab = 'Substance Abuse', ylab = 'Proportion of Cases') 
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext('EtOH', line=1) # not a target audience, therefore won't be on the poster


barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Oxymorphone_New), FUN = sum), margin = 2), col = c("grey","yellow"), ylim = c(0,0.8), beside = T, names.arg = c('Used Other Drug','Used Heroine'), xlab = 'Substance Abuse', ylab = 'Proportion of Cases') 
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext('Oxymorphone', line=1) # not a target audience, therefore won't be on the poster


barplot(prop.table(tapply(data20162017$Death, list(data20162017$IndSubsAbuse, data20162017$Oxycodone_New), FUN = sum), margin = 2), col = c("grey","yellow"), ylim = c(0,0.8), beside = T, names.arg = c('Used Other Drug','Used Heroine'), xlab = 'Substance Abuse', ylab = 'Proportion of Cases') 
legend("topleft", legend = c("Non-Substance Abuse","Substance Abuse"), col = c('grey','yellow'), pch = 15, cex = 0.9)
mtext('Oxycodone', line=1) # not a target audience


# Forming the Venn diagram for mixing drugs.
# the R's output provides only a generic structure of the diagram with no colors.
# the color work was made manually outside R.

Fentanyl_Venn <- 0
Fentanyl_Venn[data20162017$Fentanyl_New=='Y'] <- 1
Fentanyl_Venn[is.na(Fentanyl_Venn)] <- 0

Heorin_Venn <- 0
Heorin_Venn[data20162017$Heorin_New=='Y'] <- 1
Heorin_Venn[is.na(Heorin_Venn)] <- 0

Cocaine_Venn <- 0
Cocaine_Venn[data20162017$Cocaine_New=='Y'] <- 1
Cocaine_Venn[is.na(Cocaine_Venn)] <- 0

venn.diagram(
x = list(which(Fentanyl_Venn==1), which(Heorin_Venn==1), which(Cocaine_Venn==1)),
category.names = c('Fentanyl','Heorin','Cocaine'),
filename = 'combination of drugs used.png',
output=TRUE
)

# CT map
# like many other attributes in this file, the county column was recorded with many errors.
# we went manually and changed the values using Excel. Therefore, determining the colors and applying
# them on the map was made outside R.
usmap::plot_usmap("counties",
                  include = c("CT"),
                  labels = TRUE, label_color = "blue",
                  fill = "yellow", alpha = 0.25, color = "black", size = 2)


