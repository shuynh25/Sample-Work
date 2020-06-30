## Complete the following codes for your data set. If you provide your codes below each comment,
 # it would be easy to understand your steps.

## Import raw data into R
library(readr)
cps16 <- read_csv("~/Downloads/cps16.csv")
View(cps16)

## Subset the raw data by four conditions
 # condition 1: only for adults (df$AGE>=17)
 # condition 2: at least 48 weeks of work in the previous year (df$WKSWORK1>=48)
 # condition 3: more than 35 hours per week (df$UHRSWORKLY>35)
 # condition 4: The hourly wage should be greater than 0 and less than equal to 99.97
 #              99.97 is top-code.
 # or you can use multiple conditions together if you use R operator & (ampersand, it means "and")
  # for example, subset(target, condition 1 & condition 2 & condition 3 & condition 4)
cps_A <- subset(cps16, cps16$AGE>=17 & cps16$WKSWORK1>=48 & cps16$UHRSWORKLY>35 & cps16$HOURWAGE>0 & cps16$HOURWAGE<=99.97)


## Create new variables (gender dummy variables)
 # In the data description, SEX=1 means male workers and SEX=2 indicates female workers
 # Create two new variables, female (1 if SEX==2, 0 otherwise) 
 # and male (1 if SEX==1, 0 otherwise) using ifelse condition
 # for example, df$female<-ifelse(df$SEX==2, 1, 0)
cps_A$male <- ifelse(cps_A$SEX==1, 1, 0)
cps_A$female <- ifelse(cps_A$SEX==2, 1, 0)


## Create a new variable, union using ifelse condition again
# union=1 if UNION==2; 0 otherwise (UNION==1 or 3)
cps_A$union <- ifelse(cps_A$UNION==2,1,0)

## Create racial dummy variables
 # variable, white: 1 if RACE==100, 0 otherwise
 # variable, black: 1 if RACE==200, 0 otherwise
 # variable, asian: 1 if RACE==651, 0 otherwise
 # variable, others: 1 if RACE==300 | RACE==652 | RACE>=801, 0 otherwise
cps_A$white <- ifelse(cps_A$RACE==100, 1, 0)
cps_A$black <- ifelse(cps_A$RACE==200, 1, 0)
cps_A$asian <- ifelse(cps_A$RACE==651, 1, 0)
cps_A$others <- ifelse(cps_A$RACE==300 | cps_A$RACE==652 | cps_A$RACE>=801, 1, 0)


## Replace values in existing variable EDUC with numeric numbers
 # replace numerical categories into numbers in years (with approximation if necessary)
 # If you refer to the description file, the variable EDUC was recorded by numeric codes
 # for example, 2 means none or preschool, i.e. it should be changed into 2.
 # use the following codes, you just need to change df into your object name of data frame.
cps_A$EDUC[cps_A$EDUC==2]<-0 # 2 = none or preschool
cps_A$EDUC[cps_A$EDUC==12]<-2.5 # 10 = grades 1,2,3, or 4
cps_A$EDUC[cps_A$EDUC==20]<-5.5 # 20 = grades 5 or 6
cps_A$EDUC[cps_A$EDUC==30]<-7.5 # 20 = grades 7 or 8
cps_A$EDUC[cps_A$EDUC==40]<-9 # 40 = grade 9
cps_A$EDUC[cps_A$EDUC==50]<-10 # 50 = grade 10
cps_A$EDUC[cps_A$EDUC==60]<-11 # 60 = grade 11
cps_A$EDUC[cps_A$EDUC==71 | cps_A$EDUC==73]<-12 # 71 = 12th grade, no diploma, 73 = high school diploma or equivalent
cps_A$EDUC[cps_A$EDUC==81]<-13 # 80 = some college but no degree
cps_A$EDUC[cps_A$EDUC==91 | cps_A$EDUC==92]<-14 # 91 = 2 years of college, 92 = associate's degree  
cps_A$EDUC[cps_A$EDUC==111]<-16 # 111 = bachelor's degree
cps_A$EDUC[cps_A$EDUC==123 | cps_A$EDUC==124]<-18  # 123 = master's degree, 124 = professional school degree
cps_A$EDUC[cps_A$EDUC==125]<-21 # 125 = bachelor's degree


# Rename old uppercase variables with lowercase variables
# EDUC -> educ; HOURWAGE -> wage; AGE -> age; INCWAGE -> income
# for example, colnames(df)[colnames(df)=="EDUC"]<-"educ"
colnames(cps_A)[colnames(cps_A)=="EDUC"] <- "educ"
colnames(cps_A)[colnames(cps_A)=="HOURWAGE"] <- "wage"
colnames(cps_A)[colnames(cps_A)=="AGE"] <- "age"
colnames(cps_A)[colnames(cps_A)=="INCWAGE"] <- "income"

## Select only relevant variables
 # wage, income, age, educ, male, female, white, black, asian, others, union
 # new_dataframe<-subset(old_dataframe, select = c("wage", "income", ...))
cps_new <- subset(cps_A, select = c("wage", "income", "age", "educ", "male", "female", "white", "black", "asian", "others", "union"))


## Create a new variable, exper (experience) by the following definition:
 # df$exper <- df$age - df$educ - 6
cps_new$exper <- cps_new$age - cps_new$educ - 6


## Check the summary of exper
 # You will have four negative numbers, it doesn't make any sense.
 # It would be recording error or something else.
 # You can find those observations: which(df$exper<0)
summary(cps_new$exper)
which(cps_new$exper<0)

## Drop incorrect observations, i.e. negative exper (exper<0) 
 # using subset (condition, df$exper>=0)
cps_final <- subset(cps_new, cps_new$exper>=0)


## Check the structure of final data set.
 # Should have 4344 observations of 12 variables.



## Export the data set into csv format and save this codes.
 # write.csv(data frame, "filename.csv") 
 # The data set can then be used for regressions.
write.csv(cps_final, "cps.csv")
