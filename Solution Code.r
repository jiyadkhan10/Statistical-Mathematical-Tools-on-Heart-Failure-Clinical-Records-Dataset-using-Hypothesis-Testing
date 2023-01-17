getwd()
# loading the data
data<-read.csv('heart_failure_clinical_records_dataset.csv')

# Print of the data
print(data)

# Head of the data
head(data)

# view of the data
View(data)

# Summary of the data
summary(data)

# Question 1

blood_pressure = table(data$diabetes, data$high_blood_pressure)
blood_pressure

prop.table(blood_pressure, 2)
barplot(blood_pressure, main = "Diabetic Peoples being more likely to have Blood Pressure", ylab = "Total Count", xlab = "Comparison between Diabetic Peoples having Blood Pressure and Non-Diabetic Peoples having Blood Pressure", legend = rownames(blood_pressure), beside = TRUE)

chisq.test(blood_pressure)$expected
chisq.test(blood_pressure)

#Question 2

anaemia_female = table(data$anaemia, data$sex)
anaemia_female

prop.table(anaemia_female, 2)
barplot(anaemia_female, main = "People having anemia are more likely to be from female", ylab = "Total Count", xlab = "Comparison between People having anemia from Female and People not having anemia from Female", legend = rownames(anaemia_female), beside = TRUE)

chisq.test(anaemia_female)$expected
chisq.test(anaemia_female)

# Question 3

age <- subset(data, data$age > 55 & data$age < 65)
age

death_event = table(age$high_blood_pressure==1, age$DEATH_EVENT)
death_event

prop.table(death_event, 2)
barplot(death_event, main = " People whose age is in between 55 and 65 having high Blood Pressure are less likely to survive", ylab = "Total Count", xlab = "Comparison between People are less likely to survive and People are more likely to survive", legend = rownames(death_event), beside = TRUE)

chisq.test(death_event)$expected
chisq.test(death_event)

# Question 4

diabetes <- subset(data, data$diabetes == 1)
diabetes

people_smoking = table(diabetes$smoking==1, diabetes$DEATH_EVENT)
people_smoking

prop.table(people_smoking, 2)
barplot(people_smoking, main = "Diabetic Peoples who smokes are less likely to survive", ylab = "Total Count", xlab = "Comparison between People are less likely to survive and People are more likely to survive", legend = rownames(people_smoking), beside = TRUE)

chisq.test(people_smoking)$expected
chisq.test(people_smoking)

# Question 5

male_diabetes = table(data$sex, data$diabetes)
male_diabetes

prop.table(male_diabetes, 2)
barplot(male_diabetes, main = "Male Peoples being more likely to have Diabetic", ylab = "Total Count", xlab = "Comparison between Male Peoples being more likely to have Diabetic and Male Peoples being less likely to have Diabetic", legend = rownames(male_diabetes), beside = TRUE)

chisq.test(male_diabetes)$expected
chisq.test(male_diabetes)





