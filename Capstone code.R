library(ggplot2)
library(dplyr)

d_math <- read.csv2("student-mat.csv")
d_port <- read.csv2("student-por.csv")

# How many students are enrolled in both math and Portuguese? Merge based on attributes not associated with course.
d_both <- merge(d_math, d_port, by=c("school", "sex", "age", "address", 
                "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob",
                "reason", "guardian", "traveltime", "activities", "nursery",
                "higher", "internet", "romantic", "famrel", "freetime", 
                "goout", "Dalc", "Walc", "health", "absences"))
print(nrow(d_both)) # There are 100 students enrolled in both courses

# Tidy up variable types, labels of variables
d_math$studytime <- factor(d_math$studytime, labels = c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs"))
d_port$studytime <- factor(d_port$studytime, labels = c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs"))
d_math$Medu <- factor(d_math$Medu, labels = c("none", "primary", "upper primary", "secondary", "higher"))
d_port$Medu <- factor(d_port$Medu, labels = c("none", "primary", "upper primary", "secondary", "higher"))
d_math$Fedu <- factor(d_math$Fedu, labels = c("none", "primary", "upper primary", "secondary", "higher"))
d_port$Fedu <- factor(d_port$Fedu, labels = c("none", "primary", "upper primary", "secondary", "higher"))
d_math$traveltime <- factor(d_math$traveltime, labels = c("<15 min", "15-30 min", "30 min-1 hr", ">1 hr"))
d_port$traveltime <- factor(d_port$traveltime, labels = c("<15 min", "15-30 min", "30 min-1 hr", ">1 hr"))
  
# Create complete dataset d_total: combine math and Portuguese datasets with "course" as variable

course <- rep("math", times = length(d_math$school))
d_math <- cbind(d_math, course)

course <- rep("port", times = length(d_port$school))
d_port <- cbind(d_port, course)

d_total <- rbind(d_math, d_port)
View(d_total)

# EXPLORATORY DATA ANALYSIS

# Distribution of G3 grades
mean(d_total$G3, na.rm = TRUE) # ~11.34
median(d_total$G3, na.rm = TRUE) # 11

ggplot(d_total, aes(x = G3)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(d_total$G3, na.rm = TRUE), sd = sd(d_total$G3, na.rm = TRUE)),
                lwd = 1,
                col = "red") +
  xlab("Final Grade")

# How do distributions of math and Portuguese grades compare?
ggplot(d_total, aes(x = G3)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(d_total$G3, na.rm = TRUE), sd = sd(d_total$G3, na.rm = TRUE)),
                lwd = 1,
                col = "red") +
  xlab("Final Grade") +
  facet_grid(. ~ course) 

ggplot(d_total, aes(x = G3, y = ..density.., col = course)) +
  geom_freqpoly(binwidth = 1, position = "identity") +
  xlab("Final Grade")

# Who are the students with the absolute failing grades (G3 = 0 or 1)?
abs_fail <- d_total[d_total$G3 < 2, ]
print(nrow(abs_fail)) # There are 54 students in this group. I wonder what they have in common?

# What percent of math students absolutely fail compared to Portuguese students?
nrow(abs_fail[abs_fail$course == "math", ]) / nrow(d_total[d_total$course == "math", ]) # ~9.6%
nrow(abs_fail[abs_fail$course == "port", ]) / nrow(d_total[d_total$course == "port", ]) # ~2.5%

# Who are the students with the highest grades (G3 >= 18)?
highest_grades <- d_total[d_total$G3 >= 18, ]
View(highest_grades) # There are 35 students in this group.

# What percent of math students get highest grades compared to Portuguese students?
nrow(highest_grades[highest_grades$course == "math", ]) / nrow(d_total[d_total$course == "math", ]) # ~4.6%
nrow(highest_grades[highest_grades$course == "port", ]) / nrow(d_total[d_total$course == "port", ]) # ~2.6%

# Dataset filtered by school
d_GP <- filter(d_total, school == "GP")
d_MS <- filter(d_total, school == "MS")

# Correlations between final grade and the attributes associated with course

# STUDYTIME: weekly study time within the course subject (math or Portuguese)
ggplot(d_total, aes(x=studytime, y=G3)) +
  geom_boxplot(fill = "grey80", color = "blue") +
  xlab("Study Time Per Week") + ylab("Final Grade")
# No obvious correlation here
ggplot(d_total, aes(x=studytime, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) + 
  xlab("Study Time Per Week") + ylab("Final Grade")
# There do appear to be fewer low grades among students who study more. 

# Significant? Try one-way ANOVA test.
summary(aov(G3 ~ studytime, data = d_total)) # p-value is 9.92e-07, significant.

# Is it different for math and Portuguese?
summary(aov(G3 ~ studytime, data = d_math)) # p-value = ~5.2%, not significant (barely)
summary(aov(G3 ~ studytime, data = d_port)) # p-value = 1.09e-10, significant
# Interesting that study time has a  bigger impact on Portuguese grades than on math grades!

# How about among the students who earn the highest grades?
summary(aov(G3 ~ studytime, data = highest_grades)) # p-value = ~40.3%, not significant.
# This doesn't make intuitive sense to me...
ggplot(highest_grades, aes(x=studytime, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Study Time Per Week") + ylab("Final Grade")
# I see! When I look at the scatterplot of just the highest grades group, I can see no apparent correlation between studytime and grades.
# Which makes sense because the range of grades in this group is (by definition) very small.
# Among the students with the highest grades, some study a lot and some don't.
# However, among students as a whole, those who study a lot get higher grades. One-way ANOVA tests now make sense to me.

# FAILURES: number of past class failures within the course subject (math or Portuguese)
ggplot(d_total, aes(x=failures, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) + 
  xlab("# Courses Previously Failed") + ylab("Final Grade")
# There are no high grades among students with several failures.

# A portion of students absolutely fail regardless of whether they've failed before.
nrow(abs_fail[abs_fail$failures == 0, ]) / nrow(d_total[d_total$failures == 0, ]) # ~2.8%
nrow(abs_fail[abs_fail$failures == 1, ]) / nrow(d_total[d_total$failures == 1, ]) # 15%
nrow(abs_fail[abs_fail$failures == 2, ]) / nrow(d_total[d_total$failures == 2, ]) # ~18.2%
nrow(abs_fail[abs_fail$failures == 3, ]) / nrow(d_total[d_total$failures == 3, ]) # 20%
# Students who have failed one or more times previously are much more likely to absolutely fail then those who haven't.
summary(aov(G3 ~ failures, data = d_total)) # p-value < 2e-16, significant.

# Is there a difference between math and Portuguese?
summary(aov(G3 ~ failures, data = d_math)) # p-value = 1.47e-13
summary(aov(G3 ~ failures, data = d_port)) # p-value < 2e-16
# No, the relationship between failures and final grades is significant for both.

# SCHOOLSUP: extra educational support within the course subject (math or Portuguese)
ggplot(d_total, aes(x=schoolsup, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Extra Educational Support") + ylab("Final Grade")
# It looks like students receiving school support get fewer high grades and fewer fails.

summary(aov(G3 ~ schoolsup, data = d_total)) # p-value = ~1.0%, significant.

# Is there a difference between math and Portuguese?
summary(aov(G3 ~ schoolsup, data = d_math)) # p-value = 10%, not significant.
summary(aov(G3 ~ schoolsup, data = d_port)) # p-value = 9.1%, not significant.
# I'm confused. How can the p-value of d-total be lower than the p-values of the two datasets that make it up?

# FAMSUP: family educational support within the course subject (math or Portuguese)
ggplot(d_total, aes(x=famsup, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Family Educational Support") + ylab("Final Grade")
  
# No apparent correlations here, confirmed by one-way ANOVA test giving p-value = 66.7%.
summary(aov(G3 ~ famsup, data = d_total))

# PAID: extra paid classes within the course subject (math or Portuguese)
ggplot(d_total, aes(x=paid, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Extra Paid Classes") + ylab("Final Grade")
# A lot of math students pay for extra help! What percent? Compared to Portuguese students?
sum(d_math$paid == "yes") / nrow(d_math) # ~46%
sum(d_port$paid == "yes") / nrow(d_port) # ~6%

# Does paid help improve outcomes for math students?
# Of students who paid for math help, what percent failed?
nrow(d_math[d_math$G3 < 2 & d_math$paid == "yes", ]) / sum(d_math$paid == "yes") # ~4.4
# Of students who didn't pay for math help, what percent failed?
nrow(d_math[d_math$G3 < 2 & d_math$paid == "no", ]) / sum(d_math$paid == "no") # ~14%
# Yes, paid math help improves students' rate of passing math.
# One-way ANOVA analysis of impact of paid help shows it only helps for math.
summary(aov(G3 ~ paid, data = d_total)) # p-value = 11.1%, not significant.
summary(aov(G3 ~ paid, data = d_math)) # p-value = ~4.3%, significant.
summary(aov(G3 ~ paid, data = d_port)) # p-value = 16.2%, not significant.

# Correlations between final grade and the attributes NOT associated with course

# SCHOOL: student's school
ggplot(d_total, aes(x=school, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("School") + ylab("Final Grade")
# It looks like a lot more students fail math at GP than at MS
nrow(d_GP[d_GP$G3 < 2 & d_GP$course == "math", ]) / nrow(d_GP) # ~4.4%
nrow(d_MS[d_MS$G3 < 2 & d_MS$course == "math", ]) / nrow(d_MS) # ~1.5%
# GP has about 3 times the math failure rate of MS.
nrow(d_GP[d_GP$G3 < 2 & d_GP$course == "port", ]) / nrow(d_GP) # ~.26%
nrow(d_MS[d_MS$G3 < 2 & d_MS$course == "port", ]) / nrow(d_MS) # ~5.1%
# MS has a much higher rate of Portuguese failure than GP.

# Is there a significant relationship between school and final grade? Yes.
summary(aov(G3 ~ school, data = d_total)) # p-value = 3.81e-05, significant

# SEX: student's sex
ggplot(d_total, aes(x=sex, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Sex") + ylab("Final Grade")
# No apparent relationship, confirmed by one-way ANOVA test.
summary(aov(G3 ~ sex, data = d_total)) # p-value = 31%
# Do female and male students perform similarly in math and Portuguese classes? Yes.
summary(aov(G3 ~ sex, data = d_math)) # p-value = ~4.0%,  significant
summary(aov(G3 ~ sex, data = d_port)) # p-value = ~.10%, significant
# I don't understand this outcome. If sex is not a significant contributor to final grade in the whole dataset, 
# how can it be a significant contributor to final grades in the datasets that make up the whole dataset?

# Compare percent of abs_fail students who are female to abs_fail students who are male.
nrow(abs_fail[abs_fail$sex == "F", ]) / nrow(abs_fail) # ~55.6%
nrow(abs_fail[abs_fail$sex == "M", ]) / nrow(abs_fail) # ~44.4%
# Compare percent of highest_grades students who are female to highest_grades students who are male.
nrow(highest_grades[highest_grades$sex == "F", ]) / nrow(highest_grades) # ~54.3.6%
nrow(highest_grades[highest_grades$sex == "M", ]) / nrow(highest_grades) # ~44.7%
# A higher percentage of female students are among the most successful and the least successful.

# AGE: student's age
ggplot(d_total, aes(x=age, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Age") + ylab("Final Grade")
# No failures among students older than 19, though there aren't many of these. Is age significant? Yes.
summary(aov(G3 ~ age, data = d_total)) # p-value = 4.93e-05

# What age student is most likely to fail?
ggplot(abs_fail, aes(x = age)) +
  geom_histogram(aes(y = ..density.., fill = course), binwidth = 1, position = "dodge") +
  xlab("Age")
# 18-year-olds are most likely to fail.

# ADDRESS: student's home address type (rural or urban)
ggplot(d_total, aes(x=address, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Rural or Urban") + ylab("Final Grade")
# No obvious relationship, but one-way ANOVA test shows there is one.
summary(aov(G3 ~ address, data = d_total)) # p-value = ~.01%, significant

# Maybe a higher percentage of rural students fail?
nrow(d_total[d_total$address == "R" & d_total$G3 < 2, ]) / nrow(d_total[d_total$address == "R", ]) # ~7.0%
nrow(d_total[d_total$address == "U" & d_total$G3 < 2, ]) / nrow(d_total[d_total$address == "U", ]) # ~4.5%
# Yes, a higher percentage of rural students fail than urban students.
# Significant? No. P-value = 44.8%
summary(aov(G3 ~ address, data = abs_fail))
# Address is also not significant among students who receive the highest grades. P-value = 96.5%
summary(aov(G3 ~ address, data = highest_grades))

ggplot(d_total, aes(x=address, y=G3)) +
  geom_boxplot(fill = "grey80", color = "blue") +
  xlab("Rural or Urban") + ylab("Final Grade")
# Nothing obvious stands out in a boxplot, either.

# I do not know how to understand the signficance of address.

# FAMSIZE: family size
ggplot(d_total, aes(x=famsize, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Family Size") + ylab("Final Grade")
# No obvious relationship, but one-way ANOVA test shows significance.
summary(aov(G3 ~ famsize, data = d_total)) # p-value = 4.8%, significant.

ggplot(d_total, aes(x=famsize, y=G3)) +
  geom_boxplot(fill = "grey80", color = "blue") +
  xlab("Family Size") + ylab("Final Grade")

# I do not know how to understand the significance of family size.

# PSTATUS: parents' cohabitation status
ggplot(d_total, aes(x=Pstatus, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Parents' Cohabitation Status") + ylab("Final Grade")
# No obvious relationship, confirmed by one-way ANOVA test.
summary(aov(G3 ~ Pstatus, data = d_total)) # p-value = 32.2%, not significant.

# MEDU: mother's education
ggplot(d_total, aes(x=Medu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Mother's Education") + ylab("Final Grade")
# It appears students are more likely to be enrolled if their mother has a higher level of education.
# But is mother's education correlated with final grade? Yes!
summary(aov(G3 ~ Medu, data = d_total)) # p-value = 2.9e-10, significant.
# Maybe students whose mothers have less education are more likely to fail?
nrow(d_total[d_total$Medu <= 2 & d_total$G3 < 2, ]) / nrow(d_total[d_total$Medu <= 2, ]) # 6.6%
nrow(d_total[d_total$Medu >= 2 & d_total$G3 < 3, ]) / nrow(d_total[d_total$Medu >= 3, ]) # ~7.4%
# I HAVE A PROBLEM HERE. BECAUSE I CHANGED Medu TO A FACTOR, CAN'T USE <>=...
# No, in fact students whose mothers have more education are a bit more likely to fail.
# I don't really understand this ANOVA result.

# Does mother's level of education make more of a difference to female students?
ggplot(d_total, aes(x=Medu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  facet_grid(. ~ sex) +
  xlab("Mother's Education") + ylab("Final Grade")
# It appears that more female students whose mothers have little education are enrolled.
# How can I examine further?

ggplot(d_total, aes(x=Medu, y=G3)) +
  geom_boxplot(fill = "grey80", color = "blue") +
  xlab("Mother's Education") + ylab("Final Grade")

# FEDU: father's education
ggplot(d_total, aes(x=Fedu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Father's Education") + ylab("Final Grade")
# No obvious relationship, but one-way ANOVA test shows there is one.
summary(aov(G3 ~ Fedu, data = d_total)) # p-value = 2.67e-06
# Not sure how to explain this statistical result.

# MJOB: Mother's job

# FJOB: Father's job

# REASON: Reason for choosing this school

# GUARDIAN: Student's guardian

# TRAVELTIME: Home to school travel time

# ACTIVITIES: Extra-curricular activities

# NURSERY: Attended nursery school

# HIGHER: Wants to pursue higher education

# INTERNET: Internet access at home

# ROMANTIC: In a romantic relationship

# FAMREL: Quality of family relationships

# FREETIME: Free time after school

# GOOUT: Going out with friends

# DALC: Workday alcohol consumption

# WALC: Weekend alcohol consumption

# HEALTH: Current health status

# ABSENCES: Number of school absences


