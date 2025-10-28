# Exam1
Bamba Cisse
2025-10-27


library(dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)


# Load data
load("d_HHP2020_24.Rdata")


summary(d_HHP2020_24)

# Create a binary variable for feeling down (already created as 'high_down')

d_HHP2020_24$high_down <- as.numeric(d_HHP2020_24$DOWN > 2)

# 1. Boxplot of Age by Down Indicator
ggplot(d_HHP2020_24, aes(x=as.factor(high_down), y=Age)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Not Feeling Down", "Feeling Down")) +
  labs(x="Feeling Down", y="Age", title="Age Distribution by Feeling Down") 

##The boxplot reveals how ages are distributed among individuals who report feeling down versus those who do not. For example, if the median age for feeling down is lower than for not feeling down, it is saying that younger individuals are more prone to feeling down.
the question that i am concerned about:are younger age groups significantly more likely to report feeling down?
  
# 2. Bar Plot of Education Levels vs. Feeling Down
ggplot(d_HHP2020_24, aes(x=Education, fill=as.factor(high_down))) +
  geom_bar(position="fill") +
  scale_fill_discrete(name="Feeling Down", labels=c("No", "Yes")) +
  labs(x="Education Level", y="Proportion", title="Feeling Down by Education Level") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

##The plot shows the proportion of people feeling down within each education level. If the proportion is higher in less-educated groups, it is pointing education as a protective factor against feeling down or mental health issues.
the question that i am concerned about:Are there specific education levels where feeling down is particularly prevalent?
  
# 3. Gender comparison in Feeling Down (for subgroup analysis)
ggplot(d_HHP2020_24, aes(x=Gender, fill=as.factor(high_down))) +
  geom_bar(position="fill") +
  scale_fill_discrete(name="Feeling Down", labels=c("No", "Yes")) +
  labs(x="Gender", y="Proportion", title="Feeling Down by Gender")

##The graph displays differences in feelings of downness between males and females.
the question that i am concerned about:What social or biological factors might explain gender differences?

                                                                                                                                                                                                       
#Question4:
Subgroup Selection:
  I will analyze the subgroup of individuals based on their education level, as education often influences mental health outcomes and access to resources. Specifically, I will compare individuals with "less than high school" education to those with "college degree or higher."

Limiting and Data Preparation:
  To focus on this subgroup, I will filter the dataset to include only individuals in these two categories of education. This limits the analysis to relevant groups and reduces variability that might confound the results.
This subgroup analysis helps identify whether educational attainment influences feelings of being down, which may inform targeted mental health interventions or policy decisions  


#Question5:  

subset_data <- d_HHP2020_24 %>%
  filter(Education %in% c("lt hs", "college grad", "adv degree"))

summary(subset_data)

# Recoding education into two groups: less than high school and college or higher
d_HHP2020_24 <- d_HHP2020_24 %>%
  mutate(Edu_Group = ifelse(Education == "lt hs", "Less than high school", "College or higher"))

# Create contingency table
contingency_tbl <- table(d_HHP2020_24$Edu_Group, d_HHP2020_24$high_down)
print(contingency_tbl)

# Chi-squared test
chi_result <- chisq.test(contingency_tbl)
print(chi_result)

# Extract counts
n1 <- contingency_tbl[1, 2]   # Feeling down in less than high school group
n2 <- contingency_tbl[2, 2]   # Feeling down in college or higher group
N1 <- sum(contingency_tbl[1, ]) # Total in less than high school group
N2 <- sum(contingency_tbl[2, ]) # Total in college or higher group

# Proportions
p1 <- n1 / N1
p2 <- n2 / N2

# Difference in proportions
diff <- p1 - p2

# Standard error
se_diff <- sqrt( (p1*(1 - p1))/N1 + (p2*(1 - p2))/N2 )


# 95% confidence interval
z <- qnorm(0.975)
lower_ci <- diff - z * se_diff
upper_ci <- diff + z * se_diff

# Results
cat("Difference in proportions:", diff, "\n")
cat("95% Confidence interval:", lower_ci, "to", upper_ci, "\n")



**Confidence Interval: 95% , Critical Value: 1.96 , StandardError: 0.0048 ,
Point Estimate - Diff of Proportion: 0.68 , [0.045, 0.091]  , p-val: 0"
Interpretation of the Hypothesis Test:

**The null hypothesis states that there is no difference in the likelihood 
of feeling down between individuals with less than a high school education 
and those with a college degree or higher. The alternative hypothesis posits 
that a difference does exist. The chi-squared test produced a p-value of 
approximately 0, indicating strong evidence against the null hypothesis. 
The calculated difference in proportions is approximately 0.68, 
with a 95% confidence interval ranging from 0.045 to 0.091. 
This means we are 95% confident that the true difference in feeling down 
between the two education groups lies within this interval, 
with the less-educated group being significantly more likely to report feeling 
down. Overall, these results suggest that education level plays a significant 
role in influencing feelings of downness, with lower education associated with 
higher likelihood.


#Question6:

# Let's create polynomial terms for Age
d_HHP2020_24$Age_poly2 <- poly(d_HHP2020_24$Age, 2, raw=TRUE)[,2]


##Fit the linear model

str(d_HHP2020_24)

lm_model <- lm(high_down ~ Age + Age_poly2 + income_midpoint + Gender, data=d_HHP2020_24)

summary(lm_model)

plot(lm_model)

