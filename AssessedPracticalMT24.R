# We import necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(MuMIn)


# We load the data
dvis = read.csv("/Users/Giulio/Library/Mobile Documents/com~apple~CloudDocs/Oxford/
                Courses/Michaelmas Term/Applied Statistics/Practicals/Practical Week 8/
                dvis.csv")   
options(digits = 4) 




##############################################
#### SECTION 2: EXPLORATORY DATA ANALYSIS ####
##############################################

### Section 2.1: Univariate Analysis ###
# This is the data in Table 1: Distributions of binary variables
dvis %>% 
  summarise(
    female_percentage = mean(female, na.rm = TRUE) * 100,
    individuals_with_kids_under_16_percentage = mean(hhkids, na.rm = TRUE) * 100,
    married_percentage = mean(married, na.rm = TRUE) * 100,
    employed_percentage = mean(employed, na.rm = TRUE) * 100,
    private_insurance_percentage = mean(privateins, na.rm = TRUE) * 100,
    additional_health_insurance_percentage = mean(addins, na.rm = TRUE) * 100
  ) %>% print()


# This is the data in Table 2: Summary statistics of non-binary variables
print(summary(dvis$age))
sd(dvis$age)
print(summary(dvis$hhninc*1000))
sd(dvis$hhninc*1000)
print(summary(dvis$educyrs))
sd(dvis$educyrs)
print(summary(dvis$eductype))
sd(dvis$eductype)
print(summary(dvis$docvis))
sd(dvis$docvis)


# This creates Figure 1: Distributions of non-binary variables
# Age, panel (a)
ggplot(dvis, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Age", x = "Age", y = "Frequency") + theme_bw()
# Household Income * 1000, panel (b)
ggplot(dvis, aes(x = hhninc*1000)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Household Income", x = "Household Income", 
       y = "Frequency") + theme_bw()
# Years of Education, panel (c)
ggplot(dvis, aes(x = educyrs)) + 
  geom_histogram(bins = 19, fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Years of Education", x = "Years of Education", 
       y = "Frequency") + theme_bw()
# Type of Education, panel (d)
ggplot(dvis, aes(x = eductype)) + 
  geom_histogram(bins = 6, fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Type of Education", x = "Type of Education", 
       y = "Frequency") + theme_bw()
# GP visits, panel (e)
ggplot(dvis, aes(x = docvis)) + 
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Number of GP Visits", x = "Number of GP Visits", 
       y = "Frequency") + theme_bw()



### Section 2.2: Univariate Analysis ###

# This creates Figure 2: Correlation matrix
cor_matrix = cor(dvis, use = "complete.obs", method = "pearson")
corrplot(cor_matrix, method = "color", addCoef.col = "black",
         number.cex = 0.5, tl.col = "black", tl.srt = 45)   


# Here we create the "age_group" variable
dvis$age_group = cut(
  dvis$age,
  breaks = c(25, 34, 44, 54, 64), # Define the age range intervals
  labels = c("25-34", "35-44", "45-54", "55-64"), # Assign labels to the intervals
  include.lowest = TRUE)


# Here we create the "income_group" variable
dvis$income_group = cut(
  dvis$hhninc,
  breaks = c(0.05, 2.399, 3.299, 3.529, 4.199, 9.99, 15.00), 
  labels = c("0.05-2.39", "2.40-3.29", "3.30-3.52", "3.53-4.19", "4.20-9.99", 
             "10-15.00"), include.lowest = TRUE)


# This creates Figure 3: Histograms of number of GP visits grouped by the values 
# of each predictor
# Boxplots of GP visits by age group
ggplot(dvis, aes(x = age_group, y = docvis)) +
  geom_boxplot(fill = "lightblue") + labs(
    title = "Doctor Visits by Age Group",
    x = "Age Group",
    y = "Number of Visits") + theme_classic()
# Boxplots of GP visits by income group
ggplot(dvis, aes(x = income_group, y = docvis)) +
  geom_boxplot(fill = "lightgoldenrod") +
  labs(
    title = "Doctor Visits by Income Group",
    x = "Income Group",
    y = "Number of Visits") + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Boxplots of GP visits by gender
ggplot(dvis, aes(x = factor(female), y = docvis)) + 
  geom_boxplot(fill = c("pink")) + 
  labs(
    title = "Doctor Visits by Gender", 
    x = "Gender", 
    y = "Number of Visits") + theme_classic() + 
  scale_x_discrete(labels = c("Male", "Female"))
# Boxplots of GP visits by education type
ggplot(dvis, aes(x = factor(eductype), y = docvis)) + 
  geom_boxplot(fill = c("lightseagreen")) + 
  labs(
    title = "Doctor Visits by Education Type", 
    x = "Education Type", 
    y = "Number of Visits") + theme_classic() + 
  scale_x_discrete(labels = c("None", "Lower sec educ", "Sec prac educ", 
                              "Prof higher educ", "A-levels", "University"))  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Boxplots of GP visits by presence of kids under 16
ggplot(dvis, aes(x = factor(hhkids), y = docvis)) + 
  geom_boxplot(fill = c("lightcoral")) + 
  labs(
    title = "Doctor Visits by Presence of Kids under 16", 
    x = "Kids under 16 in Household", 
    y = "Number of Visits") + theme_classic() + 
  scale_x_discrete(labels = c("Absence", "Presence"))
# Boxplots of GP visits by marital status
ggplot(dvis, aes(x = factor(married), y = docvis)) + 
  geom_boxplot(fill = c("lightcyan")) +  
  labs(
    title = "Doctor Visits by Marital Status", 
    x = "Marital Status", 
    y = "Number of Visits") + theme_classic() + 
  scale_x_discrete(labels = c("Not married", "Married"))
# Boxplots of GP visits by employment status
ggplot(dvis, aes(x = factor(employed), y = docvis)) + 
  geom_boxplot(fill = c("lightgreen")) + 
  labs(
    title = "Doctor Visits by Employment Status", 
    x = "Employment Status", 
    y = "Number of Visits") + theme_classic() + 
  scale_x_discrete(labels = c("Unemployed", "Employed"))
# Boxplots of GP visits by presence of private insurance
ggplot(dvis, aes(x = factor(privateins), y = docvis)) + 
  geom_boxplot(fill = c("lightyellow")) + 
  labs(
    title = "Doctor Visits by Private Insurance", 
    x = "Private insurance", 
    y = "Number of Visits") + theme_classic() + 
  scale_x_discrete(labels = c("Absence", "Presence"))
# Boxplot of GP visits by presence of additional insurance
ggplot(dvis, aes(x = factor(addins), y = docvis)) + 
  geom_boxplot(fill = c("lightsalmon")) + 
  labs(
    title = "Doctor Visits by Additional Insurance", 
    x = "Additional Insurance", 
    y = "Number of Visits") + theme_classic() + 
  scale_x_discrete(labels = c("Absence", "Presence"))



### Section 2.3: Feature Engineering ###
# Here we create the varaibles "hhninc_per_capita" and "single_parent", and we add 
# them to our dataset
dvis$hhninc_per_capita=dvis$hhninc/(1+dvis$married+dvis$hhkids)
dvis$single_parent=dvis$single_parent=ifelse(dvis$married==0&dvis$hhkids==1,1,0)




##############################
#### SECTION 3: MODELLING ####
##############################

### Section 3.1: First Model: Full Poisson GLM ###
# Here we fit the full Poisson GLM
# We exclude the columns hhninc, educyrs, age_group, and income_group, as stated 
# in the report
full_model = glm(docvis ~ female + age + hhninc_per_capita + hhkids + married + 
               employed + addins + eductype + privateins + single_parent + 
               female * (age + hhninc_per_capita + hhkids + married + employed + 
               addins + eductype + privateins + single_parent),
             family = poisson, data = dvis)


# This is the data used in Table 3: Summary of full model
summary(full_model)



### Section 3.2: Second Model: Manual Variable Selection ###
# We test the difference in deviance between an intercept-only model and a 
# one-variable model. 
# In turn, we try every predictor as the one in the one-variable model        
# This creates the data in Table 4: Analysis of deviance between null model and 
# one-variable models (NOT typical analysis-of-deviance table)
variables = c("female", "age", "hhninc_per_capita", "hhkids", "married", "employed", 
              "addins", "eductype", "privateins", "single_parent")
for (variable in variables) {
  formula = as.formula(paste("docvis ~", variable))
  model = glm(formula, family = poisson, data = dvis)
  print(anova(model, test = "Chisq"))
  print("----------------------")
}

# This is our second model
second_model = glm(docvis ~ female + age + hhninc_per_capita + hhkids + employed + 
                   addins + eductype + privateins + female * (age + hhninc_per_capita + 
                   hhkids + employed + addins + eductype + privateins),
                 family = poisson, data = dvis)
summary(second_model)



### Section 3.3: Third Model: Automatic Variable Selection with AIC

# Here we compute the AIC for all possible subsets of variables (2^15 - 1 models)
options(na.action = "na.fail")

# This is used to create Table 5: AIC-minimising models 
# (in Table 5, rows and columns are swapped)
all_models = as.data.frame(dredge(second_model, trace = TRUE))
# These are the two models with minimum AICc
all_models[all_models$delta==min(all_models$delta), ] 


# We therefore arrive at this model
third_model = glm(docvis ~ female + age + hhninc_per_capita + hhkids + addins + 
                  eductype + female*age,
                family = poisson, data = dvis)
summary(third_model)




#####################################
#### SECTION 4: MODEL EVALUATION ####
#####################################

### Section 4.1: Leverage and Influence
# This part of the analysis is necessary to arrive at our final model
# We define the "noteworthy" variable, thus identifying the "noteworthy" observations
leverage = hatvalues(third_model)
cooks_dist = cooks.distance(third_model)
p = length(coef(third_model))
n = nrow(dvis)
leverage_threshold = (2 * p) / n
cooks_dist_threshold = 8 / (n - 2 * p)
dvis$noteworthy=as.integer(leverage>leverage_threshold&cooks_dist>cooks_dist_threshold)
sum(dvis$noteworthy) / n # 17/1204 = 1.412% of observations are noteworthy
16 / sum(dvis$addins) # 53% of people with additional insurance are "noteworthy"


# We perform another global search to minimise AIC, this time considering interactions 
# with addins
# The code below is used to get the data in Table 6: AIC-minimising model, now 
# considering interactions with addins
global_model = glm(docvis ~ female + age + hhninc_per_capita + hhkids + addins + 
                   eductype + female:age + addins * (female + age + hhninc_per_capita + 
                   hhkids + eductype), 
  family = poisson, data = dvis)

options(na.action = "na.fail")
all_models2 = dredge(global_model, trace = TRUE)

# We arrive at our final model
final_model = glm(docvis ~ female + age + hhninc_per_capita + hhkids + addins + 
                  eductype + female:age + addins * (age + hhninc_per_capita), 
  family = poisson, data = dvis)
summary(final_model)



### Section 4.2: Comparison with Full Model
# Here we perform the LRT of full_model VS final_model
lrt_stat = 2*(logLik(full_model) - logLik(final_model))
lrt_df = length(coef(full_model)) - length(coef(final_model))
# p-value of the test
pchisq(lrt_stat, df = lrt_df, lower.tail = FALSE)
# This is equivalent to:
anova(final_model, full_model, test = "Chisq")     



### Section 4.3: Deviance Goodness-of-Fit Test
# p-value of the test
pchisq(final_model$deviance, df = final_model$df.residual, lower.tail = FALSE)



### Section 4.4: Deviance-Based R-squared
1 - final_model$deviance/final_model$null.deviance



### Section 4.5: Analysis of Deviance Residuals

deviance_residuals = residuals(final_model, type = "deviance")
h_values = hatvalues(final_model)
standardised_deviance_residuals = deviance_residuals / sqrt(1 - h_values)
different_colours = as.factor(dvis$docvis)


# This creates Figure 4: Plot of standardised deviance residuals against fitted values
plot(fitted(final_model), standardised_deviance_residuals,
     col = different_colours,
     xlab = "Fitted Values",
     ylab = "Standardised Deviance Residuals",
     main = "Std Deviance Residuals vs Fitted Values")
legend("topright", legend = levels(different_colours), 
       col = seq_along(levels(different_colours)), 
       pch = 1, cex = 0.5, title = "GP visits", xpd = TRUE, inset = c(-0.13, 0))


# This is the data for Table 7: Summary statistics of standardised deviance residuals
summary(standardised_deviance_residuals)
sd(standardised_deviance_residuals)
cor(standardised_deviance_residuals, fitted(final_model))


# This creates Figure 5: N(0,1) Q-Q plot of standardised deviance residuals, 
# with reference line
qqnorm(standardised_deviance_residuals, 
       main = "Normal Q-Q Plot of Std Deviance Residuals",
       xlab = "Theoretical Quantiles",
       ylab = "Standardised Deviance Residuals")
qqline(standardised_deviance_residuals, col = "red")

# Almost 10% of the standardised deviance residuals are outside the [-2,2] range
sum(standardised_deviance_residuals > 2 | standardised_deviance_residuals < -2) / n



# Section 4.6: Estimation of Out-of-Sample Error
set.seed(1)
# We split the available data between a train and a test set
train_index = sample(seq_len(nrow(dvis)), size = 0.8 * nrow(dvis))
train_set = dvis[train_index, ]
test_set = dvis[-train_index, ]
# We fit a model on the train set
fitted_model = glm(docvis ~ female + age + hhninc_per_capita + hhkids + addins + 
                   eductype + female*age + addins * (age + hhninc_per_capita), 
    family = poisson, data = train_set)
# We use it to predict docvis for the test set
predictions = predict(fitted_model, newdata = test_set, type = "response")

# This data is needed for Table 8: Summary statistics of out-of-sample absolute errors
absolute_errors = abs(test_set$docvis - predictions)  
summary(absolute_errors)
sd(absolute_errors)

# This creates Figure 6: Histogram and density plot of out-of-sample absolute errors
ggplot(data.frame(values = absolute_errors), aes(x = values)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightcoral", color = "black", 
                 alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  labs(
    title = "Histogram and Density of Absolute Errors", 
    x = "Absolute Errors", 
    y = "Density") + theme_classic()




########################################################
#### SECTION 6: ESTIMATING THE DISPERSION PARAMETER ####
########################################################
p = length(coef(final_model))
y = dvis$docvis
mu_hat = fitted(model)
phi = 1/(n-p) * sum((y - mu_hat)^2 / mu_hat)
# This is out estimate for the dispersion parameter phi
# This is used to compute the data in Table 9: Recomputed results for our final model, 
# with dispersion parameter of 2.073
phi
