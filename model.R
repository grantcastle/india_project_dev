# This R script includes statistical modeling code on:
# Model 1: OLS regression of total central government funding on BJP vote share and various controls.
# Model 2: OLS regression of total central government funding per GDP per capita on BJP vote share and controls.
# Model 3: OLS regression of BJP vote share on the proportion of the population that is Hindu with other controls.
# Model 4: OLS regression of funding/beneficiaries for six specific programs on BJP vote share and various controls.
# Models 5 and 6: Ridge and Lasso regression on total central government funding per GDP per capita.
# Model 7: OLS regression of total central government funding per GDP per capita on standardized predictors.

# Loading necessary libraries
library(tidyverse)
library(dplyr)
library(glmnet)

# Setting the working directory
setwd("/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets")

# Loading the primary indian_data dataset
india_data <- read.csv('india_data.csv')

### Model 1 ###
# Running an OLS regression of total central government funding on BJP vote share,
# with numerous controls for other factors.
total_funding_on_bjp_ols <- lm(total_funding ~ BJP_vote_share + population + pct_rural_pop + 
                                 rural_unemp + urban_unemp + tax_revenue + gdp + power_availability + 
                                 literacy_rate + mpi_score, data = india_data)

summary(total_funding_on_bjp_ols)

# Clearly, this is largely just correlated with population. We find that every 1-unit increase
# in population, all else constant, is associated with a 1.195e+04 increase in total central
# government funding (in rupees), which is highly statistically significant with
# the p-value is less than 0.001. However, this is not a particularly useful result
# since population levels will obviously dominate funding levels and crowd out any other
# possibly useful variables. Thus, we standardize funding levels per capita and re-run the
# regression. Note that while we also see statistical significance on the urban_unemployment
# variable, suggesting that every 1-unit increase in population, all else constant, is 
# associated with a  3.030e+10 increase in total central government funding (in rupees) (which
# is statistically significant at the alpha = 0.05 significance level), this could possibly do
# to random chance.

### Model 2 ###
# Running an OLS regression of total central government funding per capita on BJP vote share,
# with a few other controls for other factors.
funding_pgpc_bjp_ols <- lm(total_funding_pgpc ~ BJP_vote_share + gdp + pct_rural_pop +
                            rural_unemp + urban_unemp + tax_revenue + power_availability + 
                            literacy_rate + mpi_score, data = india_data)

summary(funding_pgpc_bjp_ols)

# With the total government funding standardized per capita, none of variables appear
# to be statistically significant, especially BJP vote share. We conclude that BJP vote is
# therefore unlikely to explain the total central government funding transfers to states.

### Model 3 ###
# Running a regression of BJP vote share on proportion of population that is Hindu, with
# numerous other controls
bjp_vote_on_hindu <- lm(BJP_vote_share ~ pct_hindus + pct_rural_pop + rural_unemp + 
                          urban_unemp + tax_revenue + power_availability + literacy_rate + 
                          mpi_score, data = india_data)

summary(bjp_vote_on_hindu)

# As expected, we get statistical significant results on pct_hindus. The model suggests
# that, all else constant, a 1 percentage point increase int eh proportion of a state's
# population that is Hindu is associated with a 0.274 percentage point increase in the
# BJP's vote share in that state, which is statistically significant at the alpha = 0.05
# significance level. 

### Model 4 ###
# Running OLS regression of funding/beneficiaries for six specific programs on
# BJP vote share and numerous other controls.

# Placing the names of each scheme in a vector
central_schemes <- c('jjm_funding_pgpc', 'mgnrega_funding_pgpc', 'pmayg_funding_pgpc', 'sbmg_funding_pgpc',
                     'pm_kisan_beneficiaries_pgpc', 'jdy_beneficiaries_pgpc')

# Setting up a list to store each regression model
regression_models <- list()

# Setting up a for loop to iterate through each of the dependent variables (schemes)
for (scheme in central_schemes) {
 
  # Setting up the formula used in each OLS model, unique for each scheme
  formula <- paste(scheme, "~ BJP_vote_share + gdp + pct_rural_pop + rural_unemp + urban_unemp + tax_revenue + power_availability + literacy_rate + mpi_score")
  
  # Running the OLS model
  model <- lm(formula, data = india_data)
  
  # Specifying the name of each model based on the scheme name
  model_name <- paste(scheme, "_model", sep = "")
  
  # Adding the regression model into the list of models
  regression_models[[model_name]] <- model
  
  # Printing the model's name and summary results
  print(model_name)
  print(summary(model))
  }

# We now run model 2 again, except as our outcome variable, we examine the total
# government funding allocated to states under six different specific schemes rather
# rather than the total central government funding. Broadly, we do not see statistically
# significant results on BJP vote share 

# For the JJM scheme model, none of the variables are statistically significant.

# For the MGNREGA scheme model, none of the variables are statistically significant.

# For the PMAY-G scheme model, none of the variables are statistically significant.

# For the SBM-G scheme model, none of the variables are statistically significant.

# For the PM-Kisan scheme model, pct_rural_pop and literacy rates are statistically
# significant. The model suggests that, all else constant, a 1 percentage point increase
# in the proportion of a state's population that is rural is associated with an increase
# of 0.003 individuals per capita who received benefits under the PM-Kisan scheme in 2021,
# which is statistically significant at the alpha = 0.05 significance level. The PM-Kisan
# scheme is targeted at poor farmers, so it does seem logical that states with larger
# rural populations (and hence, more farmers) would have more beneficiaries per capita
# under this program. The model also suggests that all else constant, a 1 percentage point
# increase in literacy rates is associated with an increase 0.006057 individuals per 
# capita who received benefits under the PM-Kisan scheme in 2021. This result is puzzling,
# as one would expect regions with lower literacy rates to receive more, not less, support.
# However, this result is only significant at the alpha = 0.1 significance level.

# For the JDY scheme model, BJP_vote_share and mpi_score are statistically significant. 
# The model suggests that, all else constant, a 1 percentage point increase
# in the BJP's 2019 vote share in that state is associated with an increase
# of 0.00429 individuals per capita who received benefits under the JDY scheme since its inception,
# which is statistically significant at the alpha = 0.05 significance level. The statistical
# and practical effects of this result are limited, but the JDY scheme was launched by and
# is closely associated with Prime Minister Modi, suggesting that he could indeed be redirecting
# its financial inclusion benefits toward BJP voters. The model also suggests that, all else
# constant, a 1 unit increase in MPI score in a state is associated with an increase
# of 2.122 individuals per capita who received benefits under the JDY scheme since its
# inception. This is what one might expect to see, since it implies that states with more
# intense poverty (higher MPI scores) receiver more benefits under an anti-poverty scheme.
# However, this is only statistically significant at the alpha = 0.05 level and is of limited
# practical significance, since MPI scores range from 0 to 1, so a 1-unit increase would
# be an unfathomable increase in poverty.


### Models 5 and 6 ###
# To further explore the results above with more advanced regession techniques, 
# we also run ridge and lasso regression models to evaluate the importance of the 
# potential predictors of central government funding. For simplicity, we will only 
# run these regressions on the total funding outcome variable, which is by far the 
# most practically significant. 

# Setting seed for reproducibility
set.seed(4832)

# Filtering the dataset to include only the variables which we will use in the ridge and lasso regressions. 
india_data_models <- india_data |>
  select(c(1, 11:18,25,29 ))

# Removing NA values to enable matrix production.
india_data_models <- india_data_models |>
  drop_na()

# Splitting the dataset into testing (20%) and training (80%) sets
training <- sample(1:nrow(india_data_models), 22)
india_data_models_train <- india_data_models[training, ]
india_data_models_test <- india_data_models[-training, ]

# Setting up the x and y matrices for test and training to enable ridge and lasso to function
india_data_train_x <- model.matrix(total_funding_pgpc ~ BJP_vote_share + gdp + pct_rural_pop + rural_unemp + urban_unemp +
                            tax_revenue + power_availability + literacy_rate + mpi_score, data = india_data_models_train)
india_data_train_y <- india_data_models_train$total_funding_pgpc

india_data_test_x <- model.matrix(total_funding_pgpc ~ BJP_vote_share + gdp + pct_rural_pop + rural_unemp + urban_unemp +
                                    tax_revenue + power_availability + literacy_rate + mpi_score, data = india_data_models_test)
india_data_test_y <- india_data_models_test$total_funding_pgpc

# Conducting a CV using Ridge regression using the training data to determine the best lambda value
india_ridge_cv <- cv.glmnet(india_data_train_x, india_data_train_y , alpha = 0)
india_best_lambda_ridge <- india_ridge_cv$lambda.min
print(india_best_lambda_ridge)

# Running the Ridge regression model using the training data and the best lambda 
india_ridge_model <- glmnet(india_data_train_x, india_data_train_y, alpha = 0, lambda = india_best_lambda_ridge)
coefficients(india_ridge_model)

# Assessing the Ridge model performance on the training data
india_ridge_prediction_train <- predict(india_ridge_model, s = india_best_lambda_ridge, newx = india_data_train_x)
india_ridge_training_mse <- mean((india_ridge_prediction_train - india_data_train_y)^2)
india_ridge_training_mse

# Assessing the Ridge model performance on the test data
india_ridge_prediction_test <- predict(india_ridge_model, s = india_best_lambda_ridge, newx = india_data_test_x)
india_ridge_test_mse <- mean((india_ridge_prediction_test - india_data_test_y)^2)
india_ridge_test_mse 

# The ridge model shrinks the gdp, tax_revenue, and power_availability variables to nearly
# zero, suggesting that these coefficients may have limited explanatory effects on the total
# funding levels, while the remaining variables (BJP_vote_share, pct_rural_pop, rural_unemp,
# urban_unemp, literacy_rate, and mpi_score) are not shrunk toward zero as significantly, 
# suggesting that they may have more explanatory power. This aligns with the regressions
# previously which showed that several of these variables (BJP_vote_share, pct_rural_pop, 
# literacy_rate, and mpi_score) were statistically significant on at least one of the
# specific funding schemes. However, the ridge model has an extremely large training MSE
# of 214575463 and an even larger test MSE of 2450970230, suggesting that the model
# may generally be performing poorly. 

# Conducting a CV using Lasso regression using the training data to determine the best lambda value
india_lasso_cv <- cv.glmnet(india_data_train_x, india_data_train_y , alpha = 1)
india_best_lambda_lasso <- india_lasso_cv$lambda.min
print(india_best_lambda_lasso)

# Running the Lasso regression model using the test data and the best lambda 
india_lasso_model <- glmnet(india_data_train_x, india_data_train_y, alpha = 1, lambda = india_best_lambda_lasso)
coefficients(india_lasso_model)

# Assessing the Lasso model performance on the training data
india_lasso_prediction_train <- predict(india_lasso_model, s = india_best_lambda_lasso, newx = india_data_train_x)
india_lasso_training_mse <- mean((india_lasso_prediction_train - india_data_train_y)^2)
india_lasso_training_mse

# Assessing the lasso model performance on the test data
india_lasso_prediction_test <- predict(india_lasso_model, s = india_best_lambda_lasso, newx = india_data_test_x)
india_lasso_test_mse <- mean((india_lasso_prediction_test - india_data_test_y)^2)
india_lasso_test_mse 

# The lasso model shrinks the coefficients (pct_rural_pop, rural_unemp, tax_revenue, 
# power_availability, and literacy_rate) to zerom, implying that these coefficients
# have little or no explanatory power on total funding levels per capita. GDP is shrunk
# to nearly zero, while BJP_vote_share, urban_unemp, and mpi_score are still fairly large
# coefficients, suggesting that they have some explanatory power. Despite some discrepancies,
# these results broadly align with previous models suggesting that these three variables may
# be of some significance. 

# However, like the ridge model, the lasso model has an extremely large training MSE
# of 219870438 and an even larger test MSE of 2434777829, suggesting that the model
# may generally be performing poorly. This is likely given the limited number of 
# observations, and thus these models are not effective at accurate estimation. However, 
# they still do help clarify  which coefficients are likely significant in explaining
# total funding per capita to Indian states.

### Model 7 ###
# One potential concern is that we are only getting statistical significance on some variables
# due to the range associated with he values for each predictor. MPI score is from 0 to 1,
# while several other variables (BJP_vote_share, pct_hindus, pct_rural_pop, rural_unemp,
# urban_unemp and literacy_rate) are percentages with a 0 to 100 range, while other
# variables are raw counts with large values (tax_revenue and power_availability). Thus,
# We will use scale on the variables and standardize them to all have a mean 0 and and 
# standard deviation of 1, and r-run our basic regression using the standardized variables. 

# Standardizing the coefficients of interest using the scale function. 
india_data_standardized <- scale(india_data[, c("BJP_vote_share", "gdp", "pct_hindus", "pct_rural_pop", "rural_unemp", 
                                                "urban_unemp", "tax_revenue", "power_availability", 
                                                "literacy_rate", "mpi_score")])

# Merging the standardized coefficients with the predicted variable, total_funding_pgpc
india_data_standardized_combined <- cbind(india_data_standardized, total_funding_pgpc = india_data$total_funding_pgpc)

# Transforming the combined, standardized data into  a dataframe
india_data_standardized_combined <- as.data.frame(india_data_standardized_combined)

# Running an OLS model of total_funding_pgpc on numerous predictors with the standardized data
std_funding_pgpc_ols <- lm(total_funding_pgpc ~ ., data = india_data_standardized_combined)

# Get the summary of the model
summary(std_funding_pgpc_ols)

# Using scaled variables, we are getting statistically significant results on
# pct_hindus and mpi_score. The model suggests that, all else constant, a 1-unit (scaled) 
# increase in the proportion of a state's population that is Hindu is associated with a 
# large decrease of 18,801 rupees per capita in funding which that state receives from
# the central government. This runs counter to our hypothesis, which is that the BJP
# central government would provide more, not less, funding to their political supporters,
# who tend to be Hindu. This is statistically significant at the alpha = 0.05 significance level.

# The model also suggests that, all else constant, a 1-unit (scaled)
# increase in MPI score is associated with a large decrease of 13,133.6 rupees per capita
# in funding which that state receives from the central government. This also runs
# counter to common sense, since one would expect that regions with more intense poverty
# would receive more funding from the central government, not less. This is statistically
# significant at the alpha = 0.1 significance level.

# Both of these results run counter to intuition and to previous regression results,
# and given the low statistical significance, it is possible that they are both 
# statistical flukes. 

### Conclusions ###
# Broadly, these results suggest that BJP vote share does not explain variations in
# central government funding allocations to states in India, which is a positive sign 
# that the BJP is not rewarding political supporters. However, these results fail to 
# explain what else may be influencing variations in funding, One would expect that
# development / economic indicators (MPI, GDP, unemployment, etc.) would be correlated
# with government funding, but we lack consistent statistically significant evidence
# to make such claims. Therefore, it is unclear what explains variations in funding,
# but it is possible that political or personal dynamics (other than state-wise vote share)
# could still play a role, since basic development indicators do not explain the variations.
