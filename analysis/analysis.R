#######################
#### Load packages ####
#######################
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(MASS)
library(table1)
library(knitr)
library(table1)
library(kableExtra)
library(lmtest)
library(AICcmodavg)
library(car)
library(mice)
library(ggplot2)
library(jtools)
library(interactions)
install.packages("mice")
install.packages("AICcmodavg")
install.packages("interactions")

####################
#### Load data ####
###################

final_data <- read.csv("../data/netflix_data.csv")

final_data$k_4 <- as.factor(final_data$k_4)
final_data$k_5 <- as.factor(final_data$k_5)
final_data$k_6 <- as.factor(final_data$k_6)
final_data$Film <- as.factor(final_data$Film)
final_data$Netflix.Original.dummy. <- as.factor(final_data$Netflix.Original.dummy.)
summary(final_data)

scene_vol <- read.csv("../data/scene_volatility.csv")

#########################
#### Merge two data ####
########################

final_data <- final_data %>%
  inner_join(scene_vol, by = "Subtitles")


##########################
#### Data Exploration ####
##########################

# Solve log problem with Star.Power = 0 before taking log transformation 
final_data <- final_data %>%
  mutate(Star.Power = ifelse(Star.Power == 0, Star.Power+1, Star.Power))

final_data <- final_data %>%
  mutate(Budget = ifelse(Budget == 0, NA, Budget))

# Handling missing budget values
## Check MCAR, MAR
subset <- final_data %>%
  dplyr::select(k_6, Film, Rank, n_days, Critic.Rating, Critic.Votes,
                User.Votes, User.Rating, Budget, Star.Power, Runtime, scene_volatility)

x <- as.data.frame(abs(is.na(subset)))
y <- x[which(sapply(x, sd) > 0)]
cor(y, use="pairwise.complete.obs")

## Imputation
subset_2 <- subset

sapply(subset_2, function(x) sum(is.na(x)))

set.seed(1234)
init = mice(subset_2, maxit=0) 
meth = init$method
predM = init$predictorMatrix

### Run regression 5 times and create final imputed dataset
imputed = mice(subset_2, method="cart", predictorMatrix=predM, m=5)
imputed_data <- complete(imputed)

levels(imputed_data$k_6) <- c("Rags to Riches", "Cinderella",
                              "Icarus", "Man in a Hole",
                              "Oedipus", "Tragedy")
  
### Quick check our proposed model using imputed data
model_imputed <- lm(log(n_days) ~ as.factor(k_6)*log(scene_volatility) 
                    + log(User.Rating)
                    + log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
                    + log(Star.Power) + log(Budget) + log(Runtime), 
                    data=imputed_data)

summary(model_imputed)


# Summary statistics by group
## Total observations
summary_0 <- table1::table1(
  ~ n_days + k_6 + scene_volatility 
  + User.Rating
  + User.Votes + Critic.Votes + Critic.Rating
  + Star.Power + Budget + Runtime + Film,
  data = imputed_data)


t1kable(summary_0) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    font_size = 14
  )

## By emotional trajectories

summary_1 <- table1::table1(
  ~ n_days + k_6 + scene_volatility 
  + User.Rating
  + User.Votes + Critic.Votes + Critic.Rating
  + Star.Power + Budget + Runtime + Film,
  data = imputed_data[imputed_data$k_6==1,])


t1kable(summary_1) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    font_size = 14
  )

summary_2 <- table1::table1(
  ~ n_days + k_6 + scene_volatility 
  + User.Rating
  + User.Votes + Critic.Votes + Critic.Rating
  + Star.Power + Budget + Runtime + Film,
  data = imputed_data[imputed_data$k_6==2,])


t1kable(summary_2) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    font_size = 14
  )

summary_3 <- table1::table1(
  ~ n_days + k_6 + scene_volatility 
  + User.Rating
  + User.Votes + Critic.Votes + Critic.Rating
  + Star.Power + Budget + Runtime + Film,
  data = imputed_data[imputed_data$k_6==3,])


t1kable(summary_3) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    font_size = 14
  )

summary_4 <- table1::table1(
  ~ n_days + k_6 + scene_volatility 
  + User.Rating
  + User.Votes + Critic.Votes + Critic.Rating
  + Star.Power + Budget + Runtime + Film,
  data = imputed_data[imputed_data$k_6==4,])


t1kable(summary_4) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    font_size = 14
  )


summary_5 <- table1::table1(
  ~ n_days + k_6 + scene_volatility 
  + User.Rating
  + User.Votes + Critic.Votes + Critic.Rating
  + Star.Power + Budget + Runtime + Film,
  data = imputed_data[imputed_data$k_6==5,])


t1kable(summary_5) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    font_size = 14
  )


summary_6 <- table1::table1(
  ~ n_days + k_6 + scene_volatility 
  + User.Rating
  + User.Votes + Critic.Votes + Critic.Rating
  + Star.Power + Budget + Runtime + Film,
  data = imputed_data[imputed_data$k_6==6,])


t1kable(summary_6) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    font_size = 14
  )

# Correlation table
data_numeric <- imputed_data %>%
  select(n_days, scene_volatility, Critic.Rating:User.Votes, Star.Power:Runtime)

cor_results <- round(cor(data_numeric, method = "pearson", use = "complete.obs"), 2)

upper <- cor_results
upper[upper.tri(cor_results)] <- ""
upper <- as.data.frame(upper)
upper

##########################
#### Data Analysis #######
##########################

# Assumption checks

## Construct a model without interaction term for VIF
model_0 <- lm(log(n_days) ~ as.factor(k_6) + log(scene_volatility) 
              + log(User.Rating)
              + log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
              + log(Star.Power) + log(Budget) + log(Runtime), data=imputed_data)

## Multicollinearity
vif(model_0)

## Fitting full model
model_1 <- lm(log(n_days) ~ as.factor(k_6)*log(scene_volatility) 
              + log(User.Rating)
              + log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
              + log(Star.Power) + log(Budget) + log(Runtime), data=imputed_data)

summary(model_1)

## Homoscedasticity and autocorrelation
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model_1)

lmtest::bptest(model_1) #### Homoscedasticity
lmtest::dwtest(model_1) #### autocorrelation


## Compare with 4,5 clusters (No longer needed)
#model_k5 <- lm(log(n_days) ~ as.factor(k_5)*log(scene_volatility) 
              #+ log(User.Rating)
              #+ log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
              #+ log(Star.Power) + log(Budget) + log(Runtime), data=final_data)

#summary(model_k5)

#model_k4 <- lm(log(n_days) ~ as.factor(k_4)*log(scene_volatility) 
               #+ log(User.Rating)
               #+ log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
               #+ log(Star.Power) + log(Budget) + log(Runtime), data=final_data)

#summary(model_k4)

### Calculate RMSE
sqrt(mean(model_1$residuals^2))
#sqrt(mean(model_k5$residuals^2))
#sqrt(mean(model_k4$residuals^2))

# Fit emotional trajectories only
model_k6_only <- lm(log(n_days) ~ as.factor(k_6), data=imputed_data)

summary(model_k6_only)
sqrt(mean(model_k6_only$residuals^2))

# Fit emotional trajectories with scene volatility
model_interact <- lm(log(n_days) ~ as.factor(k_6)*log(scene_volatility), data=imputed_data)

summary(model_interact)
sqrt(mean(model_interact$residuals^2))

# Drop scene volatility
model_2 <- lm(log(n_days) ~ as.factor(k_6) 
              + log(User.Rating)
              + log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
              + log(Star.Power) + log(Budget), data=imputed_data)

summary(model_2)


# Tune the model
## Stepwise regression model

step_model <- stepAIC(model_1, direction = "both", 
                      trace = F)
summary(step_model)

sqrt(mean(step_model$residuals^2))

# Compute AIC scores

models <- list(model_k6_only, model_interact, model_1, step_model)
mod.names <- c("model_k6_only", "model_interact", "model_1", "step_model")
aictab(cand.set = models, modnames = mod.names)

######################
##Robustness checks###
######################

# Check 1: Missing budget values
## Create 3 models assessing whether missing budget values influence our estimation
## Drop missing budget
drop_budget_na <- na.omit(final_data)
drop_budget_na %>%
  group_by(k_6) %>%
  count()
model_without_na <- lm(log(n_days) ~ as.factor(k_6)*log(scene_volatility) 
              + log(User.Rating)
              + log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
              + log(Star.Power) + log(Budget) + log(Runtime), data=drop_budget_na)

summary(model_without_na)

## Model without the budget
model_without_budget <- lm(log(n_days) ~ as.factor(k_6)*log(scene_volatility) 
                       + log(User.Rating)
                       + log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
                       + log(Star.Power) + log(Runtime), data=imputed_data)

summary(model_without_budget)

## Model with budget after imputation see model 1


# Check 2: Detect impact of influential observations
# Outliers
qqPlot(model_1,labels=row.names(imputed_data), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

cooksd <- cooks.distance(model_1)

cutoff <- 4/(nrow(imputed_data))
plot(model_1, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

## Leverage points
highleverage <- function(fit, data = data) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  ratio <-p/n
  cooksd <- cooks.distance(fit)
  sample_size <- nrow(data)
  influential <- cooksd[(cooksd > (4/sample_size))]
  names_of_influential <- names(influential)
  plot(hatvalues(fit), main="Index Plot of Ratio")
  abline(h=c(2,3)*ratio, col="red", lty=2)
  identify(1:n, hatvalues(fit), names_of_influential)
}
highleverage(model_1, imputed_data)

## Filter influential points
sample_size <- nrow(imputed_data)

influential <- cooksd[(cooksd > (4/sample_size))]
influential

names_of_influential <- names(influential)
outliers <- imputed_data[names_of_influential,]
data_without_infl <- imputed_data %>% anti_join(outliers)
model_without_infl <- lm(log(n_days) ~ as.factor(k_6)*log(scene_volatility) 
              + log(User.Rating)
              + log(User.Votes) + log(Critic.Votes) + log(Critic.Rating)
              + log(Star.Power) + log(Budget) + log(Runtime), 
              data=data_without_infl)

### Model without the influential points
summary(model_without_infl)

### Comparison between model_1 and model_without_influentials
par(mfrow = c(2, 2))
plot(model_1)
plot(model_without_infl)


##############################
## Inspect scene volatility ##
##############################

model_vol_only <- lm(log(n_days) ~ log(scene_volatility), data=imputed_data)
summary(model_vol_only)

vol_ratings <- lm(User.Rating ~ poly(scene_volatility, 2, raw=TRUE), data=imputed_data)
summary(vol_ratings)

ggplot(imputed_data, aes(scene_volatility, User.Rating)) + geom_point() + 
  geom_smooth(method="lm", formula=y~poly(x, 2, raw=TRUE))

######################
## Interaction plot ##
######################
summary(model_1)

interact_plot(model_1, pred = scene_volatility, modx = k_6, data = imputed_data,
              x.label = "Scene volatility", y.label = "Number of days on Netflix Top 10 Global",
              legend.main = "Six emotional trajectories") +
  theme_apa() +
  theme(text=element_text(size=16,  family="Times New Roman"))







