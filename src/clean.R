#######################
#### Load packages ####
#######################
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)

####################
#### Load data ####
###################
netflix_data <- read_xlsx("../data/All_movies.xlsx")
movies <- read_xlsx("../data/Movies_Data.xlsx")

netflix_raw <- netflix_data

########################
#### Data wrangling ####
########################
netflix_data <- netflix_data %>%
  group_by(Title) %>%
  filter(Date==max(Date)) %>%
  mutate(n_days = Accu_weeks*7)

final_data <- netflix_data %>%
  select(Film, Rank, Title, n_days) %>%
  inner_join(movies, by = "Title") %>%
  distinct(ID, .keep_all = TRUE)

final_data <- final_data %>%
  filter(!is.na(Subtitles))

write.csv(final_data,"../data/final_data.csv", row.names = FALSE)


################################
#### Import cluster results ####
################################

# Load cluster data
clusters <- read.csv("../data/clusters.csv")
clusters_5 <- read.csv("../data/clusters_5.csv")
clusters_4 <- read.csv("../data/clusters_4.csv")

# Load final movie data
final_data <- read.csv("../data/final_data.csv")

# Merge movie data with the clusters
final_data <- clusters %>%
  select(k_6 = predicted_cluster, Subtitles) %>%
  mutate(k_6 = k_6+1) %>%
  inner_join(final_data, by="Subtitles")

final_data <- clusters_5 %>%
  select(k_5 = predicted_cluster, Subtitles) %>%
  mutate(k_5 = k_5+1) %>%
  inner_join(final_data, by="Subtitles")


final_data <- clusters_4 %>%
  select(k_4 = predicted_cluster, Subtitles) %>%
  mutate(k_4 = k_4+1) %>%
  inner_join(final_data, by="Subtitles")

##########################
#### Data wrangling 2 ####
##########################

# Transform other variables

final_data <- final_data %>%
  mutate(User.Votes = str_remove_all(User.Votes, "K"),
         User.Votes = as.numeric(User.Votes)) %>%
  mutate(User.Votes = ifelse(User.Votes < 10, User.Votes*1000, User.Votes)) %>%
  mutate(User.Votes = ifelse(User.Votes %in% c(2000,3000,4000,5000,6000,7000,8000,9000), 
                             User.Votes/1000, User.Votes))

final_data$Critic.Rating <- as.numeric(final_data$Critic.Rating)
final_data$Critic.Votes<- as.numeric(final_data$Critic.Votes)
final_data$Star.Power <- as.numeric(final_data$Star.Power)
final_data$User.Rating <- as.numeric(final_data$User.Rating)

final_data <- final_data %>%
  mutate(Critic.Rating = ifelse(is.na(Critic.Rating), 
                                round(mean(Critic.Rating, na.rm=T),0), 
                                Critic.Rating)) %>%
  mutate(Critic.Votes = ifelse(is.na(Critic.Votes), 
                                round(mean(Critic.Votes, na.rm=T),0), 
                                Critic.Votes))

final_data$Film <- as.factor(final_data$Film)
final_data <- final_data %>%
  mutate(Netflix.Original.dummy. = ifelse(Netflix.Original.dummy. == "yes",
                                          1, 0))
final_data$Netflix.Original.dummy. <- as.factor(final_data$Netflix.Original.dummy.)
final_data$k_6 <- as.factor(as.character(final_data$k_6))
final_data$k_5 <- as.factor(as.character(final_data$k_5))
final_data$k_4 <- as.factor(as.character(final_data$k_4))


# Create genre dummy

test <- final_data |> 
  tidyr::separate_rows(Genre, sep = ",") |> 
  filter(Genre != "") |> 
  mutate(value = 1) |> 
  pivot_wider(
    names_from = Genre,
    values_from = value,
    values_fill = 0
  )

# Transfrom budget data
## Built currency conversion function
currencycon <- function(x, from = "USD", to = "EUR"){
  # assign values: 1 usd in each currency 
  values <- c(1.000000, 0.927985, 0.810100, 107.624500, 4.57, 1.341961,
              81.71, 31.181502, 1339.30, 60.8)
  # names attribute 
  names(values) <- c("USD", "EUR", "GBP", "YEN", "MYR", "CAD", "IR", 
                     "TWD", "WON", "RUB")
  # calculate (convert from into USD, and divide to)
  values[to] / (values[from] / x)
}

test[grepl('MYR', test$Budget), "Budget"] <- as.character(currencycon(8000000, "MYR", "USD"))

test[grepl('NT', test$Budget), "Budget"] <- as.character(currencycon(45000000, "TWD", "USD"))

test[grepl('CA', test$Budget), "Budget"] <- as.character(currencycon(700000, "CAD", "USD"))

test[grepl('₩', test$Budget), "Budget"] <- as.character(currencycon(23500000000, "WON", "USD"))

test[grepl('₹310', test$Budget), "Budget"] <- as.character(currencycon(310000000, "IR", "USD"))

test[grepl('₹280', test$Budget), "Budget"] <- as.character(currencycon(280000000 , "IR", "USD"))

test[grepl('₹100', test$Budget), "Budget"] <- as.character(currencycon(100000000 , "IR", "USD"))

test[grepl('₹100', test$Budget), "Budget"] <- as.character(currencycon(100000000 , "IR", "USD"))

test[grepl('₹5', test$Budget), "Budget"] <- as.character(currencycon(5000000000, "IR", "USD"))

test[grepl('₹1', test$Budget), "Budget"] <- as.character(currencycon(1800000000, "IR", "USD"))

test[grepl('640', test$Budget), "Budget"] <- as.character(currencycon(640000000, "RUB", "USD"))

test[grepl('RUR', test$Budget), "Budget"] <- as.character(currencycon(190000000, "RUB", "USD"))

test[grepl('¥', test$Budget), "Budget"] <- as.character(currencycon(500000000, "YEN", "USD"))

test <- test %>%
  mutate(Budget = str_remove_all(Budget, "\\$"),
         Budget = str_remove_all(Budget, "\\€"),
         Budget = str_remove_all(Budget, ","),
         Budget = ifelse(is.na(Budget), 0, Budget)) %>%
  mutate(Budget = as.numeric(Budget))


# Save latest data as csv
write.csv(test,"../data/netflix_data.csv", row.names = FALSE)



