# Basic Political Science Datasets for GOVT 10
# This script generates the core datasets needed for the course
# Run this script to generate all CSV files

library(tidyverse)
set.seed(12345)

# Simple voter turnout data for Week 1
voter_turnout_simple <- tibble(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
            "Colorado", "Connecticut", "Delaware", "Florida", "Georgia"),
  turnout_2020 = c(63.1, 58.9, 60.0, 54.9, 64.5, 76.4, 65.2, 66.0, 66.2, 66.2),
  turnout_2016 = c(59.0, 61.5, 56.0, 53.2, 58.4, 71.9, 65.7, 61.8, 65.3, 59.4),
  population_millions = c(5.0, 0.7, 7.3, 3.0, 39.5, 5.8, 3.6, 1.0, 21.5, 10.7)
)
write_csv(voter_turnout_simple, "voter_turnout_simple.csv")

# Congressional approval survey data
n_respondents <- 2000
congressional_approval <- tibble(
  respondent_id = 1:n_respondents,
  age = sample(18:85, n_respondents, replace = TRUE),
  education = sample(c("High School", "Some College", "Bachelor's", "Graduate"), 
                    n_respondents, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15)),
  party_id = sample(c("Democrat", "Republican", "Independent"), 
                   n_respondents, replace = TRUE, prob = c(0.35, 0.33, 0.32)),
  income_category = sample(c("Under $30k", "$30k-$60k", "$60k-$100k", "Over $100k"),
                          n_respondents, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
  region = sample(c("Northeast", "South", "Midwest", "West"), 
                 n_respondents, replace = TRUE, prob = c(0.18, 0.38, 0.21, 0.23))
)

# Add approval ratings based on party ID
congressional_approval <- congressional_approval %>%
  mutate(
    congress_approval = ifelse(party_id == "Democrat", 35, 
                              ifelse(party_id == "Republican", 25, 20)) +
                       ifelse(education == "Graduate", 5,
                             ifelse(education == "Bachelor's", 2, 0)) +
                       ifelse(age > 65, 3, ifelse(age < 30, -2, 0)) +
                       rnorm(n_respondents, 0, 12),
    congress_approval = pmax(0, pmin(100, congress_approval))
  )
write_csv(congressional_approval, "congressional_approval.csv")

# Congressional elections data
congressional_elections <- tibble(
  district_id = 1:435,
  state = sample(state.name, 435, replace = TRUE),
  incumbent_running = sample(c(0, 1), 435, replace = TRUE, prob = c(0.25, 0.75)),
  challenger_quality = sample(1:5, 435, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.08, 0.02)),
  district_partisanship = rnorm(435, 0, 8),
  campaign_spending_incumbent_thousands = ifelse(incumbent_running == 1, 
                                                pmax(200, rnorm(435, 1500, 800)), 0),
  campaign_spending_challenger_thousands = pmax(100, rnorm(435, 800, 600)),
  economic_growth_local = rnorm(435, 2.1, 1.8),
  presidential_approval_local = rnorm(435, 45, 8)
)

# Calculate vote share
congressional_elections <- congressional_elections %>%
  mutate(
    incumbent_vote_share = 50 + 
                          6 * incumbent_running +
                          -0.8 * challenger_quality +
                          0.4 * district_partisanship +
                          0.001 * campaign_spending_incumbent_thousands +
                          -0.0008 * campaign_spending_challenger_thousands +
                          1.1 * economic_growth_local +
                          0.15 * (presidential_approval_local - 45) +
                          rnorm(435, 0, 5),
    incumbent_vote_share = pmax(25, pmin(75, incumbent_vote_share)),
    total_spending = campaign_spending_incumbent_thousands + campaign_spending_challenger_thousands,
    competitive_race = ifelse(abs(incumbent_vote_share - 50) < 10, 1, 0),
    margin_of_victory = abs(incumbent_vote_share - 50),
    incumbent_won = ifelse(incumbent_vote_share > 50, 1, 0)
  )
write_csv(congressional_elections, "congressional_elections.csv")

# GOTV experiment
gotv_experiment <- tibble(
  voter_id = 1:5000,
  treatment = sample(c("Control", "Postcard", "Phone Call", "Personal Visit"), 
                    5000, replace = TRUE),
  age_group = sample(c("18-29", "30-49", "50-64", "65+"), 
                    5000, replace = TRUE, prob = c(0.22, 0.35, 0.25, 0.18)),
  education = sample(c("High School", "Some College", "Bachelor's", "Graduate"), 
                    5000, replace = TRUE, prob = c(0.28, 0.32, 0.25, 0.15)),
  baseline_turnout_prob = case_when(
    age_group == "18-29" ~ 0.35,
    age_group == "30-49" ~ 0.52,
    age_group == "50-64" ~ 0.68,
    age_group == "65+" ~ 0.74
  ),
  treatment_effect = case_when(
    treatment == "Control" ~ 0,
    treatment == "Postcard" ~ 0.05,
    treatment == "Phone Call" ~ 0.08,
    treatment == "Personal Visit" ~ 0.12
  ),
  voted_2022 = rbinom(5000, 1, pmin(0.95, baseline_turnout_prob + treatment_effect))
)
write_csv(gotv_experiment, "gotv_experiment.csv")

# Media and political knowledge
media_knowledge <- tibble(
  person_id = 1:1200,
  primary_news_source = sample(c("TV", "Newspaper", "Social Media", "Podcast"), 
                              1200, replace = TRUE, prob = c(0.4, 0.2, 0.3, 0.1)),
  education_years = sample(10:20, 1200, replace = TRUE),
  age = sample(18:80, 1200, replace = TRUE),
  knowledge_base = case_when(
    primary_news_source == "Newspaper" ~ 8.3,
    primary_news_source == "Podcast" ~ 7.0,
    primary_news_source == "TV" ~ 6.2,
    primary_news_source == "Social Media" ~ 4.8
  ),
  political_knowledge_score = pmax(0, pmin(10, knowledge_base + 0.1 * education_years + rnorm(1200, 0, 1.5)))
)
write_csv(media_knowledge, "media_knowledge.csv")

# Political trust data
political_trust <- tibble(
  respondent_id = 1:2000,
  education_level = sample(c("High School", "Some College", "Bachelor's", "Graduate"), 
                          2000, replace = TRUE, prob = c(0.28, 0.30, 0.27, 0.15)),
  income_category = sample(c("Low", "Middle", "High"), 
                          2000, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  media_trust = runif(2000, 1, 5),
  political_efficacy = runif(2000, 1, 5),
  trust_base = 2.5 +
               ifelse(education_level == "Graduate", 0.3,
                     ifelse(education_level == "Bachelor's", 0.1, -0.1)) +
               ifelse(income_category == "High", 0.3,
                     ifelse(income_category == "Low", -0.3, 0)) +
               0.3 * media_trust + 0.2 * political_efficacy +
               rnorm(2000, 0, 0.8),
  political_trust_scale = pmax(1, pmin(5, trust_base))
)
write_csv(political_trust, "political_trust.csv")

# Presidential approval over time
presidential_approval <- tibble(
  date = seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month"),
  president = "Biden",
  months_in_office = as.numeric(difftime(date, as.Date("2021-01-20"), units = "days")) / 30.44,
  approval_base = 48 + 10 * exp(-months_in_office / 12) - 0.1 * months_in_office + rnorm(length(date), 0, 3),
  approval_rating = pmax(30, pmin(70, approval_base))
) %>%
  filter(months_in_office >= 0)
write_csv(presidential_approval, "presidential_approval.csv")

# State voting patterns
state_voting_patterns <- expand_grid(
  state = state.name[1:20],  # Subset for manageable size
  year = c(2016, 2020)
) %>%
  mutate(
    region = case_when(
      state %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire") ~ "Northeast",
      state %in% c("Illinois", "Indiana", "Iowa", "Michigan") ~ "Midwest", 
      state %in% c("Florida", "Georgia", "North Carolina", "Virginia") ~ "South",
      TRUE ~ "West"
    ),
    republican_vote_share = case_when(
      region == "South" ~ 55 + rnorm(n(), 0, 5),
      region == "Midwest" ~ 48 + rnorm(n(), 0, 5),
      region == "West" ~ 45 + rnorm(n(), 0, 5),
      region == "Northeast" ~ 42 + rnorm(n(), 0, 5)
    ) + ifelse(year == 2020, -2, 0),  # Trump effect
    republican_vote_share = pmax(25, pmin(75, republican_vote_share)),
    democratic_vote_share = 100 - republican_vote_share,
    winner = ifelse(republican_vote_share > 50, "Republican", "Democrat")
  )
write_csv(state_voting_patterns, "state_voting_patterns.csv")

cat("Basic datasets generated successfully!\n")
cat("Generated files:\n")
cat("- voter_turnout_simple.csv\n")
cat("- congressional_approval.csv\n") 
cat("- congressional_elections.csv\n")
cat("- gotv_experiment.csv\n")
cat("- media_knowledge.csv\n")
cat("- political_trust.csv\n")
cat("- presidential_approval.csv\n")
cat("- state_voting_patterns.csv\n")