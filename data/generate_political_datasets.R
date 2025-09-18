# Political Science Data Generation Scripts
# This script creates realistic synthetic datasets for GOVT 10 course
# Run this script to generate all CSV files used in exercises

library(tidyverse)
set.seed(12345)  # For reproducible results

# =============================================================================
# WEEK 1: Basic Data Structures
# =============================================================================

# Simple voter turnout data for Week 1 exercises
voter_turnout_simple <- tibble(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
            "Colorado", "Connecticut", "Delaware", "Florida", "Georgia"),
  turnout_2020 = c(63.1, 58.9, 60.0, 54.9, 64.5, 76.4, 65.2, 66.0, 66.2, 66.2),
  turnout_2016 = c(59.0, 61.5, 56.0, 53.2, 58.4, 71.9, 65.7, 61.8, 65.3, 59.4),
  population_millions = c(5.0, 0.7, 7.3, 3.0, 39.5, 5.8, 3.6, 1.0, 21.5, 10.7)
)

write_csv(voter_turnout_simple, "voter_turnout_simple.csv")

# =============================================================================
# WEEK 2: Research Designs and Summary Statistics
# =============================================================================

# Congressional approval by demographics (larger realistic dataset)
congressional_approval <- tibble(
  respondent_id = 1:2500,
  age = sample(18:85, 2500, replace = TRUE),
  education = sample(c("High School", "Some College", "Bachelor's", "Graduate"), 
                    2500, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15)),
  party_id = sample(c("Democrat", "Republican", "Independent"), 
                   2500, replace = TRUE, prob = c(0.35, 0.33, 0.32)),
  income_category = sample(c("Under $30k", "$30k-$60k", "$60k-$100k", "Over $100k"),
                          2500, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
  region = sample(c("Northeast", "South", "Midwest", "West"), 
                 2500, replace = TRUE, prob = c(0.18, 0.38, 0.21, 0.23)),
  # Create realistic approval ratings based on party ID
  congress_approval = case_when(
    party_id == "Democrat" ~ pmax(0, pmin(100, rnorm(sum(party_id == "Democrat"), 35, 15))),
    party_id == "Republican" ~ pmax(0, pmin(100, rnorm(sum(party_id == "Republican"), 25, 12))),
    party_id == "Independent" ~ pmax(0, pmin(100, rnorm(sum(party_id == "Independent"), 20, 10)))
  )
) %>%
  mutate(
    # Add some demographic effects
    congress_approval = congress_approval + 
      case_when(
        education == "Graduate" ~ 5,
        education == "Bachelor's" ~ 2,
        TRUE ~ 0
      ) +
      case_when(
        age > 65 ~ 3,
        age < 30 ~ -2,
        TRUE ~ 0
      ) + rnorm(2500, 0, 3),
    congress_approval = pmax(0, pmin(100, congress_approval))
  )

write_csv(congressional_approval, "congressional_approval.csv")

# Media consumption and political knowledge
media_knowledge <- tibble(
  person_id = 1:1500,
  primary_news_source = sample(c("TV", "Newspaper", "Social Media", "Podcast", "Radio"), 
                              1500, replace = TRUE, prob = c(0.35, 0.15, 0.30, 0.12, 0.08)),
  hours_news_daily = pmax(0, rnorm(1500, 1.5, 1.2)),
  age = sample(18:80, 1500, replace = TRUE),
  education_years = sample(10:20, 1500, replace = TRUE, prob = c(rep(0.05, 3), rep(0.1, 4), rep(0.15, 4))),
  political_knowledge_score = case_when(
    primary_news_source == "Newspaper" ~ 6.2 + 2.1 + rnorm(sum(primary_news_source == "Newspaper"), 0, 1.5),
    primary_news_source == "Podcast" ~ 6.2 + 0.8 + rnorm(sum(primary_news_source == "Podcast"), 0, 1.5),
    primary_news_source == "TV" ~ 6.2 + rnorm(sum(primary_news_source == "TV"), 0, 1.5),
    primary_news_source == "Radio" ~ 6.2 + 0.3 + rnorm(sum(primary_news_source == "Radio"), 0, 1.5),
    primary_news_source == "Social Media" ~ 6.2 - 1.4 + rnorm(sum(primary_news_source == "Social Media"), 0, 1.5)
  ) + 0.1 * education_years + rnorm(1500, 0, 1),
  political_knowledge_score = pmax(0, pmin(10, political_knowledge_score))
)

write_csv(media_knowledge, "media_knowledge.csv")

# =============================================================================
# WEEK 3: Causality and Interpretation  
# =============================================================================

# Minimum wage and employment (state-level data)
minimum_wage_employment <- tibble(
  state = state.name,
  state_abbr = state.abb,
  min_wage_2023 = c(7.25, 11.73, 14.20, 11.00, 16.00, 14.42, 15.00, 11.75, 11.00, 7.25,
                   12.00, 7.25, 13.25, 7.25, 7.25, 7.25, 7.25, 7.25, 14.00, 14.00,
                   15.00, 10.10, 7.25, 7.25, 12.00, 7.25, 10.30, 7.25, 7.25, 7.25,
                   7.25, 15.00, 7.25, 7.25, 7.25, 7.25, 13.50, 7.25, 7.25, 7.25,
                   13.69, 7.25, 7.25, 7.25, 7.25, 13.50, 16.28, 7.25, 7.25, 7.25),
  unemployment_rate_2023 = c(2.9, 3.2, 4.1, 3.4, 4.6, 3.4, 4.1, 4.4, 2.8, 3.2,
                            3.0, 3.0, 4.3, 3.1, 2.4, 2.9, 3.8, 4.6, 3.7, 3.1,
                            3.5, 3.8, 2.2, 3.4, 2.7, 3.1, 2.2, 3.9, 3.2, 2.8,
                            4.4, 4.1, 3.9, 2.1, 3.8, 3.6, 4.2, 4.4, 3.7, 2.9,
                            3.4, 2.8, 3.2, 3.5, 3.1, 3.3, 4.2, 2.7, 2.9, 3.6),
  cost_of_living_index = rnorm(50, 100, 15),
  population_density = c(97.5, 1.3, 64.9, 58.4, 253.9, 56.4, 746.7, 508.8, 401.4, 186.6,
                        87.6, 36.2, 231.5, 186.1, 56.9, 46.7, 115.8, 109.9, 43.8, 171.1,
                        894.4, 177.2, 25.4, 7.5, 55.2, 4.2, 2.8, 2.6, 153.7, 493.8,
                        11.3, 427.8, 220.2, 10.7, 287.2, 11.7, 163.2, 57.1, 1,027.2, 154.2,
                        11.1, 3.3, 197.9, 94.2, 105.2, 10.9, 180.0, 178.2, 27.2, 68.7),
  # Add some realistic correlations
  republican_vote_share_2020 = case_when(
    min_wage_2023 > 12 ~ pmax(20, pmin(60, rnorm(sum(min_wage_2023 > 12), 40, 10))),
    TRUE ~ pmax(35, pmin(80, rnorm(sum(min_wage_2023 <= 12), 58, 8)))
  )
)

write_csv(minimum_wage_employment, "minimum_wage_employment.csv")

# =============================================================================
# WEEK 4: Regression Analysis
# =============================================================================

# Congressional elections data (large realistic dataset)
congressional_elections <- tibble(
  district_id = 1:435,
  state = sample(state.name, 435, replace = TRUE),
  incumbent_running = sample(c(0, 1), 435, replace = TRUE, prob = c(0.25, 0.75)),
  challenger_quality = sample(1:5, 435, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.08, 0.02)),
  district_partisanship = rnorm(435, 0, 8),  # Negative = more Democratic
  campaign_spending_incumbent_thousands = ifelse(incumbent_running == 1, 
                                                pmax(200, rnorm(sum(incumbent_running == 1), 1500, 800)), 0),
  campaign_spending_challenger_thousands = pmax(100, rnorm(435, 800, 600)),
  economic_growth_local = rnorm(435, 2.1, 1.8),
  presidential_approval_local = rnorm(435, 45, 8),
  # Create realistic vote share
  incumbent_vote_share = 50 + 
                        6 * incumbent_running +
                        -0.8 * challenger_quality +
                        0.4 * district_partisanship +
                        0.001 * campaign_spending_incumbent_thousands +
                        -0.0008 * campaign_spending_challenger_thousands +
                        1.1 * economic_growth_local +
                        0.15 * (presidential_approval_local - 45) +
                        rnorm(435, 0, 5),
  incumbent_vote_share = pmax(25, pmin(75, incumbent_vote_share))
) %>%
  # Add some additional realistic variables
  mutate(
    total_spending = campaign_spending_incumbent_thousands + campaign_spending_challenger_thousands,
    spending_ratio = ifelse(campaign_spending_challenger_thousands > 0,
                           campaign_spending_incumbent_thousands / campaign_spending_challenger_thousands, 
                           NA),
    competitive_race = ifelse(abs(incumbent_vote_share - 50) < 10, 1, 0),
    margin_of_victory = abs(incumbent_vote_share - 50),
    incumbent_won = ifelse(incumbent_vote_share > 50, 1, 0)
  )

write_csv(congressional_elections, "congressional_elections.csv")

# Campaign finance and fundraising data
campaign_fundraising <- tibble(
  candidate_id = 1:800,
  party = sample(c("Democrat", "Republican", "Independent", "Green"), 
                800, replace = TRUE, prob = c(0.43, 0.43, 0.12, 0.02)),
  incumbent_status = sample(c("Incumbent", "Challenger", "Open Seat"), 
                           800, replace = TRUE, prob = c(0.45, 0.45, 0.10)),
  district_competitiveness = runif(800, 0, 1),
  candidate_quality = sample(1:5, 800, replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.20, 0.05)),
  previous_office = sample(c("None", "Local", "State", "Federal"), 
                          800, replace = TRUE, prob = c(0.4, 0.3, 0.25, 0.05)),
  # Create realistic fundraising
  fundraising_thousands = case_when(
    party == "Democrat" ~ 420,
    party == "Republican" ~ 440,
    party == "Independent" ~ 85,
    party == "Green" ~ 35
  ) +
  case_when(
    incumbent_status == "Incumbent" ~ 350,
    incumbent_status == "Open Seat" ~ 150,
    TRUE ~ 0
  ) +
  120 * candidate_quality +
  400 * district_competitiveness +
  case_when(
    previous_office == "Federal" ~ 200,
    previous_office == "State" ~ 100,
    previous_office == "Local" ~ 50,
    TRUE ~ 0
  ) + rnorm(800, 0, 180),
  fundraising_thousands = pmax(10, fundraising_thousands)
)

write_csv(campaign_fundraising, "campaign_fundraising.csv")

# =============================================================================
# WEEK 5: Advanced Regression (Categorical Variables and Interactions)
# =============================================================================

# GOTV experiment data
gotv_experiment <- tibble(
  voter_id = 1:8000,
  treatment = sample(c("Control", "Postcard", "Phone Call", "Personal Visit", "Digital Ad"), 
                    8000, replace = TRUE),
  age_group = sample(c("18-29", "30-49", "50-64", "65+"), 
                    8000, replace = TRUE, prob = c(0.22, 0.35, 0.25, 0.18)),
  education = sample(c("High School", "Some College", "Bachelor's", "Graduate"), 
                    8000, replace = TRUE, prob = c(0.28, 0.32, 0.25, 0.15)),
  party_registration = sample(c("Democrat", "Republican", "Independent", "Unaffiliated"), 
                             8000, replace = TRUE, prob = c(0.38, 0.35, 0.15, 0.12)),
  previous_turnout_elections = sample(0:4, 8000, replace = TRUE, prob = c(0.2, 0.25, 0.25, 0.2, 0.1)),
  baseline_turnout_prob = case_when(
    age_group == "18-29" ~ 0.35,
    age_group == "30-49" ~ 0.52,
    age_group == "50-64" ~ 0.68,
    age_group == "65+" ~ 0.74
  ) + 
  case_when(
    education == "Graduate" ~ 0.12,
    education == "Bachelor's" ~ 0.08,
    education == "Some College" ~ 0.03,
    TRUE ~ 0
  ) + 
  0.08 * previous_turnout_elections,
  # Treatment effects vary by age group
  treatment_effect = case_when(
    treatment == "Control" ~ 0,
    treatment == "Postcard" & age_group == "65+" ~ 0.08,
    treatment == "Postcard" ~ 0.03,
    treatment == "Phone Call" & age_group %in% c("30-49", "50-64") ~ 0.09,
    treatment == "Phone Call" ~ 0.04,
    treatment == "Personal Visit" & age_group == "18-29" ~ 0.15,
    treatment == "Personal Visit" ~ 0.07,
    treatment == "Digital Ad" & age_group == "18-29" ~ 0.06,
    treatment == "Digital Ad" ~ 0.01
  ),
  voted_2022 = rbinom(8000, 1, pmin(0.95, baseline_turnout_prob + treatment_effect))
)

write_csv(gotv_experiment, "gotv_experiment.csv")

# Political trust by demographics
political_trust <- tibble(
  respondent_id = 1:3500,
  education_level = sample(c("High School", "Some College", "Bachelor's", "Graduate"), 
                          3500, replace = TRUE, prob = c(0.28, 0.30, 0.27, 0.15)),
  income_category = sample(c("Low", "Middle", "Upper Middle", "High"), 
                          3500, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
  age = sample(18:85, 3500, replace = TRUE),
  race_ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 
                         3500, replace = TRUE, prob = c(0.62, 0.13, 0.18, 0.06, 0.01)),
  region = sample(c("Northeast", "South", "Midwest", "West"), 
                 3500, replace = TRUE, prob = c(0.17, 0.38, 0.21, 0.24)),
  media_trust_scale = runif(3500, 1, 5),
  political_efficacy_scale = runif(3500, 1, 5),
  political_trust_scale = 2.5 +
                         case_when(
                           education_level == "High School" ~ -0.2,
                           education_level == "Some College" ~ -0.1,
                           education_level == "Bachelor's" ~ 0.1,
                           education_level == "Graduate" ~ 0.25
                         ) +
                         case_when(
                           income_category == "Low" ~ -0.3,
                           income_category == "Middle" ~ 0,
                           income_category == "Upper Middle" ~ 0.2,
                           income_category == "High" ~ 0.3
                         ) +
                         0.35 * media_trust_scale +
                         0.25 * political_efficacy_scale +
                         # Add some interaction effects
                         case_when(
                           education_level == "Graduate" & income_category == "High" ~ 0.4,
                           education_level == "High School" & income_category == "Low" ~ -0.3,
                           TRUE ~ 0
                         ) +
                         rnorm(3500, 0, 0.8),
  political_trust_scale = pmax(1, pmin(5, political_trust_scale))
)

write_csv(political_trust, "political_trust.csv")

# =============================================================================
# WEEK 6: Uncertainty and Statistical Tests
# =============================================================================

# Election prediction data with uncertainty
election_predictions <- tibble(
  state = rep(state.name, each = 12),  # 12 months of polling
  month_before_election = rep(12:1, 50),
  poll_margin_dem = rnorm(600, 0, 6),
  poll_sample_size = sample(400:2000, 600, replace = TRUE),
  poll_margin_error = 1.96 * sqrt(0.25 / poll_sample_size),  # Realistic MOE
  economic_conditions = rnorm(600, 0, 1),
  incumbent_approval = rnorm(600, 45, 8),
  # Create final election results (state-level)
  final_dem_margin = rep(rnorm(50, 0, 8), each = 12),
  # Polls get more accurate closer to election
  poll_accuracy = 1 - 0.05 * month_before_election,
  predicted_margin = poll_accuracy * final_dem_margin + (1 - poll_accuracy) * poll_margin_dem,
  prediction_error = abs(predicted_margin - final_dem_margin)
) %>%
  arrange(state, month_before_election)

write_csv(election_predictions, "election_predictions.csv")

# Clinical trial style experiment (political intervention)
political_intervention <- tibble(
  participant_id = 1:1200,
  treatment_group = sample(c("Control", "Information", "Deliberation"), 
                          1200, replace = TRUE),
  pre_treatment_knowledge = rnorm(1200, 5, 1.5),
  age = sample(18:75, 1200, replace = TRUE),
  education_years = sample(10:20, 1200, replace = TRUE),
  political_interest = sample(1:7, 1200, replace = TRUE),
  # Treatment effects
  treatment_effect = case_when(
    treatment_group == "Control" ~ 0,
    treatment_group == "Information" ~ 0.8,
    treatment_group == "Deliberation" ~ 1.4
  ),
  post_treatment_knowledge = pre_treatment_knowledge + treatment_effect + 
                            0.1 * education_years + 
                            0.05 * political_interest +
                            rnorm(1200, 0, 1.2),
  knowledge_gain = post_treatment_knowledge - pre_treatment_knowledge
)

write_csv(political_intervention, "political_intervention.csv")

# =============================================================================
# WEEK 7: Data Visualization
# =============================================================================

# Presidential approval over time (multiple presidents)
presidential_approval <- tibble(
  date = seq(as.Date("2000-01-01"), as.Date("2023-12-31"), by = "month"),
  president = case_when(
    date < as.Date("2001-01-20") ~ "Clinton",
    date < as.Date("2009-01-20") ~ "Bush",
    date < as.Date("2017-01-20") ~ "Obama",
    date < as.Date("2021-01-20") ~ "Trump",
    TRUE ~ "Biden"
  ),
  party = case_when(
    president %in% c("Clinton", "Obama", "Biden") ~ "Democrat",
    TRUE ~ "Republican"
  ),
  months_in_office = case_when(
    president == "Clinton" ~ as.numeric(date - as.Date("1993-01-20")) / 30.44,
    president == "Bush" ~ as.numeric(date - as.Date("2001-01-20")) / 30.44,
    president == "Obama" ~ as.numeric(date - as.Date("2009-01-20")) / 30.44,
    president == "Trump" ~ as.numeric(date - as.Date("2017-01-20")) / 30.44,
    president == "Biden" ~ as.numeric(date - as.Date("2021-01-20")) / 30.44
  ),
  # Create realistic approval patterns
  base_approval = case_when(
    president == "Clinton" ~ 58,
    president == "Bush" ~ 55,
    president == "Obama" ~ 52,
    president == "Trump" ~ 42,
    president == "Biden" ~ 48
  ),
  # Add honeymoon period and general decline
  honeymoon_effect = pmax(0, 10 * exp(-months_in_office / 12)),
  time_trend = -0.1 * months_in_office,
  # Add major events (simplified)
  event_effect = case_when(
    president == "Bush" & date >= as.Date("2001-09-11") & date <= as.Date("2002-03-11") ~ 15,
    president == "Bush" & date >= as.Date("2008-09-01") & date <= as.Date("2009-01-20") ~ -20,
    president == "Obama" & date >= as.Date("2008-11-01") & date <= as.Date("2009-06-01") ~ 8,
    president == "Trump" & date >= as.Date("2020-03-01") & date <= as.Date("2020-11-01") ~ -8,
    president == "Biden" & date >= as.Date("2021-08-01") & date <= as.Date("2021-10-01") ~ -12,
    TRUE ~ 0
  ),
  approval_rating = base_approval + honeymoon_effect + time_trend + event_effect + rnorm(length(date), 0, 3),
  approval_rating = pmax(25, pmin(75, approval_rating))
) %>%
  filter(!is.na(months_in_office), months_in_office >= 0)

write_csv(presidential_approval, "presidential_approval.csv")

# State-level voting patterns over time
state_voting_patterns <- expand_grid(
  state = state.name,
  year = seq(2000, 2020, 4)
) %>%
  mutate(
    region = case_when(
      state %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
                   "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont") ~ "Northeast",
      state %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", 
                   "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin") ~ "Midwest",
      state %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", 
                   "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", 
                   "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "South",
      TRUE ~ "West"
    ),
    # Base Republican vote share by region and state characteristics
    base_republican = case_when(
      region == "South" ~ 58,
      region == "Midwest" ~ 48,
      region == "West" ~ 45,
      region == "Northeast" ~ 42
    ),
    # Add state-specific adjustments
    state_adjustment = case_when(
      state %in% c("California", "New York", "Massachusetts", "Vermont", "Hawaii") ~ -15,
      state %in% c("Wyoming", "Idaho", "Utah", "Oklahoma", "Alabama") ~ 15,
      state %in% c("Florida", "Ohio", "Pennsylvania", "Wisconsin", "Michigan") ~ 0,  # Swing states
      TRUE ~ rnorm(1, 0, 5)
    ),
    # Time trends
    time_trend = case_when(
      region == "South" ~ -0.3 * (year - 2000),  # South becoming less Republican
      region == "West" ~ -0.2 * (year - 2000),   # West becoming more Democratic
      TRUE ~ 0
    ),
    republican_vote_share = base_republican + state_adjustment + time_trend + rnorm(n(), 0, 3),
    republican_vote_share = pmax(20, pmin(80, republican_vote_share)),
    democratic_vote_share = 100 - republican_vote_share,
    margin = republican_vote_share - 50,
    winner = ifelse(republican_vote_share > 50, "Republican", "Democrat")
  )

write_csv(state_voting_patterns, "state_voting_patterns.csv")

# =============================================================================
# Print summary of generated datasets
# =============================================================================

cat("Data generation complete!\n\n")
cat("Generated datasets:\n")
cat("- voter_turnout_simple.csv (n=10) - Basic state turnout data\n")
cat("- congressional_approval.csv (n=2,500) - Individual-level approval by demographics\n") 
cat("- media_knowledge.csv (n=1,500) - News consumption and political knowledge\n")
cat("- minimum_wage_employment.csv (n=50) - State-level minimum wage and unemployment\n")
cat("- congressional_elections.csv (n=435) - House election results and predictors\n")
cat("- campaign_fundraising.csv (n=800) - Campaign finance by candidate characteristics\n")
cat("- gotv_experiment.csv (n=8,000) - Get-out-the-vote experiment with interactions\n")
cat("- political_trust.csv (n=3,500) - Political trust by demographics\n")
cat("- election_predictions.csv (n=600) - Polling accuracy over time\n")
cat("- political_intervention.csv (n=1,200) - Experimental treatment effects\n")
cat("- presidential_approval.csv (n=288) - Presidential approval 2000-2023\n")
cat("- state_voting_patterns.csv (n=306) - State voting patterns 2000-2020\n\n")
cat("Real datasets copied from current course:\n")
cat("- campaign.csv - Real campaign finance data\n")
cat("- STAR.csv - Tennessee STAR education experiment\n")
cat("- ipv.csv - Intimate partner violence attitudes\n")
cat("- afghan.csv - Afghanistan war attitudes\n")
cat("- natural_disasters_2024.csv - Natural disaster data\n")
cat("- nfss.csv - Family structure survey\n\n")
cat("All datasets saved to data/ directory.\n")