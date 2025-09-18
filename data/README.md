# GOVT 10 Course Datasets

This directory contains all datasets used in the GOVT 10 course. Datasets are a mix of real data from political science research and realistic synthetic data generated specifically for pedagogical purposes.

## Real Datasets (copied from current course)

### `afghan.csv` 
- **Source**: Real survey data about Afghanistan war attitudes
- **Use**: Week 3 causality exercises, Week 6 uncertainty
- **Size**: ~1,500 observations
- **Key variables**: Support for war, demographics, attitudes

### `campaign.csv`
- **Source**: Real campaign finance data  
- **Use**: Week 4 regression exercises, midterm
- **Size**: ~6,000 observations
- **Key variables**: Spending, vote share, candidate characteristics

### `STAR.csv`
- **Source**: Tennessee STAR education experiment
- **Use**: Week 4 regression, experimental design examples
- **Size**: ~11,000 observations  
- **Key variables**: Class size, test scores, demographics

### `ipv.csv`
- **Source**: Intimate partner violence attitudes (cross-national)
- **Use**: Quiz examples, causality discussions
- **Size**: ~150 observations (country-level)
- **Key variables**: Attitudes by country, demographics, culture

### `natural_disasters_2024.csv`
- **Source**: Real natural disaster data
- **Use**: Week 6 problem set, uncertainty analysis
- **Size**: ~500 observations
- **Key variables**: Disaster type, location, damage, response

### `nfss.csv`
- **Source**: National Family Structure Survey
- **Use**: Quiz 4, uncertainty exercises  
- **Size**: ~3,000 observations
- **Key variables**: Family structure, outcomes, demographics

## Synthetic Datasets (generated for course)

### `voter_turnout_simple.csv`
- **Week 1**: Basic data operations
- **Size**: 10 states
- **Variables**: state, turnout_2020, turnout_2016, population_millions

### `congressional_approval.csv`
- **Week 2**: Summary statistics and demographics  
- **Size**: 2,000 respondents
- **Variables**: age, education, party_id, income_category, region, congress_approval

### `congressional_elections.csv`
- **Week 4**: Regression analysis (main dataset)
- **Size**: 435 districts (all House seats)
- **Variables**: incumbent_running, challenger_quality, district_partisanship, campaign spending, economic_growth, presidential_approval, vote_share

### `gotv_experiment.csv`
- **Week 5**: Categorical variables and interactions
- **Size**: 5,000 voters
- **Variables**: treatment, age_group, education, voted_2022, baseline_turnout_prob

### `media_knowledge.csv`
- **Week 5**: Categorical predictors
- **Size**: 1,200 people  
- **Variables**: primary_news_source, education_years, age, political_knowledge_score

### `political_trust.csv`
- **Week 5**: Multiple categorical variables
- **Size**: 2,000 respondents
- **Variables**: education_level, income_category, media_trust, political_efficacy, political_trust_scale

### `presidential_approval.csv`
- **Week 7**: Data visualization over time
- **Size**: 36 months (Biden presidency)
- **Variables**: date, president, months_in_office, approval_rating

### `state_voting_patterns.csv`  
- **Week 7**: Geographic and temporal visualization
- **Size**: 40 state-years (20 states × 2 elections)
- **Variables**: state, year, region, republican_vote_share, democratic_vote_share, winner

## Data Generation

All synthetic datasets are created by the `generate_basic_datasets.R` script. This script:
- Uses realistic parameter values based on actual political science research
- Creates appropriate correlations between variables
- Includes sufficient sample sizes for meaningful analysis
- Generates data with realistic noise and variation

To regenerate all synthetic datasets:
```r
source("data/generate_basic_datasets.R")
```

## Loading Data in Exercises

All exercises should load data using:
```r
library(tidyverse)
data_name <- read_csv("data/filename.csv")
```

## Dataset Documentation

Each dataset includes:
- Realistic variable names and ranges
- Appropriate correlations for teaching concepts
- Sufficient complexity for advanced exercises
- Clear variable meanings for student interpretation
- Sample sizes appropriate for statistical power

## Notes for Instructors

- Real datasets provide authenticity and connection to actual research
- Synthetic datasets are designed specifically for pedagogical clarity
- All datasets work together to build concepts progressively across weeks
- Variable naming is consistent across related datasets
- Missing data is minimal to focus on core concepts rather than data cleaning