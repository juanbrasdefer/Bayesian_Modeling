# cleaning ANES 2024 for Stan

# packages
library(tidyverse)
library(here)

# directory
here::i_am("ResearchNote/code/cleaning_ANES.R")


# load data ---------------------------------------------------------

survey_raw <- read_csv(here("ResearchNote/data/anes_timeseries_2024_csv_20250430.csv"))

# clean and transform ----------------------------------------------------------

survey_clean <- survey_raw %>%
  select("V242096x", # vote summary
         "V241566x", # income (household)
         "V242438", # left-right self-placement
         "V241166", # thermometer democrats
         "V241167", # thermometer republicans
         "V241004" # attention to politics
         ) %>% 
  mutate(across(everything(), ~ ifelse(. < 0, # re-coding the ANES missing values as NA
                                       NA, .))) %>% # ANES uses -9, -8, -7 etc to mean different NA reasons
  drop_na() %>% # drop missing values in any columns
  mutate(repvote_yn = if_else(V242096x == 2, 1, 0)) %>% # convert vote choice into binary
  mutate(income_level = case_when( # condense income groups from 28 groups to 11 groups for better convergence and explainability
    V241566x %in% 1 ~ 1,     # 1. Under $5,000
    V241566x %in% 2 ~ 2,      # 2. $5,000-9,999
    V241566x %in% 3:4 ~ 3,   # 3. $10,000-12,499 # 4. $12,500-14,999
    V241566x %in% 5:6 ~ 4,   # 5. $15,000-17,499 # 6. $17,500-19,999
    V241566x %in% 7:10 ~ 5,  # 7. $20,000-22,499 # 8. $22,500-24,999 # 9. $25,000-27,499 # 10. $27,500-29,999 
    V241566x %in% 11:14 ~ 6, # 11. $30,000-34,999 # 12. $35,000-39,999 # 13. $40,000-44,999 # 14. $45,000-49,999 
    V241566x %in% 15:18 ~ 7, # 15. $50,000-54,999 # 16. $55,000-59,999 # 17. $60,000-64,999 # 18. $65,000-69,999
    V241566x %in% 19:22 ~ 8, # 19. $70,000-74,999 # 20. $75,000-79,999 # 21. $80,000-89,999 # 22. $90,000-99,999
    V241566x %in% 23:26 ~ 9, # 23. $100,000-109,999 # 24. $110,000-124,999 # 25. $125,000-149,999 # 26. $150,000-174,999
    V241566x %in% 27 ~ 10,   # 27. $175,000-249,999
    V241566x %in% 28 ~ 11    # 28. $250,000 or more
  )) %>%
  mutate(leftright_self = V242438 - 5) %>% # RE-CENTER LEFTRIGHT to be centered at 0, not at 5
  mutate(therm_dems = ((V241166/10)-5)) %>% # RE-SCALE THERMS to be [0,10] and RE-CENTER to be at 0, not at 5
  mutate(therm_reps = ((V241167/10)-5)) %>% # RE-SCALE THERMS to be [0,10] and RE-CENTER to be at 0, not at 5
  mutate(attention_pol = case_when( # change the 'direction', so that higher num means more attention
    V241004 == 1 ~ 5, # original encoding: 1 = always, new encoding: 5 = always
    V241004 == 2 ~ 4, # new encoding: 4 = most of the time
    V241004 == 3 ~ 3, # new encoding: 3 = about half of the time
    V241004 == 4 ~ 2, # new encoding: 2 = some of the time
    V241004 == 5 ~ 1  # new encoding: 1 = never
    ))




# CLASS IMBALANCE: Incomes -------------------------------------------------
# temp <- survey_raw %>%
#   select("V242096x", # vote summary
#          "V241566x", # income (household)
#          "V242438", # left-right self-placement
#          "V241166", # thermometer democrats
#          "V241167", # thermometer republicans
#          "V241004" # attention to politics
#   ) %>%
#   mutate(across(everything(), ~ ifelse(. < 0, # re-coding the ANES missing values as NA
#                                        NA, .))) %>% # ANES uses -9, -8, -7 etc to mean different NA reasons
#   drop_na() %>%
#   group_by(V241566x) %>%
#   summarize(counts = n())

# some as few as 29 observations in raw data, 
# and as few as 17 after we remove missing values across all our
# variables of interest


# CLASS IMBALANCE: D vs. R -------------------------------------------------
# representative survey?
# temp <- survey_raw %>%
#   select("V242096x" ) %>% 
#   mutate(across(everything(), ~ ifelse(. < 0, # re-coding the ANES missing values as NA
#                                        NA, .))) %>% # ANES uses -9, -8, -7 etc to mean different NA reasons
#     drop_na() %>%
#   group_by(V242096x) %>%
#   summarize( vote_prop = n())
# 
# 2201/ (2201+1747+ 4 + 13 + 25 +63)
# 1747 / (2201+1747+ 4 + 13 + 25 +63)

# no! we have a class imbalance, dems overrepresented
# True (real-world) national popular vote counts put this proportion at 
    # 49.8% Republican and 
    # 48.3% Democrat (American Presidency Project, 2024), 
# while this dataset over-represents the democratic vote, 
  # with 54.3% voting Democrat and 
    #only 43.1% voting Republican. 






# variables used, encoding ------------------------------------------------------
#V242096x PRE-POST: SUMMARY: 2024 PRESIDENTIAL VOTE
#-7. Insufficient partial, interview deleted -6. No post interview
# -5. Sufficient partial, breakoff
# 1. Kamala Harris
# 2. Donald Trump
# 3. Robert F. Kennedy, Jr 
# 4. Cornel West
# 5. Jill Stein
# 6. Other


# V241566x PRE: SUMMARY: TOTAL (HOUSEHOLD) INCOME
# -9. Refused
# -5. Break off, sufficient partial 
# -4. Error
# -1. Inapplicable
# 1. Under $5,000
# 2. $5,000-9,999
# 3. $10,000-12,499 # 4. $12,500-14,999
# 5. $15,000-17,499 # 6. $17,500-19,999
# 7. $20,000-22,499 # 8. $22,500-24,999 # 9. $25,000-27,499 # 10. $27,500-29,999 
# 11. $30,000-34,999 # 12. $35,000-39,999 # 13. $40,000-44,999 # 14. $45,000-49,999 
# 15. $50,000-54,999 # 16. $55,000-59,999 # 17. $60,000-64,999 # 18. $65,000-69,999
# 19. $70,000-74,999 # 20. $75,000-79,999 # 21. $80,000-89,999 # 22. $90,000-99,999
# 23. $100,000-109,999 # 24. $110,000-124,999 # 25. $125,000-149,999 # 26. $150,000-174,999
# 27. $175,000-249,999
# 28. $250,000 or more


# V242438 POST: CSES5-Q18: LEFT-RIGHT-SELF
# -9. Refused
# -8. Don’t know
# -7. Insufficient partial, interview deleted 
# -6. No post interview
# -5. Sufficient partial, breakoff
# -1. Inapplicable
# 0. Left
# 10. Right

# V241166 PRE: FEELING THERMOMETER: DEMOCRATIC PARTY
# -9. Refused
# -8. Don’t know


# V241167 PRE: FEELING THERMOMETER: REPUBLICAN PARTY
# -9. Refused
#-8. Don’t know 
# -1. Inapplicable

# V241004 PRE: HOW OFTEN DOES R PAY ATTENTION TO POLITICS AND ELECTIONS
# -9. Refused
# 1. Always
# 2. Most of the time
# 3. About half the time 
# 4. Some of the time 
# 5. Never


# df %>%
# ggplot(aes(x= VARIABLE)) + 
#   geom_histogram(aes(y= after_stat(density)), colour="black", fill="white")+
#   geom_density(alpha=.2, fill="#FF6666") 

# other vars considered
# V242431 POST: CSES5-Q14B: WHO PEOPLE VOTE FOR MAKES A BIG DIFFERENCE
# V241363x PRE: SUMMARY: HOW MUCH LARGER IS INCOME GAP TODAY
# V241229 PRE: HOW OFTEN TRUST GOVERNMENT IN WASHINGTON TO DO WHAT IS RIGHT [REVISED]

