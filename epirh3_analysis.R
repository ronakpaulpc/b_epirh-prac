# Practice code from the "Analysis" section of the Epi R Handbook.
# We have created the sections as per the handbook.
# NOTE: Section headers can be entered using ctrl+shift+R or manually typed.
# Each section represents one chapter of the handbook.


#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C17 - Descriptive tables ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we use pkgs janitor, dplyr, gtsummary, rstatix, and base R 
# - to summarise data and create tables with descriptive statistics.
# This page covers how to create the underlying tables, whereas the 
# Tables for presentation page covers how to nicely format and print them.


# 17.1 Preparation --------------------------------------------------------

# ** Load packages ====
# Here we load the packages required for analysis
# install.packages(c("rio", "here", "skimr", "tidyverse", "gtsummary", 
#                    "rstatix", "janitor", "scales", "flextable"))
library(easypackages)
libraries(
    "rio", 
    "here", 
    "skimr", 
    "tidyverse", 
    "gtsummary", 
    "rstatix", 
    "janitor", 
    "scales", 
    "flextable"
)

# ** Import data ====
linelist <- import(here("data_prac", "linelist_cleaned.rds"))
head(linelist) |> view(title = "linelist")
glimpse(linelist)


# 17.2 Browse data --------------------------------------------------------

# ** skimr package ====
# By using the skimr package, you can get a detailed and aesthetically 
# pleasing overview of each of the vars in your dataset.
skim(linelist)
# We can use the base R summary() fn to get info about an entire dataset.
summary(linelist)
# NOTE: This output can be more difficult to read than using skimr.


# ** Summary statistics ====
# We can use base R functions to return summary statistics of a numeric col.
summary(linelist$age_years)
# We can access and save one specific part of it with index brackets.
summary(linelist$age_years)[[2]]
# Altly, we can also use the element name.
summary(linelist$age_years)[["1st Qu."]]

# You can use the get_summary_stats() function from rstatix to return 
# summary statistics in a dataframe format. This can be helpful for 
# performing subsequent operations or plotting on the numbers.
linelist |> get_summary_stats(
        age, wt_kg, ht_cm, ct_blood, temp,
        type = "common"
    )


# 17.3 janitor package ----------------------------------------------------
# The janitor packages offers the tabyl() function to produce tabulations 
# and cross-tabulations, which can be “adorned” or modified with helper 
# functions to display percent, proportions, counts, etc.

# ** Simple tabyl ====
# Using tabyl() on a specific column produces the unique values, counts, 
# and column-wise “percents” (actually proportions).
linelist |> tabyl(age_cat)
linelist |> tabyl(age_cat) |> adorn_rounding(digits = 3)


# ** Cross-tabulation ====
# Cross-tabulation counts are achieved by adding one or more additional 
# columns within tabyl().
linelist |> tabyl(age_cat, gender)


# ** "Adorning" the tabyl ====
# A simple one-way table with percent instead of the default proportions.
linelist |> 
    tabyl(age_cat) |> 
    adorn_pct_formatting()

# A cross-tabulation with a total row and row percent.
linelist |> 
    tabyl(age_cat, gender) |> 
    adorn_totals(where = "row") |> 
    adorn_percentages(denominator = "row") |> 
    adorn_pct_formatting(digits = 1)

# A cross-tabulation adjusted so that both counts and percent are displayed.
linelist |> 
    tabyl(age_cat, gender) |> 
    adorn_totals(where = "row") |> 
    adorn_percentages(denominator = "col") |> 
    adorn_pct_formatting() |> 
    adorn_ns(position = "front") |> 
    adorn_title(
        row_name = "Age category",
        col_name = "Gender"
    )


# ** Printing the tabyl ====
# By default, the tabyl will print the raw table to your R console.
# Altly, you can pass the tabyl to flextable or similar package to print 
# as a “pretty” image in the RStudio Viewer, which could be exported 
# as .png, .jpeg, .html, etc.
linelist |> 
    tabyl(age_cat, gender) |> 
    adorn_totals(where = "col") |> 
    adorn_percentages(denominator = "col") |> 
    adorn_pct_formatting() |> 
    adorn_ns(position = "front") |> 
    adorn_title(
        row_name = "Age category",
        col_name = "Gender",
        placement = "combined"
    ) |> 
    flextable() |> 
    autofit()


# ** Use on other tables ====
linelist |> 
    count(hospital) |> 
    adorn_totals()


# ** Saving the tabyl ====








# TBC ####




#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C27 - Survival analysis -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we learn about survival analysis.
# Let's go!

# 27.1 Overview -----------------------------------------------------------
# Survival analysis focuses on describing for a given individual or group of 
# individuals, a defined point of event called the failure (occurrence of a 
# disease, cure from a disease, death, relapse after response to treatment…) 
# that occurs after a period of time called failure time (or follow-up time 
# in cohort/population-based studies) during which individuals are observed.

# NO CODE.


# 27.2 Preparation --------------------------------------------------------

# ** Load packages ====
# To run survival analyses in R, one the most widely used package is the 
# survival package. We first install it and then load it as well as the 
# other packages that will be used in this section.


# ** Import dataset ====
# We import the dataset of cases from a simulated Ebola epidemic.
linelist_case_data <- import(here("data_prac", "linelist_cleaned.rds"))
glimpse(linelist_case_data)


# ** Data management and transformation ====
# We define:
# 1. a new data frame linelist_surv for this analysis.
# 2. our event of interest as being “death” (hence our survival probability
#    will be the probability of being alive after a certain time after the
#    time of origin).
# 3. the follow-up time (futime) as the time between the time of onset and 
#    the time of outcome in days.
# 4. censored patients as those who recovered or for whom the final outcome 
#    is not known i.e., the event “death” was not observed (event=0).

# Create a new data called linelist_surv from the linelist_case_data.
linelist_surv <- linelist_case_data |> 
    
    # remove obs with wrong or missing dates
    filter(date_outcome > date_onset) |> 
    
    mutate(
        
        # create event var, 1 = patient died, 0 = right censored
        event = ifelse(is.na(outcome) | outcome == "Recover", 0 , 1),
        
        # create the var on follow-up time in days
        futime = as.double(date_outcome - date_onset),
        
        # create new age cat var with only 3 levels
        age_cat_small = case_when(
            age_years < 5 ~ "0-4",
            age_years >= 5 & age_years < 20 ~ "5-19",
            age_years >= 20 ~ "20+"
        ),
        
        # now convert it to factor and specify the levels
        age_cat_small = fct_relevel(age_cat_small, "0-4", "5-19", "20+")
    )
glimpse(linelist_surv)

# We can verify the new columns we have created.
summary(linelist_surv$futime)
# Cross tabulate the new event var and the outcome var from which it was
# created to make sure the code did what it was intended to.
linelist_surv |> 
    tabyl(outcome, event)
# Cross tabulate the age vars to ensure correct assignments.
linelist_surv |> 
    tabyl(age_cat_small, age_cat)

# We review the 10 first obsns of the linelist_surv looking at specific vars.
linelist_surv |> 
    select(case_id, age_cat_small, date_onset, date_outcome, 
           outcome, event, futime) |> 
    head(10)
# We can also cross-tabulate the columns age_cat_small and gender to have 
# more details on the distribution of this new column by gender.
linelist_surv |> 
    tabyl(gender, age_cat_small, show_na = F) |> 
    adorn_totals(where = "both") |> 
    adorn_percentages() |> 
    adorn_pct_formatting() |> 
    adorn_ns(position = "front")


# 27.3 Basics of survival analysis  ---------------------------------------

# ** Building a surv-type object ====
# Use syntax for right-censored data.
survobj <- Surv(time = linelist_surv$futime, event = linelist_surv$event)
# To review, here are the first 10 rows of the linelist_surv data.
linelist_surv |> 
    select(case_id, date_onset, date_outcome, futime, outcome, event) |> 
    head(10)







# TBC ####












