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
# We can use janitor’s “adorn” functions to add totals or convert to 
# proportions, percents, or otherwise adjust the display.
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

# A cross-tabulation adjusted so that both counts and percents are displayed.
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
# We can use janitor’s adorn_*() functions on other tables, such as those
# created by summarise() and count() from dplyr, or table() from base R. 
linelist |> count(hospital)         # no totals
linelist |> count(hospital) |>      # dplyr function
    adorn_totals()                  # adorn function


# ** Saving the tabyl ====
# If you convert the table to a “pretty” image with a package like flextable
# you can save it with functions from that package.
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
    autofit() |> 
    save_as_docx(path = "sample_tbl_c17.docx")


# ** Statistics ====
# You can apply statistical tests on tabyls, from the stats package. 
age_by_outcome <- linelist |> 
    tabyl(age_cat, outcome, show_na = F)
age_by_outcome
chisq.test(age_by_outcome)
# NOTE: Missing values are not allowed so they are excluded from 
# the tabyl with show_na = FALSE.


# ** Other tips ====
# You can read more detail in the janitor page and this tabyl vignette.
# NO CODE.


# 17.4 dplyr package ------------------------------------------------------
# Creating tables with dplyr functions summarise() and count() is a useful 
# approach to calculating summary statistics and summary tables and 
# pass them to ggplot().

# summarise() creates a new, summary data frame. If the data are ungrouped 
# it will return a one-row dataframe of the specified summary statistic.
# If the data are grouped, the new data frame will have one row per group.

# NOTE: This function works with both UK and US spellings, summarise() 
# and summarize().


# ** Get counts ====
# The most simple function to apply within summarise() is n(). 
# Leave the parentheses empty to count the number of rows.
linelist |> 
    summarize(n_rows = n())

# If we have grouped the data beforehand, we get num of rows per group.
linelist |> 
    group_by(age_cat) |> 
    summarize(n_rows = n())

# Altly, we count use the count() fn which does the following:
# - Groups the data by the columns provided to it,
# - Summarizes them with n() (creating column n),
# - Un-groups the data.
linelist |> count(age_cat)

# Counts of two or more grouping columns are returned in “long” format
# with the counts in the n column. 
linelist |> count(age_cat, outcome)


# ** Show all levels ====
# If you are tabling a column of class factor you can ensure that 
# all levels are shown (not just the levels with values in the data) 
# by adding .drop = FALSE into the summarise() or count() command.

# NO CODE.


# ** Proportions ====
# Proportions can be added by piping the table to mutate() to create 
# a new column. Define the new column as the counts column (n by default) 
# divided by the sum() of the counts column (this will return a proportion).
age_summary <- linelist |> 
    count(age_cat) |> 
    mutate(percent = n/sum(n))
age_summary

# To easily display percents, you can wrap the proportion in the 
# function percent() from the scales package.
# NOTE: This converts the percent col to class character.
age_summary <- linelist |> 
    count(age_cat) |> 
    mutate(
        percent = scales::percent(n/sum(n))
    )
age_summary

# Now we learn to calculate proportions for groups within groups.
age_by_outcome <- linelist |> 
    group_by(outcome) |> 
    count(age_cat) |> 
    mutate(
        percent = scales::percent(n / sum(n))
    )
print(age_by_outcome, n = 30)


# ** Plotting ====
# To display a “long” table output like the above with ggplot() is easy.
df_age_outcome <- linelist |> count(age_cat, outcome)
df_age_outcome
# ggplot graph
df_age_outcome |> ggplot() +
    geom_col(aes(x = outcome, y = n, fill = age_cat))


# ** Summary statistics ====


# TBC ####



# 17.5 gtsummary package --------------------------------------------------
# You can use the gtsummary package and its function tbl_summary() to 
# print your summary statistics in a pretty, publication-ready graphic.
# You can also add the results of statistical tests to gtsummary tables.

# ** Summary table ====
# The default behaviour of tbl_summary() is quite incredible - it takes 
# the columns you provide and creates a summary table in one command.
linelist |> 
    select(age_years, gender, outcome, fever, temp, hospital) |> 
    tbl_summary()


# ** Adjustments ====
# Now we learn how the function works and how to make adjustments.
# CASE: Simple example of using statistic arg to print col mean.
linelist |> 
    select(age_years) |> 
    tbl_summary(
        statistic = age_years ~ "{mean}"
    )
# Using the max and min values within parentheses and separated by a comma.
linelist |> 
    select(age_years) |> 
    tbl_summary(
        statistic = age_years ~ "({min}, {max})"
    )

# CASE: Each of the arguments are used to modify the original summary table.
linelist |> 
    
    # keeping only cols of interest
    select(age_years, gender, outcome, fever, temp, hospital) |> 
    
    tbl_summary(
        by = outcome,
        statistic = list(
            all_continuous()    ~ "{mean} ({sd})",
            all_categorical()   ~ "{n} / {N} ({p}%)"
        ),
        digits = all_continuous() ~ 1,
        type = all_categorical() ~ "categorical",
        
        # including outcome for label gives ERROR
        label = list(
            age_years   ~ "Age (years)",
            gender      ~ "Gender",
            temp        ~ "Temperature",
            hospital    ~ "Hospital"
        ),
        missing_text = "Missing"
    )


# ** Multi-line stats for continuous variables ====
# You can print multiple lines of statistics for continuous variables 
# by setting the type = to “continuous2”.
linelist |> 
    select(age_years, temp) |> 
    tbl_summary(
        type = all_continuous() ~ "continuous2",
        statistic = all_continuous() ~ c(
            "{mean} ({sd})",
            "{median} ({p25}, {p75})",
            "{min}, {max}"
        )
    )


# 17.6 base R -------------------------------------------------------------
# You can use the function table() to tabulate and cross-tabulate columns. 
# Unlike the options above, you must specify the dataframe each time you 
# reference a column name.
table(linelist$outcome, useNA = "always")

# Multiple columns can be cross-tabulated by listing them one after the other
# separated by commas. 
age_by_outcome <- table(linelist$age_cat, linelist$outcome, useNA = "always")
age_by_outcome


# ** Proportions ====


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












