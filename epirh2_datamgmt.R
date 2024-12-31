# Practice code from the "Data management" section of the Epi R Handbook
# We have created the sections as per the handbook
# NOTE: Section headers can be entered using ctrl+shift+R or manually typed
# Each section represents one chapter of the handbook


#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C8 - Cleaning data and core functions -----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we see common steps used in the process of “cleaning” a dataset
# Also we see the use of many essential R data management functions.


# 8.1 Cleaning pipeline ---------------------------------------------------
# NO CODE.


# 8.2 Loading the required packages ---------------------------------------
library(pacman)
p_load(
    rio,                            # importing data
    here,                           # relative file pathways
    janitor,                        # data cleaning and tables
    lubridate,                      # working with dates
    matchmaker,                     # dictionary based cleaning
    epikit,                         # age_categories() function
    skimr,                          # summary of dataframe
    tidyverse                       # data management and visualization
)
p_loaded()


# 8.3 Import data -----------------------------------------------------
linelist_raw <- import(here("data_prac", "linelist_raw.xlsx"))
head(linelist_raw, 50)              # checking first 50 data records
View(linelist_raw)                  # data check
skim_without_charts(linelist_raw)   # data summary


# 8.4 Column names --------------------------------------------------------
names(linelist_raw)                     # shows the variable names
# View(linelist_raw$`infection date`)   # shows column with spaces

# ** Cleaning the column names automatically ====
linelist <- linelist_raw |> 
    clean_names()
names(linelist)


# ** Renaming columns manually ====
# UPDATING THE CLEANING PIPE CHAIN
linelist_raw <- import(here("data_prac", "linelist_raw.xlsx"))  # data import
# Cleaning chain start
linelist <- linelist_raw |> 
    
    # standardize the column names
    clean_names() |> 
    
    # manually rename the columns
    rename(
        date_infection = infection_date,
        date_hospitalisation = hosp_date,
        date_outcome = date_of_outcome
    )


# ** Renaming columns via select() ====
# NOTE: select() renames and KEEPS ONLY the columns named in fn
names(linelist_raw)
linelist <- linelist_raw |> 
    select(
        # NEW NAME = # OLD NAME
        date_infection = `infection date`,
        date_hosp = `hosp date`
    )
names(linelist)


# ** Rename by column position ====
names(linelist_raw)
linelist <- linelist_raw |> 
    rename(
        date_infection = 3,
        date_onset = 4
    )
names(linelist)


# 8.5 Select or re-order columns ------------------------------------------
# Use select() from dplyr to retain the required columns. 
# Also used to specify the column order in the data frame.
# We use the linelist data.frame for demonstration
names(linelist)                 # variables in data.frame

# 8.5.1 Keeping the required columns
linelist |> 
    select(case_id, date_onset, date_hospitalisation, fever) |> 
    names()                     # show column names


# 8.5.2 "tidyselect" helper functions
# Moving some variables to beginning
# everything() means all other cols not yet mentioned
linelist |> 
    select(date_onset, date_hospitalisation, everything()) |> 
    names()

# Selecting columns of numeric class
linelist |> 
    select(where(is.numeric)) |> 
    names()

# Selecting columns containing specific characters
linelist |> 
    select(contains("date")) |> 
    names()

# Selecting columns matching multiple characters
linelist |> 
    select(
        matches("onset|hosp|fev")
    ) |> 
    names()

# Selecting using any_of()
linelist |> 
    select(
        any_of(
            c("date_onset", 
              "village_origin", 
              "village_detection", 
              "village_residence", 
              "village_travel")
        )
    ) |> 
    names()

# Removing columns
linelist |> 
    select(
        -c(date_onset, fever:vomit)
    ) |> 
    names()
# Alternatively
linelist$date_onset <-  NULL        # deleting col
names(linelist)

# UPDATING THE CLEANING PIPE CHAIN
# Data import
linelist_raw <- import(here("data_prac", "linelist_raw.xlsx"))
# Cleaning chain start
linelist <- linelist_raw |> 
    
    # standardize the column names
    clean_names() |> 
    
    # manually rename columns
    rename(
        date_infection = infection_date,
        date_hospitalisation = hosp_date,
        date_outcome = date_of_outcome
    ) |> 
    
    # remove the required columns
    select(-c(row_num, merged_header, x28))


# * 8.6 De-duplication ----------------------------------------------------
# Here, we simply delete the duplicate rows/obs in data.frame
nrow(linelist)                          # 6611
linelist <- linelist |> 
    distinct()              # removes duplicate observations
nrow(linelist)                          # 6608

# UPDATING THE CLEANING PIPE CHAIN
# Data import
linelist_raw <- import(here("data_prac", "linelist_raw.xlsx"))
# Cleaning chain start
linelist <- linelist_raw |> 
    
    # standardize the column names
    clean_names() |> 
    
    # manually rename columns
    rename(
        date_infection = infection_date,
        date_hosp = hosp_date,
        date_outcome = date_of_outcome
    ) |> 
    
    # removing columns
    select(-c(row_num, merged_header, x28)) |> 
    
    # de-duplicate
    distinct()


# * 8.7 Column creation and transformation --------------------------------
# 8.7.1 New columns
# Creating a new column with value 10
llmod <- linelist |> 
    mutate(new_col = 10)
names(linelist)                         # check

# New column from other columns
llmod <- linelist |> 
    mutate(bmi = (wt_kg / (ht_cm/100)^2))
names(linelist)                         # check

# Creating multiple new columns
new_col_demo <- linelist |> 
    mutate(
        new_var_dup = case_id,          # new column = duplicate of another col
        new_var_static = 7,             # new column = all the values same
        # overwrite a column. it can also be calculated from another column
        new_var_static = new_var_static + 5,
        # new column = paste together values from another column
        new_var_paste = stringr::str_glue("{hospital} on ({date_hosp})") 
    ) |> 
    # show the new cols
    select(c(case_id, hospital, date_hosp, contains("new")))
names(new_col_demo)                     # check

# Removing the new columns
# NOTE: Does not work as expected
# linelist <- linelist |> 
#     select(-contains("new_var")) |> 
#     names()


# 8.7.2 Convert column class
# Re-run the linelist cleaning pipe chain
skim_without_charts(linelist)
# checking column class
class(linelist$age)             # age is character but should be numeric
class(linelist$date_onset)      # date is character but should be numeric
# Converting age var class to numeric
llmod <- linelist |> 
    mutate(age = as.numeric(age))
class(llmod$age)                # age is converted to numeric


# 8.7.3 Grouped data
# Age normalized to mean of all observations
mod_linelist <- linelist |> 
    mutate(
        m_age = mean(age, na.rm = T)
    ) |> 
    mutate(
        age_norm = age / m_age
    ) |> 
    select(case_id, age, m_age, age_norm)
head(mod_linelist, 10)
# Age normalized to mean of hospital group
mod_linelist <- linelist |> 
    group_by(hospital) |> 
    mutate(
        m_age = mean(age, na.rm = T)
    ) |> 
    mutate(
        age_norm = age / m_age
    ) |> 
    select(case_id, hospital, age, m_age, age_norm)
head(mod_linelist, 10)

    
# 8.7.4 Transform multiple columns
# Converting the multiple columns to character class
llmod <- linelist |> 
    mutate(
        across(
            .cols = c(temp, ht_cm, wt_kg), 
            .fns = as.character
        )
    )
skim_without_charts(llistmod)                  # check

# Changing all cols to character class
llmod <- linelist |> 
    mutate(
        across(
            .cols = everything(), 
            .fns = as.character
        )
    )
skim_without_charts(llmod)                  # check

# Convert to character all columns where the name contains the string “date”    
llmod <- linelist |> 
    mutate(
        across(
            .cols = contains("date"), 
            .fns = as.character
        )
    )
skim_without_charts(llmod)                  # check

# Mutating the columns that are currently class POSIXct
llmod <- linelist |> 
    mutate(
        across(
            .cols = where(is.POSIXct), 
            .fns = as.Date
        )
    )
skim_without_charts(llmod)                   # check


# 8.7.5 Using the coalesce() function
# This fn finds the first non-missing value at each position. 
# It “fills-in” missing values with the first non-miss value -
# - in an order we specify
# Example with vectors
village_detection <- c("a", "b", NA, NA)
village_residence <- c("a", "c", "a", "d")
village <- coalesce(village_detection, village_residence)
village                         # check

# Example with data.frame
vlg_df <- data.frame(village_residence, village_detection)
vlg_df                          # check
# Missing value fill-up
vlg_df <- vlg_df |> 
    mutate(
        village = coalesce(village_detection, village_residence)
    )
vlg_df                          # check


# 8.7.6 Cumulative math
# Example with vectors
sum(c(2, 4, 15, 10))            # returns only one number
cumsum(c(2, 4, 15, 10))         # returns cum sum at each step

# Example with data.frame
# Calculate the cumulative number of cases per day in an outbreak
cum_case_counts <- linelist |> 
    count(date_onset) |>            # count of rows/day as col 'n'
    mutate(cum_cases = cumsum(n))   # new col, cum sum of each row
head(cum_case_counts, 20)

# UPDATING THE CLEANING PIPE CHAIN
# Data import
linelist_raw <- import(here("data_prac", "linelist_raw.xlsx"))
# Cleaning chain start
linelist <- linelist_raw |> 
    
    # standardize the column names
    clean_names() |> 
    
    # manually rename columns
    rename(
        date_infection = infection_date,
        date_hosp = hosp_date,
        date_outcome = date_of_outcome
    ) |> 
    
    # remove columns
    select(-c(row_num, merged_header, x28)) |> 
    
    # de-duplicate
    distinct() |> 
    
    # add new columns
    mutate(bmi = wt_kg / (ht_cm/100)^2) |> 
    
    # convert column classes
    mutate(
        across(
            .cols = contains("date"), 
            .fns = as.Date
        ),
        generation = as.numeric(generation),
        age = as.numeric(age)
    )


# * 8.8 Recode values -----------------------------------------------------
# 8.8.1 Recoding specific values
# Use recode() within mutate()
# Example code - correcting dates
llmod <- linelist |> 
    mutate(date_onset = recode(date_onset, "2014-14-15" = "2014-04-15"))

# Correcting entries in 'hospital' variable    
# Checking unique entries
table(linelist$hospital, useNA = "always")
# Recoding
llmod <- linelist |> 
    mutate(hospital = recode(
        hospital, 
        "Central Hopital" = "Central Hospital",
        "Military Hopital" = "Military Hospital",
        "Mitylira Hopital" = "Military Hospital",
        "Mitylira Hospital" = "Military Hospital",
        "Port Hopital" = "Port Hospital",
        "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
        )
    )
# Re-checking unique values
table(llmod$hospital, useNA = "always")


# 8.8.2 Recoding by simple logic
# Using replace() fn
# Example - changing gender of one specific obs
llmod <- linelist |> 
    mutate(gender = replace(gender, case_id == "2195", "Female"))
# Alt syntax - base R
llmod <- linelist
llmod$gender[llmod$case_id == "2195"] <- "Female"

# Using ifelse()
tabyl(linelist$source)                      # var check
llmod <- linelist |> 
    mutate(
        source_known = ifelse(
            !is.na(source),
            "known",
            "unknown"
        )
    )
tabyl(llmod$source_known)                   # var check

# Using if_else()
# if_else() is a special version from dplyr that handles dates
# Create a date of death column, which is NA if patient has not died
# NOTE: CODE DOES NOT WORK
# llmod <- linelist |> 
#     mutate(
#         date_death = if_else(
#             outcome == "Death",
#             date_outcome,
#             NA_real_
#         )
#     )
# tabyl(llmod$date_death)


# 8.8.3 Recoding by complex logic
# Here we use case_when()
# Example - Using the cols age and age_unit to create a col age_years
llmod <- linelist |> 
    mutate(
        age_years = case_when(
            age_unit == "years" ~ age,
            age_unit == "months" ~ age/12,
            is.na(age_unit) ~ age
        )
    )
summary(llmod$age_years)

# Example - creating a patient classification column
llmod <- linelist |> 
    mutate(
        case_status = case_when(
            ct_blood < 20 ~ "Confirmed",
            !is.na(source) & fever == "yes" ~ "Suspect",
            TRUE ~ "To investigate"
        )
    )
tabyl(llmod$case_status)


# 8.8.4 Recoding missing values
# replace_na() - to change missing values to a specific value
tabyl(linelist$hospital)
llmod <- linelist |> 
    mutate(
        # changing NA to "Missing" in hospital col
        hospital = replace_na(hospital, "Missing")
    )
tabyl(llmod$hospital)

# fct_explicit_na() - It converts a col to class factor - 
# - and changes NA values to the character “(Missing)”
llmod <- linelist |> 
    mutate(
        hospital = fct_explicit_na(hospital)
    )
tabyl(llmod$hospital)

# na_if() - convert a specific value to NA
llmod <- linelist |> 
    mutate(
        # changing NA to "Missing" in hospital col
        hospital = replace_na(hospital, "Missing")
    )
tabyl(llmod$hospital)                # var check
# Reverting the change
llmod <- llmod |> 
    mutate(
        hospital = na_if(hospital, "Missing")
    )
tabyl(llmod$hospital)                # var check

# Creating missing values using logical criteria
# Example 1 - Convert temperatures above 40 to NA
llmod <- linelist |> 
    mutate(
        temp = replace(temp, temp > 40, NA)
    )
summary(llmod$temp)
# Example 2 - Convert onset dates earlier than 1 Jan 2000 to NA
llmod <- linelist |> 
    mutate(
        date_onset = replace(
            date_onset, 
            date_onset > as.Date("2000-01-01"),
            NA
        )
    )


# 8.8.5 Cleaning dictionary
# NOT DONE

# UPDATING THE CLEANING PIPE CHAIN
# Data import
linelist_raw <- import(here("data_prac", "linelist_raw.xlsx"))
# Cleaning chain start
linelist <- linelist_raw |> 
    
    # standardize the column names
    clean_names() |> 
    
    # manually rename columns
    rename(
        date_infection = infection_date,
        date_hosp = hosp_date,
        date_outcome = date_of_outcome
    ) |> 
    
    # remove columns
    select(-c(row_num, merged_header, x28)) |> 
    
    # de-duplicate
    distinct() |> 
    
    # add new columns
    mutate(bmi = wt_kg / (ht_cm/100)^2) |> 
    
    # convert column classes
    mutate(
        across(
            .cols = contains("date"), 
            .fns = as.Date
        ),
        generation = as.numeric(generation),
        age = as.numeric(age)
    ) |> 
    
    # add column: delay to hospitalisation
    mutate(
        days_onset_hosp = as.numeric(date_hosp - date_onset)
    ) |> 
    
    # clean values of hospital column
    mutate(
        hospital = recode(
            hospital,
            "Mitylira Hopital" = "Military Hospital",
            "Mitylira Hospital" = "Military Hospital",
            "Military Hopital" = "Military Hospital",
            "Port Hopital" = "Port Hospital",
            "Central Hopital" = "Central Hospital",
            "other" = "Others",
            "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital"
        )
    ) |> 
    
    # Re-assigning missing value in one var
    mutate(
        hospital = replace_na(hospital, "Missing")
    ) |> 
    
    # Create age_years col from age and age_unit
    mutate(
        age_years = case_when(
            age_unit == "years" ~ age,
            age_unit == "months" ~ age/12,
            is.na(age_unit) ~ age,
            TRUE ~ NA_real_
        )
    )


# * 8.9 Numeric categories ------------------------------------------------
# 8.9.1 Review distribution
# Here we will create an age_cat column using the age_years column
class(linelist$age_years)                   # class check
# Examine distribution
ggplot(data = linelist, aes(x = age_years)) +
    geom_histogram(binwidth = 5)
summary(linelist$age_years, na.rm = T)


# 8.9.2 Using age_categories() from epikit package
# library(epikit)                           # load package
# Example 1 - Manually splitting into age groups
llmod <- linelist |> 
    mutate(
        age_cat = age_categories(
            age_years,                      # numeric col to make groups from
            breakers = c(0, 5, 10, 15,      # break points
                    20, 30, 40, 50, 60, 70)
        )
    )
tabyl(llmod$age_cat)                        # table check

# Example 2 - includes upper end of same categories
llmod <- linelist |> 
    mutate(
        age_cat = age_categories(
            age_years,
            breakers = c(0, 6, 11, 16, 21, 31, 41, 51, 61, 71)
        )
    )
tabyl(llmod$age_cat)                        # table check

# Example 3 - no open-ended cat with ceiling set to TRUE
llmod <- linelist |> 
    mutate(
        age_cat = age_categories(
            age_years,
            breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70),
            ceiling = TRUE      # 70 is ceiling and values above becomes NA
        )
    )
tabyl(llmod$age_cat)                        # table check

# Example 4 - uniform categories instead of manual breakers
llmod <- linelist |> 
    mutate(
        age_cat = age_categories(
            age_years,
            lower = 0,
            upper = 100,
            by = 10
        )
    )
tabyl(llmod$age_cat)                        # table check


# 8.9.3 Using cut() from base R
# It is a base R alternative to age_categories()
# Example 1 - Manually splitting into age groups
llmod <- linelist |> 
    mutate(
        age_cat = cut(
            age_years,
            breaks = c(0, 5, 10, 15, 20, 30, 50, 70, 100),
            include.lowest = TRUE           # include 0 in lowest group
        )
    )
tabyl(llmod$age_cat)                        # table check
# Checking by cross-tabulation
table(
    # Table axis titles specified
    "Age values" = llmod$age_years, 
    "Age categories" = llmod$age_cat, 
    useNA = "always"                        # missing values included
)

# Example 2 - Relabelling NA values
llmod <- linelist |> 
    mutate(
        # create the factor var using cut()
        age_cat = cut(
            age_years,
            breaks = c(0, 5, 10, 15, 20, 30, 50, 70, 100),
            right = FALSE,
            include.lowest = TRUE,
            labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-49", 
                       "50-69", "70-100")
        ),
        
        # make missing values explicit
        age_cat = fct_explicit_na(
            age_cat,
            na_level = "Mising age"     # label for NA values
        )
    )
tabyl(llmod$age_cat)                        # table check

# Example 3 - Quickly make breaks and labels
# Make break points from 0 to 90 by 5
age_seq = seq(from = 0, to = 90, by = 5)
age_seq
# Make labels for the above categories, assuming default cut() settings
age_labels = paste0(age_seq + 1, "-", age_seq + 5)
age_labels
# check that both vectors are the same length
length(age_seq) == length(age_labels)


# 8.9.4 Quantile breaks
# Example 1 - Getting quantile break points for age
quantile(
    llmod$age_years,
    probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95),
    na.rm = T
)

# Example 2 - Creating age cats using quantile breaks
llmod <- llmod |> 
    mutate(
        age_deciles = cut(
            age_years,
            breaks = quantile(
                age_years,
                probs = seq(from = 0, to = 1, by = 0.1),
                na.rm = T
            ),
            include.lowest = T
        )
    )
tabyl(llmod$age_deciles)                        # table check


# 8.9.5 Evenly-sized groups
# make groups with ntile()
ntile_data <- linelist |> 
    mutate(
        even_groups = ntile(age_years, 10)
    )
# make table of counts and proportions by group
ntile_table <- ntile_data |> 
    tabyl(even_groups)
ntile_table                                     # check
# attach min/max values to demonstrate ranges
ntile_ranges <- ntile_data |> 
    group_by(even_groups) |> 
    summarise(
        min = min(age_years, na.rm = T),
        max = max(age_years, na.rm = T)
    )
ntile_ranges                                    # check
# combine and print table
# NOTE: Values are present in multiple groups
left_join(ntile_table, ntile_ranges, by = "even_groups")


# 8.9.6 Using case_when()
# NO CODE


# UPDATING THE CLEANING PIPE CHAIN
# Load the required packages
# Data import
linelist_raw <- import(here("data_prac", "linelist_raw.xlsx"))
# Cleaning chain start
linelist <- linelist_raw |> 
    
    # standardize the column names
    clean_names() |> 
    
    # manually rename columns
    rename(
        # NEW NAME          # OLD NAME
        date_infection = infection_date,
        date_hosp = hosp_date,
        date_outcome = date_of_outcome
    ) |> 
    
    # remove columns
    select(-c(row_num, merged_header, x28)) |> 
    
    # de-duplicate
    distinct() |> 
    
    # add new columns
    mutate(bmi = wt_kg / (ht_cm/100)^2) |> 
    
    # convert column classes
    mutate(
        across(
            .cols = contains("date"), 
            .fns = as.Date
        ),
        generation = as.numeric(generation),
        age = as.numeric(age)
    ) |> 
    
    # add column: delay to hospitalisation
    mutate(
        days_onset_hosp = as.numeric(date_hosp - date_onset)
    ) |> 
    
    # clean values of hospital column
    mutate(
        hospital = recode(
            hospital,
            # OLD NAME              # NEW NAME
            "Mitylira Hopital" = "Military Hospital",
            "Mitylira Hospital" = "Military Hospital",
            "Military Hopital" = "Military Hospital",
            "Port Hopital" = "Port Hospital",
            "Central Hopital" = "Central Hospital",
            "other" = "Others",
            "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital"
        )
    ) |> 
    
    # Re-assigning missing value in one var
    mutate(
        hospital = replace_na(hospital, "Missing")
    ) |> 
    
    # Create age_years col from age and age_unit
    mutate(
        age_years = case_when(
            age_unit == "years" ~ age,
            age_unit == "months" ~ age/12,
            is.na(age_unit) ~ age,
            TRUE ~ NA_real_
        )
    ) |> 
    
    # Create age categories var
    mutate(
        # Custom age cats
        age_cat = age_categories(
            age_years,
            breakers = c(0, 5, 10, 15, 20, 30, 50, 70)
        ),
        
        # Age cats from 0-85 by 5 years
        age_cat5 = age_categories(
            age_years,
            breakers = seq(from = 0, to = 85, by = 5)
        )
    )


# * 8.10 Add rows ---------------------------------------------------------
# 8.10.1 Adding rows one-by-one
# We can add rows manually one-by-one using add_row() from dplyr pkg
# NOTE: Each col must contain values of only one class 
# (either character, numeric, logical, etc.)
# Example code - does not work
llmod <- linelist |> 
    add_row(
        row_num = 666,
        case_id = "abc",
        generation = 4,
        `infection date` = as.Date("2020-10-10"),
        .before = 2
    )


# 8.10.2 Bind rows
# NO CODE


# 8.11 Filter rows --------------------------------------------------------
# 8.11.1 Simple filter
nrow(linelist)                      # Before filter: 6608
llmod <- linelist |> 
    filter(gender == "f")           # keep only rows with "f" gender
nrow(llmod)                         # After filter: 3139


# 8.11.2 Filter out missing values
llmod <- linelist |> 
    drop_na(case_id, age_years)     # drop rows with mv for case_id and age_years
nrow(llmod)                         # After filter: 6377


# 8.11.3 Filter by row number
# View first 10 rows
linelist |> head(10)
# View last 10 rows
linelist |> tail(10)
# Show row 5 only
linelist |> filter(row_number() == 5)
# View rows 2-20 and three specific columns
linelist |> 
    filter(row_number() %in% 2:20) |> 
    select(date_onset, outcome, age)
# NOTE: You can also convert the row numbers to a true column 
# Pipe your dataframe to the tibble function rownames_to_column() 
# (do not put anything in the parentheses).


# 8.11.4 Complex filter
# Examining the data by graph plot
ggplot(data = linelist, aes(x = date_onset)) +
    geom_histogram(bins = 50, colour = "black", fill = "cornsilk")
# Alt graph - Base R
hist(linelist$date_onset, breaks = 50)

# Designing the filter
# Examine a cross-tabulation to make sure we exclude only the correct rows
table(
    Hospital = linelist$hospital,                   # hospital name
    Onset_year = year(linelist$date_onset),         # year of date_onset
    useNA = "always"                                # show missing values
)
# We see that
# The first epidemic in 2012 & 2013 occurred at Hospital A, Hospital B
# - and that there were also 10 cases at Port Hospital.
# Hospitals A & B did not have cases in second epidemic, but Port Hospital did.

nrow(linelist)                      # Before filter: 6608
# Filter statement
llmod <- linelist |> 
    filter(
        date_onset > as.Date("2013-06-01") | (is.na(date_onset) 
                        & (!hospital %in% c("Hospital A", "Hospital B")))
    )
nrow(llmod)                         # After filter: 6019

# Cross-tabulation check
# keep rows where onset is after 1 June 2013 OR where onset is missing
# - and it was a hospital OTHER than Hospital A or B.
table(
    Hospital = llmod$hospital,                   # hospital name
    Onset_year = year(llmod$date_onset),         # year of date_onset
    useNA = "always"                             # show missing values
)


# 8.11.5 Standalone
# standalone cmd using dplyr
nrow(linelist)                      # Before filter: 6608
llmod <- filter(linelist, !is.na(case_id))
nrow(llmod)                         # After filter: 6473

# base R filter cmd
nrow(linelist)                      # Before filter: 6608
llmod <- linelist[!is.na(linelist$case_id), ]
nrow(llmod)                         # After filter: 6473


# 8.11.6 Quickly review records
# Quickly viewing data.frame
View(linelist)
# With dplyr functions filter() and select()
View(
    linelist |> 
        filter(case_id %in% c("11f8ea", "76b97a", "47a5f5")) |> 
        select(case_id, date_onset, date_hosp)
)


# UPDATING THE CLEANING PIPE CHAIN
# Load the required packages beforehand
# Data import
linelist_raw <- import(here("data_prac", "linelist_raw.xlsx"))
# Cleaning chain start
linelist <- linelist_raw |> 
    
    # standardize the column names
    clean_names() |> 
    
    # manually rename columns
    rename(
        # NEW NAME          # OLD NAME
        date_infection = infection_date,
        date_hosp = hosp_date,
        date_outcome = date_of_outcome
    ) |> 
    
    # remove columns
    select(-c(row_num, merged_header, x28)) |> 
    
    # de-duplicate
    distinct() |> 
    
    # add new columns
    mutate(bmi = wt_kg / (ht_cm/100)^2) |> 
    
    # convert column classes
    mutate(
        across(
            .cols = contains("date"), 
            .fns = as.Date
        ),
        generation = as.numeric(generation),
        age = as.numeric(age)
    ) |> 
    
    # add column: delay to hospitalisation
    mutate(
        days_onset_hosp = as.numeric(date_hosp - date_onset)
    ) |> 
    
    # clean values of hospital column
    mutate(
        hospital = recode(
            hospital,
            # OLD NAME              # NEW NAME
            "Mitylira Hopital" = "Military Hospital",
            "Mitylira Hospital" = "Military Hospital",
            "Military Hopital" = "Military Hospital",
            "Port Hopital" = "Port Hospital",
            "Central Hopital" = "Central Hospital",
            "other" = "Others",
            "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital"
        )
    ) |> 
    
    # Re-assigning missing value in one var
    mutate(
        hospital = replace_na(hospital, "Missing")
    ) |> 
    
    # Create age_years col from age and age_unit
    mutate(
        age_years = case_when(
            age_unit == "years" ~ age,
            age_unit == "months" ~ age/12,
            is.na(age_unit) ~ age,
            TRUE ~ NA_real_
        )
    ) |> 
    
    # Create age categories var
    mutate(
        # Custom age cats
        age_cat = age_categories(
            age_years,
            breakers = c(0, 5, 10, 15, 20, 30, 50, 70)
        ),
        
        # Age cats from 0-85 by 5 years
        age_cat5 = age_categories(
            age_years,
            breakers = seq(from = 0, to = 85, by = 5)
        )
    ) |> 
    
    filter(
        # keep only rows with non-missing case_id
        !is.na(case_id),
        
        # keep only rows from second outbreak
        date_onset > as.Date("2013-06-01") | (is.na(date_onset) 
                            & !hospital %in% c("Hospital A", "Hospital B"))
    )


# 8.12 Row-wise calculations ----------------------------------------------
# Calculating the number of symptoms of each person
llmod <- linelist |> 
    rowwise() |> 
    mutate(
        num_symptoms = sum(
            c(fever, chills, cough, aches, vomit) == "yes"
        )
    ) |> 
    ungroup()
# For display
llmod |> 
    select(case_id, fever, chills, cough, aches, vomit, num_symptoms) |> 
    head(10)

# Example - Row-wise operation using tidyselect helper functions
# Counts the number of cols with missing date
llmod <- linelist |> 
    rowwise() |> 
    mutate(
        num_NA_dates = sum(
            is.na(c_across(contains("date")))
        )
    ) |> 
    ungroup()
# For display
llmod |> 
    select(case_id, num_NA_dates, contains("date")) |> 
    head(10)

# Example - Using other functions like max() to get most recent date
llmod <- linelist |> 
    rowwise() |> 
    mutate(
        latest_date = max(
            c_across(contains("date"))
            , na.rm = T
        )
    ) |> 
    ungroup()
# For display
llmod |> 
    select(case_id, contains("date")) |> 
    head(10)


# 8.13 Arrange and sort ---------------------------------------------------
# Use the dplyr function arrange() to sort or order the rows by column values
# Example - sort the rows by "hospital", then by "date_onset" in desc order
# Original data check
linelist |> 
    select(case_id, hospital, contains("date")) |> 
    head(10)
# Sorting data
llmod <- linelist |> 
    arrange(hospital, desc(date_onset))
# Sort check
llmod |> 
    select(case_id, hospital, contains("date")) |> 
    head(10)



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C9 - Working with dates -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we offer some tools and examples to learn working with dates in R
# NOTE:
# It is important to make R recognize when a column contains dates.
# Dates are an object class and can be tricky to work with.
# Here we present several ways to convert date columns to Date class.


# 9.1 Loading the required packages and datasets --------------------------
# Loading Packages
library(pacman)
p_load(
    rio,                            # importing data
    here,                           # relative file pathways
    skimr,                          # summary of dataframe
    tidyverse,                      # data management and visualization
    janitor,                        # data cleaning and tables
    lubridate,                      # general pkg for working with dates
    parsedate,                      # fns to guess messy dates
    aweek,                          # alter pkg for converting dates to weeks
    zoo                             # additional date/time fns
)
p_loaded()

# Loading the data
linelist <- import(
    here("data_prac", "linelist_cleaned.xlsx")
)
skim_without_charts(linelist)       # Data summary


# 9.2 Current date --------------------------------------------------------
# Example - with Base R
# get the system date - this is a DATE class
Sys.Date()
# get the system time - this is a DATETIME class
Sys.time()

# Example with lubridate pkg
today()             # current system date
now()               # current system time
date()              # current system date and time

# TBC ####



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C11 - Factors -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# In R, factors are a class of data that allow for ordered categories with 
# a fixed set of acceptable values. 
# Typically, you would convert a col from character or numeric class to a 
# factor if you want to set an intrinsic order to the values (“levels”) so 
# they can be displayed non-alphabetically in plots and tables. Another 
# common use of factors is to standardise the legends of plots so they 
# do not fluctuate if certain values are temporarily absent from the data.


# 11.1 Preparation --------------------------------------------------------

# ** Load packages ====
# Here we load packages required for analysis
library(easypackages)
libraries(
    "rio", 
    "here", 
    "lubridate", 
    "forcats", 
    "aweek", 
    "janitor", 
    "tidyverse"
)


# ** Import data ====
# We import the dataset of cases from a simulated Ebola epidemic.
linelist <- import(here("data_prac", "linelist_cleaned.rds"))
glimpse(linelist)


# ** New categorical variable ====
# For demonstration in this page we will use a common scenario - the creation 
# of a new categorical variable.
# NOTE: If you convert a numeric col to class factor, you will not be able 
# to calculate numeric statistics on it.

# Create column
# We use the existing column days_onset_hosp (days from symptom onset to 
# hospital admission) and create a new col delay_cat by classifying each 
# row into one of several categories.
linelist <- linelist |> 
    mutate(
        delay_cat = case_when(
            days_onset_hosp < 2                             ~ "<2 days",
            days_onset_hosp >= 2 & days_onset_hosp < 5      ~ "2-5 days",
            days_onset_hosp >= 5                            ~ ">5 days",
            is.na(days_onset_hosp)                          ~ NA_character_,
            TRUE                                            ~ "check me"
        )
    )
# The new col delay_cat is a categorical col of class Character - not yet 
# a factor. Thus, in a frequency table, we see that the unique values appear
# in a default alpha-numeric order that does not make intuitive sense
class(linelist$delay_cat)
table(linelist$delay_cat, useNA = "always")
# Likewise, if we make a bar plot, the values also appear in this order 
# on the x-axis.
ggplot(data = linelist) +
    geom_bar(aes(x = delay_cat))


# 11.2 Convert to factor --------------------------------------------------
# To convert a character or numeric col to class factor, you can use any 
# fn from the forcats pkg. For example using fct_relevel() lets you manually 
# specify the level order. The fn as_factor() simply converts the class 
# without any further capabilities.
linelist <- linelist |> 
    mutate(delay_cat = fct_relevel(delay_cat))
# The unique “values” in this col are now considered “levels” of the factor. 
# The levels have an order, which can be printed with the base R fn levels(), 
# or alternatively viewed in a count table via table() from base R or tabyl() 
# from janitor. By default, the order of the levels will be alpha-numeric.
levels(linelist$delay_cat)
linelist |> tabyl(delay_cat)

# fct_relevel() has the additional utility of allowing you to manually 
# specify the level order. Simply write the level values in order, in 
# quotation marks, separated by commas, as shown below.
linelist <- linelist |> 
    mutate(
        delay_cat = fct_relevel(delay_cat, "<2 days", "2-5 days", ">5 days")
    )
# NOTE: The spelling must exactly match the values.
# Now see that the levels are ordered.
levels(linelist$delay_cat)
# Now the plot order makes more intuitive sense as well.
ggplot(data = linelist, aes(x = delay_cat)) +
    geom_bar()


# 11.3 Add or drop levels -------------------------------------------------

# ** Add ====
# You can add levels to a factor using fct_expand(). Just write the 
# col name followed by the new levels (separated by commas).
linelist |> tabyl(delay_cat)
linelist |> 
    mutate(
        delay_cat = fct_expand(delay_cat, 
                               "Not admitted to hospital",
                               "Transfer to other jurisdiction")
    ) |> 
    
    # print table
    tabyl(delay_cat)

# ** Drop ====
# If you use fct_drop(), the “unused” levels with zero counts 
# will be dropped from the set of levels.
linelist |> 
    mutate(delay_cat = fct_drop(delay_cat)) |> 
    tabyl(delay_cat)


# 11.4 Adjust level order -------------------------------------------------
# The package forcats offers useful functions to easily adjust the order 
# of a factor’s levels (after a column been defined as class factor).

# ** Manually ====
# fct_relevel() is used to manually order the factor levels. If used on 
# a non-factor column, the column will first be converted to class factor.
linelist <- linelist |> 
    mutate(
        delay_cat = fct_relevel(delay_cat, 
                                c("<2 days", "2-5 days", ">5 days"))
    )
# If you only want to move one level, you can specify it to fct_relevel() 
# alone and give a number to the after = argument to indicate where in 
# the order it should be.
linelist |> 
    mutate(
        delay_cat = fct_relevel(
            delay_cat, "<2 days", after = 1
        )
    ) |> 
    tabyl(delay_cat)


# ** Within a plot ====
# The forcats cmds “wrap around” the column name within the ggplot() 
# plotting command. You can reverse/relevel/etc. the transformation will 
# only apply within that plot.
# CASE: ggplot with no adjustment.
ggplot(data = linelist) +
    geom_bar(aes(x = delay_cat))
# CASE: ggplot with factor level order adjusted.
ggplot(data = linelist) +
    geom_bar(
        aes(x = fct_relevel(delay_cat, c("<2 days", "2-5 days", ">5 days")))
    )


# ** Reverse ====
# NO CODE.


# ** By frequency ====
# To order by frequency that the value appears in the data, use fct_infreq(). 
# Any missing values (NA) will automatically be included at the end, unless 
# they are converted to an explicit level.
# CASE: ggplot ordered by frequency.
ggplot(data = linelist, aes(x = fct_infreq(delay_cat))) +
    geom_bar() +
    labs(
        x = "Delay onset to admission (days)",
        title = "Ordered by frequency" 
    )
# CASE: reversed frequency.
ggplot(data = linelist, aes(x = fct_rev(fct_infreq(delay_cat)))) +
    geom_bar() +
    labs(
        x = "Delay onset to admission (days)",
        title = "Reverse of order by frequency"
    )


# ** By appearance ====
# NO CODE.


# ** By summary statistic of another column ====
# You can use fct_reorder() to order the levels of one col by a summary 
# statistic of another col. Visually, this can result in pleasing plots 
# where the bars/points ascend or descend steadily across the plot.

# CASE: boxplots ordered by original factor levels
ggplot(data = linelist) +
    geom_boxplot(
        aes(x = delay_cat,
            y = ct_blood,
            fill = delay_cat)
    ) +
    labs(
        x = "Delay onset to admission (days)",
        y = NULL,
        title = "Ordered by original alpha-numeric levels",
    ) +
    theme_classic() +
    theme(legend.position = "none")

# CASE: boxplots ordered by median CT value
ggplot(data = linelist) +
    geom_boxplot(
        aes(x = fct_reorder(delay_cat, ct_blood, "median"), 
            y = ct_blood, 
            fill = delay_cat)
    ) +
    labs(
        x = "Delay onset to admission (days)",
        y = NULL,
        title = "Ordered by median CT value in group"
    ) +
    theme_classic() +
    theme(legend.position = "none")


# ** By "end" value =====
# fct_reorder2() orders the levels (and therefore the legend) to align with 
# the vertical ordering of the lines at the “end” of the plot. Technically 
# speaking, it “orders by the y-values associated with the largest x values.”
epidemic_data <- linelist |> 
    # cut-off date for visual clarity
    filter(date_onset < as.Date("2014-09-21")) |> 
    
    # get case count per week and by hospital
    count(
        epiweek = lubridate::floor_date(date_onset, unit = "week"),
        hospital
    )
glimpse(epidemic_data)

ggplot(data = epidemic_data) +
    geom_line(
        aes(
            x = epiweek, y = n, 
            colour = fct_reorder2(hospital, epiweek, n)
        ),
        linewidth = 1
    ) +
    labs(
        title = "Factor levels (and legend) by line height at end of plot",
        colour = "Hospital"
    ) +
    theme(legend.position = "bottom")


# 11.5 Missing values -----------------------------------------------------
# If you have NA values in your factor column, you can easily convert them 
# to a named level such as “Missing” with fct_explicit_na().
# NOTE: fct_explicit_na() was deprecated.
# SOL: We use fct_na_value_to_level() instead
linelist |> 
    mutate(
        delay_cat = fct_na_value_to_level(delay_cat, 
                                          level = "Missing delay")
    ) |> 
    tabyl(delay_cat)


# 11.6 Combine levels -----------------------------------------------------
# Let's go!

# ** Manually ====
# You can adjust the level displays manually manually with fct_recode().
# fct_recode() has a different syntax than recode(). 
# recode() uses OLD = NEW, whereas fct_recode() uses NEW = OLD.

# The current levels of delay_cat are:
levels(linelist$delay_cat)
# The new levels are created using syntax 
# fct_recode(column, "new" = "old", "new" = "old", "new" = "old"):
linelist |> 
    mutate(
        delay_cat = fct_recode(
            delay_cat,
            "Less than 2 days" = "<2 days",
            "2 to 5 days" = "2-5 days",
            "More than 5 days" = ">5 days"
        )
    ) |> 
    tabyl(delay_cat)
# Now we manually combine the levels
linelist |> 
    mutate(
        delay_cat = fct_recode(
            delay_cat,
            "Less than 5 days" = "<2 days",
            "Less than 5 days" = "2-5 days",
            "More than 5 days" = ">5 days"
        )
    ) |> 
    tabyl(delay_cat)


# ** Reduce into "Other" ====
# You can use fct_other() to manually assign factor levels to “Other” level.
# Below, all levels in the column hospital, aside from “Port Hospital” 
# and “Central Hospital”, are combined into “Other”.  
linelist |> tabyl(hospital)
linelist |> mutate(
    hospital = fct_other(hospital,
        keep = c("Port Hospital", "Central Hospital"),  # keep these separate
        other_level = "Other Hospital"      # all others as "Other Hospital"
    )
) |> 
tabyl(hospital)    


# ** Reduce by frequency ====
# The least-frequent factor levels can be combined using fct_lump().
# To “lump” together many low-frequency levels into an “Other” group
# do one of the following:
# 1. Set n = as the number of groups you want to keep. The n most-frequent 
#    levels will be kept, and all others will combine into “Other”.
# 2. Set prop = as the threshold frequency proportion for levels above which
#    you want to keep. All other values will combine into “Other”.
linelist |> 
    mutate(
        hospital = fct_lump(
            hospital, 
            n = 2, other_level = "Other Hospital"
        )
    ) |> 
    tabyl(hospital)


# 11.7 Show all levels ----------------------------------------------------
# Benefit of using factors is to standardise the appearance of plot legends 
# and tables, regardless of which values are actually present in a dataset.

# ** In plots ====
ggplot(data = linelist) +
    geom_bar(aes(x = hospital, fill = age_cat)) +
    # show all age groups in the legend, even those that are not present
    scale_fill_discrete(drop = F) +
    labs(
        title = "All age groups will appear in legend", 
        subtitle = "even if not present in data"
    )

# ** In tables ====
# NO CODE.


# 11.8 Epiweeks -----------------------------------------------------------
# Please see the extensive discussion of how to create 
# epidemiological weeks in the Grouping data page.
# Please also see the Working with dates page for tips on how to 
# create and format epidemiological weeks.

# NOT RELEVANT NOW. WILL DO IT WHEN REQUIRED.



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C12 - Pivoting data -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we learn about the conversion of a table from long to wide format
# or vice versa. 
# It is useful to be aware of the idea of “tidy data” in which 
# - each variable has it’s own column, 
# - each observation has it’s own row, 
# - and each value has it’s own cell.


# 12.1 Preparation --------------------------------------------------------

# ** Load packages ====
# Here we load packages required for analysis.
# install.packages("kableExtra")
library(easypackages)
libraries(
    "rio",
    "here",
    "flextable",
    "janitor",
    "kableExtra",
    "knitr",
    "tidyverse"
)


# ** Import data ====
# First we import the malaria count data, fictional dataset of 
# daily malaria cases, by facility and age group.
count_data <- import(here("data_prac", "malaria_facility_count_data.rds"))

# Import the linelist data which are simulated cases of Ebola epidemic.
linelist <- import(here("data_prac", "linelist_cleaned.rds"))


# 12.2 Wide-to-long -------------------------------------------------------

# ** "Wide" format ====
# In a wide format data a subject’s characteristics or responses are stored 
# in a single row. While this may be useful for presentation, it is not 
# ideal for some types of analysis.

# Now we check the malaria count data.
glimpse(count_data)
count_data |> view()
count_data$data_date |> min()
count_data$data_date |> max()

# Visualising the total malaria counts over time. We can do this with 
# the current wide format data.
ggplot(data = count_data, aes(x = data_date, y = malaria_tot)) +
    geom_col(width = 1)
# However, what if we wanted to display the relative contributions of each 
# age group to this total count? In this case, we need to ensure that the 
# variable of interest (age group), appears in the dataset in a single column 
# that can be passed to {ggplot2}’s “mapping aesthetics” aes() argument.

# We can do so with the tidyr function pivot_longer() makes data “longer”.


# ** Standard pivoting ====
df_long <- count_data |> 
    pivot_longer(
        cols = c(`malaria_rdt_0-4`, `malaria_rdt_5-14`, `malaria_rdt_15`, `malaria_tot`)
    )
df_long
# NOTE: The dataset dimensions have changed. Num of rows have increased and 
# num of columns have decreased.
dim(count_data)
dim(df_long)

# Altly, since the names of these four columns all begin with the 
# prefix malaria_, we could have made use of the handy “tidyselect” 
# function starts_with() to achieve the same result.
df_long <- count_data |>
    pivot_longer(cols = starts_with("malaria_"))
glimpse(count_data)
glimpse(df_long)
# Altly, we could have referred to cols by position.
count_data |> pivot_longer(cols = 6:9)
# Altly, we could have referred by named range.
count_data |> pivot_longer(cols = "malaria_rdt_0-4":"malaria_tot")

# The two new cols are given the default names of name and value, but we can
# provide more meaningful names, using the names_to and values_to arguments.
df_long <- count_data |> 
    pivot_longer(
        cols = "malaria_rdt_0-4":"malaria_tot",
        names_to = "age_group",
        values_to = "counts"
    )
df_long

# We can now graph the bar chart by using this new dataset and map the 
# new col count to the y-axis and new col age_group to the fill = argument.
ggplot(data = df_long) +
    geom_col(
        mapping = aes(x = data_date, y = counts, fill = age_group),
        width = 1
    )
# NOTE: There is something wrong in this plot.
# We have also included the total counts from the malaria_tot column 
# so the magnitude of each bar in the plot is twice as high as it should be.

# One way to handle this is to filter the data before plotting.
df_long |> filter(age_group != "malaria_tot") |> 
    ggplot(aes(x = data_date, y = counts, fill = age_group)) +
    geom_col(width = 1)
# Altly, we could have excluded malaria_tot when we ran pivot_longer.
count_data |> pivot_longer(
    cols = "malaria_rdt_0-4":"malaria_rdt_15",
    names_to = "age_group",
    values_to = "counts"
)


# ** Pivoting data of multiple classes ====
# One particularly common problem you will encounter will be the need 
# to pivot columns that contain different classes of data. This pivot 
# will result in storing these different data types in a single column
# which is not a good situation.
# There is an important step you can take using pivot_longer() 
# to avoid creating such a situation yourself.
df <- tribble(
    ~id,    ~obs1_date,     ~obs1_status,   ~obs2_date,     ~obs2_status,   ~obs3_date,     ~obs3_status,
    "A",	"2021-04-23",	"Healthy",	    "2021-04-24",	"Healthy",	    "2021-04-25",	"Unwell",
    "B",	"2021-04-23",	"Healthy",	    "2021-04-24",	"Healthy",	    "2021-04-25",	"Healthy",
    "C",	"2021-04-23",	"Missing",	    "2021-04-24",	"Healthy",	    "2021-04-25",	"Healthy"
)
df

# In order to work with these data, we need to transform the dataframe 
# to long format, but keeping the separation between a date column and 
# a character (status) column, for each observation for each item.
df |> 
    pivot_longer(
        cols = -id,
        names_to = c("observation")
    )
# Above, our pivot has merged dates and characters into a single value column.
# The utility of dates are lost.

# SOL: We can leverage names_sep arg to keep these two data types 
# in separate columns after the pivot.
df_long <- df |> 
    pivot_longer(
        cols = -id,
        names_to = c("observation", ".value"),
        names_sep = "_"
    )
df_long
# We further refine the code to set appropriate column types.
df_long <- df_long |> 
    mutate(
        date = as_date(date),
        observation = observation |> str_remove_all("obs") |> as.numeric()
    )
df_long

# Now, we can work with the data, starting by plotting a heat tile.
ggplot(data = df_long, mapping = aes(x = date, y = id, fill = status)) +
    geom_tile(colour = "black") +
    scale_fill_manual(
        values = c(
            "Healthy" = "green",
            "Unwell" = "red",
            "Missing" = "grey"
        )
    )


# 12.3 Long-to-wide -------------------------------------------------------
# Sometimes, we may wish to convert a dataset to a wider format. 
# For this, we can use the pivot_wider() function.
# CASE: When we want to transform the results of an analysis into a format 
# which is more digestible for the reader like, a Table for presentation.

# ** Data ====
# In this section we will use the linelist dataset.
# linelist <- import(here("data_prac", "linelist_cleaned.rds"))

# Suppose that we want to know the counts of individuals in the 
# different age groups, by gender
df_wide <- linelist |> 
    count(age_cat, gender)
df_wide
# This gives us a long dataset that is great for producing visualisations 
# in ggplot2, but not ideal for presentation in a table.
ggplot(data = df_wide) +
    geom_col(aes(x = age_cat, y = n, fill = gender))


# ** Pivot wider ====
# Therefore, we can use pivot_wider() to transform the data into a 
# better format for inclusion as tables in our reports.
table_wide <- df_wide |> 
    pivot_wider(
        id_cols = age_cat,
        names_from = gender,
        values_from = n
    )
table_wide
# This table is much more reader-friendly, and therefore better for 
# inclusion in our reports. 

# We can further convert it into a pretty table with several packages 
# including flextable and knitr (see Tables for presentation) section.
table_wide |> 
    adorn_totals(c("row", "col")) |> 
    kable() |> 
    row_spec(row = 10, bold = TRUE) |> 
    column_spec(column = 5, bold = TRUE)


# 12.4 Fill ---------------------------------------------------------------
# In some situations after a pivot, and more commonly after a bind, we are 
# left with gaps in some cells that we would like to fill.

# ** Data ====
# CASE: We take two datasets, each with obsns for the measurement number, 
# the name of the facility, and the case count at that time. 
df1 <- tribble(
    ~Measurement,   ~Facility,  ~Cases,
    1,              "Hosp 1",   66,
    2,              "Hosp 1",   26,
    3,              "Hosp 1",   8,
    1,              "Hosp 2",   71,
    2,              "Hosp 2",   62,
    3,              "Hosp 2",   70,
    1,              "Hosp 3",   47,
    2,              "Hosp 3",   70,
    3,              "Hosp 3",   38
)
df1

# However, the second dataset also has a variable Year.
df2 <- tribble(
    ~Year,  ~Measurement,   ~Facility,  ~Cases,
    2000,   1,              "Hosp 4",   82,
    2001,   2,              "Hosp 4",   87,
    2002,   3,              "Hosp 4",   46
)
df2

# When we perform a bind_rows() to join the two datasets together 
# the Year variable is filled with NA for those rows where there was 
# no prior information (i.e. the first dataset)
df_combined <- bind_rows(df1, df2) |> 
    arrange(Measurement, Facility)
df_combined


# ** fill() ====
# Here, Year is a useful variable to include, particularly if we want to 
# explore trends over time. Therefore, we use fill() to fill in those 
# empty cells, by specifying the column to fill and the direction (here up)
df_combined |> fill(Year, .direction = "up")

# Altly, we can rearrange the data so that we would need to fill 
# in a downward direction.
df_combined <- df_combined |> arrange(Measurement, desc(Year))
df_combined
df_combined <- df_combined |> fill(Year, .direction = "down")
df_combined

# We now have a useful dataset in long format for plotting.
ggplot(data = df_combined, aes(x = Year, y = Cases, fill = Facility)) +
    geom_col()

# However, this format is less useful for presenting in a table so 
# let’s convert this long, untidy dataframe into a wider, tidy dataframe.
df_combined |> 
    pivot_wider(
        id_cols = c(Measurement, Facility),
        names_from = Year,
        values_from = Cases
    ) |> 
    arrange(Facility) |> 
    adorn_totals(where = c("row", "col")) |> 
    kable() |> 
    row_spec(row = 5, bold = T) |> 
    column_spec(column = 5, bold = T)

# NOTE: In this case, we had to specify to only include the three vars 
# Facility, Year, and Cases as the additional var Measurement would 
# interfere with the creation of the table
df_combined |> pivot_wider(
    names_from = Year,
    values_from = Cases
) |> 
    kable()


# TBC ####






