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
glimpse(linelist)
head(linelist) |> view(title = "linelist")


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
# One major advantage of dplyr and summarise() is the ability to return 
# more advanced statistical summaries like median(), mean(), max(), min(), 
# sd() (standard deviation), and percentiles.
summary_table <- linelist |> 
  group_by(hospital) |> 
  summarize(
    cases       = n(),
    delay_max   = max(days_onset_hosp, na.rm = T),
    delay_mean  = mean(days_onset_hosp, na.rm = T) |> round(1),
    delay_sd    = sd(days_onset_hosp, na.rm = T) |> round(1),
    delay_3     = sum(days_onset_hosp >= 3, na.rm = T),
    pct_delay_3 = scales::percent(delay_3 / cases)
  )
summary_table


# ** Conditional statistics ====
# You may want to return conditional statistics - e.g. the maximum of rows 
# that meet certain criteria. This can be done by subsetting the column 
# with square brackets [ ].
linelist |> 
  group_by(hospital) |> 
  summarize(
    max_temp_fev = max(temp[fever == "yes"], na.rm = T),
    min_temp_fev = min(temp[fever == "no"], na.rm = T)
  )


# ** Glueing together ====
# The function str_glue() from stringr is useful to combine values from 
# several columns into one new column. Here, this is typically used 
# after the summarise() command.
summary_table |> 
  mutate(
    delay = str_glue("{delay_mean} ({delay_sd})")
  ) |> 
  select(-c(delay_mean, delay_sd)) |> 
  adorn_totals(where = "row") |> 
  select(
    "Hospital name"   = hospital,
    "Cases"           = cases,
    "Max delay"       = delay_max,
    "Mean (sd)"       = delay,
    "Delay 3+ days"   = delay_3,
    "% delay 3+ days" = pct_delay_3
  )


# *** Percentiles
# get default percentile values of age (0%, 25%, 50%, 75%, 100%)
linelist |> 
  summarize(
    age_percentiles = quantile(age_years, na.rm = T)
  )
# ERROR: Returning more (or less) than 1 row per `summarise()` group was 
# deprecated in dplyr 1.1.0.

linelist |> reframe(
  age_percentiles = quantile(
    age_years, 
    probs = c(0.05, 0.5, 0.75, 0.95), 
    na.rm = T
  )
)

# Get manually-specified percentile values of age (5%, 50%, 75%, 98%)
linelist |> 
  group_by(hospital) |> 
  summarize(
    p05 = quantile(age_years, probs = 0.05, na.rm = T),
    p50 = quantile(age_years, probs = 0.50, na.rm = T),
    p75 = quantile(age_years, probs = 0.75, na.rm = T),
    p95 = quantile(age_years, probs = 0.95, na.rm = T)
  )

# Altly, we can produce summary statistics from the rstatix package.
linelist |> 
  group_by(hospital) |> 
  get_summary_stats(age, type = "quantile")
linelist |> get_summary_stats(age, type = "quantile")


# ** Summarize aggregated data ====
linelist_agg <- linelist |> 
  drop_na(gender, outcome) |> 
  count(outcome, gender)
linelist_agg

linelist_agg |> 
  group_by(outcome) |> 
  summarize(
    total_cases   = sum(n, na.rm = T),
    male_cases    = sum(n[gender == "m"], na.rm = T),
    female_cases  = sum(n[gender == "f"], na.rm = T)
  )


# ** across() multiple columns ====
# You can use summarise() across multiple columns using across(). This makes 
# life easier when you want to calculate the same statistics for many columns. 
linelist |> 
  group_by(outcome) |> 
  summarize(
    across(
      .cols = c(age_years, temp, wt_kg, ht_cm),
      ~ mean(.x, na.rm = T)
    )
  )

# We can run multiple functions at once.
linelist |> 
  group_by(outcome) |> 
  summarize(
    across(
      .cols = c(age_years, temp, wt_kg, ht_cm),
      .fns = list(
        ~ mean(.x, na.rm = T), 
        ~ sd(.x, na.rm = T)
      )
    )
  )

# CASE: To return the mean of every numeric column use where() and 
# provide the function as.numeric() (without parentheses).
linelist |> 
  group_by(outcome) |> 
  summarize(across(
    .cols   = where(is.numeric),
    .fns    = ~ mean(.x, na.rm = T)
  ))


# ** Pivot wider ====
# If you prefer your table in “wide” format you can transform it using the 
# pivot_wider() fn. You will likely need to re-name the columns with rename().
age_by_outcome <- linelist |> 
  group_by(outcome) |> 
  count(age_cat) |> 
  mutate(
    percent = scales::percent(n / sum(n))
  )
age_by_outcome
# Pivoting to wide only the number column.
age_by_outcome |> 
  select(-percent) |> 
  pivot_wider(names_from = age_cat, values_from = n)
# Pivoting to wide only the number and percentage column.
age_by_outcome |> 
  pivot_wider(names_from = age_cat, values_from = c(n, percent))


# ** Total rows ====
# When summarise() operates on grouped data it does not automatically 
# produce “total” statistics. Below, two approaches to adding a total row.

# *** janitor's adorn_totals()
linelist |> 
  group_by(gender) |> 
  summarize(
    known_outcome   = sum(!is.na(outcome)),
    n_death         = sum(outcome == "Death", na.rm = T),
    n_recover       = sum(outcome == "Recover", na.rm = T)
  ) |> 
  adorn_totals() |> 
  adorn_percentages("col") |> 
  adorn_pct_formatting() |> 
  adorn_ns(position = "front")


# *** summarise() on "total" data and then bind_rows()
by_hospital <- linelist |> 
  filter(!is.na(outcome) & hospital != "Missing") |> 
  group_by(hospital, outcome) |> 
  summarize(
    N         = n(),
    ct_value  = median(ct_blood, na.rm = T)
  )
by_hospital
# To get the totals, run the same summarize() command but only group the 
# data by outcome (not by hospital).
totals <- linelist |> 
  filter(!is.na(outcome) & hospital != "Missing") |> 
  group_by(outcome) |> 
  summarize(
    N         = n(),
    ct_value  = median(ct_blood, na.rm = T)
  )
totals
# We can bind these two data frames together. After binding the rows
# we convert these empty spaces to “Total” using replace_na().
table_long <- bind_rows(by_hospital, totals) |> 
  mutate(hospital = replace_na(hospital, "Total"))
table_long
# Optionally, you can pivot this table wider to make it more readable. 
# You can also add more columns, and arrange it nicely.
table_long_ff <- table_long |> 
  # pivot wider and format
  mutate(hospital = replace_na(hospital, "Total")) |> 
  pivot_wider(
    names_from    = outcome,
    values_from   = c(ct_value, N)
  ) |> 
  mutate(
    N_Known       = N_Death + N_Recover,
    Pct_Death     = scales::percent(N_Death / N_Known, 0.1),
    Pct_Recover   = scales::percent(N_Recover / N_Known, 0.1)
  ) |> 
  select(
    hospital, N_Known, N_Recover, Pct_Recover, ct_value_Recover, 
    N_Death, Pct_Death, ct_value_Death
  ) |> 
  arrange(N_Known)
table_long_ff
# Convert to flextable
table_long_ff |> flextable()


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
# Get proportions of table defined above, by rows, rounded
prop.table(age_by_outcome, margin = 1) |> round(2)


# ** Totals ====
# Pass the table to addmargins() to add row and column totals.
addmargins(age_by_outcome)


# ** Convert to dataframe ====
# Converting a table() object directly to a dataframe is not straight-forward.
# One approach is demonstrated below:
table(fct_na_value_to_level(linelist$age_cat, level = "Missing"), 
      fct_na_value_to_level(linelist$outcome, level = "Missing")
      ) |> 
    addmargins() |> 
    as.data.frame.matrix() |> 
    rownames_to_column(var = "Age category") |> 
    flextable()



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C18 - Simple statistical tests ------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This page demonstrates how to conduct simple statistical tests using 
# base R, rstatix, and gtsummary.
# Each of the above packages bring certain advantages and disadvantages:
# 1.  Use base R functions to print a statistical outputs to the R Console.
# 2.  Use rstatix functions to return results in a dataframe, or if you want 
#     tests to run by group.
# 3.  Use gtsummary if you want to quickly print publication-ready tables.


# 18.1 Preparation --------------------------------------------------------

# ** Load packages ====
# Installing required packages
# install.packages("corrr")
# Loading required packages
library(easypackages)
libraries(
  "rio",
  "here",
  "skimr",
  "tidyverse",
  "gtsummary",
  "rstatix",
  "corrr",
  "janitor",
  "flextable"
)

# ** Import ====
linelist <- import(here("data_prac", "linelist_cleaned.rds"))
glimpse(linelist)
linelist |> head(10) |> view()


# 18.2 base R -------------------------------------------------------------
# We can use base R functions to conduct statistical tests. The commands are 
# relatively simple and results will print to the R Console for simple viewing.
# However, the outputs are usually lists and so are harder to manipulate if 
# you want to use the results in subsequent operations.

# ** T-tests ====
# A t-test, also called “Student’s t-Test”, is typically used to determine 
# if there is a significant difference between the means of some numeric 
# variable between two groups.
# Syntax 1:
t.test(age_years ~ gender, data = linelist)
# Syntax 2:
t.test(linelist$wt_kg, linelist$ht_cm)
# This syntax can also be used to compare numeric vectors from two 
# different datasets.

# You can also use a t-test to determine whether a sample mean is 
# significantly different from some specific value.
t.test(linelist$age_years, mu = 45)


# ** Shapiro-Wilk test ====
# The Shapiro-Wilk test can be used to determine whether a sample came from 
# a normally-distributed population.
linelist |> ggplot(aes(x = age_years)) + geom_histogram()
linelist |> filter(age_years >= 18) |> view(title = "18plus")
linelist |> 
  filter(age_years >= 18) |> 
  ggplot(aes(x = age_years)) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(
    breaks = c(18, seq(20, 90, 5))
  )
linelist_small <- linelist |> filter(age_years >= 18)
shapiro.test(linelist_small$age_years)
# This test can only be used on a sample between 3 and 5000 observations. 
# For larger samples a quantile-quantile plot may be helpful.


# ** Wilcoxon rank sum test ====
# The Wilcoxon rank sum test, also called the Mann–Whitney U test, is 
# often used to help determine if two numeric samples are from the same 
# distribution when their populations are not normally distributed or 
# have unequal variance.
wilcox.test(age_years ~ outcome, data = linelist)


# ** Kruskal-Wallis test ====
# The Kruskal-Wallis test is an extension of the Wilcoxon rank sum test 
# that can be used to test for differences in the distribution of more 
# than two samples. When only two samples are used it gives identical 
# results to the Wilcoxon rank sum test.
kruskal.test(age_years ~ outcome, data = linelist)


# ** Chi-squared test ====
# Pearson’s Chi-squared test is used in testing for significant differences 
# between categorical groups.
chisq.test(linelist$gender, linelist$outcome)


# 18.3 rstatix package ----------------------------------------------------
# The rstatix package offers the ability to run statistical tests and 
# retrieve results in a “pipe-friendly” framework. The results are
# automatically in a dataframe so that you can perform subsequent 
# operations on the results.

# ** Summary statistics ====
# The fn get_summary_stats() is a quick way to return summary statistics.
# Simply pipe your dataset to this fn and provide the columns to analyse.
linelist |> 
  get_summary_stats(age, temp)

# It can be used with grouped data as well, such that a row is returned 
# for each grouping-variable:
linelist |> 
  group_by(hospital) |> 
  get_summary_stats(age, temp, type = "common")


# ** T-test ====
linelist |> t_test(age_years ~ gender)
# We can use ~ 1 and specify mu = for a one-sample T-test.
linelist |> t_test(age_years ~ 1, mu = 30)
# If applicable the statistical tests can be done by group.
linelist |> 
  group_by(gender) |> 
  t_test(age_years ~ 1, mu = 18)


# ** Shapiro-Wilk test ====
linelist |> head(500) |> 
  shapiro_test(age_years)


# ** Wilcoxon rank sum test ====
linelist |> 
  wilcox_test(age_years ~ gender)


# ** Kruskal-Wallis test ====
# Also known as the Mann-Whitney U test.
linelist |> 
  kruskal_test(age_years ~ outcome)


# Chi-squared test ====
linelist |> 
  tabyl(gender, outcome) |> 
  select(-1) |> 
  chisq_test()
chisq_test(linelist$gender, linelist$outcome)  


# 18.4 gtsummary package --------------------------------------------------
# Performing statistical tests of comparison with tbl_summary is done by 
# adding the add_p function to a table and specifying which test to use. 
# It is possible to get p-values corrected for multiple testing by using 
# the add_q function.


# ** Chi-squared test ====
# The default statistical test for add_p() when applied to a categorical var 
# is to perform a chi-squared test of independence with continuity correction.
linelist |> 
  select(gender, outcome) |> 
  tbl_summary(by = outcome) |> 
  add_p()


# ** T-tests ====
# Compare the difference in means for a continuous variable in two groups.
linelist |> 
  select(age_years, outcome) |> 
  tbl_summary(
    statistic = age_years ~ "{mean} ({sd})",
    by = outcome
  ) |> 
  add_p(age_years ~ "t.test")


# ** Wilcoxon rank sum test ====
# Compare the distribution of a continuous variable in two groups.
linelist |> 
  select(age_years, outcome) |> 
  tbl_summary(
    statistic = age_years ~ "{median} ({p25}, {p75})",
    by = outcome
  ) |> 
  add_p(age_years ~ "wilcox.test")


# ** Kruskal-Wallis test ====
# Compare the distribution of a continuous variable in two or more groups 
# regardless of whether the data is normally distributed.

# difference in age by outcome
linelist |> 
  select(age_years, outcome) |> 
  tbl_summary(
    by = outcome
  ) |> 
  add_p(age_years ~ "kruskal.test")

# difference in age across hospitals
linelist |> 
  select(age_years, hospital) |> 
  tbl_summary(
    statistic = age_years ~ "{median} ({p25}, {p75})",
    by = hospital
  ) |> 
  add_p(age_years ~ "kruskal.test")


# 18.5 Correlations -------------------------------------------------------
# The tidyverse corrr package allows you to compute correlations using 
# Pearson, Kendall tau or Spearman rho. The package creates a table and 
# also has a function to automatically plot the values.
correlation_tab <- linelist |> 
  # keeping only numeric vars
  select(generation, age, ct_blood, days_onset_hosp, wt_kg, ht_cm) |> 
  correlate()
correlation_tab
# remove duplicate entries
correlation_tab <- correlation_tab |> shave()
correlation_tab
# plot the correlations
rplot(correlation_tab, colors = c("red","grey", "green"))



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C19 - Univariate and multivariable regression ---------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we learn the use of base R regression functions such as glm() and 
# the gtsummary package to look at associations between variables 
# (e.g. odds ratios, risk ratios and hazard ratios). It also uses functions 
# like tidy() from the broom package to clean-up regression outputs.

# NOTE: We use the term multivariable to refer to a regression with 
# multiple explanatory variables. In this sense a multivariate model would 
# be a regression with several outcomes.


# 19.1 Preparation --------------------------------------------------------

# ** Load packages ====
library(easypackages)
libraries(
  "rio",
  "here",
  "tidyverse",
  "stringr",
  "purrr",
  "janitor",
  "gtsummary",
  "broom",
  "flextable",
  "lmtest",
  "parameters",
  "see"
)


# ** Import data ====
linelist <- import(here("data_prac", "linelist_cleaned.rds"))


# ** Clean data ====
# *** Store explanatory variables
# We store the names of the explanatory columns as a character vector. 
# This will be referenced later.
explanatory_vars <- c("gender", "fever", "chills", "cough", "aches", "vomit")
explanatory_vars

# *** Convert to 1's and 0's
# Below we convert the explanatory columns from “yes”/“no”, “m”/“f”, 
# and “dead”/“alive” to 1 / 0, to cooperate with the expectations of 
# logistic regression models.
# convert all dichotomous variables to 0/1
linelist <- linelist |> 
  mutate(
    across(
      .cols = all_of(c(explanatory_vars, "outcome")),
      .fns  = ~ case_when(
        .x %in% c("m", "yes", "Death")  ~ 1,
        .x %in% c("f", "no", "Recover") ~ 0,
        .default = NA_real_
      )
    )
  )

# *** Drop rows with missing values
# To drop rows with missing values, can use the tidyr function drop_na(). 
# However, we only want to do this for rows that are missing values in 
# the columns of interest.
# add in age_category to the explanatory vars
explanatory_vars <- c(explanatory_vars, "age_cat")
explanatory_vars
# drop rows with missing information for variables of interest
linelist <- linelist |>
  select(all_of(explanatory_vars), outcome) |> 
  drop_na()


# 19.2 Univariate ---------------------------------------------------------
# Just like in the page on Descriptive tables, your use case will determine 
# which R package you use. 
# We present two options for doing univariate analysis - base R, gtsummary.


# ** base R - Linear regression ====
# The function lm() performs linear regression, assessing the relationship 
# between numeric response and explanatory variables that are assumed to 
# have a linear relationship.

# First we provide the equation as a formula. Define the model results 
# as an R object, to use later.
lm_results <- lm(data = linelist, ht_cm ~ age, )
# Then we run summary() on the model results to see the coefficients 
# (Estimates), P-value, residuals, and other measures.
summary(lm_results)

# Altly, you can use the tidy() function from the broom package to 
# pull the results in to a table. 
tidy(lm_results)

# You can then also use this regression to add it to a ggplot. To do this 
# we first pull the points for the observed data and the fitted line into 
# one dataframe using the augment() function from broom.
points <- augment(lm_results)
points
# Now, we plot the data using age as the x-axis.
ggplot(data = points, aes(x = age)) +
  geom_point(aes(y = ht_cm)) +
  geom_line(
    aes(y = .fitted),
    linewidth = 1.5,
    colour = "red"
  )

# Altly, you can add a simple linear regression straight in ggplot 
# using the geom_smooth() function.
ggplot(data = linelist, aes(x = age, y = ht_cm)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    linewidth = 1.5, colour = "red"
  )


# ** base R - Logistic regression ====
# The function glm() from the stats package (part of base R) is used to 
# fit Generalized Linear Models (GLM).
# NO CODE.


# ** base R - Univariate glm() ====
# CASE: We are assessing the association between different age categories 
# and the outcome of death (coded as 1 in the Preparation section).
model <- glm(data = linelist, outcome ~ age_cat, family = "binomial")
summary(model)
# NOTE: The estimates provided are the log-odds and that the baseline level 
# is the first factor level of age_cat (“0-4”).

# To alter the baseline level of a given var, ensure the col is class Factor 
# and move the desired level to the first position with fct_relevel().
model_2 <- linelist |> 
  mutate(
    age_cat = fct_relevel(age_cat, "20-29", after = 0)
  ) |> 
  glm(formula = outcome ~ age_cat, family = "binomial")
summary(model_2)
tidy(model_2)


# ** base R - Printing results ====
# The function tidy() from the package broom is convenient for making the 
# model results presentable.

# Here we demonstrate how to combine model outputs with a table of counts.
# 1. Get the exponentiated log odds ratio estimates and confidence intervals.
model <- glm(data = linelist, outcome ~ age_cat, family = "binomial") |> 
  tidy(exponentiate = T, conf.int = T) |> 
  mutate(
    across(
      .cols   = where(is.numeric),
      .fns    = ~ round(.x, digits = 2)
    )
  )
model
# 2. Combine these model results with a table of counts.
counts_table <- linelist |> tabyl(age_cat, outcome)
counts_table
# 3. Now we can bind the counts_table and the model results together 
# horizontally with bind_cols() (dplyr).
combined <- bind_cols(counts_table, model) |> 
  select(term, 2:3, estimate, conf.low, conf.high, p.value) |> 
  mutate(
    across(
      .cols = where(is.numeric),
      .fns  = ~ round(.x, digits = 2)
    )
  )
combined
# NOTE: With bind_cols() the rows in the two data frames must be aligned 
# perfectly.

# Now we print the dataframe nicely as an image using flextable.
combined |> qflextable()


# ** Looping multiple univariate models ====
# Below we present a method using glm() and tidy().
explanatory_vars |> str_c("outome ~ ", explanatory_vars)
explanatory_vars

models <- explanatory_vars |> 
  str_c("outcome ~ ", explanatory_vars) |> 
    map(
      \(x) glm(data = linelist, formula = as.formula(x), family  = "binomial")
    ) 
  
  map(
    .f = ~ tidy(
      .x, 
      exponentiate = T,
      conf.int = T
    )
  )
  
  
models




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












