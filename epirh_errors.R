# Errors in the book are documented here.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C17 - Descriptive tables ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 17.4 dplyr package ------------------------------------------------------

# ** Glueing together ====
# ERROR:
# Returning more (or less) than 1 row per `summarise()` group was deprecated 
# in dplyr 1.1.0.
# Please use `reframe()` instead.
# When switching from `summarise()` to `reframe()`, remember that `reframe()` 
# always returns an ungrouped dataframe and adjust accordingly.

# ** across() multiple columns ====
# ERROR 1:
linelist |> 
    group_by(outcome) |>  
    summarise(across(.cols = c(age_years, temp, wt_kg, ht_cm),
                     .fns = mean,
                     na.rm = T))
# WARNING: There was 1 warning in `summarise()`.
# In argument: `across(...)`.
# In group 1: `outcome = "Death"`.
# Caused by warning:
# The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
# Supply arguments directly to `.fns` through an anonymous function instead.
# Previously
# across(a:b, mean, na.rm = TRUE)
# Now
# across(a:b, \(x) mean(x, na.rm = TRUE))

# ERROR 2:
linelist |> 
    group_by(outcome) |> 
    summarize(
        across(
            .cols = c(age_years, temp, wt_kg, ht_cm),
            .fns = list("mean" = mean, "sd" = sd),
            na.rm = T
        )
    )
# The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
# Supply arguments directly to `.fns` through an anonymous function instead.
# Previously
# across(a:b, mean, na.rm = TRUE)
# Now
# across(a:b, \(x) mean(x, na.rm = TRUE))


# 17.6 base R -------------------------------------------------------------
# ** Convert to dataframe ====
# ERROR: 
# `fct_explicit_na()` was deprecated in forcats 1.0.0.
# Please use `fct_na_value_to_level()` instead.



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C18 - Simple statistical tests ------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 18.2 base R -------------------------------------------------------------

# Chi-squared test ====
# SPELLING MISTAKE
# Pearson’s Chi-squared test is used in testing for significant differences 
# between categorical <croups>.



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C19 - Univariate and multivariable regression ---------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 19.1 Preparation --------------------------------------------------------

# ** base R - Linear regression ====
# SUGGESTION: Prepare the data for logistic regression model in a separate 
# object called "linelist_glm". Not doing so leads to error as linear 
# regression models use the same linelist dataset with different set of vars.


# 19.2 Univariate ---------------------------------------------------------

# ** base R - Linear regression ====
# GRAMMATICAL MISTAKE
# The base R function lm() <perform> linear regression

# REPEATED WORD
# It is also possible to add a simple linear regression straight <straight> 
# in ggplot using the geom_smooth() function.


# ** base R - Printing results ====

# 1. Get the exponentiated log odds ratio estimates and confidence intervals.
# ERROR: NEED TO USE FUNCTIONAL FORM
model <- glm(outcome ~ age_cat, family = "binomial", data = linelist) |>  
    tidy(exponentiate = TRUE, conf.int = TRUE) |> 
    mutate(across(where(is.numeric), round, digits = 2))
# Warning message:
#   There was 1 warning in `mutate()`.
#   In argument: `across(.cols = where(is.numeric), .fns = round, digits = 2)`.
# Caused by warning:
#   The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
# Supply arguments directly to `.fns` through an anonymous function instead.

# 3. Now we can bind the counts_table and the model results together 
#    horizontally with bind_cols() (dplyr).
# ERROR: WE ARE USING THE WRONG PLACEHOLDER. Base pipe(|>) uses "_" instead 
# of "." in margrittr pipe (%>%).
combined <- counts_table |> 
    bind_cols(., model) |> 
    select(term, 2:3, estimate, conf.low, conf.high, p.value) |> 
    mutate(
        across(
            .cols = where(is.numeric),
            .fns  = ~ round(.x, digits = 2)
        )
    )


# ** base R - Looping multiple univariate models ====
# MULTIPLE ERRORS: 
# 1. WRONG PIPE PLACEHOLDER.
# 2. .x object in not there.
models <- explanatory_vars %>%       # begin with variables of interest
    str_c("outcome ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
    
    # iterate through each univariate formula
    map(                               
        .f = ~glm(                       # pass the formulas one-by-one to glm()
            formula = as.formula(.x),      # within glm(), the string formula is .x
            family = "binomial",           # specify type of glm (logistic)
            data = linelist)) %>%          # dataset
    
    # tidy up each of the glm regression outputs from above
    map(
        .f = ~tidy(
            .x, 
            exponentiate = TRUE,           # exponentiate 
            conf.int = TRUE)) %>%          # return confidence intervals
    
    # collapse the list of regression outputs in to one data frame
    bind_rows() %>% 
    
    # round all numeric columns
    mutate(across(where(is.numeric), round, digits = 2))
# explanatory_vars |> str_c("outcome ~ ", .)
# Error: object '.' not found
# NOTE: This error arises because of using the base pipe instead of the
# margrittr pipe.

# ERROR: WRONG DESCRIPTION.
# The count table in univ_tab_base are actually crosstabs.  
# Check the following code and compare: 
# linelist |> tabyl(gender, outcome)


# 19.4 Multivariable ------------------------------------------------------

# ** Conduct multivariable ====
# ERROR: Word omitted.
# Specify which variable selection direction you want <to> use when 
# building the model.


# 19.5 Forest plot --------------------------------------------------------

# ** easystats packages ====
# ERROR: Extra word.
# An alternative, if you do not want <to> the fine level of control that 
# ggplot2 provides, is to use a combination of easystats packages.






