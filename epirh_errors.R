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

# 19.2 Univariate ---------------------------------------------------------

# ** base R - Linear regression ====
# GRAMMATICAL MISTAKE
# The base R function lm() <perform> linear regression

# REPEATED WORD
# It is also possible to add a simple linear regression straight <straight> 
# in ggplot using the geom_smooth() function.


# ** base R - Printing results ====
# Warning message:
#   There was 1 warning in `mutate()`.
#   In argument: `across(.cols = where(is.numeric), .fns = round, digits = 2)`.
# Caused by warning:
#   The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
# Supply arguments directly to `.fns` through an anonymous function instead.


# ** base R - Looping multiple univariate models ====
# ERROR
# explanatory_vars |> str_c("outcome ~ ", .)
# Error: object '.' not found


# 19.4 Multivariable ------------------------------------------------------

# ** Conduct multivariable ====
# WORD OMITTED
# Specify which variable selection direction you want <to> use when 
# building the model.



