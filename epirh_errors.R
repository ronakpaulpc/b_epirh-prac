# Errors in the book are documented here.


# C17 - Descriptive tables ------------------------------------------------

# 17.4 dplyr package ====

# In subsection "Glueing together"
# ERROR:
# Returning more (or less) than 1 row per `summarise()` group was deprecated 
# in dplyr 1.1.0.
# Please use `reframe()` instead.
# When switching from `summarise()` to `reframe()`, remember that `reframe()` 
# always returns an ungrouped dataframe and adjust accordingly.

# In subsection "across() multiple columns"
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

# In subsection "across() multiple columns"
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


# 17.6 base R ====

# In subsection "Convert to dataframe"
# ERROR: 
# `fct_explicit_na()` was deprecated in forcats 1.0.0.
# Please use `fct_na_value_to_level()` instead.













