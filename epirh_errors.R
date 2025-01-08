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


# 17.6 base R ====

# In subsection "Convert to dataframe"
# ERROR: 
# `fct_explicit_na()` was deprecated in forcats 1.0.0.
# Please use `fct_na_value_to_level()` instead.













