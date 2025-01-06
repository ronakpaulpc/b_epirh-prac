# Practice code from the "Data visualization" section of the Epi R Handbook
# We have created the sections as per the handbook
# NOTE: Section headers can be entered using ctrl+shift+R or manually typed
# Each section represents one chapter of the handbook


#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C29 - Tables for Presentation -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we learn how to convert summary dataframes into presentation-ready 
# tables with the flextable package. These tables can be inserted into 
# powerpoint slides, HTML pages, PDF or Word documents, etc.
# NOTE: Before using flextable, you convert the summary table to a dataframe.


# 29.1 Preparation --------------------------------------------------------

# ** Load packages ====
# Here we load the packages required for analysis
library(easypackages)
libraries(
    "rio",
    "here",
    "janitor",
    "tidyverse",
    "flextable",
    "officer"
)


# ** Import data ====
linelist <- import(here("data_prac", "linelist_cleaned.rds"))
glimpse(linelist)


# ** Prepare table ====
# Before beginning to use flextable you will need to create your table 
# as a dataframe. You must arrange the content in rows and columns as 
# you want it displayed. Then, the dataframe will be passed to flextable 
# to display it with colors, headers, fonts, etc.























