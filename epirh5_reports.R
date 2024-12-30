# Practice code from the "Reports and Dashboards" section of the Epi R Handbook
# We have created the sections as per the handbook
# NOTE: Section headers can be entered using ctrl+shift+R or manually typed
# Each section represents one chapter of the handbook


#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C40 - Reports with R Markdown -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R Markdown is a widely-used tool for creating automated, reproducible, 
# and share-worthy outputs, such as reports.
# We will make static or dynamic outputs, in Word, pdf, html, ppt formats.


# The background steps to generate files using R Markdown are:
# creating and feeding the *.Rmd file to knitr
# knitr executes the R code chunks and creates a new *.md (markdown) file
# the *.md file includes the R code and its rendered output
# The *.md file is then processed by pandoc to create the finished product
# The final output may be a MS Word doc, HTML file, MS PPT doc, pdf, etc.


# 40.0 Installing and Loading the packages and datasets -------------------
# Installing the packages
# install.packages(
#     c(
#     "rmarkdown",            # R markdown for reports 
#     "tinytex"               # Latex distribution for R
#     )
# )

# Loading packages
library(pacman)
p_load(
    rio,                            # importing data
    here,                           # relative file pathways
    skimr,                          # summary of dataframe
    tidyverse,                      # data management and visualization
    janitor,                        # data cleaning and tables
    lubridate,                      # general pkg for working with dates
    rmarkdown,                      # additional date/time fns
    tinytex
)
p_loaded()


# 40.3 R Markdown components ----------------------------------------------
# NO CODE
    






















