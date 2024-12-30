# This script has practice code from the "Basics" section of the Epi R Handbook
# We have created the sections as per the handbook
# NOTE: Section headers can be entered using ctrl+shift+R or manually typed


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C0 - Installing and loading the required packages -----------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we will install and load the useful packages
# For this we will use the p_load() from pacman package
# It load installed packages and install new packages automatically
# install.packages("pacman")
library(pacman)

# * Packages from CRAN ----------------------------------------------------
library(pacman)
p_load(
    
    # Learning R
    # learnr,
    # swirl,
    
    # Project and file management
    here,                           # relative file paths for R project
    rio,                            # single cmd for many file import/export
    openxlsx,                       # specialized for handling excel files
    
    # Package install and management
    # installr,                     # for installing and updating R
    renv,
    remotes,
    
    # General data management
    tidyverse,                      # data management and visualization ninja
    # tidyverse_packages()          # lists all packages in the tidyverse
    # dplyr
    # tidyr
    # ggplot2
    # stringr
    # forcats
    # lubridate
    # purr
    # haven  
    skimr,                          # for data.frame summary
    linelist,
    naniar,
    
    # Statistics
    janitor,
    gtsummary,
    rstatix,
    broom,
    lmtest,
    easystats,
    # parameters
    # see
    
    # Epidemic modelling
    epicontacts,
    EpiNow2,
    EpiEstim,
    projections,
    incidence2,
    i2extras,
    epitrix,
    distcrete,
    
    # Graphs - general
    RColorBrewer,
    viridis,
    cowplot,
    patchwork,
    ggnewscale,
    
    # Graphs - specific types
    DiagrammeR,
    gghighlight,
    # incidence2,
    gganimate,
    ggrepel,
    plotly,
    
    # GIS
    sf,
    tmap,
    # OpenStreetMap,   # Failed to load
    spdep,
    
    # Routine reports
    rmarkdown,
    reportfactory,
    officer,
    
    # Dashboards
    flexdashboard,
    shiny,
    
    # Tables for presentation
    knitr,
    flextable,
    kableExtra,
    # DT,
    # gt,
    # huxtable,
    
    # Phylogenetics packages
    # ggtree, # not available for this version of R
    # treeio,  # there is no package called ‘treeio’
    ape    
)
p_loaded()              # loaded packages


# * Packages from github --------------------------------------------------
# library(pacman)
# Dev version of "epicontacts" (for transmission chains with a time x-axis)
p_install_gh("reconhub/epicontacts@timeline")
# The package for this handbook, which includes all the example data  
p_install_gh("appliedepi/epirhandbook")


# * Other useful packages -------------------------------------------------
install.packages("DHS.rates")    # Calculate demographic rates from DHS data
install.packages("clipr")        # Importing and exporting from clipboard
                   
# library(pacman)
p_load(
    DHS.rates,
    clipr
)




#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C2 - Download handbook and data -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We offer an R package called "epirhandbook". It includes a function 
# download_book(). It downloads the handbook file from our Github repository
# to your computer. This package also contains a function get_data(). It 
# downloads all the example data to your computer. Run the code below to 
# install "epirhandbook" R package from the Github repository appliedepi. 
install.packages("epirhandbook")
# Gives error that this pkg is unavailable for current R version.
# NOTE: This package is not on CRAN
 
# Then we use the special function p_install_gh() and install from 
# Github directly.
install.packages("pacman")
pacman::p_install_gh("appliedepi/epirhandbook")
pacman::p_load(epirhandbook)
# Download the offline handbook on the computer
download_book()
# Download all the example data
get_data("all")




#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C3 - R Basics -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package for updating R from RStudio in windows
# install.packages("installr")
library(installr)

# Checking for current R version
R.Version()                   # check for installed version 
sessionInfo()                 # checking alternative
# Checking for updates and updating R
check.for.updates.R()
updateR()


# Updating Rtools
install.Rtools()

# Updating the R packages
update.packages(ask = F)

# Functions
sqrt(49)




#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C4 - Transition to R ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NO CODE.




#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C5 - Suggested packages -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SEE C0.




#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C6 - R projects ---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NO CODE.




#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C7 - Import and export --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Checking root directory of the project using here fn.
here()

# * 7.5 Import data -------------------------------------------------------
# Importing from Excel file
linelist <- import(here("data", "linelist_raw.xlsx"))
View(linelist)


# * 7.8 Manual data entry -------------------------------------------------
# Entry by rows
manual_entry_rows <- tibble::tribble(
    ~colA, ~colB,
    "a",  1,
    "b",  2,
    "c",  3,
    "d",  4
)
manual_entry_rows

# Entry by columns
patientid <- c(235, 452, 778, 111)
treatment <- c("Yes", "No", "Yes", "Yes")
death <- c(1, 0, 1, 0)
manual_entry_cols <- data.frame(patientid, treatment, death)
manual_entry_cols


# * 7.11 Export -----------------------------------------------------------
# With rio package
export(manual_entry_cols, "exdata.xlsx")

# Exporting to clipboard
clipr::write_clip(manual_entry_cols)




