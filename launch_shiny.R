# =============================================================================#
# File Name: launch_shiny.R                                                   #
# Original Creator: Kim To                                                    #
# Contact Information: comptox@ils-inc.com                                    #
# Date Created: 2021-12-03                                                    #
# License: MIT                                                                #
# Version: 0.9                                                                #
# Description: Installs required packages and launches app                    #
# Required Packages:                                                          #
#   - renv                                                                    #
#   - shiny                                                                   #
# =============================================================================#

# Install renv
if (!"renv" %in% rownames(installed.packages())) {
  install.packages("renv", repos = "http://cran.us.r-project.org")
}

# Load required packages
renv::restore(lockfile = "renv.lock")

# Launch app in browser
shiny::runApp(launch.browser = TRUE)