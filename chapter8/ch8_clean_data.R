# https://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page

# Options
options(stringsAsFactors = F)
# Load desired packages
library(pacman)
p_load(data.table, lubridate, magrittr, readxl, stringr)
# Set directory
setwd("/Users/edwardarubin/Dropbox/Research/MyBooks/DataScience/Textbook/chapter8")
# Load data and join data
full_dt <- lapply(
  X = c(
    "rollingsales_bronx.xls",
    "rollingsales_brooklyn.xls",
    "rollingsales_manhattan.xls",
    "rollingsales_queens.xls",
    "rollingsales_statenisland.xls"
  ),
  FUN = function(x) {
    # Load
    tmp <- read_xls(x, skip = 4) %>% data.table()
    # Change names
    setnames(tmp, names(tmp) %>% str_to_lower() %>% str_replace_all(" ", "_"))
    # Grab some variables
    tmp <- tmp[, .(
      borough, neighborhood, building_class_category, zip_code,
      residential_units, commercial_units, total_units,
      land_square_feet, gross_square_feet, year_built,
      sale_price, sale_date
    )]
    # Limit to single-family dwelling types
    tmp <- tmp[building_class_category %in% c(
      "01 ONE FAMILY DWELLINGS",
      "09 COOPS - WALKUP APARTMENTS", "10 COOPS - ELEVATOR APARTMENTS",
      "12 CONDOS - WALKUP APARTMENTS", "13 CONDOS - ELEVATOR APARTMENTS",
      "17 CONDO COOPS"
    )]
    # Require sale price between 10,000 and 10,000,000
    tmp <- tmp[between(sale_price, 1e4, 1e7)]
    # Require gross square feet between 0 and 5000
    tmp <- tmp[between(gross_square_feet, 0, 5000, incbounds = F)]
    # Return tmp data
    return(tmp)
  }
) %>% rbindlist()
# Add: year-of-sale and age
full_dt[, sale_year := sale_date %>% ymd() %>% year()]
full_dt[, age := sale_year - year_built]
# Drop year_build == 0
full_dt <- full_dt[year_built != 0]
# Drop age > 120
full_dt <- full_dt[between(age, 0, 120)]
# Save
fwrite(
  x = full_dt,
  file = "home_sales_nyc.csv"
)
