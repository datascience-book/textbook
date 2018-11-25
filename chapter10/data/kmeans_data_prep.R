####################################################
#Chapter 10 k-means clustering example data set   ##
####################################################

#Set up
  setwd("/Users/jeff/Downloads/chapter10")
  library(readxl)

#Read in files
  #Emp - Census CBP
  load("data/cbp16co.Rda")
  
  #USDA - https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
  ed <- read_excel("data/Education.xls", skip = 4)

  #pov
  pov <- read_excel("data/PovertyEstimates.xls", skip = 3)
  colnames(pov) <- c("fips", "pct.pov", "median.inc")
  
#Keep ACS data 2012-2016
  cty <- ed[, c(1,2,3,44,45,46,47)]
  colnames(cty) <- c("fips", "state", "name", "pct.less.hs", 
                     "pct.hs", "pct.some.ba", "pct.ba" )
  cty[is.na(cty)] <- 0
  cty[is.nan(cty)] <- 0
  cty[is.infinite(cty)] <- 0
  
#Employ numbers from County Business Patterns
  # 5112 Software publishers
  # 5161 Internet publishing and broadcasting
  # 5179 Other telecommunications
  # 5181 Internet service providers and Web search portals
  # 5182 Data processing, hosting, and related services
  # 5415 Computer systems design and related services
  # 5417 Scientific research-and-development services
  
  # Create FIPS
  emp$fips <- paste0(sprintf("%02d", emp$fipstate),
                     sprintf("%03d", emp$fipscty))
  
  #Cut down to relevant industries
  emp$naics <- gsub("[[:punct:]]", "",emp$naics)
  digital_ind <- c("5112", "5161", "5179", "5181", "5182", "5415","5417","454111","")
  emp <- emp[emp$naics %in% digital_ind,]
  emp$naics[emp$naics==""] <- "all"
  
  #Create employment numbers by industry
  est <- aggregate(cbind(est,emp) ~ naics + fips, 
                   data = emp,
                   FUN = sum) 
  est <- reshape(est,
                 idvar = "fips",
                 timevar = "naics",
                 direction = "wide")
  
  #Calculate the scores
  est[is.na(est)] <- 0
  
  est$emp.target <-  est$emp.5182 + est$emp.5112 + est$emp.454111+
                  est$emp.5179 + est$emp.5415 + est$emp.5417
  
  #Get Employment Calculated
  emp <- est[,c("fips",  "emp.all", "emp.target")]
  emp[,3:ncol(emp)] <- 100*emp[,3:ncol(emp)]/emp$emp.all
  emp <- emp[emp$emp.all!=0,]
  
  #Get Establishments calculated
  est$est.target <- est$est.5182 + est$est.5112 + est$est.454111
                  est$est.5179 + est$est.5415 + est$est.5417
  est <- est[,c("fips", "est.all", "est.target")]
  est[,3:ncol(est)] <- 100*est[,3:ncol(est)]/est$est.all
  est[is.na(est)] <- 0
  est <- est[est$est.all != 0,]
  est$est.all <-  NULL
  
#Join together
  cty <- merge(cty, emp, by = "fips")
  cty <- merge(cty, est, by = "fips")
  cty <- merge(cty, pov, by = "fips")
  cty <- cty[!is.nan(cty$emp.target), ]
  cty <- cty[!is.nan(cty$est.target), ]

  
#drop 
  cty$pct.less.hs <- cty$pct.hs <- cty$pct.some.ba <- NULL
  colnames(cty)[4:9] <- c("ba", "all.emp","pct.tech", "est", "pov", "inc")
  

  
#County save
  save(cty, file = "data/county_compare.Rda")
  
  
  