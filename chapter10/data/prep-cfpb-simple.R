#Library
  library(readr)
  library(tidytext)
  library(topicmodels)
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  library(tidyr)
  library(ranger)

#WD
  #setwd("/Users/jeff/Documents/Github/textbook/chapter10/data")

#Load
  comps <- read.csv("~/Downloads/complaints-2019-05-17_03_35.csv")
  colnames(comps) <- tolower(colnames(comps))
  
#cleaning
  comps$company <- tolower(comps$company)
  comps$consumer.complaint.narrative <- tolower(comps$consumer.complaint.narrative)
  comps$clean.narrative <- gsub("[[:punct:][:digit:]]", "", comps$consumer.complaint.narrative)

  #Clean "Issues" Field
    comps$issue <- trimws(comps$issue)
    comps$issue[comps$issue %in% c("Advertising", "Advertising and marketing, including promotional offers", "Advertising, marketing or disclosures")] <- "Advertising and marketing"
    comps$issue[comps$issue %in% c("Applying for a mortgage or refinancing an existing mortgage")] <- "Applying for a mortgage"
    comps$issue[comps$issue %in% c("Struggling to pay your loan")] <- "Struggling to repay your loan"
    comps$issue[comps$issue %in% c("Trouble using the card")] <- "Trouble using your card"
    comps$issue[comps$issue %in% c("Unexpected/Other fees")] <- "Unexpected or other fees"
    comps$issue[comps$issue %in% c("Unauthorized transactions/trans. issues")] <- "Unauthorized transactions or other transaction problem"
    comps$issue[comps$issue %in% c("Unable to get your credit report or credit score")] <- "Unable to get credit report/credit score"
    comps$issue[comps$issue %in% c("Other service issues","Other transaction problem")] <- "Other transaction problem"
    comps$issue[comps$issue %in% c("Other transaction issues")] <- "Other transaction problem"
    comps$issue[comps$issue %in% c("Overdraft, savings, or rewards features")] <- "Overdraft, savings or rewards features"
    comps$issue[comps$issue %in% c("Managing, opening, or closing your mobile wallet account")] <- "Managing, opening, or closing account"
    comps$issue[comps$issue %in% c("Incorrect information on your report")] <- "Incorrect information on credit report"
    comps$issue[comps$issue %in% c("Incorrect information on your report")] <- "Incorrect information on credit report"
    comps$issue[comps$issue %in% c("Getting a loan")] <- "Getting a loan or lease"
    comps$issue[comps$issue %in% c("Getting the loan")] <- "Getting a loan or lease"
    comps$issue[comps$issue %in% c("Fees")] <- "Fees or interest"
    comps$issue[comps$issue %in% c("Improper use of your report")] <- "Improper use of my credit report"
    comps$issue[comps$issue %in% c("Dealing with your lender or servicer")] <- "Dealing with my lender or servicer"
    comps$issue[comps$issue %in% c("Customer service/Customer relations")] <- "Customer service / Customer relations"
    comps$issue[comps$issue %in% c("Credit monitoring or identity theft protection services")] <- "Credit monitoring or identity protection"
    comps$issue[comps$issue %in% c("Closing/Cancelling account")] <- "Closing your account"
    comps$issue[comps$issue %in% c("Can't contact lender")] <- "Can't contact lender or servicer"
    comps$issue[comps$issue %in% c("Applying for a mortgage or refinancing an existing mortgage")] <- "Applying for a mortgage"
    
    comp_count <- comps %>%
      count(issue, sort = TRUE) 
    comp_count <- comp_count[comp_count$n < 10,]
    comps$issue <- as.character(comps$issue)
    comps$issue[comps$issue %in% comp_count$issue] <- "other"
    comps$issue <- as.factor(comps$issue)
    
    #Companies
    comp_count <- comps %>%
      count(company, sort = TRUE) 
    comp_count <- comp_count[comp_count$n < 10,]
    comps$company <- as.character(comps$company)
    comps$company[comps$company %in% comp_count$company] <- "other"
    comps$company <- as.factor(comps$company)

  #Split sample
    rand <- sample(1:nrow(comps))
   train <- comps[rand < 40000, grep("(company.response.to.consumer|issue|^product|^company$)", colnames(comps))]
   test <- comps[rand >=40000, grep("(company.response.to.consumer|issue|^product|^company$)", colnames(comps))]
   
   mod.rf <- ranger(company.response.to.consumer ~ .,
                 data = train)
   test.rf <- predict(mod.rf, 
                   test,
                   type = "response")
   a <- table(test$company.response.to.consumer, test.rf$predictions)
   (a[1,1] + a[2,2])/sum(a)
   
   mod.glm <- glm(company.response.to.consumer ~ ., data = train,family=binomial)
   test.glm <- predict(mod.glm, 
                   test,
                   type = "response")
   a <- table(test$company.response.to.consumer, test.glm > 0.5)
   (a[1,1] + a[2,2])/sum(a)
   