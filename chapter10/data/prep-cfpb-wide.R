#Library
  library(readr)
  library(tidytext)
  library(topicmodels)
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  library(tidyr)
  library(ranger)
  library(SnowballC)

#WD
  setwd("/Users/jeff/Documents/Github/textbook/chapter10/data")

#Load
  comps <- read.csv("~/Downloads/complaints-2019-05-17_03_35.csv")
  save(comps, file = "cfpb_raw.Rda")
  
#Change column names
  colnames(comps) <- tolower(colnames(comps))
  
#Cleanse values
  
  #Company names
  comps$company <- tolower(comps$company)
  
  #Change headers
  colnames(comps)[grep("(^product$)", colnames(comps))] <- "product.type"
  colnames(comps)[15] <- "target.series"
  
  #Clean complaint status
  comps$target.series <- as.character(comps$target.series)
  comps$target.series[grep("non-monetary", comps$target.series)] <- "Non-monetary" 
  comps$target.series[grep("with monetary", comps$target.series)] <- "Monetary" 
  comps$target.series <- as.factor(comps$target.series)
  
  #Set up sample partition
  set.seed(123)
  rows <- format(as.Date(comps$date.received, "%m/%d/%y"), "%Y")
  comps <- cbind(partition = (rows == "2016"),
                 comps)
  rand <- ave(runif(nrow(comps)), paste0(comps$target.series, comps$partition), FUN = rank)
  rand <- (rand <= 5000 & comps$partition == T) |  (comps$partition == F)
  comps <- comps[rand==T, ]
  
  #Clean narratives
  comps$consumer.complaint.narrative <- tolower(comps$consumer.complaint.narrative)
  comps$clean.narrative <- gsub("[[:punct:][:digit:]]", "", comps$consumer.complaint.narrative)
  
  #Standardize issues
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
  

#Tokenize
  toks1 <- comps %>%
    unnest_tokens(word, clean.narrative) %>%
    anti_join(stop_words)  %>%
    mutate(word = wordStem(word))
  
  toks1 <- toks1[-grep("xx", toks1$word),]
  
  #Bigram
  toks1b <- comps %>%
    unnest_tokens(word, clean.narrative, token = "ngrams", n = 2)  %>%
    separate(word, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    mutate(word1 = wordStem(word1)) %>%
    mutate(word2 = wordStem(word2))
  
  toks1b$word <- paste(toks1b$word1, toks1b$word2)
  toks1b$word1 <- toks1b$word2 <- NULL
  toks1b <- toks1b[-grep("xx", toks1b$word),]
  
  #Trigram
  toks1c <- comps %>%
    unnest_tokens(word, clean.narrative, token = "ngrams", n = 3)  %>%
    separate(word, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word) %>%
    mutate(word1 = wordStem(word1)) %>%
    mutate(word2 = wordStem(word2)) %>%
    mutate(word3 = wordStem(word3))
  
  toks1c$word <- paste(toks1c$word1, toks1c$word2, toks1c$word3)
  toks1c$word1 <- toks1c$word2 <- toks1c$word3 <- NULL
  toks1c <- toks1c[-grep("xx", toks1c$word),]
  
  toks3 <- rbind(toks1, toks1b, toks1c)

#Trim down
  tok_count <- toks3 %>%
    count(word, sort = TRUE) 
  
  #drop short words
  vals <- strsplit(tok_count$word, " ")
  drop <- rep(FALSE, length(vals))
  for(i in 1:length(vals)){
      if(min(nchar(trimws(unlist(vals[i])))) <=3 || any(is.na(unlist(vals[i])))){
        print(TRUE)
        drop[i] <- TRUE
      } 
  }
  
  tok_count <- tok_count[!drop,]
  
 #Drop words with low counts
  tok_count <- toks3[toks3$partition == T,] %>%
    count(word, sort = TRUE) 
  tok_count <- tok_count[tok_count$n > 10,]
  toks3 <- toks3[toks3$word %in% tok_count$word, ]
  
#Roll up
  tok4 <- toks3 %>%
          count(word, complaint.id, partition) 
  tok4 <- tok4[tok4$n > 1,]
  rm(toks1, toks1b, toks1c, toks3, vals, drop)
  wide <- dcast(tok4, complaint.id + partition ~ word)
  
#Join DTM to main file
  
  comps2 <- inner_join(comps[, c("complaint.id", "partition", "date.received", "product.type", "issue", "consumer.complaint.narrative","target.series")],
                        wide, by = c("complaint.id", "partition"))
  
  #Fill holes
    comps2[is.na(comps2)] <- 0

  #Column type
    comps2$issue <- as.factor(comps2$issue)
    
  #Column names
    colnames(comps2) <- trimws(gsub("[[:punct:][:space:][:digit:]]",".", colnames(comps2)))
    comps2 <- comps2[,!duplicated(colnames(comps2))]
    comps2 <- comps2[,nchar(colnames(comps2)) > 3]
    colnames(comps2)[-c(1:7)] <- paste0("t", 1:(ncol(comps2)-7), ".", colnames(comps2)[-c(1:7)])
    
#Split sample
  train <- comps2[comps2$partition == T, ]
  train$partition <- NULL
  test <- comps2[comps2$partition == F, ]
  test$partition <-  NULL
  
  save(train, test, file = "cfpb_dtm.Rda")
  
#Train
  mod.rf <- ranger(target.series ~ .,
                   data = train[, -c(1:5)],
                   importance = "impurity",
                   mtry = 80, 
                   seed = 123)
  
  #Importance
  mod.rf$variable.importance
  
  #assemble importance table
  imp <- data.frame(var = names(mod.rf$variable.importance), 
                    impurity = mod.rf$variable.importance)
  imp$var <- gsub("(t\\d{1,4}.)","", imp$var)
  imp <- imp[order(-imp$impurity),]
  
  
  imp_table <- cbind(imp[1:10, 1:2], 
                    imp[575:584, 1:2],
                    imp[4648:4657, 1:2])
  colnames(imp_table) <- c("var.high", "imp.high", "var.mean", "imp.mean", "var.min", "imp.min")
  
  #Predict
  test.rf <- predict(mod.rf, test,
                     type = "response")
  train.rf <- predict(mod.rf, train,
                     type = "response")
  
  #Results
  a <- table(train$target.series, train.rf$predictions)
  (a[1,1] + a[2,2])/sum(a)
  
  a <- table(test$target.series, test.rf$predictions)
  (a[1,1] + a[2,2])/sum(a)
  