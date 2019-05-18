#Library
  library(readr)
  library(tidytext)
  library(topicmodels)
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  library(tidyr)

#Load
  comps <- read.csv("Downloads/complaints-2019-05-17_03_35.csv")
  colnames(comps) <- tolower(colnames(comps))
  
#cleaning
  comps$company <- tolower(comps$company)
  comps$consumer.complaint.narrative <- tolower(comps$consumer.complaint.narrative)
  comps$clean.narrative <- gsub("[[:punct:][:digit:]]", "", comps$consumer.complaint.narrative)

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
  
#Split sample
  set.seed(123)
  rows <- rank(runif(nrow(comps)), ties.method = "random")
  comps <- comps[rows <= 15000, ]
  test <- comps[rows > 15000 & rows <=20000, ]

#Tokenize
  toks1 <- comps %>%
    unnest_tokens(word, clean.narrative) %>%
    anti_join(stop_words)
  toks1 <- toks1[-grep("xx", toks1$word),]
  
  #Bigram
  toks1b <- comps %>%
    unnest_tokens(word, clean.narrative, token = "ngrams", n = 2)  %>%
    separate(word, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  toks1b$word <- paste(toks1b$word1, toks1b$word2)
  toks1b$word1 <- toks1b$word2 <- NULL
  toks1b <- toks1b[-grep("xx", toks1b$word),]
  
  #Trigram
  toks1c <- comps %>%
    unnest_tokens(word, clean.narrative, token = "ngrams", n = 3)  %>%
    separate(word, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word)
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
      if(min(nchar(unlist(vals[i]))) <=3 || any(is.na(unlist(vals[i])))){
        print(TRUE)
        drop[i] <- TRUE
      } 
  }
  
  tok_count <- tok_count[!drop,]
  
 #Drop words with low counts
  tok_count <- tok_count[tok_count$n > 10,]
  toks3 <- toks3[toks3$word %in% tok_count$word, ]
  
#Roll up
  tok4 <- toks3 %>%
          count(word, complaint.id) 
  rm(toks1, toks1b, toks1c, vals, drop)
  
#DTM
  dtm <- tok4 %>% cast_dtm(term = "word", 
                           document = "complaint.id", 
                           value = "n")
  
#Latent Dirichlet Model
  lda_mod <- LDA(dtm, k = 50, control = list(seed = 1234))
  
  cfpb_td <- tidy(lda_mod)
  
  top_terms <- cfpb_td %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  theme_set(theme_bw())
  
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ topic, scales = "free") +
    theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))

  #View(top_terms)
   save(dtm, cfpb_td, lda_mod, file = "cfpb_lda.Rda")
  