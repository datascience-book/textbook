
library(RCurl)
library(XML)

google.counts <- function(search.term){
  #
  # Retrieve number of search results for exact query
  #
  # Args:
  
  #Construct search url
  g.url <- "http://www.google.com/search?q="
  search.url <- paste(g.url,  
                      '"',  gsub(" ", "+", search.term), '"',
                      sep = "")
  
  #Get URL from Google
  search.html <- getURL(search.url)
  
  #Parse HTML
  parse.search <- htmlTreeParse(search.html, useInternalNodes = TRUE)
  
  #Extract result statistics
  nodes <- getNodeSet(parse.search, "//div[@id='resultStats']")
  value <- strsplit(xmlValue(nodes[[1]])," ", fixed = TRUE)[[1]][2]
  return(as.numeric(gsub(",", "", value, fixed = TRUE)))
  
}

##Example
  term <- "government shutdown"
  results <- data.frame() #log for the results

#Loop through
for( i in 2004:2019){
  keyword <- paste(term, i)
  results <- rbind(results,
                   data.frame(year = i, 
                              cnt = google.counts(keyword)))
  print(keyword)
}

#Plot
  plot(results, type = "l")
  