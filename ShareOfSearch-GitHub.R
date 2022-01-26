library(data.table)
library(xlsx)
library(httr)
library(rvest)

### import search Terms and prep the data
### I have omitted my folder structure for privacy reasons
searchTerms <- read.csv('makroKeywords.csv', header = TRUE)
searchTerms <- unlist(searchTerms[,1])
searchTerms <- gsub('\\,', '', searchTerms)

### Function that scrapes the data
sosScrape <- function(st, productsPerPage, onlyScrapeFirstPage){
  
   ### Get how many pages of results exist
   lastPageIndex <- as.numeric(html_text(html_nodes(read_html(paste0('https://www.makro.co.za/search/?text=', st)), xpath = '/html/body/main/div[11]/div[3]/div/div/div[3]/div[2]/div/div/div[1]/div/div[2]/div/ul/li[6]/a'))[1])
   
   ### Vector of page numbers from 0 to the last page
   pages <- 0 : (lastPageIndex - 1)
  
   
   ### Scrape either only the first page of each product or all pages of each product - based on parameter 'onlyScrapeFirstPage'
  if(onlyScrapeFirstPage == 0){
     ### scrape all pages
     productNames <- lapply(pages, function(x) unlist(lapply(html_text(html_nodes(read_html(paste0('https://www.makro.co.za/search?q=', st, '%3Arelevance&page=', x)), xpath = '//a[@class = "product-tile-inner__productTitle js-gtmProductLinkClickEvent"]')), function(x) trimws(gsub('\n|\t', '', x), 'right'))))
     links <- lapply(pages, function(x) unlist(lapply(html_text(html_nodes(read_html(paste0('https://www.makro.co.za/search?q=', st, '%3Arelevance&page=', x)), xpath = '//a[@class = "product-tile-inner__img js-gtmProductLinkClickEvent"]/@href')), function(x) trimws(gsub('\n|\t', '', x), 'right'))))
  }else if(onlyScrapeFirstPage == 1){
      ### scrape first page
      productNames <- trimws(gsub('\n|\t', '', html_text(html_nodes(read_html(paste0('https://www.makro.co.za/search?q=', st, '%3Arelevance&page=', '0')), xpath = '//a[@class = "product-tile-inner__productTitle js-gtmProductLinkClickEvent"]'))), 'right')
      links <- trimws(gsub('\n|\t', '', html_text(html_nodes(read_html(paste0('https://www.makro.co.za/search?q=', st, '%3Arelevance&page=', '0')), xpath = '//a[@class = "product-tile-inner__img js-gtmProductLinkClickEvent"]/@href'))), 'right') 
  }
  
  
  ### substitute for link and product name
  if(length(productNames) == 0){
    links <- c('tmp', 'tmp')
    productNames <- c('tmp', 'tmp')
  }

  ### create DF of product name and link
  tmp <- data.frame(link = unlist(links), name = unlist(productNames))
  
  ### add Position on Page column - loop
  tmp$posOnPage <- 1
  for(i in 2 : nrow(tmp)){
    if(tmp$posOnPage[i-1] != 20){tmp$posOnPage[i] <- tmp$posOnPage[i-1] + 1}else{
      tmp$posOnPage[i] <- 1
    }
  }
  
  ### add position on page column - Vectorised operation - WIP
  # tmp$posOnPage <- 1
  # tmp$posOnPage <- lapply(tmp$posOnPage, function(x) if(x[which(x != 20, arr.ind = TRUE) - 1]) return(x[which(x != 20, arr.ind = TRUE) - 1] + 1) else return(1))
  
  ### Add page number column to see which page the result was on
  tmp$pageNumber[1] <- 1
  for(i in 2:nrow(tmp)){
    if(tmp$posOnPage[i - 1] != 20) {tmp$pageNumber[i] <- tmp$pageNumber[i - 1]}else{
      tmp$pageNumber[i] <- tmp$pageNumber[i - 1] + 1
    }
  }
  
  
  ### calculate the overall position of the product in the search
  tmp$position <- productsPerPage * (tmp$pageNumber - 1) + tmp$posOnPage
  
  ### populate the search term column
  tmp$searchTerm <- st
  
  ### remove unnecessary columns
  tmp <- tmp[, -which(names(tmp) == c('posOnPage', 'pageNumber'))]
  
  
  return(tmp)
}

### call function to get data
### record start time of scrape
sTime <- Sys.time()

### The search times out after approx 347 products. broken the search up into chunks to avoid this. Working on a more sophisticated method of doing this
### to work with any number of search terms
one <- 1:347
two <- 348 : 694
three <- 695 : 1023
indexList <- list(one, two, three)

### test case for using only 3 search terms
one <- 1:3
indexList <- list(one)


### Run the function declared above - sosScrape
for(i in 1:length(indexList)){
  if(i == 1) {
    finalTmp <- rbindlist(lapply(searchTerms[indexList[[i]]], sosScrape, productsPerPage = 20, onlyScrapeFirstPage = 1), use.names = TRUE)
    final <- finalTmp} else {final <- rbind(final, rbindlist(lapply(searchTerms[indexList[[i]]], sosScrape), use.names = TRUE))}
}

### Alternative way of running the function above
finalTest <- lapply(1:length(indexList), function(y) 
  if(y == 1) return(rbindlist(lapply(searchTerms[indexList[[y]]], sosScrape, productsPerPage = 20, onlyScrapeFirstPage = 1), use.names = TRUE, fill = TRUE)) 
  else return(rbind(finalTest, rbindlist(lapply(searchTerms[indexList[[y]]], sosScrape), use.names = TRUE, fill = TRUE))))

unlisted <- cbind(finalTest[[1]][[1]], finalTest[[1]][[2]], finalTest[[1]][[3]], finalTest[[1]][[4]])
unlisted <- as.data.frame(unlisted)
colnames(unlisted) <- c('link', 'name', 'position', 'searchTerm')
final <- unlisted

### Clean the data up
final$link <- paste0('www.makro.co.za', final$link)
final$searchTerm <- gsub('\\+', ' ', final$searchTerm)


### Export data
write.xlsx(final, file.path(getwd(), '/unlistedSos.xlsx'), sheetName = 'data', row.names = FALSE)

### Record end time of scrape
eTime <- Sys.time()

### Calculate length of scrape
fTime <- eTime - sTime
