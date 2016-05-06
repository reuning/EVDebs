library(rvest)
library(stringr)
library(RCurl)
library(qdap)
library(magrittr)
library(data.table)

read.url <- function(url){
  page <- tryCatch(
    { 
      out <- read_html(url)
    },
    error = function(cond) {
      f <- file("Debug.txt", open="at")
      writeLines(paste("URL Failed: ", url, "\n",cond, sep=""), f)
      close(f)
      
      return(NULL)
    },
    warning = function(cond) {
      f <- file("Debug.txt", open="at")
      writeLines(paste("URL Failed: ", url, "\n",cond, sep=""), f)
      close(f)
      
      return(NULL)
    }
  )
  return(page)
}

news.extract <- function(search.term, days, count.term=NULL, store.text=T) {
  #   Can be use to search the three major news channels and all news recorded by the internet archive
  #   Seee the archive's search at: https://archive.org/details/tv
  #   Currently will pause in between slightly in order to be nice to internet archive.
  
  if(is.null(count.term))  count.term <- search.term
  
  file.name <- paste("counts-", count.term, "-Media.csv", sep = "")
  check.file <- paste("check-", count.term, ".csv", sep="")
  if(store.text)   text.file <- paste("text-", count.term, ".csv", sep="")
  if(!(file.name %in% dir())){
    f.database <- data.frame(days=days, total=0, polarity=0)
    write.csv(f.database, file=file.name, row.names=F)
    f.check <- data.frame(check=0)
    write.csv(f.check, file=check.file, row.names=F)
    if(store.text){
      f.text <- data.frame(days=days, text=NA)
      write.csv(f.text, file=text.file, row.names=F)
    }
  } else {
    f.database <- read.csv(file.name, stringsAsFactors = F)
    f.database$days <- as.Date(f.database$days)
    f.check <- read.csv(check.file)
    if(store.text){
      f.text <- read.table(text.file, header=T)
      f.text$days <- as.Date(f.text$days)
    }
  }
  
  results.left <- T
  while(results.left) {
    search.num <- f.check$check
    
    url <-
      paste(
        "https://archive.org/details/tv?q=%22", search.term, "%22%20AND%20(%20channel:MSNBC%20OR%20channel:MSNBCW%20OR%20channel:FOXNEWS%20OR%20channel:FOXNEWSW%20OR%20channel:CNN%20OR%20channel:CNNW%20)&sort=start+asc&time=20110501-20120103", "&start=",search.num, sep =
          ""
      )
    
    kk <- 0
    ### Tries URL until it works (or well for 5 times)
    while(kk<5){
      page <- read.url(url)
      if(is.null(page)){
        kk <- kk + 1
        print("Trying URL Again")
        Sys.sleep(runif(1, 10, 30))
      } else kk <- 6
    }
    
    if(is.null(page)) break
    text.data <- character()
    
    
    #### The magittr is weird, it allows you to basically feed the output into the following function
    #### This isa boring case as it just feeds the page variable into the html_nodes function
    #### The below uses show how they can be chained. 
    events <- page %>% html_nodes("div[class='tvcol topinblock']") 
    text <- NULL
    text.sent <- NULL
    
    if(length(events)==0){
      ## if no events, ends loop, returns 0 for this search
      results.left <- F
      tmp.tot <- 0
      
    } else {
      ## if events then pulls each one and counts
      
      urls <- suppressWarnings(events %>% html_node("a") %>%
                                 html_attr("href"))
      tmp <- events %>% html_node("div[class='sniptitle']") %>% html_nodes("div") %>% html_text()
      tmp <- tmp[1:length(events)*3]
      tmp.time <- str_sub(tmp, 1, -4)
      tmp.tz <- str_sub(tmp, -3, -1)
      tmp.time <- strptime(tmp.time, format="%b %e, %Y %I:%M%p ")
      for(ll in 1:length(events)){
        if(tmp.tz[ll]=="PST" | tmp.tz[ll]=="PDT" ){
          tmp.time[ll] <- tmp.time[ll] + 3
        } else if(tmp.tz[ll] =="CST" | tmp.tz[ll]=="CDT" ) {
          tmp.time[ll] <- tmp.time[ll] + 1
        } else if(tmp.tz[ll] == "MST" | tmp.tz[ll]=="MDT" ) {
          tmp.time[ll] <- tmp.time[ll] + 2
        }
      }
      day.event <- as.Date(tmp.time)
      
      for(jj in 1:length(urls)){
        tmp.url <- paste("https://archive.org", urls[jj], sep="")
        mm <- 0
        ### Tries URL until it works (or well for 5 times)
        while(mm<5){
          event.page <- read.url(tmp.url)
          if(is.null(page)){
            mm <- mm + 1
            print("Trying URL Again")
            Sys.sleep(runif(1, 10, 30))
          } else mm <- 6
        }
        
        if(is.null(event.page)) break
        
        text <- event.page %>% html_nodes("div[class='snipin nosel']") %>% html_text()
        

        ### The lapply down there is where the magic is, it takes the list of text and 
        ### Puts it in the str_count to count the mentions of candidates last name 
        f.database[f.database$days==day.event[jj],"total"] <- f.database[f.database$days==day.event[jj],"total"] + sum(unlist(lapply(tolower(text), str_count, pattern=count.term)))

        if(store.text){
          if(is.na(f.text[f.text$days==day.event[jj], "text"])){
            f.text[f.text$days==day.event[jj], "text"] <-   text.sent
          } else {
            f.text[f.text$days==day.event[jj], "text"] <- paste(f.text[f.text$days==day.event[jj], "text"], text.sent, sep=" ")
          }
        }

        
        Sys.sleep(runif(1, 0, 2))
        closeAllConnections()

      }
      
    }
    
    f.check$check <- f.check$check +50 
    write.csv(f.database, file=file.name, row.names=F)
    write.csv(f.check, file=check.file, row.names=F)
    if(store.text) write.table(f.text, file=text.file, row.names=F)
  }
}

