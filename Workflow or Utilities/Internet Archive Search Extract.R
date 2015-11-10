news.extract <- function(term, start.day, end.day, by = "day") {
#   Can be use to search the three major news channels and all news recorded by the internet archive
#   Seee the archive's search at: https://archive.org/details/tv
#   Currently will pause in between slightly in order to be nice to internet archive.
  
  if(!require(jsonlite)) stop("Must have jsonlite installed")
  if( attr(start.day, "class")!="Date" | attr(start.day, "class")!="Date") stop("Please enter dates only using as.Date()")
  days <- seq(start.day, end.day, by=by)
  fox.count <- numeric(length(days))
  t <- Sys.time()
  
  for (ii in 1:length(days)) {
    date <- as.character(days[ii], format = "%Y%m%d")
    for (jj in c(0,50,100,150,200,250)) {
      url <-
        paste(
          "https://archive.org/details/tv?q=%22", term, "%22&output=json&fq=channel:%22FOXNEWSW%22&time=", date, "&start=",jj, sep =
            ""
        )
      kk <- 0
      while(kk<5){
        page <- tryCatch(
          { 
            out <- suppressWarnings(readLines(url))
          },
          error = function(cond) {
            print("URL FAILED:")
            print(url)
            print(cond)
            return(NULL)
          },
          warning = function(cond) {
            print("URL Failed")
            print(url)
            print(cond)
            return(NULL)
          }
        )
        if(is.null(page)){
          kk <- kk + 1
          print("Trying URL Again")
          Sys.sleep(runif(1, 10, 30))
        } else kk <- 6
      }
      
      if(is.null(page)) break
      
      
      tmp.page <- gsub("},,", "},", paste(page,collapse = "", sep = ""))
      tmp.page <- gsub("},]", "}]", tmp.page)
      
      tmp.data <-
        tryCatch(
          fromJSON(tmp.page), error = function(cond) {
            print("JSON FAILED:")
            print(url)
            return(NULL)
          },
          warning = function(con) {
            print("JSON FAILED:")
            print(url)
            return(NULL)
          }
        )
      if (is.null(tmp.data)) break
      
      
      if (is.null(nrow(tmp.data))) {
        break
      } else {
        text <- paste(tmp.data$snip, sep = " ", collapse = " ")
        fox.count[ii] <-
          fox.count[ii] + length(gregexpr(term, text, ignore.case = T)[[1]])
      }
      Sys.sleep(runif(1,1,5))
    }
    
  }
  
  
  cnn.count <- numeric(length(days))
  for (ii in 1:length(days)) {
    date <- as.character(days[ii], format = "%Y%m%d")
    for (jj in c(0,50,100,150,200,250)) {
      url <-
        paste(
          "https://archive.org/details/tv?q=%22", term, "%22&output=json&fq=channel:%22CNNW%22&time=", date, "&start=",jj, sep =
            ""
        )
      
      kk <- 0
      while(kk<5){
        page <- tryCatch(
          { 
            out <- suppressWarnings(readLines(url))
          },
          error = function(cond) {
            print("URL FAILED:")
            print(url)
            print(cond)
            return(NULL)
          },
          warning = function(cond) {
            print("URL Failed")
            print(url)
            print(cond)
            return(NULL)
          }
        )
        if(is.null(page)){
          kk <- kk + 1
          print("Trying URL Again")
          Sys.sleep(runif(1, 10, 30))
        } else kk <- 6
      }
      
      if(is.null(page)) break
      
      
      tmp.page <- gsub("},,", "},", paste(page,collapse = "", sep = ""))
      tmp.page <- gsub("},]", "}]", tmp.page)
      
      tmp.data <-
        tryCatch(
          fromJSON(tmp.page), error = function(cond) {
            print("JSON FAILED:")
            print(url)
            return(NULL)
          },
          warning = function(con) {
            print("JSON FAILED:")
            print(url)
            return(NULL)
          }
        )
      if (is.null(tmp.data)) break
      
      
      if (is.null(nrow(tmp.data))) {
        break
      } else {
        text <- paste(tmp.data$snip, sep = " ", collapse = " ")
        cnn.count[ii] <-
          cnn.count[ii] + length(gregexpr(term, text, ignore.case = T)[[1]])
      }
      Sys.sleep(runif(1,1,5))
    }
    
  }
  
  
  msnbc.count <- numeric(length(days))
  for (ii in 1:length(days)) {
    date <- as.character(days[ii], format = "%Y%m%d")
    for (jj in c(0,50,100,150,200,250)) {
      url <-
        paste(
          "https://archive.org/details/tv?q=%22", term, "%22&output=json&fq=channel:%22MSNBCW%22&time=", date, "&start=",jj, sep =
            ""
        )
      
      kk <- 0
      while(kk<5){
        page <- tryCatch(
          { 
            out <- suppressWarnings(readLines(url))
          },
          error = function(cond) {
            print("URL FAILED:")
            print(url)
            print(cond)
            return(NULL)
          },
          warning = function(cond) {
            print("URL Failed")
            print(url)
            print(cond)
            return(NULL)
          }
        )
        if(is.null(page)){
          kk <- kk + 1
          print("Trying URL Again")
          Sys.sleep(runif(1, 10, 30))
        } else kk <- 6
      }
      
      if(is.null(page)) break
      
      
      tmp.page <- gsub("},,", "},", paste(page,collapse = "", sep = ""))
      tmp.page <- gsub("},]", "}]", tmp.page)
      
      tmp.data <-
        tryCatch(
          fromJSON(tmp.page), error = function(cond) {
            print("JSON FAILED:")
            print(url)
            return(NULL)
          },
          warning = function(con) {
            print("JSON FAILED:")
            print(url)
            return(NULL)
          }
        )
      if (is.null(tmp.data)) break
      if (is.null(tmp.data)) break
      
      if (is.null(nrow(tmp.data))) {
        break
      } else {
        text <- paste(tmp.data$snip, sep = " ", collapse = " ")
        msnbc.count[ii] <-
          msnbc.count[ii] + length(gregexpr(term, text, ignore.case = T)[[1]])
      }
      Sys.sleep(runif(1,1,5))
    }
    
  }
  
  all.count <- numeric(length(days))
  for (ii in 1:length(days)) {
    date <- as.character(days[ii], format = "%Y%m%d")
    for (jj in c(0,50,100,150,200,250,300,350,400,500)) {
      url <-
        paste(
          "https://archive.org/details/tv?q=%22", term, "%22&output=json&time=", date, "&start=",jj, sep =
            ""
        )
      
      kk <- 0
      while(kk<5){
        page <- tryCatch(
          { 
            out <- suppressWarnings(readLines(url))
          },
          error = function(cond) {
            print("URL FAILED:")
            print(url)
            print(cond)
            return(NULL)
          },
          warning = function(cond) {
            print("URL Failed")
            print(url)
            print(cond)
            return(NULL)
          }
        )
        if(is.null(page)){
          kk <- kk + 1
          print("Trying URL Again")
          Sys.sleep(runif(1, 10, 30))
        } else kk <- 6
      }
      
      if(is.null(page)) break
      
      
      tmp.page <- gsub("},,", "},", paste(page,collapse = "", sep = ""))
      tmp.page <- gsub("},]", "}]", tmp.page)
      
      tmp.data <-
        tryCatch(
          fromJSON(tmp.page), error = function(cond) {
            print("JSON FAILED:")
            print(url)
            return(NULL)
          },
          warning = function(con) {
            print("JSON FAILED:")
            print(url)
            return(NULL)
          }
        )
      if (is.null(tmp.data)) break
      
      if (is.null(nrow(tmp.data))) {
        break
      } else {
        text <- paste(tmp.data$snip, sep = " ", collapse = " ")
        all.count[ii] <-
          all.count[ii] + length(gregexpr(term, text, ignore.case = T)[[1]])
      }
      Sys.sleep(runif(1,5,10))
    }
    
  }
  
  total <- msnbc.count + cnn.count + fox.count
  df <-
    data.frame(
      "day" = days, "MSNBC" = msnbc.count, "CNN" = cnn.count, "FOX" = fox.count, "Cable-News-Total" =
        total, "All" = all.count
    )
  write.csv(df, paste(term, "-Media.csv", sep = ""), row.names = F)
  print("Took a total of:")
  print(Sys.time() - t)
}
