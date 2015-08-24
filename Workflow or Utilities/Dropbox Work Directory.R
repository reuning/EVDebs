drop.wd <- function(s.loc = "") {
  ### Code adapted from: http://applyr.blogspot.com/2012/08/get-path-to-your-dropbox-folder.html
  ### defaults to setting the dropbox folder itself as the WD.
  ### currently checks for the dropbox location in to settings folders. 
  ### The try errors are overly complicated but something with tryCatch wouldn't work. 

  if (!require(RCurl)) stop ("You need to install RCurl package.")
  if (Sys.info()["sysname"]!="Windows") stop("Currently, 'get.dropbox.folder' works for Windows only. Sorry.")
  
  base64coded <- try(
    {
      db.file <- paste(Sys.getenv('APPDATA'), '\\Dropbox\\host.db', sep='')
      suppressWarnings(readLines(db.file))[2]
    }, silent=T)
  if(any(attr(base64coded, "class")=="try-error" , !is.null(attr(base64coded, "class")))){
    base64coded <- try(
      {
        db.file <- paste(Sys.getenv('LOCALAPPDATA'), '\\Dropbox\\host.db', sep='')
        suppressWarnings(readLines(db.file))[2]
      }, silent=T)
  }
  if(any(attr(base64coded, "class")=="try-error", !is.null(attr(base64coded, "class")))) stop("Cannot find Dropbox")
  
  
  
  wd <- paste(base64(base64coded, encode=F), s.loc, sep="\\")
  setwd(wd)
  paste("Work Directory set to:",wd)
}