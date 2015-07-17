drop.wd <- function(s.loc = "") {
  ### Code adapted from: http://applyr.blogspot.com/2012/08/get-path-to-your-dropbox-folder.html
  ### defaults to setting the dropbox folder itself as the WD.
  if (!require(RCurl)) stop ("You need to install RCurl package.")
  if (Sys.info()["sysname"]!="Windows") stop("Currently, 'drop.wd' works for Windows only. Sorry.")
  
  s.loc <- gsub("/", "\\\\", s.loc) ### convert to \\ which is  what base64 outputs
  db.file <- paste(Sys.getenv('APPDATA'), '\\Dropbox\\host.db', sep='')
  base64coded <- readLines(db.file, warn=FALSE)[2]
  
  wd <- paste(base64(base64coded, encode=F), s.loc, sep="\\")
  setwd(wd)
  paste("Work Directory set to:",normalizePath(wd, winslash="/"), sep=" ")
}


