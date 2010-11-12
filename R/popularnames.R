babyname <- function(name, sex = "N", nyrs = 10)
#gets data from bayname.cgi
{
  base <- "http://www.socialsecurity.gov/cgi-bin/babyname.cgi"
  ret <- postForm(base, name = name,
    sex = sex, nyrs = nyrs, style = "POST")
  retp <- htmlParse(ret)
  raw <- sapply(getNodeSet(retp, "//td//td"), xmlValue)

  #check to see if there is the a subscript
  if(any(raw == "a")){
    drop <- length(raw) - 2
  } else {
    drop <- length(raw) - 1
  }

  raw[raw == "a"] <- NA

  
  
  m <- as.data.frame(matrix(raw[1:drop], ncol = 2, byrow=TRUE))
  
  names(m) <- c("year", "rank")
  m$year <- as.numeric(as.character(m$year))
  m$rank <- as.numeric(as.character(m$rank))

  return(m)
}
  
popularname <- function(year, top=20, number="n")
#gets data from popularnames.cgi
{
  base <- "http://www.socialsecurity.gov/cgi-bin/popularnames.cgi"
  ret <- postForm(base, year = year,
    top = top, number = number, style = "POST")
  retp <- htmlParse(ret)

  raw <- sapply(getNodeSet(retp, "//td//td"), xmlValue)
  m <- as.data.frame(matrix(raw[-length(raw)], ncol = 5, byrow=TRUE))
  m2 <- data.frame(rank = rep(m[,1], 2),
                   sex = rep(c("male", "female"), each = nrow(m)),
                   name = unlist(m[ ,c(2, 4)]),
                   value = unlist(m[ ,c(3, 5)]))
  m2$number <- gsub(",", "", m2$number)
  m2$rank <- as.numeric(as.character(m2$rank))
  m2$value <- as.numeric(as.character(m2$value))
                        
  rownames(m2) <- seq(nrow(m2))
  m2
}

