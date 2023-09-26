
times.dep.func <- function(start, end){
  times.pred <- cbind("maxTemp"          = c(start,end),
                      "meanTemp"           = c(start,end),
                      "minTemp"            = c(start,end),
                      'RH' = c(start,end),
                      "rain"         = c(start,end),
                      "rain30"                = c(start,end),
                      "rain60"                = c(start,end),
                      "rain120"       = c(start,end),
                      "windSMean"            = c(start,end),
                      "windSMax"          = c(start,end),
                      "NDVI_A"      = c(start,end),
                      "P300m"      = c(start,end),
                      "P500m"      = c(start,end),
                      "V_Density"      = c(start,end),
                      "A_HDB_H"      = c(start,end),
                      "a_HDB_P"      = c(start,end),
                      "A_HDB_A"      = c(start,end),
                      "D_To_Drain"      = c(start,end),
                      "length_D"      = c(start,end),
                      "Forest_P"      = c(start,end),
                      "Grass_P"      = c(start,end),
                      "MVege_P"      = c(start,end),
                      "Building_P"      = c(start,end),
                      "Condo_n"  = c(start,end),
                      "Landed_n" = c(start,end),
                      "HDB_RU" = c(start,end)
  )
  
  return(times.pred)
}

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

normalize.pop <- function(x, pop) {
  for (i in 1:ncol(x)) {
    p <- pop$pop[pop$PA == colnames(x)[i]]
    x[,i] <- x[,i]/p
  }
  return(x)
}


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}



getCI <- function(org, bb){
  # @org: original synthetic control estimate
  # @bb: list of matrices containing synthetic control of  bootstrapped samples
  row <- nrow(bb[[1]])
  col <- ncol(bb[[1]])
  s <- matrix(nrow=row,ncol=col) 
  for (c in 1:col) {
    for (r in 1:row) {
      # extract 100 bootsratp samples for each sector and eweek-year and get standard deviation
      s[r,c] <- sd(unlist(rapply(bb, \(x) x[r,c,drop=F], how='list')))
    }
  }
  # order of org & bb cols - bukit batok, choa chu kang, yishun, tampines
  root.n <- sqrt(length(bb))
  ub <- org +1.96*(s/root.n)
  lb <- org -1.96*(s/root.n)
  
  return(list(ub=ub,lb=lb))
  }
  
  




