# set working directory
setwd(getwd())
data<-read.csv("garymorphlm.csv",header = TRUE)

# define variables
mxl<-data$maxl # maximum length
mxw<-data$maxw # maximum width
mxth<-data$maxth # maximum thickness

# assign type-varieties based upon linear metrics
data$type3<-NA
for (i in 1:(nrow(data))){
  ml <- mxl[i]
  mw <- mxw[i]
  mt <- mxth[i]

  lcheck1 <- ml %in% 80:140
  lcheck2 <- ml %in% 46:79
  lcheck3 <- ml %in% 33:45
  wcheck1 <- mw %in% 30:42
  wcheck2 <- mw %in% 20:45
  wcheck3 <- mw %in% 19:32
  tcheck <- mt %in% 5:10

  res <- if(lcheck1 && wcheck1) "L"
    else if(lcheck2 && wcheck2) "M/T"
    else if(lcheck3 && wcheck3 && tcheck) "S"
    else "UID"

  data$type3[i]<-res
}
