
library(stringr)
library(lubridate)

filenames = list.files("RedTide/gov req/")

SBNUM_MONdayyear = c(3:7,9,10)

date = NA

for (i in filenames[SBNUM_MONdayyear]) {
  
  r = list.files(paste0("RedTide/gov req/", i))
  
  print(r)

  mon = paste0(str_sub(r, start = -11, end = -10),
  str_sub(str_extract(r, "_\\D*\\D"), start = 2L, end = -2),
  str_sub(r, start = -6, end = -5))
  
  print(mon)
  
  mon = dmy(mon)
  
  if(is.na(date)){
    date = mon
  }
  else{
    date = c(date, mon)
  }
  
}

write.csv(as.data.frame(date[order(date)]), "lableprep.csv")


