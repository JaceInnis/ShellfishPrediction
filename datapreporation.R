#data preparation
#anomoly detection



res = read.csv("7dayData/")[,2]
fada = read.csv("Final7daydata/matariano.csv")

fada[,2] = as.integer(res)

write.csv(fada, "Final7daydata/", row.names = FALSE)




library(lubridate)
library(stringr)

valsplit = 0.8


#### \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////

fada = read.csv("7dayData/matariano.csv")

fada[,1] = ymd(fada[,1])

nonhycom = (1:length(fada))[str_detect(colnames(fada), c("precipitationCal", "pr_wtr"))]

#fada[1:39, 2] = 0

#### \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////


# filters hycom data with anomolus reading (greater than 3 sd s)


dayd = 1:as.integer((nrow(fada))*valsplit) #gets the training data interval



for (i in 2:length(fada)) {
  fada[,i] = as.numeric(fada[,i])
}


for (i in 2:length(fada)) {
  plot(fada[,i], main = paste(colnames(fada)[i], i))
}
# this loop will take anomolys data and replace it with NAs

few = c(3,4,10,11,15)
# 1 3 1 3 3
man = c(6,7,13,14,17,18)

for (i in few) { # removes precipitaiton (-7)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  sa = (fada[!is.na(fada[,i]),i])
  error = abs((fada[,i] - mean(sa))/sd(sa))
  a = sum(is.na(fada[,i]))
  fada[(error>5 & !is.na(error)),i] = NA
  print(sum(is.na(fada[,i])) - a)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  
}

few = c(4,11,15)
# 2 2 2
for (i in few) { # removes precipitaiton (-7)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  sa = (fada[!is.na(fada[,i]),i])
  error = abs((fada[,i] - mean(sa))/sd(sa))
  a = sum(is.na(fada[,i]))
  fada[(error>5 & !is.na(error)),i] = NA
  print(sum(is.na(fada[,i])) - a)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  
}


for (i in man) { # removes precipitaiton (-7)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  sa = (fada[!is.na(fada[,i]),i])
  error = abs((fada[,i] - mean(sa))/sd(sa))
  a = sum(is.na(fada[,i]))
  fada[(error>2.5 & !is.na(error)),i] = NA
  print(sum(is.na(fada[,i])) - a)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  
}

for (i in man) { # removes precipitaiton (-7)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  sa = (fada[!is.na(fada[,i]),i])
  error = abs((fada[,i] - mean(sa))/sd(sa))
  a = sum(is.na(fada[,i]))
  fada[(error>3 & !is.na(error)),i] = NA
  print(sum(is.na(fada[,i])) - a)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  
}


for (i in c(6,17)) { # removes precipitaiton (-7)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  sa = (fada[!is.na(fada[,i]),i])
  error = abs((fada[,i] - mean(sa))/sd(sa))
  a = sum(is.na(fada[,i]))
  fada[(error>2 & !is.na(error)),i] = NA
  print(sum(is.na(fada[,i])) - a)
  plot(fada[,i], main = paste(colnames(fada)[i], i))
  
}


#plot(fada[(fada[,18]<1000),14], main = colnames(fada)[i])

#### \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////
# new na replacement 


for (i in 3:length(fada)) { # removes precipitaiton (-7)
  for (w in 1:nrow(fada)) {
    if(is.na(fada[w,i])){
      fada[w,i] = ((fada[w-1,i]+fada[w+1,i])/2)
    }
  }
}




sum(is.na)




#### \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////

#                                         Anno lm

fada[,1][yday(fada[,1])==366] = fada[,1][yday(fada[,1])==366] -1 # filters leap years

day = (yday(fada[,1]))[dayd] # takes dates from df

tim = ymd("20030101") + (0:((364*19)+9)) # creates 3k+ vector of every day for the plots
  
for (i in 2:length(fada)) {
  plot(fada[,i], main = paste(colnames(fada)[i], i))
}

#fada = fada[,-18]

q = length(fada)

for (w in 3:length(fada)) { # creates sin model and adds to data frame the new anomoly data and normalizes it
  
  fada[,w] = (fada[,w] - mean(fada[dayd, w], na.rm = TRUE))/sd(fada[dayd, w], na.rm = TRUE) # normalizes the data using training interval 
  
  annolm <- lm( fada[dayd, w]  ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
                +sin((4*pi/365)*day)+cos((4*pi/365)*day))
  
  mod = predict(annolm, data.frame(day = 1:365))
  
  mod = as.data.frame(mod)$mod
  
  fada[is.na(fada[,w]),w] = (mod[yday(fada[is.na(fada[,w]),1])])
  
  plot(fada[,1], fada[,w] , main = paste(colnames(fada)[w],w,(w+q-2)))
  points( rep(mod,19) ~ tim[1:length(rep(mod,19))], type = "l", col = "red")
  
  fada = cbind( fada, x =  (fada[,w] - mod[yday(fada[,1])]))
  
  colnames(fada) = c(colnames(fada)[1:(length(fada)-1)], (paste0(colnames(fada)[w], "anno" )))

}

colnames(fada)

plot(fada[,7]~fada[,14])

#fada = fada[,c(1:4,7,11,14,15,16,17,18,19,21,24,25,29)]     # filters non necessary annom data




# include day
fada$sday = sin(yday(fada$date)/58)


### \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////


for (l in 2:length(fada)) {
  plot(fada[,l] ~ fada[,1], main = colnames(fada)[l])
  
  print(sum(is.na(fada[,l])))
}

sum(fada[,14]==0)



write.csv(fada, "Final7daydata/matariano.csv", row.names = FALSE)


