#data preparation
#anomoly detection


library(lubridate)
library(stringr)

valsplit = 0.7


#### \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////

fada = read.csv("7dayData/irong-irong.csv")

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
  plot(fada[,i], main = colnames(fada)[i])
}
# this loop will take anomolys data and replace it with NAs

for (i in (3: length(fada))[((nonhycom * -1)+2)]) { # removes precipitaiton (-7)
  sa = (fada[!is.na(fada[,i]),i])
  error = abs((fada[,i] - mean(sa))/sd(sa))
  print(sum((error>2 & !is.na(error))))
  fada[(error>2 & !is.na(error)),i] = NA
}
  
for (i in 2:length(fada)) {
  plot(fada[,i], main = colnames(fada)[i])
}

#plot(fada[(fada[,18]<1000),14], main = colnames(fada)[i])



#### \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////

#                                         Anno lm

fada[,1][yday(fada[,1])==366] = fada[,1][yday(fada[,1])==366] -1 # filters leap years

day = (yday(fada[,1]))[dayd] # takes dates from df

keep = 3:length(fada) # int used in the below loop

tim = ymd("20120101") + (0:((364*10)+9)) # creates 3k+ vector of every day for the plots
  

for (w in keep) { # creates sin model and adds to data frame the new anomoly data and normalizes it
  
  fada[,w] = (fada[,w] - mean(fada[dayd, w], na.rm = TRUE))/sd(fada[dayd, w], na.rm = TRUE) # normalizes the data using training interval 
  
  annolm <- lm( fada[dayd, w]  ~ sin((2*pi/365)*day)+cos((2*pi/365)*day)
                +sin((4*pi/365)*day)+cos((4*pi/365)*day))
  
  mod = predict(annolm, data.frame(day = 1:365))
  
  mod = as.data.frame(mod)$mod
  
  fada[is.na(fada[,w]),w] = (mod[day(fada[is.na(fada[,w]),1])])
  
  plot(fada[,1], fada[,w] , main = colnames(fada)[w])
  points( rep(mod,10) ~ tim, type = "l", col = "red")
  
  fada = cbind( fada, x =  (fada[,w] - mod[yday(fada[,1])]))
  
  colnames(fada) = c(colnames(fada)[1:(length(fada)-1)], (paste0(colnames(fada)[w], "anno" )))

}


#fada = fada[,c(-30:-34, -26, -15 )]     # filters non necessary annom data


# include day
fada$sday = sin(sday/58)


### \\\\\\\\\/////////////\\\\\\\\\/////////////\\\\\\\\\\\///////////\\\\\\\//////////


for (l in 2:length(fada)) {
  plot(fada[,l] ~ fada[,1], main = colnames(fada)[l])
  
  print(sum(is.na(fada[,l])))
}

#fada = fada[-1:-450,]


