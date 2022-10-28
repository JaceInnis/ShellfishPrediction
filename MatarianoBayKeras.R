library(keras)

library(lubridate)
data = read.csv("MatarianoBayMaster.csv")
par( mfrow= c(1,1) )

data = data[,c(-1, -2, -4)]
#data$date = mdy(data$date)

min  = 360
max = 400
plot(data$site[min:max]~data$date[min:max])
(max - min)*7

col = colnames(data)
for (i in 1:length(data)) {
  plot(data[,i] ~ data$day, ylab = col[i])
}


head(data)

rep = colnames(daa)
for (i in 3:length(daa)) {
   plot(daa[,i], main = rep[i], xlab = i)
 }


# -------------------------------------------------------------------


train_data <- data[1:300,c(-1, -3 )]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data[,c(-1, -3 )] <- scale(data[,c(-1, -3 )], center = mean, scale = std)

#data$num = 1:nrow(data)
#tlist = data$num[data[,1] == 1][1:36]
#data = data[,-16]

data = data.matrix(data)
datahold = data
velv = c(-15, -11, -7)
velu = velv+1
sal = velv+2
temp = velv+3


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------

# 9 is salf
colnames(datahold)
data = datahold[,c(1, 3 ,5,10, 11, 14 ,15,16,17,19)]
#data = datahold

#with predictive power
# 14 8 3 5 9 7 11 15
#---------------


#without predictive power
# 13 12 10 6 2 
#---------------

#data = datahold


# -------------------------------------------------------------------
{
vmin = 480
lookback <- 15
delay <- 2
copn = 0

maxin = vmin-1
leng =  (sum(data[(1):(maxin),1])*copn)
vmax = nrow(data)-delay
}
# ----------------------------------------------------------------------------------------------------------------------------------


{
  
  minin = lookback + 1
  
  samp =  array(NA, dim = c(maxin - minin+ leng, lookback, -1 + dim(data)[-1]))
  lab = rep(NA, (maxin - minin+ leng))
  
  for (i in 1:(maxin - minin)) {
    for (w in 1:lookback) {
      samp[i,w,] = data[(i-1)+minin-(w-1), -1 ]
    }
  }
  
  for(q in 1:(maxin - minin)){
    lab[q] = data[(q-1)+minin+delay,1]
  }
  
  
  if(leng> 1){
    o = 1
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      for (w in 1:lookback) {
        samp[i,w,] = data[(tlist[o])-(w-1), -1 ]
      }
      o = o + 1
      if(o == 36){o = 1}
    }
    
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      lab[i] = 1
    }
    
  }
  
  
  val =  array(NA, dim = c(vmax-vmin, lookback, -1 + dim(data)[-1]))
  vlab = rep(NA, vmax-vmin)
  
  for (i in 1:(vmax-vmin)) {
    for (w in 1:lookback) {
      val[i,w,] = data[ (i-1) + vmin - (w-1), -1 ]
    }
  }
  
  for(q in 1:(vmax-vmin)){
    vlab[q] = data[(q-1)+vmin+delay,1]
  }
  
} # no incl


{
  
  minin = lookback + 1
  
  samp =  array(NA, dim = c(maxin - minin+ leng, lookback,  dim(data)[-1]))
  lab = rep(NA, (maxin - minin+ leng))
  
  for (i in 1:(maxin - minin)) {
    for (w in 1:lookback) {
      samp[i,w,] = data[(i-1)+minin-(w-1),  ]
    }
  }
  
  for(q in 1:(maxin - minin)){
    lab[q] = data[(q-1)+minin+delay,1]
  }
  
  
  if(leng> 1){
    o = 1
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      for (w in 1:lookback) {
        samp[i,w,] = data[(tlist[o])-(w-1),  ]
      }
      o = o + 1
      if(o == 36){o = 1}
    }
    
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      lab[i] = 1
    }
    
  }
  
  
  val =  array(NA, dim = c(vmax-vmin, lookback,  dim(data)[-1]))
  vlab = rep(NA, vmax-vmin)
  
  for (i in 1:(vmax-vmin)) {
    for (w in 1:lookback) {
      val[i,w,] = data[ (i-1) + vmin - (w-1) , ]
    }
  }
  
  for(q in 1:(vmax-vmin)){
    vlab[q] = data[(q-1)+vmin+delay,1]
  }
  
} 


 {
  
  minin = lookback + 1
  maxin = vmin-1
  
  
  samp =  array(NA, dim = c(maxin - minin+ leng, lookback, 0 + dim(data)[-1])) # here 
  lab = rep(NA, (maxin - minin+ leng))
  
  for (i in 1:(maxin - minin)) {
    for (w in 1:lookback) {
      samp[i,w,] = data[(i-1)+minin -(lookback - (w)),  ] # here 
    }
  }
  
  for(q in 1:(maxin - minin)){
    lab[q] = data[(q-1)+minin+delay,1]
  }
  
  
  if(leng> 1){
    o = 1
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      for (w in 1:lookback) {
        samp[i,w,] = data[(tlist[o])  -(lookback - (w)),  ]  # here 
      }
      o = o + 1
      if(o == 36){o = 1}
    }
    
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      lab[i] = 1
    }
    
  }
  
  
  val =  array(NA, dim = c(vmax-vmin, lookback, 0 + dim(data)[-1]))    # here 
  vlab = rep(NA, vmax-vmin)
  
  for (i in 1:(vmax-vmin)) {
    for (w in 1:lookback) {
      val[i,w,] = data[(i-1) + vmin  -(lookback - (w)),  ]  # here 
    }
  }
  
  for(q in 1:(vmax-vmin)){
    vlab[q] = data[(q-1)+vmin+delay,1]
  }
  
} # reverse


{
  
  minin = lookback + 1
  maxin = vmin-1
  
  
  samp =  array(NA, dim = c(maxin - minin+ leng, lookback, -1 + dim(data)[-1])) # here 
  lab = rep(NA, (maxin - minin+ leng))
  
  for (i in 1:(maxin - minin)) {
    for (w in lookback:1) {
      samp[i,w,] = data[(i-1)+minin-(lookback - (w)), -1  ] # here 
    }
  }
  
  for(q in 1:(maxin - minin)){
    lab[q] = data[(q-1)+minin+delay,1]
  }
  
  
  if(leng> 1){
    o = 1
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      for (w in lookback:1) {
        samp[i,w,] = data[(tlist[o])-(lookback - (w)),  -1]  # here 
      }
      o = o + 1
      if(o == 36){o = 1}
    }
    
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      lab[i] = 1
    }
    
  }
  
  
  val =  array(NA, dim = c(vmax-vmin, lookback, -1 + dim(data)[-1]))    # here 
  vlab = rep(NA, vmax-vmin)
  
  for (i in 1:(vmax-vmin)) {
    for (w in lookback:1) {
      val[i,w,] = data[ (i-1) + vmin - (lookback - (w)), -1 ]  # here 
    }
  }
  
  for(q in 1:(vmax-vmin)){
    vlab[q] = data[(q-1)+vmin+delay,1]
  }
  
} # reverse no incl



# -------------------------------------------------------------------

dropout = 0.3


model <- keras_model_sequential() %>%
  layer_lstm(units = 50, 
             dropout = dropout, 
             recurrent_dropout = dropout,
             input_shape = list(NULL, (dim(samp)[[3]]))
  ) %>%
  layer_dense(units = 15 , activation = "sigmoid") %>% 
    layer_dense(units = 1 , activation = "sigmoid") %>% 
  compile(
  optimizer = "rmsprop",
  loss = "Poisson",
  metrics = c(metric_true_positives())
)



lo = rep(0, 150)

gifdata = data.frame(date = NA, Value = NA, Model = NA, Set = NA, Epoch = NA)


for (i in 1:50) {
  
  history <- model %>% fit(
    samp,
    lab,
    epochs = 1,
    # validation_data = list(val, vlab),
    shuffle = TRUE )
  
  print(i)
  
  
  res = model %>% predict(samp)
  vres = model %>% predict(val)
  lo[(sum(lo>0))+1] = history$metrics$loss
  
  tempgif = rbind(data.frame(date = 1:length(lab), Value = lab, Model = "Reported", Set = "Training", Epoch = i),
  data.frame(date = 1:length(lab), Value = res, Model = "Modeled", Set = "Training", Epoch = i),
  data.frame(date = (length(lab)+1):(length(lab)+length(vlab)), Value = vlab, Model = "Reported", Set = "Validation", Epoch = i),
  data.frame(date = (length(lab)+1):(length(lab)+length(vlab)), Value = vres, Model = "Modeled", Set = "Validation", Epoch = i))
  
  gifdata = rbind(gifdata, tempgif)
  
  plot(lab, col = "red", xlab = NA)
  points(res) 

  plot(vlab, col = 'red', xlab = NA)
  points(vres)
  lines(as.data.frame(lo))
    
  readline(prompt = "space")

}


############# save ###############

model %>% save_model_hdf5("MatarianoBay-2weeks-15-50-10/22-2.h5")

############# evaluation ###############

library(ggplot2)
library(dplyr)
library(caret)


#function to get ideal threshold


model = load_model_hdf5("MatarianoBay-2weeks-15-50-10/22-1.h5")

delay <- 2


{
  
  minin = lookback + 1
  
  samp =  array(NA, dim = c(maxin - minin+ leng, lookback, -1 + dim(data)[-1]))
  lab = rep(NA, (maxin - minin+ leng))
  
  for (i in 1:(maxin - minin)) {
    for (w in 1:lookback) {
      samp[i,w,] = data[(i-1)+minin-(w-1), -1 ]
    }
  }
  
  for(q in 1:(maxin - minin)){
    lab[q] = data[(q-1)+minin+delay,1]
  }
  
  
  if(leng> 1){
    o = 1
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      for (w in 1:lookback) {
        samp[i,w,] = data[(tlist[o])-(w-1), -1 ]
      }
      o = o + 1
      if(o == 36){o = 1}
    }
    
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      lab[i] = 1
    }
    
  }
  
  
  val =  array(NA, dim = c(vmax-vmin, lookback, -1 + dim(data)[-1]))
  vlab = rep(NA, vmax-vmin)
  
  for (i in 1:(vmax-vmin)) {
    for (w in 1:lookback) {
      val[i,w,] = data[ (i-1) + vmin - (w-1), -1 ]
    }
  }
  
  for(q in 1:(vmax-vmin)){
    vlab[q] = data[(q-1)+vmin+delay,1]
  }
  
} # no incl

res = model %>% predict(samp)
vres = model %>% predict(val)
lab
vlab

re = c(as.vector(res), as.vector(vres))
labs = c(lab, vlab)

length(labs)



low = seq(0.2, 0.8, .01)
sol = rep(NA, length(low))

for (i in 1:length(low)) {

  sol[i] = sum(((labs == 1) == ( re < low[i])))

}

plot(sol~low)


# make the data frame

thresh = 0.5

plotdf = data.frame(Reported = labs == 0,
          Predicted = re < thresh,
          Set = c(rep("Training", length(lab)), rep("Validation",length(vlab))))

plotdf[,-3][plotdf[,-3] == TRUE] = "SAFE"
plotdf[,-3][plotdf[,-3] == FALSE] = "TOXIC"
plotdf[,1] = factor(plotdf[,1])
plotdf[,2] = factor(plotdf[,2])

# plot of points with color


# confusion matrices


confusionMatrix(plotdf[,1], plotdf[,2])

table <- data.frame(confusionMatrix(plotdf[,1], plotdf[,2])$table)

plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

ggplot(plotTable, aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference))) + 
  guides(fill = "none")






