library(keras)
library(lubridate)
data = read.csv("davosMasterdata.csv")
data = data[,c(-1, -2 , -3 )]


# rep = colnames(data)
# for (i in 1:length(data)) {
#    plot(data[,i], main = rep[i], xlab = i, type = "l")
#  }

 
# -------------------------------------------------------------------


train_data <- data[1:300,-1]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data[,-1] <- scale(data[,-1], center = mean, scale = std)

data$num = 1:nrow(data)
tlist = data$num[data[,1] == 1][1:36]
data = data[,-16]

data = data.matrix(data)
datahold = data
velv = c(-15, -11, -7)
velu = velv+1
sal = velv+2
temp = velv+3


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------

data = datahold[,c(-13, -5, -6, -7)]

data = datahold


# -------------------------------------------------------------------

vmin = 380
vmax = 560
lookback <- 24
delay <- 5
copn = 0
maxin = vmin-1

leng =  (sum(data[(1):(maxin),1])*copn)

# ----------------------------------------------------------------------------------------------------------------------------------
for (u in 1) {
  
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
      val[i,w,] = data[ (i-1) + vmin - (w-1) + delay, -1 ]
    }
  }
  
  for(q in 1:(vmax-vmin)){
    vlab[q] = data[(q-1)+vmin+delay,1]
  }
  
}



for (u in 1) {
  
  minin = lookback + 1
  maxin = vmin-1
  
  
  samp =  array(NA, dim = c(maxin - minin+ leng, lookback, -1 + dim(data)[-1]))
  lab = rep(NA, (maxin - minin+ leng))
  
  for (i in 1:(maxin - minin)) {
    for (w in lookback:1) {
      samp[i,w,] = data[(i-1)+minin-(w-1), -1 ]
    }
  }
  
  for(q in 1:(maxin - minin)){
    lab[q] = data[(q-1)+minin+delay,1]
  }
  
  
  if(leng> 1){
    o = 1
    for (i in (maxin - minin + 1): (maxin - minin+ leng)) {
      for (w in lookback:1) {
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
    for (w in lookback:1) {
      val[i,w,] = data[ (i-1) + vmin - (w-1) + delay, -1 ]
    }
  }
  
  for(q in 1:(vmax-vmin)){
    vlab[q] = data[(q-1)+vmin+delay,1]
  }
  
} # reverse



# -------------------------------------------------------------------

dropout = 0.4
unitsl1 = 4
unitsl2 = 1


model <- keras_model_sequential() %>%
  layer_lstm(units = 4, 
            dropout = dropout, 
            recurrent_dropout = dropout,
            input_shape = list(NULL, (dim(samp)[[3]]))
            ) %>%
  layer_dense(units = 1 , activation = "sigmoid")
model %>% compile(
  optimizer = "rmsprop",
  loss = "Poisson",
  metrics = c(metric_true_positives())
)




for (i in 1:300) {

history <- model %>% fit(
  samp,
  lab,
  epochs = 1,
  validation_data = list(val, vlab),
  shuffle = TRUE )

print(i)

res = model %>% predict(samp)
vres = model %>% predict(val)

plot(lab, col = "red", xlab = NA)
points(res) 

plot(vlab, col = 'red', xlab = NA)
points(vres)


}

 par( mfrow= c(2,1) )


  plot(vlab, col = 'red')
points(vres)


  results = rbind(results, data.frame(res.inclu = "NO", vmin = vmin, vmax = vmax, minin = minin, maxin = maxin, 
                                    lookback = lookback, delay = delay, batch_size = batch_size, dropout = dropout, 
                                    units = unitsl1, loss = tail(history[[2]][[1]],1), accuracy = tail(history[[2]][[2]],1),
                                    val_loss = tail(history[[2]][[3]],1), val_accuracy = tail(history[[2]][[4]],1),
                                    epochs = history[[1]][[2]], steps = history[[1]][[3]], other = "Only the second event is predicted") )


1-mean(data[vmin:vmax,1])



# results = data.frame(res.inclu = NA, vmin = NA, vmax = NA, minin = NA, maxin = NA, lookback = NA,
#                     delay = NA, batch_size = NA,dropout = NA, units = NA, loss = NA,
#                     accuracy =  NA, val_loss = NA, val_accuracy = NA, epochs = NA, steps = NA, other = NA)





res = rep(NA, 572)

for (i in vmin:vmax) {
  
  res[i] = abs(data[,1][i] - data[,1][i+3])
}

1- mean(res, na.rm = TRUE)






for (i in 1:(vmax-vmin)) {
  for (w in 1:lookback) {
    test[i,w,] = data[(i-1)+vmin-(w-1), -1 ]
  }
}



mean()


res = val_gen()

res[[1]][2,,1]


data[380:399,2]


test[1,,1]


for (i in 1) {
  train_gen <- generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = minin,
    max_index = maxin,
    shuffle = TRUE,
    step = step,
    batch_size = batch_size
  )
  
  val_gen = generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = vmin,
    max_index = vmax,
    shuffle = FALSE,
    step = step,
    batch_size = batch_size
  )
  
  val_steps <- (vmax - vmin - lookback) / batch_size
}















