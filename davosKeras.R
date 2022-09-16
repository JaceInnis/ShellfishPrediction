library(keras)
library(lubridate)
data = read.csv("davosMasterdata.csv")
data = data[,c(-1,-2)]


plot(data$toxbi ~ yday(mdy(data$date)))
plot(data$toxbi)
rep = colnames(data)
 
 for (i in 1:length(data)) {
   plot(data[,i], main = rep[i], xlab = i, type = "l")
 }

#results = data.frame(res.inclu = NA, vmin = NA, vmax = NA, minin = NA, maxin = NA, lookback = NA,
#                     delay = NA, batch_size = NA,dropout = NA, unitsl1 = NA, unitsl2 = NA, loss = NA, 
#                     accuracy =  NA, val_loss = NA, val_accuracy = NA) 
 
# -------------------------------------------------------------------


train_data <- data[1:300,-1]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data[,-1] <- scale(data[,-1], center = mean, scale = std)

data = data.matrix(data)


generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index)) max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                ( -1 +     dim(data)[[-1]])))
    targets <- array(0, dim = c(length(rows)))
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,    -1    ]
      targets[[j]] <- data[rows[[j]] + delay,1]
    }
    list(samples, targets)
  }
}


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------

#571


plot(data[380:571,1])



vmin = 400
vmax = 560
minin = 1
maxin = vmin-1
lookback <- 24
step <- 1
delay <- 5
batch_size <- 24

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
  shuffle = TRUE,
  step = step,
  batch_size = batch_size
)

val_steps <- (vmax - vmin - lookback) / batch_size
}

# -------------------------------------------------------------------


dropout = 0.6
unitsl1 = 2
unitsl2 = 1


model <- keras_model_sequential() %>%
  layer_gru(units = unitsl1, 
            dropout = dropout, 
            recurrent_dropout = dropout,
            input_shape = list(NULL, (-1   + dim(data)[[-1]]))
            ) %>%
  layer_dense(units = 1 , activation = "sigmoid")
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)



history <- model %>% fit(
  train_gen, 
  steps_per_epoch = 100,
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)


results = rbind(results, data.frame(res.inclu = "NO", vmin = vmin, vmax = vmax, minin = minin, maxin = maxin, 
                          lookback = lookback, delay = delay, batch_size = batch_size, dropout = dropout, 
                          units = unitsl1, loss = tail(history[[2]][[1]],1), accuracy = tail(history[[2]][[2]],1),
                          val_loss = tail(history[[2]][[3]],1), val_accuracy = tail(history[[2]][[4]],1),
                          epochs = history[[1]][[2]], steps = history[[1]][[3]], other = "rmsprop,binary_crossentropy") )





test = array(NA, dim = c(vmax-vmin, lookback, (-1 + dim(data)[[-1]])))


for (i in 1:(vmax-vmin)) {
  for (w in 1:lookback) {
    test[i,w,] = data[(i-1)+vmin-(w-1-delay), -1 ]
  }
}



res = model %>% predict(test)

plot(data[vmin:vmax,1])
points(res) 








#results = data.frame(res.inclu = NA, vmin = NA, vmax = NA, minin = NA, maxin = NA, lookback = NA,
#                     delay = NA, batch_size = NA,dropout = NA, units = NA, loss = NA, 
#                     accuracy =  NA, val_loss = NA, val_accuracy = NA, epochs = NA, steps = NA, other = NA) 





res = rep(NA, 572)

for (i in vmin:vmax) {
  
  res[i] = abs(data[,1][i] - data[,1][i+3])
}

1- mean(res, na.rm = TRUE)



