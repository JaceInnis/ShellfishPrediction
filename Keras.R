library(keras)

weekdata = read.csv("newweek.csv")

data = read.csv("prepeddata.csv")

rep = colnames(data)

plot(weekdata$site)

data$resp = weekdata$site


plot(data[1:480,1], type = "l")


plot(data$resp)

for (i in 1:length(data)) {
  plot(data[,i], main = rep[i], xlab = i)
}

include = 0


data = data[,c(2,3,4, 74, 72, 59, 44, 35, 32, 29, 23, 9, 7)]


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
                                (dim(data)[[-1]])))
    targets <- array(0, dim = c(length(rows)))
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,1]
    }
    list(samples, targets)
  }
}


generator <- function(data, lookback, delay, min_index, max_index,# minus response
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
                                (dim(data)[[-1]]-include)))
    targets <- array(0, dim = c(length(rows)))
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,-1]
      targets[[j]] <- data[rows[[j]] + delay,1]
    }
    list(samples, targets)
  }
}



vmin = 25
vmax = 200


lookback <- 24
step <- 1
delay <- 2
batch_size <- 24
train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 200,
  max_index = 450,
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
  step = step,
  batch_size = batch_size
)
test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 501,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)
val_steps <- (vmax - vmin - lookback) / batch_size
test_steps <- (nrow(data) - 501 - lookback) / batch_size


# -------------------------------------------------------------------




model <- keras_model_sequential() %>%
  layer_gru(units = 4, dropout = 0.1, recurrent_dropout = 0.1,
            input_shape = list(NULL, (dim(data)[[-1]]-include))) %>%
  layer_dense(units = 3)
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)



history <- model %>% fit(
  train_gen,
  steps_per_epoch = 60,
  epochs = 30,
  validation_data = val_gen,
  validation_steps = val_steps
)


plot(history)




data[,1]




res = rep(NA, 600)

for (i in vmin:vmax) {

  res[i] = abs(data[,1][i])
}

mean(res, na.rm = TRUE)



