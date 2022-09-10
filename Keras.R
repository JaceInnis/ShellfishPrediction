
load("realmasterdata.Rdata")


weekdata = read.csv("weekdata.csv")

locats = c("Vis","SouSib","Sib","Tab","Sur","Min")
varnames = c("temp","sal","velu","velv")
atmova = c("clo","rain")
rawano = c("anno", "raw")



gendata = data.frame(weekdata$site)


colnames = c("resp")

  for (q in 1:6) {
    for (w in 1:4) {
      for (e in 1:2) {
        gendata = cbind(gendata, realmastdat[[q]][[1]][[w]][[e]])
        colnames = c(colnames, paste0(locats[q],varnames[w],rawano[e]))
        
      }
      if(q>1 & q<7){
        gendata = cbind(gendata, realmastdat[[q]][[2]][[w]])
        colnames = c(colnames, paste0(locats[q],varnames[w],"deep"))
      }
    }
  }
  for (r in 1:2) {
    for (t in 1:2) {
      gendata = cbind(gendata, realmastdat[[7]][[r]][[t]])
      colnames = c(colnames, paste0("atmo",atmova[r],rawano[t]))
      
    }
  }


realmastdat$Visayan$top$temp


colnames(gendata) = colnames

train_data <- gendata[1:300,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(gendata, center = mean, scale = std)

f = 0
for (i in nrow(data)) {
  for(w in ncol(data)){
    if(is.na(data[i,w])){
      f = f + 1
    }
}}

f







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
                                dim(data)[[-1]]))
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




lookback <- 20
step <- 1
delay <- 5
batch_size <- 100
train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 400,
  shuffle = TRUE,
  step = step,
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 301,
  max_index = 533,
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
val_steps <- (533 - 301 - lookback) / batch_size
test_steps <- (nrow(data) - 501 - lookback) / batch_size





model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1) %>%
  compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)
history <- model %>% fit(
  train_gen,
  steps_per_epoch = 80,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)













res = rep(NA, 500)

for (i in 1:500) {

  res[i] = abs(data[,1][i] - data[,1][i+5])
}

mean(res)



