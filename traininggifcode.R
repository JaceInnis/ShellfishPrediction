
library(gapminder)
library(ggplot2)
library(gganimate)
library(dplyr)
library(ggpubr)

head(gapminder)

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p


p + transition_time(year) +
  labs(title = "Year: {frame_time}")


gifdata %>% 
  filter(Set == "Validation") -> gifdatafil


datadate = read.csv("MatarianoBayMaster.csv")

datadate$date

gifdata$datec = NA

for (i in 1:nrow(gifdata)) {
  gifdata$datec[i] = datadate$date[gifdata$date[i]]
}

gifdata$datec = mdy(gifdata$datec)


gifdata = gifdata[-1,]
gifdata$Source = gifdata$Model
gifdata$date = gifdata$datec


ggarrange

colnames(gifdata)

ggplot(gifdata, aes(x = date, y = Value, colour = Source)) +
  geom_point()+
  facet_wrap(vars(Set), scales = "free_x", nrow =2)+
  theme_pubclean()+
  transition_time(Epoch) + 
  labs(y = "Toxic Level ",title = "Training The Neural Network                 Epoch: {frame_time}")













### grave yard ###


plot(lab, col = "red", xlab = NA)
points(res) 

plot(vlab, col = 'red', xlab = NA)
points(vres)


plot = data.frame(date = rep(mdy(daa$date[480:(nrow(daa)-5)]),2), Toxicity = c(vlab, (vres*1.2)), Source = c(rep("Repots",109),rep("Model",109)))


ggplot(aes(date,Toxicity), data =  plot) +
  geom_point(aes(color = Source)) +
  labs(y= "Toxicity (yes no)", title = "Preddicting the Toxicity of Matariano Bay 4 weeks into the future")




model$summary()

daa[vmin:(vmin+9),3]
lab[vmin:(vmin+9)]
samp[vmin:(vmin+9),1,1]


data[vmin:(vmin+10),1]
val[1:10,1,1]
vlab[1:10]
vres[1:10]

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------




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



# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------


res = rep(NA, 572)

for (i in vmin:vmax) {
  
  res[i] = abs(data[,1][i] - data[,1][i+3])
}

1- mean(res, na.rm = TRUE)

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------




plot(vlab, col = 'red', xlab = NA)
points(vres)

ln = length(vres)

show = data.frame(Toxic_status = c(vlab,vres),time = rep(daa$date[(nrow(data)-ln):(nrow(daa)-1)], 2),type = c(rep("Observed",ln),rep("Predicted",ln)))

show$time = mdy(show$time)


library(ggplot2)

ggplot(aes(time, Toxic_status), data = show)+
  geom_point(aes(color = type)) +
  geom_line(aes(color = type))



