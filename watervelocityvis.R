library(dplyr)



list = wvtolist(ROI = ROI, date = "20200905", interval = 5, scale = 20000, depth = 0)

#converts the ree list to a r dataframe
df = geetodf(list)

# tis = ssetolist(ROI = ROI, date = "20180805", interval = 5, scale = 20000)
# 
# df2 = geetodf(tis)
# 
# df = inner_join(df, df2)


awr = thin(df, 1) # A function that would limit the number of drawn arrows by 2



ggplot(df, aes(lon, lat)) +
  geom_raster(aes( fill = abs(velocity_u_0)+abs(velocity_v_0) ), interpolate=TRUE)+
  geom_segment(aes(x = lon,y =  lat, xend = lon + velocity_u_0/1200, yend = lat + velocity_v_0/1200),
               size = 0.4 , arrow = arrow(length = unit(0.1,"cm")), 
               data = df[awr,] )+
  scale_fill_gradientn(colours=c("blue","green","red"))+
  borders("world", 
          xlim = c(min(df$lon), max(df$lon)), 
          ylim = c(min(df$lat), max(df$lat)),
          fill = "dark green") +
  coord_fixed(xlim = c(min(df$lon), max(df$lon)),
              ylim = c(min(df$lat), max(df$lat)))+
  theme(legend.position="none")
