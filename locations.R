
library(RGEEtools)

start()

# bi-layer (record data from top and botum layer)

#--------Create straits as objects saved in GEE --------------------

tablas = ROI("[121.80126515441958, 11.834026310107726],
          [121.87816945129458, 12.129567783142],
          [121.87267628723208, 12.521325198447002],
          [121.59801808410708, 12.60175030245947],
          [121.65294972473208, 12.467694520183667],
          [121.55407277160708, 12.430146437237653],
          [121.70238820129458, 12.188637044413314],
          [121.65294972473208, 11.877034212374848]")

surigao = ROI("[124.83301362364946, 9.855460853817496],
          [124.83301362364946, 9.563079212595822],
          [125.37134370177446, 9.563079212595822],
          [125.37134370177446, 9.855460853817496]")

Mindoro = ROI("[120.25656683463001, 12.767886397454996],
              [120.28677923697376, 12.57494906715718],
              [120.53397161978626, 12.210117861244434],
              [120.91025335806751, 12.507922910047975],
              [120.75378002379263, 12.682111047172194],
              [120.68503363150501, 12.877687883392603],
              [120.53397161978626, 12.968706318480535]")

southsibuyan  = ROI("[122.25589609586541, 12.283384961761387],
          [122.25589609586541, 11.971892456572283],
          [122.53055429899041, 11.971892456572283],
          [122.53055429899041, 12.283384961761387]")


sibuyan  = ROI("[122.28885508024038, 13.140738859964724],
      [122.28885508024038, 12.653473238577904],
      [122.77774668180288, 12.653473238577904],
      [122.77774668180288, 13.140738859964724]")

Visayan = ROI("[123.47118702941958, 11.774879402681744],
          [123.47118702941958, 11.473571776283169],
          [123.83922902160708, 11.473571776283169],
          [123.83922902160708, 11.774879402681744]")

# mindoro srait
# SurigaoStrait
# Panay Strait
# Sulu Sea???


lines = read.csv("modeledAnno.csv")



MasterLoc = list(Visayan = list( Visayan, 0, 8000, NA,"Visayan"  , var =  list(temp = lines$visayantemp, Sal = lines$visayansal, Velu =  lines$visayanvelu, Velv = lines$visayanvelv)),
                 southsibuyan = list( southsibuyan, 1, 7200, 1000, "southsibuyan",  var = list(temp = lines$southsibuyantemp, Sal = lines$southsibuyansal, Velu =  lines$southsibuyanvelu, Velv = lines$southsibuyanvelv)),
                 sibuyan = list( sibuyan, 1, 13000, 1000, "sibuyan",  var = list(temp = lines$sibuyantemp, Sal = lines$sibuyansal, Velu =  lines$sibuyanvelu, Velv = lines$sibuyanvelv)),
                 tablas = list( tablas, 1, 10000, 400, "tablas",  var = list(temp = lines$tablastemp, Sal = lines$tablassal, Velu =  lines$tablasvelu, Velv = lines$tablasvelv)),
                 surigao = list( surigao, 1, 10000, 1000, "surigao",  var = list(temp = lines$surigaotemp, Sal = lines$surigaosal, Velu =  lines$surigaovelu, Velv = lines$surigaovelv)),
                 Mindoro = list( Mindoro, 1, 13000, 700, "Mindoro",  var = list(temp = lines$mindorotemp, Sal = lines$mindorosal, Velu =  lines$mindorovelu, Velv = lines$mindorovelv)))




