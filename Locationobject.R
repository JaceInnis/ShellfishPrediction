


library(RGEEtools)
start()


BaliteBay = "[126.22868997203807, 6.84429547870094],
      [126.22868997203807, 6.52393657854729],
      [126.51725274669627, 6.52393657854729],
      [126.51725274669627, 6.84429547870094]"

BolinaoandAndamid = "[119.97166044508393, 16.508667816060626],
       [119.97166044508393, 16.283385036737233],
       [120.34656889234955, 16.283385036737233],
       [120.34656889234955, 16.508667816060626]"

BolinaoandAndabot =     "[120.08094330408574, 16.28843194469022],
       [120.08094330408574, 16.03386090448112],
       [120.40915985682011, 16.03386090448112],
       [120.40915985682011, 16.28843194469022]"

BolinaoandAndatop =     "[119.69092865564824, 16.641381172266843],
       [119.69092865564824, 16.409665980022822],
       [119.98618622400761, 16.409665980022822],
       [119.98618622400761, 16.641381172266843]"

bataandeep = "[120.14853645292713, 14.443385684496922],
       [120.14853645292713, 13.982773320517529],
       [120.56189704863026, 13.982773320517529],
       [120.56189704863026, 14.443385684496922]"

bataanbay =     "[120.57315795966308, 14.839022623514788],
       [120.57315795966308, 14.243499042590209],
       [120.96042602606933, 14.243499042590209],
       [120.96042602606933, 14.839022623514788]"

MatarianoBayin = "[125.54455407936655, 11.352570933086124],
       [125.54455407936655, 11.155252338961418],
       [125.7120955832728, 11.155252338961418],
       [125.7120955832728, 11.352570933086124]"

MatarianoBayfar = "[125.71564495406822, 11.40504816537882],
       [125.71564495406822, 11.0939132822312],
       [125.9175187333651, 11.0939132822312],
       [125.9175187333651, 11.40504816537882]"

Milagrosmid = "[123.25578001286262, 12.223302268972073],
       [123.25578001286262, 11.772629254755058],
       [123.77831724430793, 11.772629254755058],
       [123.77831724430793, 12.223302268972073]"

Milagrosright = "[123.83059895221155, 11.749458222321538],
       [123.83059895221155, 11.221910497496191],
       [124.30301106158655, 11.221910497496191],
       [124.30301106158655, 11.749458222321538]"

Milagrosleft = "[122.5644246358053, 12.058522125259222],
       [122.5644246358053, 11.48042452369144],
       [123.1906453389303, 11.48042452369144],
       [123.1906453389303, 12.058522125259222]"

CarigaraBay = "[124.4033242718989, 12.09856505907856],
       [124.4033242718989, 11.388649945240273],
       [125.01581206486765, 11.388649945240273],
       [125.01581206486765, 12.09856505907856]"

CarigaraBaydeep =     "[124.05305364889986, 12.670130539833263],
       [124.05305364889986, 11.738617269380422],
       [124.35792425436861, 11.738617269380422],
       [124.35792425436861, 12.670130539833263]"




surfaceandatmo = list(SE = c("HYCOM/sea_surface_elevation", "surface_elevation"),
                    ST = c("HYCOM/sea_temp_salinity", "water_temp_0", "salinity_0"),
                    SV = c("HYCOM/sea_water_velocity", "velocity_u_0", "velocity_v_0"),
                    ST = c("NCEP_RE/surface_wv", "pr_wtr"),
                    SV = c("NASA/GPM_L3/IMERG_V06", "precipitationCal"))

surfaceydeep = list(SE = c("HYCOM/sea_surface_elevation", "surface_elevation"),
                         ST = c("HYCOM/sea_temp_salinity", "water_temp_0", "salinity_0"),
                         SV = c("HYCOM/sea_water_velocity", "velocity_u_0", "velocity_v_0"),
                         STD = c("HYCOM/sea_temp_salinity", "water_temp_100", "salinity_100"),
                         SVD = c("HYCOM/sea_water_velocity", "velocity_u_100", "velocity_v_100"))


surface = list(SE = c("HYCOM/sea_surface_elevation", "surface_elevation"),
                      ST = c("HYCOM/sea_temp_salinity", "water_temp_0", "salinity_0"),
                      SV = c("HYCOM/sea_water_velocity", "velocity_u_0", "velocity_v_0"))





BaliteBay = list(loc = ROI(BaliteBay), bands = surfaceydeep)
  
BolinaoandAndamid = list(loc = ROI(BolinaoandAndamid), bands = surface)
  
BolinaoandAndabot = list(loc = ROI(BolinaoandAndabot), bands = surfaceandatmo)

BolinaoandAndatop = list(loc = ROI(BolinaoandAndatop), bands = surfaceydeep)
  
MatarianoBayin = list(loc = ROI(MatarianoBayin), bands = surfaceandatmo)

MatarianoBayfar = list(loc = ROI(MatarianoBayfar), bands = surfaceydeep)

Milagrosmid = list(loc = ROI(Milagrosmid), bands = surfaceandatmo)

Milagrosright = list(loc = ROI(Milagrosright), bands = surfaceydeep)

Milagrosleft = list(loc = ROI(Milagrosleft), bands = surfaceydeep)

CarigaraBay = list(loc = ROI(CarigaraBay), bands = surfaceandatmo)

CarigaraBaydeep = list(loc = ROI(CarigaraBaydeep), bands = surfaceydeep)

bataanbay = list(loc = ROI(bataanbay), bands = surfaceandatmo)

bataandeep = list(loc = ROI(bataandeep), bands = surfaceydeep)



mlist = list(BaliteBay, BolinaoandAndamid, BolinaoandAndabot, BolinaoandAndatop,
  MatarianoBayin, MatarianoBayfar, Milagrosmid, Milagrosright, Milagrosleft,
  CarigaraBay, CarigaraBaydeep, bataanbay,bataandeep)


nlist = c("BaliteBay", "BolinaoandAndamid", "BolinaoandAndabot", "BolinaoandAndatop",
             "MatarianoBayin", "MatarianoBayfar", "Milagrosmid", "Milagrosright", "Milagrosleft",
             "CarigaraBay", "CarigaraBaydeep", "bataanbay",'bataandeep')
