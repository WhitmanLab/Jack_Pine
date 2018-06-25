#Nitrogen 
n = aov(data = Whitman_Coon_Fork_soil_run_6_29_17, N ~ Sample) 
summary(n)

tukey.n = TukeyHSD(x = n, "Sample")
tukey.n
#preO-lab p=0.08895
#postO-lab p =0.0003
#preO-postO p=0.0190622
#preA-postA p=0.254

#Carbon
c = aov(data = Whitman_Coon_Fork_soil_run_6_29_17, C ~ Sample)
summary(c)

tukey.c = TukeyHSD(x = c, "Sample")
tukey.c
#postO-lab p=0.000125
#preO-lab p=0.0013
#preA-postA p=0.1333
#preO-postO p=0.0114692

#pH
pH  = aov(data = SFAL_Soil_Analysis, pH ~ Sample)
summary(pH)

tukey.pH = TukeyHSD(x = pH, "Sample")
tukey.pH
#preO-lab p=0.000
#postO-lab p =0.000
#preO-postO p=0.9723
#preA-postA p=0.7524

#Sikora Buffer
SB = aov(data = SFAL_Soil_Analysis, SikoraBuffer ~ Sample)
summary(SB)

tukey.SB = TukeyHSD(x = SikoraBuffer, "Sample")
tukey.SB
#preO-lab p=0.000
#postO-lab p =0.000
#preO-postO p=0.9839
#preA-postA p=0.3785

#Phosphorus, plant availabel 
P = aov(data = SFAL_Soil_Analysis, P ~ Sample)
summary(P)

tukey.P = TukeyHSD(x = P, "Sample")
tukey.P
#preO-lab p=0.000
#postO-lab p =0.000
#preO-postO p=1.0000
#preA-postA p=1.000

#Potassium, plant available
K = aov(data = SFAL_Soil_Analysis, K ~ Sample)
summary(K)

tukey.K = TukeyHSD(x = K, "Sample")
tukey.K
#preO-lab p=0.1563
#postO-lab p =0.9974
#preO-postO p=0.2459
#preA-postA p=0.9655

#Organic matter
OM = aov(data = SFAL_Soil_Analysis, OM ~ Sample)
summary(OM)

tukey.OM = TukeyHSD(x = OM, "Sample")
tukey.OM
#preO-lab p=0.00006
#postO-lab p =0.00003
#preO-postO p=0.8267
#preA-postA p=0.9999

#Sodium
Na = aov(data = SFAL_Soil_Analysis, Na ~ Sample)
summary(Na)

tukey.Na = TukeyHSD(x = Na, "Sample")
tukey.Na
#preO-lab p=0.9993
#postO-lab p =0.9720
#preO-postO p=0.9951
#preA-postA p=0.3891

#NO3-N
NO3 = aov(data = SFAL_Soil_Analysis, NO3N ~ Sample)
summary(NO3)

tukey.NO3 = TukeyHSD(x = NO3, "Sample")
tukey.NO3
#preO-lab p=0.9214
#postO-lab p =0.9807
#preO-postO p=0.9986
#preA-postA p=0.8726

#NH4-N
NH4 = aov(data = SFAL_Soil_Analysis, NH4N ~ Sample)
summary(NH4)

tukey.NH4 = TukeyHSD(x = NH4, "Sample")
tukey.NH4
#preO-lab p=0.02414
#postO-lab p =0.03803
#preO-postO p=0.9980
#preA-postA p=0.9984


  
  
  
  
  
