#Importing data and checking it imported right
Survival <- read_excel("~/Box Sync/WhitmanLab/Projects/JackPine/Data/Lab_Data/Seedlot_assay/JP_Seed_Lot_Assay_Height.xlsx")
head(Survival)
Survival = data.frame(Survival)
class(Survival)

#Running Stats, *= interaction
a = aov(data = Survival, Survival ~ Soil_Type + Seed_Type + Soil_Type*Seed_Type)
summary(a)
a

tukey.a = TukeyHSD(x = a, "Soil_Type")
tukey.a
tukey.a = TukeyHSD(x = a, "Seed_Type")
tukey.a
tukey.a = TukeyHSD(x = a, "Seed_Type*Soil_Type")
tukey.a

#Plotting initial data
p = ggplot(Survival, aes(x=Set, y=Survival))
p = p + geom_col()
p

#Coloring by Set
p = ggplot(Survival, aes(x=Set, y=Survival, fill = Soil_Type))
p = p + geom_col()
p

#Faciting by Seed Type
p = ggplot(Survival, aes(x=Set, y=Survival, fill = Soil_Type))
p = p + geom_col()
p = p + facet_grid(~Seed_Type)
p

#dply package to find average by seed type in each soil tpye
##Something is going wrong here
df = Survival %>%
  dplyr::group_by(Seed_Type, Soil_Type) %>%
  dplyr::summarise(mean_survival = mean(Survival, na.rm = TRUE))
df

head(df)
levels(Survival$Soil_Type)     #read out is null
class(Survival$Seed_Type)      #read out is character
tail(df)

#dplyr package to find the standard error and deviation
df = Survival %>%
  dplyr::group_by(Seed_Type, Soil_Type) %>%
  dplyr::summarise(mean_survival = mean(Survival, na.rm = TRUE), StdDev = sd(Survival, na.rm = TRUE),Count=4) %>%
  mutate(StdErr = StdDev / (Count)^0.5)
df

#Adding the standard error into ggplot2
p = ggplot(df, aes(x=mean_survival, y=Survival, fill = Soil_Type))
p = p + geom_col()
p = p + facet_grid(~Seed_Type)
p

#Adding standard error bars into ggplot2
p = ggplot(df, aes(x=mean_survival, y=Survival, fill = Soil_Type))
p = p + geom_col()
p = p + facet_grid(~Seed_Type)
p + geom_errorbar(aes(ymin = mean_survival - StdErr, ymax = mean_survival + StdErr))
p

#Background and color
p = ggplot(df, aes(x=mean_survival, y=Survival, fill = Soil_Type))
p = p + geom_col()
p = p + facet_grid(~Seed_Type)
p + geom_errorbar(aes(ymin = mean_survival - StdErr, ymax = mean_survival + StdErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=14), axis.title = element_text(size=14))
p = p + scale_color_manual(values=c("#CC0000","#666666"))
p

#----------------------------------------------------------------------------------------------------------------------------------
#Faciting by Soil Type
p = ggplot(Survival, aes(x=Set, y=Survival, fill = Seed_Type))
p = p + geom_col()
p = p + facet_grid(~Soil_Type)
p

#dply package to find average by seed type in each soil tpye
df = Survival %>%
  dplyr::group_by(Seed_Type, Soil_Type) %>%
  dplyr::summarise(mean_survival = mean(Survival, na.rm = TRUE))
df

head(df)
levels(Survival$Soil_Type)     #read out is null
class(Survival$Seed_Type)      #read out is character
tail(df)

#dplyr package to find the standard error and deviation
df = Survival %>%
  dplyr::group_by(Seed_Type, Soil_Type) %>%
  dplyr::summarise(mean_survival = mean(Survival, na.rm = TRUE), StdDev = sd(Survival, na.rm = TRUE),Count=4) %>%
  mutate(StdErr = StdDev / (Count)^0.5)
df

#Adding the standard error into ggplot2
p = ggplot(df, aes(x=Seed_Type, y=mean_survival, fill = Seed_Type))
p = p + geom_col()
p = p + facet_grid(~Soil_Type)
p

#Adding standard error bars into ggplot2
p = ggplot(df, aes(x=Seed_Type, y=mean_survival, fill = Seed_Type))
p = p + geom_col()
p = p + facet_grid(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_survival - StdErr, ymax = mean_survival + StdErr))
p

#Background and color
p = ggplot(df, aes(x=Seed_Type, y=mean_survival, fill = Seed_Type))
p = p + geom_col()
p = p + facet_grid(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_survival - StdErr, ymax = mean_survival + StdErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12, angle = 45, hjust = 1))
p = p + scale_fill_manual(values=c("#CC0000","#666666"))
p = p + ylab("Survival (%)")
p = p + xlab("Seed Lot Location")
p

#Remove title for legend
p = ggplot(df, aes(x=Seed_Type, y=mean_survival, fill = Seed_Type))
p = p + geom_col()
p = p + facet_grid(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_survival - StdErr, ymax = mean_survival + StdErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12, angle = 45, hjust = 1))
p = p + scale_fill_manual(values=c("#CC0000","#666666"))
p = p + ylab("Survival (%)")
p = p + xlab("Seed Lot Location")
p = p + guides(fill=guide_legend(title=NULL))
p

#Using scales to change the legend names of sets
p = ggplot(df, aes(x=Seed_Type, y=mean_survival, fill = Seed_Type))
p = p + geom_col()
p = p + facet_grid(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_survival - StdErr, ymax = mean_survival + StdErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12, angle = 45, hjust = 1))
p = p + scale_fill_manual(values=c("#CC0000","#666666"))
p = p + ylab("Survival (%)")
p = p + xlab("Seed Lot Location")
p = p + guides(fill=guide_legend(title=NULL))
p = p + scale_fill_manual(values=c("#CC0000","#666666"), name = NULL,
                            breaks = c("C", "NW"),
                            labels = c("Central", "Northwest"))
p

#Changing the x axis levels of a factor
df$Seed_Type = plyr::revalue(df$Seed_Type, c("NW" = "Northwest", "C" = "Central"))
df$Soil_Type = as.factor(df$Soil_Type)
df$Soil_Type = plyr::revalue(df$Soil_Type, c("PottingMix" = "Potting Mix", "PreBurn" = "Pre-Burn Soil"))
#df$Soil_Type = plyr::revalue(df$Soil_Type, c("Potting Mix" = "Potting\nMix", "Pre-Burn Soil"="Pre-Burn\nSoil"))
class(df$Soil_Type)
levels(df$Soil_Type)

#Changing teh facet names
p = ggplot(df, aes(x=Seed_Type, y=mean_survival, fill = Seed_Type))
p = p + geom_col()
p = p + facet_grid(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_survival - StdErr, ymax = mean_survival + StdErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=35), axis.text = element_text(size=35), axis.title = element_text(size=35), axis.text.x = element_text(size=25))
p = p + scale_fill_manual(values=c("#CC0000","#666666"))
p = p + ylab("Survival (%)")
p = p + xlab("Seed Lot Location")
p = p + guides(fill=guide_legend(title=NULL))
p = p + scale_fill_manual(values=c("#CC0000","#666666"), name = NULL,
                          breaks = c("C", "NW"),
                          labels = c("Central", "Northwest"))
p

#paper formating 
p = ggplot(df, aes(x=Seed_Type, y=mean_survival, fill = Seed_Type))
p = p + geom_col()
p = p + facet_grid(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_survival - StdErr, ymax = mean_survival + StdErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=10), axis.text = element_text(size=10), axis.title = element_text(size=10), axis.text.x = element_text(size=10))
p = p + scale_fill_manual(values=c("#CCCCCC","#333333"))
p = p + ylab("Survival (%)")
p = p + xlab("Seed Lot Source")
p = p + guides(fill=guide_legend(title=NULL))
p = p + scale_fill_manual(values=c("#CCCCCC","#333333"), name = NULL,
                          breaks = c("C", "NW"),
                          labels = c("Central", "Northwest"))
p
