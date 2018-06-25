#Packages that we will need
#library("reshape")
#library(ggplot2)
#library(dplyr)

#Importing data and checking it imported right
Germ <- read_excel("~/Box Sync/WhitmanLab/Projects/JackPine/Data/Lab_Data/Core_Deepot_Assay/CFB_Core_Germination_R.xlsx")
head(Germ)
Germ = data.frame(Germ)
class(Germ)

#Combining or melting the bured vs unbured columns
Germ = melt(Germ, id=c("Week", "StDev_B", "StDev_UB"))
head(Germ)
tail(Germ)

#Combining the StDev columns
for (i in 1:dim(Germ)[1]){
  if (Germ$variable[i] == "Unburned") {Germ$StDev[i] = Germ$StDev_UB[i]}  
  if (Germ$variable[i] == "Burned") {Germ$StDev[i] = Germ$StDev_B[i]}  
}

#Making Germ2 to play with a new data frame with the columns we want only
Germ2 = Germ[ , c(1, 4, 5, 6)]
head(Germ2)

#Changing the colunm names to what we want on the graphs
colnames(Germ2) = c("Week", "Soil", "Germination", "StDev")
colnames(Germ2)
head(Germ2)
tail(Germ2)

#Changing the levels
levels(Germ$Week)
class(levels(Germ$Week))
Germ$Week = as.numeric(Germ$Week)
levels(Germ$Week) = c(1:19)

#Ploting initial figure
p = ggplot(Germ2, aes(x=Week, y=Germination))
p = p + geom_point()
p

#Coloring by burned vs non-burned
p = ggplot(Germ2, aes(x=Week, y=Germination, color = Soil))
p = p + geom_point()
p

#Faciting by burned vs non-burned
p = ggplot(Germ2, aes(x=Week, y=Germination, color = Soil))
p = p + geom_point()
p = p + facet_wrap(~Soil)
p

#dplyr package, adding the standard error by set
df = Germ2 %>%
  group_by(Week, Soil) %>%
  mutate(StErr = StDev / (21)^0.5)
df

#Adding the standard error into ggplot2
p = ggplot(df, aes(x = Week, y = Germination, color = Soil)) 
p = p + geom_point()
p = p + facet_wrap(~Soil)
p

#Add error bars into ggplot2
p = ggplot(df, aes(x = Week, y = Germination, color = Soil)) 
p = p + geom_point()
p = p + facet_wrap(~Soil)
p = p + geom_errorbar(aes(ymin = Germination - StErr, ymax = Germination + StErr))
p

#Background
p = ggplot(df, aes(x = Week, y = Germination, color = Soil)) 
p = p + geom_point()
p = p + facet_wrap(~Soil)
p = p + geom_errorbar(aes(ymin = Germination - StErr, ymax = Germination + StErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=14), axis.title = element_text(size=14))
p

#Colors
p = ggplot(df, aes(x = Week, y = Germination, color = Soil)) 
p = p + geom_point()
#p = p + facet_wrap(~Soil)
p = p + geom_errorbar(aes(ymin = Germination - StErr, ymax = Germination + StErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=30), axis.text = element_text(size=30) + axis.title = element_text(size=30))
p = p + scale_color_manual(values=c("#CC0000","#666666"))
p

#Figure for paper
p = ggplot(df, aes(x = Week, y = Germination, color = Soil)) 
p = p + geom_point()
#p = p + facet_wrap(~Soil)
p = p + geom_errorbar(aes(ymin = Germination - StErr, ymax = Germination + StErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=10), axis.text = element_text(size=10) + axis.title = element_text(size=10))
p = p + scale_color_manual(values=c("#CCCCCC","#333333"))
p = p + theme(legend.text = element_text(size = 7))
p = p + theme(legend.position = c(.85,.5))
p = p + ylab("Germination (%)")
p = p + scale_y_continuous(breaks=c(25,50,75,100))
p



#Line Graph
p = ggplot(df, aes(x = Week, y = Germination, color = Soil)) 
p = p + geom_line()
p = p + facet_wrap(~Soil)
p = p + geom_errorbar(aes(ymin = Germination - StErr, ymax = Germination + StErr))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=10), axis.title = element_text(size=14))
p = p + scale_color_manual(values=c("#CC0000","#666666"), guide=FALSE)
p





