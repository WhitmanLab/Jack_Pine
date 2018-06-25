library(plyr)
library(dplyr)
library(reshape)

#Importing data and checking it imported right
Height <- read_excel("~/Box Sync/WhitmanLab/Projects/JackPine/Data/Lab_Data/Core_Deepot_Assay/CFB_Core_Germination_R2.xlsx")
head(Height)
Height = data.frame(Height)
class(Height)

#Stats
a = aov(data = Height, Week19_Height_cm ~ Burned)
summary(a)

#Turning the timeline data into variables and vlaues to better represent in R
Height = melt(Height, id=c("Core_Number","Burned", "Paired_Core_Number"))
head(Height)
tail(Height)

#Changing the colunm names to what we want on the graphs
colnames(Height) = c("Core_Number","Burned", "Paired_Core_Number" , "Week", "Height_cm"  )
colnames(Height)
head(Height)

#Changing the levels
levels(Height$Week)
class(levels(Height$Week))

Height$Week = as.numeric(Height$Week)
levels(Height$Week) = c(1:19)
head(Height)

#Ploting initial figure
p = ggplot(Height, aes(x=Week, y=Height_cm))
p = p + geom_point()
p

#Coloring by burned vs non-burned
p = ggplot(Height, aes(x=Week, y=Height_cm, color = Burned))
p = p + geom_point()
p

#Faciting by burned vs non-burned
p = ggplot(Height, aes(x=Week, y=Height_cm, color = Burned))
p = p + geom_point()
p = p + facet_wrap(~Burned)
p

#Dplyer package, adding the averages by set
df = Height %>%
  dplyr::group_by(Week, Burned) %>%
  dplyr::summarise(mean_height = mean(Height_cm, na.rm = TRUE))
df

head(Height)
levels(Height$Week)
class(Height$Burned)
tail(df)

#dplyr package, adding the standard error by set
df = Height %>%
  dplyr::group_by(Week, Burned) %>%
  dplyr::summarise(mean_height = mean(Height_cm, na.rm = TRUE), Std_Dev = sd(Height_cm, na.rm = TRUE),Count=21) %>%
  mutate(Std_Err = Std_Dev / (Count)^0.5)
df

#Adding the standard error into ggplot2
p = ggplot(df, aes(x = Week, y = mean_height, color = Burned)) 
p = p + geom_point()
p = p + facet_wrap(~Burned)
p

#Add error bars into ggplot2
p = ggplot(df, aes(x = Week, y = mean_height, color = Burned)) 
p = p + geom_point()
p = p + facet_wrap(~Burned)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p

#Background and color
p = ggplot(df, aes(x = Week, y = mean_height, color = Burned)) 
p = p + geom_point()
p = p + facet_wrap(~Burned)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=14), axis.title = element_text(size=14))
p = p + scale_color_manual(values=c("#CC0000","#666666"))
p

#Y axis title change
p = p + geom_point()
p = p + facet_wrap(~Burned)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=10), axis.title = element_text(size=14))
p = p + scale_color_manual(values=c("#CC0000","#666666"), guide=FALSE)
p = p + ylab("Mean Height (cm)")
p

#figure for paper
p = p + geom_point()
p = p + facet_wrap(~Burned)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=10), axis.text = element_text(size=10), axis.title = element_text(size=10))
p = p + scale_color_manual(values=c("#CCCCCC","#333333"), guide=FALSE)
p = p + ylab("Mean Height (cm)")
p



