library("reshape")
library(ggplot2)
library(dplyr)

#install.packages("reshape")
Height <- read_excel("~/Box Sync/WhitmanLab/Projects/JackPine/Data/Lab_Data/Core_Deepot_Assay/CFB_Deepot_Germination_R.xlsx")
head(Height)
Height = data.frame(Height)
class(Height)

#REMOVE POTITNG Mix
Height = Height %>%
  filter(Soil_Type!="Potting_Mix")
head(Height)

levels(as.factor(Height$Soil_Type))


#Statistics
a = aov(data = Height, Week9_Height_cm ~ Soil_Type)
summary(a)

a = aov(data = Height, Week9_Height_cm ~ Seed_Type)
summary(a) 

a = aov(data = Height, Week9_Height_cm ~ Seed_Type*Soil_Type)
summary(a)



#Using teh melt funstion to organize the data
Height = melt(Height, id=c("Set","Soil_Type","Seed_Type"))
head(Height)

colnames(Height) = c("Set","Soil_Type" ,"Seed_Type", "Week" , "Height_cm"  )
colnames(Height)

levels(Height$Week)
class(levels(Height$Week))

Height$Week = as.numeric(Height$Week)

levels(Height$Week) = c(1:24)

head(Height)

#Figure 1
p = ggplot(Height, aes(x=Week, y=Height_cm))
p = p + geom_point()
p

#Figure 2
p = ggplot(Height, aes(x=Week, y=Height_cm, color = Set))
p = p + geom_point()
p

#Figure 3
p = ggplot(Height, aes(x=Week, y=Height_cm, color = Set))
p = p + geom_point()
p = p + facet_wrap(~Soil_Type)
p

#Figure 5
p = ggplot(Height, aes(x=Week, y=Height_cm, color = Seed_Type))
p = p + geom_point()
p = p + facet_wrap(~Soil_Type)
p

#Deplyer package
df = Height %>%
  group_by(Set, Seed_Type, Week, Soil_Type) %>%
  summarise(mean_height = mean(Height_cm, na.rm = TRUE))
df

tail(df)

#Standard Error
df = Height %>%
  group_by(Set, Seed_Type, Week, Soil_Type) %>%
  summarise(mean_height = mean(Height_cm, na.rm = TRUE), Std_Dev = sd(Height_cm, na.rm = TRUE),Count=n()) %>%
  mutate(Std_Err = Std_Dev / (Count)^0.5)
df

head(df[df$Soil_Type=="PostBurn",])
Height[Height$Soil_Type=="PostBurn" & Height$Week == 6,]

#With STE
p = ggplot(df, aes(x = Week, y = mean_height, color = Seed_Type)) 
p = p + geom_point()
p = p + facet_wrap(~Soil_Type)
p

#Add error bars
p = ggplot(df, aes(x = Week, y = mean_height, color = Seed_Type)) 
p = p + geom_point()
p = p + facet_wrap(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p

#Axis lables
p = ggplot(df, aes(x = Week, y = mean_height, color = Seed_Type)) 
p = p + geom_point()
p = p + facet_wrap(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p = p + ylab("Average Height (cm)")
p

#background
p = ggplot(df, aes(x = Week, y = mean_height, color = Seed_Type)) 
p = p + geom_point(size=3)
p = p + facet_wrap(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p = p + ylab("Average Height (cm)")
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=14), axis.title = element_text(size=14))
p

#colours and getting rid of legend title
p = ggplot(df, aes(x = Week, y = mean_height, color = Seed_Type, shape=Seed_Type)) 
p = p + geom_point(size=3)
p = p + facet_wrap(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p = p + ylab("Average Height (cm)") + xlab("Time (weeks)")
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=14), axis.title = element_text(size=14))
p = p + scale_color_manual(values=c("#CC0000","#666666"))
p = p + guides(color=guide_legend(title=NULL), shape=guide_legend(title=NULL))
p = p + theme(legend.position = c (.85,.9))
p

#Figure for paper
p = ggplot(df, aes(x = Week, y = mean_height, color = Seed_Type, shape=Seed_Type)) 
p = p + geom_point(size=2)
p = p + facet_wrap(~Soil_Type)
p = p + geom_errorbar(aes(ymin = mean_height - Std_Err, ymax = mean_height + Std_Err))
p = p + ylab("Average Height (cm)") + xlab("Time (weeks)")
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=10), axis.text = element_text(size=10), axis.title = element_text(size=10))
p = p + scale_color_manual(values=c("#CCCCCC","#333333"))
p = p + guides(color=guide_legend(title=NULL), shape=guide_legend(title=NULL))
p = p + theme(legend.position = c (.85,.85))
p = p + theme(legend.text = element_text(size=7))
p




#Stats by soil tpye
##This is not working
a = CFB_Deepot_Germination_R[CFB_Deepot_Germination_R$Soil_Type == "Lab-Burn", ]
a

anova.a = aov(Week24_Height_cm ~ Seed_Type*Soil_Type, a)
summary(anova.a)
anova.a 












