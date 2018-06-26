library(readxl)
Deepot_Dry_Mass <- read_excel("~/Box Sync/WhitmanLab/Projects/JackPine/Data/Lab_Data/Core_Deepot_Assay/Deepot_Dry_Mass.xlsx")
View(Deepot_Dry_Mass)
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library(dplyr)

#Simple bar plot of dry mass
class(Deepot_Dry_Mass)
# What type of R object is Deepot_dry_mass?

head(Deepot_Dry_Mass)
# Look at top part of table

#remove control sets
Deepot_Dry_Mass = Deepot_Dry_Mass %>%
  filter(Soil_Type!="Control")

#Stats Roots
r = Deepot_Dry_Mass[Deepot_Dry_Mass$Part == "Roots", ]
r

anova.r = aov(Dry_Mass_g ~ Soil_Type * Seed_Type, r)
summary(anova.r)
anova.r 

tukey.r = TukeyHSD(x = anova.r, "Soil_Type")
tukey.r
tukey.r = TukeyHSD(x = anova.r, "Seed_Type")
tukey.r
tukey.r = TukeyHSD(x = anova.r, "Seed_Type*Soil_Type")
tukey.r


#Stats Shoots
s = Deepot_Dry_Mass[Deepot_Dry_Mass$Part == "Shoots", ]
s

anova.s = aov(Dry_Mass_g ~ Soil_Type, s)
summary(anova.s)
anova.s

tukey.s = TukeyHSD(x = anova.s, "Soil_Type")
tukey.s
tukey.s = TukeyHSD(x = anova.s, "Seed_Type")
tukey.s

#Stats for just the NW seed lot - SHOOTS
s.NW = Deepot_Dry_Mass[Deepot_Dry_Mass$Part == "Shoots" & Deepot_Dry_Mass$Seed_Type == "Northwest", ]
s.NW

anova.s.NW = aov(Dry_Mass_g ~ Soil_Type, s.NW)
summary(anova.s.NW)
anova.s.NW

tukey.s.NW = TukeyHSD(x = anova.s.NW, "Soil_Type")
tukey.s.NW
#post-lab p=0.000043
#pre-lab p=0.0047280
#pre-post p=0.0567


#Stats for the NW seed lot - ROOTS
r.NW = Deepot_Dry_Mass[Deepot_Dry_Mass$Part == "Roots" & Deepot_Dry_Mass$Seed_Type == "Northwest", ]
r.NW

anova.r.NW = aov(Dry_Mass_g ~ Soil_Type, r.NW)
summary(anova.r.NW)
anova.s.NW

tukey.r.NW = TukeyHSD(x = anova.r.NW, "Soil_Type")
tukey.r.NW

#Stats with no central seeds in field soil 
head(s)
s.nofield = s %>%
  filter(Set != "PreBurn_C_Shoots")%>%
  filter(Set != "PostBurn_C_Shoots")

s.nofield$Set = as.factor(s.nofield$Set)

s.nofield.anova = aov(Dry_Mass_g ~ Set, s.nofield)
summary(s.nofield.anova)

tukey.s.nofield = TukeyHSD(x = s.nofield.anova, "Set")
tukey.s.nofield


r.nofield = r %>%
  filter(Set != "PreBurn_C_Roots")%>%
  filter(Set != "PostBurn_C_Roots")

r.nofield$Set = as.factor(r.nofield$Set)

r.nofield.anova = aov(Dry_Mass_g ~ Set, r.nofield)
summary(r.nofield.anova)

tukey.r.nofield = TukeyHSD(x = r.nofield.anova, "Set")
tukey.r.nofield

levels(as.factor(s$Set))

#Basic bar plot  
Deepot_Dry_Mass$`Dry_Mass_(g)`
barplot.default(Deepot_Dry_Mass$`Dry_Mass_(g)`)

#ggplot - where is the data, formating, what are the varriables?
p = ggplot(Deepot_Dry_Mass, aes(x=Set, y=Dry_Mass_g))
p = p + geom_boxplot()
p

#faciting
p = ggplot(Deepot_Dry_Mass, aes(x=Set, y=Dry_Mass_g))
p = p + geom_boxplot()
p = p + facet_wrap(~Part)
p

p = ggplot(Deepot_Dry_Mass, aes(x=Set, y=Dry_Mass_g))
p = p + geom_boxplot()
p = p + facet_wrap(~Part + Seed_Type)
p

p = ggplot(Deepot_Dry_Mass, aes(x=Soil_Type, y=Dry_Mass_g))
p = p + geom_boxplot()
p = p + facet_grid(~Part ~ Seed_Type, scales = "free")
p

#color
p = ggplot(Deepot_Dry_Mass, aes(x=Soil_Type, y=Dry_Mass_g, color = Seed_Type))
p = p + geom_boxplot()
p = p + facet_grid(~Part, scales = "free")
p

#stacked barplot
df = Deepot_Dry_Mass %>%
  group_by(Seed_Type)%>%
    summarize(n())


df =  summarize(group_by(Deepot_Dry_Mass, Seed_Type),n())
# These are doing the sampe thing

df = Deepot_Dry_Mass %>%
    group_by(Seed_Type,Soil_Type,Part)%>%
    summarize(Mean_Dry_Mass_g=mean(Dry_Mass_g),Std_Dev_Dry_Mass_g = sd(Dry_Mass_g),Count=n()) %>%
    mutate(Std_Err_Dry_Mass_g = Std_Dev_Dry_Mass_g / (Count)^0.5)%>%
    mutate(Seed_Soil_Type = paste(Seed_Type, Soil_Type))

#making roots negative for stacked bar plot
df$Mean_Dry_Mass_g[df$Part == "Roots"]=df$Mean_Dry_Mass_g[df$Part == "Roots"]*-1
df

#write.csv(df,"Summary_Deepot_means_DryMass")

#Stacked bar plot
p = ggplot(df, aes(Soil_Type, Mean_Dry_Mass_g, fill = Part))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Seed_Type)
p

#added Standard error
p = ggplot(df, aes(Soil_Type, Mean_Dry_Mass_g, fill = Part))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Seed_Type)
p = p + geom_errorbar(aes(ymin = Mean_Dry_Mass_g - Std_Err_Dry_Mass_g, ymax = Mean_Dry_Mass_g + Std_Err_Dry_Mass_g))
p

#Background and color
p = ggplot(df, aes(Soil_Type, Mean_Dry_Mass_g, fill = Part))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Seed_Type)
p = p + geom_errorbar(aes(ymin = Mean_Dry_Mass_g - Std_Err_Dry_Mass_g, ymax = Mean_Dry_Mass_g + Std_Err_Dry_Mass_g))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12, angle = 45, hjust = 1))
p = p + scale_fill_manual(values=c("#666666","#CC0000"))
p = p + ylab("Mean Dry Mass (g)")
p = p + xlab("Soil Type")
p

#Remove title for legend
p = ggplot(df, aes(Soil_Type, Mean_Dry_Mass_g, fill = Part))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Seed_Type)
p = p + geom_errorbar(aes(ymin = Mean_Dry_Mass_g - Std_Err_Dry_Mass_g, ymax = Mean_Dry_Mass_g + Std_Err_Dry_Mass_g))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=35), axis.text = element_text(size=35), axis.title = element_text(size=35), axis.text.x = element_text(size=25, angle = 45, hjust = 1))
p = p + scale_fill_manual(values=c("#666666","#CC0000"))
p = p + ylab("Mean Dry Mass (g)")
p = p + xlab("Soil Type")
p = p + guides(fill=guide_legend(title=NULL))
p = p + theme(legend.text = element_text(size = 25))
p

#Figure for paper 
p = ggplot(df, aes(Soil_Type, Mean_Dry_Mass_g, fill = Part))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Seed_Type)
p = p + geom_errorbar(aes(ymin = Mean_Dry_Mass_g - Std_Err_Dry_Mass_g, ymax = Mean_Dry_Mass_g + Std_Err_Dry_Mass_g))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=10), axis.text = element_text(size=10), axis.title = element_text(size=10), axis.text.x = element_text(size=10, angle = 45, hjust = 1))
p = p + scale_fill_manual(values=c("#CCCCCC","#333333"))
p = p + ylab("Mean Dry Mass (g)")
p = p + xlab("Soil Type")
p = p + guides(fill=guide_legend(title=NULL))
p = p + theme(legend.text = element_text(size = 7), legend.key.size = unit(.3, "cm"))
p = p + theme(legend.position = c(.85,.9))
p = p + scale_y_continuous(breaks=c(-1,-.5,0,.5,1,1.5))
p


#---------------------------------------------------------------------------------------------------------
### Combined NW and Central seeds

df = Deepot_Dry_Mass %>%
  group_by(Soil_Type,Part)%>%
  summarize(Mean_Dry_Mass_g=mean(Dry_Mass_g),Std_Dev_Dry_Mass_g = sd(Dry_Mass_g),Count=n()) %>%
  mutate(Std_Err_Dry_Mass_g = Std_Dev_Dry_Mass_g / (Count)^0.5)
df

#making roots negative for stacked bar plot
df$Mean_Dry_Mass_g[df$Part == "Roots"]=df$Mean_Dry_Mass_g[df$Part == "Roots"]*-1
df

p = ggplot(df, aes(Soil_Type, Mean_Dry_Mass_g, fill = Part))
p = p + geom_col(position = position_stack())
p = p + geom_errorbar(aes(ymin = Mean_Dry_Mass_g - Std_Err_Dry_Mass_g, ymax = Mean_Dry_Mass_g + Std_Err_Dry_Mass_g))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=10), axis.text = element_text(size=10), axis.title = element_text(size=10), axis.text.x = element_text(size=10, angle = 45, hjust = 1))
p = p + scale_fill_manual(values=c("#CCCCCC","#333333"))
p = p + ylab("Mean Dry Mass (g)")
p = p + xlab("Soil Type")
p = p + guides(fill=guide_legend(title=NULL))
p = p + theme(legend.text = element_text(size = 7))
p = p + theme(legend.position = c(.85,.9))
p


