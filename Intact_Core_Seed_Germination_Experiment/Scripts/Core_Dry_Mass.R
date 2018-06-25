Core_Dry_Mass <- read_excel("~/Box Sync/WhitmanLab/Projects/JackPine/Data/Lab_Data/Core_Deepot_Assay/Core_Dry_Mass.xlsx")
head(Core_Dry_Mass)
Core_Dry_Mass = data.frame(Core_Dry_Mass)
class(Core_Dry_Mass)

# What type of R object is Deepot_dry_mass?
class(Core_Dry_Mass)

# Look at top part of table
head(Core_Dry_Mass)

Core_Dry_Mass$Burned = revalue(Core_Dry_Mass$Burned, c("n" = "Unburned", "y" = "Burned"))
Core_Dry_Mass$Burned


p = ggplot(Core_Dry_Mass, aes(x=Core_Number, y=Dry_Mass_g))
p = p + geom_boxplot()
p

#faciting
p = ggplot(Core_Dry_Mass, aes(x=Core_Number, y=Dry_Mass_g))
p = p + geom_boxplot()
p = p + facet_wrap(~Burned)
p

#Axis labels
p = ggplot(Core_Dry_Mass, aes(x=Core_Number, y=Dry_Mass_g))
p = p + geom_boxplot()
p = p + facet_wrap(~Burned)
p = p + ylab("Mean Dry Mass (g)") 
p = p + xlab(NULL)
p

#Getting rid of the x scale
p = ggplot(Core_Dry_Mass, aes(x=Core_Number, y=Dry_Mass_g))
p = p + geom_boxplot()
p = p + facet_wrap(~Burned, strip.position = "bottom")
p = p + ylab("Mean Dry Mass (g)") 
p = p + theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
p = p + xlab("Burned")
p

#Background and color
p = ggplot(Core_Dry_Mass, aes(x=Core_Number, y=Dry_Mass_g, fill = Burned))
p = p + geom_boxplot()
p = p + facet_wrap(~Burned, strip.position = "bottom")
p = p + ylab("Mean Dry Mass (g)") 
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=14), axis.title = element_text(size=14))
p = p + scale_fill_manual(values=c("#CC0000","#666666"), guide=FALSE)
p = p + theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
p = p + xlab("Burned")
p

#figure for paper
p = ggplot(Core_Dry_Mass, aes(x=Core_Number, y=Dry_Mass_g, fill = Burned))
p = p + geom_boxplot()
p = p + facet_wrap(~Burned, strip.position = "bottom")
p = p + ylab("Mean Above Ground Dry Mass (g)") 
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=10), axis.text = element_text(size=10), axis.title = element_text(size=10))
p = p + scale_fill_manual(values=c("#CCCCCC","#555555"), guide=FALSE)
p = p + theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
p = p + xlab(NULL)
p


#---------------------------------------------------------------------------------
#Colunm Plot
p = ggplot(Core_Dry_Mass, aes(x=Core_Number, y=Dry_Mass_g))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Burned)
p

#Colunm Plot
p = ggplot(Core_Dry_Mass, aes(x=Burned, y=Dry_Mass_g))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Burned)
p

#Deplyer package
df = Core_Dry_Mass %>%
  group_by(Burned) %>%
  summarise(mean_mass = mean(Dry_Mass_g, na.rm = TRUE))
df

tail(df)

#Standard Error
df = Core_Dry_Mass %>%
  group_by(Burned) %>%
  summarise(mean_mass = mean(Dry_Mass_g, na.rm = TRUE), Std_Dev = sd(Dry_Mass_g, na.rm = TRUE),Count=21 %>%
  mutate(Std_Err = Std_Dev / (Count)^0.5))
df

#Adding Standard Error into ggplot2
p = ggplot(df, aes(x=Burned, y=Dry_Mass_g))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Burned)
p

#Adding standard error into the plot
p = ggplot(df, aes(x=Burned, y=Dry_Mass_g))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Burned)
p = p + geom_errorbar(aes(ymin = mean_mass - Std_Err, ymax = mean_mass + Std_Err))
p

#Axix labels
p = ggplot(df, aes(x=Burned, y=Dry_Mass_g))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Burned)
p = p + geom_errorbar(aes(ymin = mean_mass - Std_Err, ymax = mean_mass + Std_Err))
p = p + ylab("Mean Dry Mass (g)") 
p

#Background and color
p = ggplot(df, aes(x=Burned, y=Dry_Mass_g))
p = p + geom_col(position = position_stack())
p = p + facet_wrap(~Burned)
p = p + geom_errorbar(aes(ymin = mean_mass - Std_Err, ymax = mean_mass + Std_Err))
p = p + ylab("Mean Dry Mass (g)") 
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=14), axis.text = element_text(size=14), axis.title = element_text(size=14))
p = p + scale_color_manual(values=c("#CC0000","#666666"))
p






