#Importing stats data
Core_Dry_Mass_Stats <- read_excel("~/Box Sync/WhitmanLab/Projects/JackPine/Data/Lab_Data/Core_Deepot_Assay/Core_Dry_Mass_Stats.xlsx")
View(Core_Dry_Mass_Stats)

Core_Dry_Mass_Stats = Core_Dry_Mass_Stats%>%
  arrange(Core_Match)

#Stats
Core_Dry_Mass_Burned = Core_Dry_Mass_Stats$Dry_Mass_g[Core_Dry_Mass_Stats$Burned == "y"]
Core_Dry_Mass_Burned
Core_Dry_Mass_Unburned = Core_Dry_Mass_Stats$Dry_Mass_g[Core_Dry_Mass_Stats$Burned == "n"]
Core_Dry_Mass_Unburned

t = t.test(Core_Dry_Mass_Burned, Core_Dry_Mass_Unburned, paired=TRUE,alternative = "two.sided")
t

#Built in T test
a = aov(data = Core_Dry_Mass_Stats, Dry_Mass_g ~ Burned + Core_Match)
summary(a)

#No t test
a = aov(data = Core_Dry_Mass_Stats, Dry_Mass_g ~ Burned)
summary(a)

