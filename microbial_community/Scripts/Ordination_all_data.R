library("phyloseq")
library("dplyr")
library("ggplot2")
library("vegan")
library("gridExtra")
library("DESeq2")


setwd("~/Box Sync/WhitmanLab/Projects/JackPine/Data/Seq/OTU_table")
OTU_table = read.csv("feature-table.txt", sep="\t", row.names = 1)
OTU_table = otu_table(OTU_table, taxa_are_rows = TRUE)
head(OTU_table)
#write.csv(colnames(OTU_table), "Sample_Data.csv")
#-----------------------------------------------------------------------
 OTU_table[21,]
  
TaxTab = read.table("taxonomy.tsv",sep="\t", fill=TRUE)
# Gets the taxonomy table separated into the name+size, taxonomy, and two scores.
head(TaxTab)
TaxTab[23,]

V1split = read.table(textConnection(as.character(TaxTab$V1)), sep=";",fill=TRUE, header=FALSE)
# Split out the otu ID and count
head(V1split)

V2split = read.table(textConnection(as.character(TaxTab$V2)), sep=";",fill=TRUE, header=FALSE)
# Split out the taxonomy and read it
head(V2split)

levels(V2split$V8)
# Checking to make sure there is no useful info in V8 column (beyond species)

TaxTab[,1] = V1split
# Adding the OTU names

TaxTab[,2:8] = V2split[,1:7]
# Adding the taxonomy

head(TaxTab)

colnames(TaxTab) = c("OTU","Domain","Phylum","Class","Order","Family","Genus","Species")
# Assigning the proper column names

TaxTab$Domain = gsub("^.*?__","",TaxTab$Domain)
TaxTab$Phylum = gsub("^.*?__","",TaxTab$Phylum)
TaxTab$Class = gsub("^.*?__","",TaxTab$Class)
TaxTab$Order = gsub("^.*?__","",TaxTab$Order)
TaxTab$Family = gsub("^.*?__","",TaxTab$Family)
TaxTab$Genus = gsub("^.*?__","",TaxTab$Genus)
TaxTab$Species = gsub("^.*?__","",TaxTab$Species)
# Getting rid of the weird "D_0__" prefixes

TaxTabPs = TaxTab[2:nrow(TaxTab),]
# Cutting off the top row, which had the original rownames

TaxTabPs = as.matrix(TaxTabPs)
# Turn it into a matrix

row.names(TaxTabPs) = TaxTabPs[,1]
# Make the row names the OTU column (column 1)

TaxTabPs = TaxTabPs[,2:8]
# Drop the column 1, now that the OTU names are just the rownames

head(TaxTabPs)

TaxTabPs = tax_table(TaxTabPs)
# Create phyloseq object

head(TaxTabPs)
dim(TaxTabPs)


SampleData = read.csv("Sample_Data.csv",row.names=1)
head(SampleData)

SampleData = sample_data(SampleData)

ps = phyloseq(TaxTabPs, OTU_table, SampleData)
ps

#Don't get rid of the blanks first! Looking at all of the data 
ps.norm = transform_sample_counts(ps, function(x) x / sum(x) )
#ps.hellinger = transform_sample_counts(ps, function(x) (x / sum(x))^0.5 )

# report abundances as fraction of total seqs in each sample
sample_sums(ps.norm)
#sample_sums(ps.hellinger)


#Stacked bar by phylum
p = plot_bar(ps.norm, fill = "Phylum")
p = p + geom_bar(aes(color=Phylum), stat = "identity", position = "stack")
p


ps.act = prune_taxa(data.frame(tax_table(ps.norm))$Phylum=="Actinobacteria",ps.norm)
ps.act

#Stacked bar by family
p = plot_bar(ps.act, fill = "Family")
p = p + geom_bar(aes(color=Family), stat = "identity", position = "stack")
p



#-----------------------------------
#no blanks
ps.norm.nb = prune_samples(sample_data(ps.norm)$Soil_Type %in% c("O", "A"), ps.norm)
ps.norm.nb
MyOrdination =  ordinate(physeq = ps.norm.nb, method="PCoA", distance="bray", trymax=1000)
sample_data(ps.norm.nb)


p = plot_ordination(physeq = ps.norm.nb, ordination = MyOrdination, type = "samples", axes = 1:2, shape = "Burned",color="Plot")
#p = p + scale_colour_manual(values=c("red", "orange", "green", "blue", "purple", "pink"))
p = p + guides(size=guide_legend(title="Horizon"),colour=guide_legend(title="Site ID"))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12))
p = p + theme(legend.text = element_text(size = 12))
p = p + theme(legend.title = element_text(size = 12))
p = p + theme(legend.title = element_text(face = "bold"))
p = p + facet_wrap(~sample_data(ps.norm.nb)$Soil_Type)
p = p + geom_point(size=3)
p = p + scale_fill_identity(name="Site ID", guide="legend", labels=c("No", "Yes")) + scale_color_manual(name="Site_ID", values=c("red", "orange", "green", "blue", "purple", "pink"), labels=c("A","B", "C", "D", "E", "F"))
p 

#Stats with no blanks
d = phyloseq::distance(ps.norm.nb, method = "bray")
samdat = sample_data(ps.norm.nb)
groups = as.factor(samdat$Burned)
x = betadisper(d, groups)
boxplot(x, ylab = "Distance to centroid")
anova(x)
#p = 0.02695 for Soil Type
#p = 0.9568 for Burned
#p = 0.6097 for plot


d.adonis = adonis(d~
                      sample_data(ps.norm.nb)$Plot + sample_data(ps.norm.nb)$Soil_Type + sample_data(ps.norm.nb)$Burned)
d.adonis
#p=0.001 for plot (plot does have an effect
#p=0.056 forsoil type



#Only O horizon state with no blanks 
ps.norm.o = prune_samples(sample_data(ps.norm)$Soil_Type %in% c("O"), ps.norm)
d.o = phyloseq::distance(ps.norm.o, method = "bray")
samdat = sample_data(ps.norm.o)
groups = as.factor(samdat$Plot)
x = betadisper(d.o, groups)
boxplot(x, ylab = "Distance to centroid")
anova(x)
#p = 0.5735 for burned

d.adonis.o = adonis(d.o~
                      sample_data(ps.norm.o)$Plot + sample_data(ps.norm.o)$Burned)
d.adonis.o
#p=0.001 for plot
#p=0.365 for burned

d.o.pcoa = ordinate(ps.norm.o,method="PCoA",distance="bray")

#Only O horizon PCoA
p = plot_ordination(physeq = ps.norm.o, ordination = d.o.pcoa, type = "samples", axes = 1:2, shape = "Burned",color="Plot")
#p = p + geom_point(aes(size=sample_data(ps.norm.o)$Soil_Type))
#p = p + scale_shape_manual(values=c(0,1,2,3,4,5))
#p = p + scale_size_manual(values= c(3,9))
#p = p + scale_colour_manual(values=c("blue", "green"))
#p = p + guides(size=guide_legend(title="Horizon"),colour=guide_legend(title="Burned"))
#p = p + scale_colour_manual(guide=FALSE) + scale_shape_manual(guide=FALSE)
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12))
p = p + theme(legend.text = element_text(size = 12))
p = p + theme(legend.title = element_text(size = 12))
p = p + theme(legend.title = element_text(face = "bold"))
p 

p.O = p


#Only A stats with no blanks
ps.norm.a = prune_samples(sample_data(ps.norm)$Soil_Type %in% c("A"), ps.norm)
d.a = phyloseq::distance(ps.norm.a, method = "bray")
samdat = sample_data(ps.norm.a)
groups = as.factor(samdat$Plot)
x = betadisper(d.a, groups)
boxplot(x, ylab = "Distance to centroid")
anova(x)
#p = 0.5735 for burned 

d.adonis.a = adonis(d.a~
                      sample_data(ps.norm.a)$Plot + sample_data(ps.norm.a)$Burned)
d.adonis.a
#p=0.001 for plot
#p=0.345 for burned

d.a.pcoa = ordinate(ps.norm.a,method="PCoA",distance="bray")

#Only A horizon NMDA
p = plot_ordination(physeq = ps.norm.a, ordination = d.a.pcoa, type = "samples", axes = 1:2, shape = "Burned",color="Plot")
#p = p + geom_point(aes(size=sample_data(ps.norm.a)$Soil_Type))
#p = p + scale_shape_manual(values=c(0,1,2,3,4,5))
#p = p + scale_size_manual(values= c(3,9))
#p = p + scale_colour_manual(values=c("blue", "green"))
p = p + guides(size=guide_legend(title="Horizon"),colour=guide_legend(title="Burned"))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12))
p = p + theme(legend.text = element_text(size = 12))
p = p + theme(legend.title = element_text(size = 12))
p = p + theme(legend.title = element_text(face = "bold"))
p
p.A = p

grid.arrange(p.O,p.A,ncol=2)

#---------------------------------------------------

#With the blanks
MyOrdination = ordinate(physeq = ps.norm, method="PCoA", distance="bray", trymax=1000)
#remove # to run the hellinger 
#MyOrdination = ordinate(physeq = ps.hellinger, method="PCoA", distance="bray", trymax=1000)
sample_data(ps.norm)

p = plot_ordination(physeq = ps.norm, ordination = MyOrdination, type = "samples", axes = 2:3, color = "Burned",shape="Plot")
p = p + geom_point(aes(size=sample_data(ps.norm)$Soil_Type))
p = p + scale_shape_manual(values=c(0,1,2,3,4,5,7,8))
p = p + scale_size_manual(values= c(3,4,7,9))
p = p + scale_colour_manual(values=c("#660000","#330000", "blue", "green"))
p = p + guides(size=guide_legend(title="Horizon"),colour=guide_legend(title="Burned"))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12))
p = p + theme(legend.text = element_text(size = 12))
p = p + theme(legend.title = element_text(size = 12))
p = p + theme(legend.title = element_text(face = "bold"))
p 
#---------------------------------------------------------------------------------
  
### Plotting phylum relative abundance

df = psmelt(ps.norm.nb)
df
df.plot = df%>%
  group_by(Sample, Phylum, Burned, Soil_Type)%>%
  summarize(Abundance = sum(Abundance))
df.plot

cutoff = 0.002

phylumtokeep = df.plot%>%
  group_by(Phylum)%>%
  summarise(meanabundance = mean(Abundance))%>%
  filter(meanabundance >= cutoff)
phylumtokeep

phylumtokeep$Phylum

df.plot = df.plot%>%
  filter(Phylum %in% phylumtokeep$Phylum)
df.plot


p = ggplot(df.plot, aes(x=Soil_Type, y=Abundance, color=Burned))
p = p + geom_boxplot()
p = p + facet_wrap(~Phylum, scales="free")
p

#Paper formating 
p = ggplot(df.plot, aes(x=Soil_Type, y=Abundance, color=Burned))
p = p + geom_boxplot()
p = p + facet_wrap(~Phylum, scales="free")
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12, face="italic"), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12))
p = p + xlab("Soil Type") + ylab("Relative Abundance")
p = p + scale_color_manual(values=c("#000000","#CC0000"))
p = p + expand_limits(y=0)
p

# Stats for Phylum abundances

t = df.plot %>%
    mutate(Trtmt=paste(Burned,Soil_Type))

anova_function = function(phylum){
  m = t %>%
    filter(Phylum==phylum)
  aov = aov(Abundance~Trtmt,m)
  return(summary(aov))
}
phyla = as.vector(data.frame(phylumtokeep[,1])[,1])
phyla

sapply(phyla,anova_function)
# None of the phyla are signficantly different from O to A and burned/unburned, testing all 4 at once


# You'll need to install the package DESeq2 before you can load it
# Commands:
# source("https://bioconductor.org/biocLite.R")
# biocLite("DESeq2")
#library(DESeq2)

#DESeq2 takes raw reads, not relative abundances,
# So we make a non-normalized no blank ps object
ps.nb = prune_samples(sample_data(ps)$Soil_Type %in% c("O", "A"), ps)

# Making a variable with just the taxonomy
taxonomy = data.frame(tax_table(ps.nb))

# Making the deseq object. The first value is the phyloseq object
# The second value is the variables that we are interested in.
# We are controlling for Plot and for Soil_Type, then testing for Burned
# Could also do this separately for A and O horizons, but I think all together might be better.
dseq = phyloseq_to_deseq2(ps.nb, ~Plot+Soil_Type+Burned)

# relevel tells it which value is the baseline/default value
# For us, that is the not burned treatment
dseq$Burned = relevel(dseq$Burned,"N")
# And we will arbitrarily set the baseline Soil_Type to A... don't think this matters
dseq$Soil_Type = relevel(dseq$Soil_Type,"A")

# Now we run the deseq fit (Will take a while to process)
dseq = DESeq(dseq, quiet = TRUE, fitType = "local")

# These are the various contrasts we could pull out,
# which were included in the analysis
resultsNames(dseq)


# Creating an object to hold the results
results = results(dseq, contrast=c("Burned","Y","N"), cooksCutoff=TRUE)
# (You can change Cooks Cutoff to control outliers or not)
head(results)

# Just making one for the O vs A contrast too.
results.OA = results(dseq, contrast=c("Soil_Type","O","A"), cooksCutoff=TRUE)
head(results.OA)

# Join the results back up with the OTU taxonomy 
results = data.frame(results,taxonomy,row.names(taxonomy))
head(results)

threshold = function (thresh){
  dplyr::filter(results, baseMean >= thresh) %>% 
    dplyr::mutate(padj = p.adjust(pvalue,"BH")) %>%
    dplyr::summarize(cutoff=thresh, count=sum(padj<=0.05, na.rm = TRUE))
}
tail(results)

range = seq(0,2,0.05)
range
# Creates a range of numbers we are interested in for adjusted p values
thresh.s = plyr::ldply(range, threshold)
# Applys the Threshold function we created above to the range of numbers we created above.

plot(thresh.s$count~thresh.s$cutoff)
# We can plot the threshold for base Mean value against the number of samples that will pass under this cutoff.
# We can see the optimum value to use here (here, 0.8 or so)

# Filter out any taxa below the basemean (abundance) cutoff
# Then adjust their p-values using the Benjamini Hochberg correction
cutoff = 0.05
results = results %>%
  filter(baseMean>=0.8) %>% 
  mutate(padj = p.adjust(pvalue,"BH"))%>%
  mutate(Sig = ifelse(padj<cutoff,"significant","not significant"))

# You can see this results in dropping quite a few OTUs
# Also, most p-values are not significant - that makes sense.
dim(results)
head(results)


d = results %>%
  filter(Phylum %in% phylumtokeep$Phylum)
dim(d)

p = ggplot(d, aes(x = Phylum, y = log2FoldChange, fill=Phylum, alpha=Sig, size=baseMean))
# establishing our plot is based on the data table d, with our x being the phylum, and the y being log2fold change,
# and the colour being phylum, relative size proportional to mean abundance, and alpha (transparency) based on
# whether or not the adjusted p value is significant.
p = p + geom_jitter(shape=21, width=0.1) # Using shape=21 to get the option that has an outline and a fill colour
p = p + theme_bw()
# sets a theme - adjusts a bunch of aesthetics at once

p = p + theme(strip.text.x = element_text(size = 14),
              strip.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, face="italic"),
              axis.title.x = element_text(size = 14),
              axis.text.y = element_text(size=14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size=14),
              legend.text = element_text(size = 10),
              #legend.position = "none",
              strip.background = element_blank()) + guides(fill=FALSE, size=FALSE,alpha=FALSE)
# sets a bunch of visual paramters for the legend (none) and other text

p = p + labs(x = "Phylum")
# sets the label for the x axis.

p = p + labs(y = expression(paste("", log[2]," fold change vs. unburned",sep="")))
# sets the label for the y axes.

p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Gets rid of the default gridlines

p = p + geom_hline(yintercept=0)

p

############### Making same plot but at finer taxonomic level ##########################
SigOrders = d %>%
    filter(Sig == "significant")%>%
    group_by(Phylum,Order)%>%
    summarize(N=n())%>%
    arrange(Phylum)
OrderKeep = SigOrders$Order
d.orders = d %>%
    filter(Order %in% OrderKeep)%>%
    filter(!is.na(Order))%>%
    filter(Order != "Unknown Order")%>%
    filter(Order != "")
OrderKeep
d.orders$Order = ordered(d.orders$Order, levels = OrderKeep)

p = ggplot(d.orders, aes(x = Order, y = log2FoldChange, fill=Phylum, alpha=Sig, size=baseMean))
# establishing our plot is based on the data table d, with our x being the phylum, and the y being log2fold change,
# and the colour being phylum, relative size proportional to mean abundance, and alpha (transparency) based on
# whether or not the adjusted p value is significant.
p = p + geom_jitter(shape=21, width=0.1) # Using shape=21 to get the option that has an outline and a fill colour
p = p + theme_bw()
# sets a theme - adjusts a bunch of aesthetics at once

p = p + theme(strip.text.x = element_text(size = 14),
              strip.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, face="italic"),
              axis.title.x = element_text(size = 14),
              axis.text.y = element_text(size=14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size=14),
              legend.text = element_text(size = 10),
              #legend.position = "none",
              strip.background = element_blank()) + guides(size=FALSE,alpha=FALSE)
# sets a bunch of visual paramters for the legend (none) and other text

p = p + labs(x = "Order")
# sets the label for the x axis.

p = p + labs(y = expression(paste("", log[2]," fold change vs. unburned",sep="")))
# sets the label for the y axes.

p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Gets rid of the default gridlines

p = p + geom_hline(yintercept=0)

p



# Considering O vs. A instead of Burned/Unburned

results.OA = data.frame(results.OA,taxonomy, row.names(taxonomy))
head(results.OA)

threshold = function (thresh){
  dplyr::filter(results.OA, baseMean >= thresh) %>% 
    dplyr::mutate(padj = p.adjust(pvalue,"BH")) %>%
    dplyr::summarize(cutoff=thresh, count=sum(padj<=0.10, na.rm = TRUE))
}

range = seq(0,2,0.05)
# Creates a range of numbers we are interested in for adjusted p values
thresh.s = plyr::ldply(range, threshold)
# Applys the Threshold function we created above to the range of numbers we created above.

plot(thresh.s$count~thresh.s$cutoff)
# We can plot the threshold for base Mean value against the number of samples that will pass under this cutoff.
# We can see the optimum value to use here (here, 1 or so)

# Filter out any taxa below the basemean (abundance) cutoff
# Then adjust their p-values using the Benjamini Hochberg correction
results.OA = results.OA %>%
  filter(baseMean>=1) %>% 
  mutate(padj = p.adjust(pvalue,"BH"))%>%
  mutate(Sig = ifelse(padj<cutoff,"significant","not significant"))
# You can see this results in dropping quite a few OTUs
# Also, most p-values are not significant - that makes sense.
dim(results.OA)

# Let's add a categorical column that states whether the adjusted pvalue (padj) is less than a cutoff

cutoff = 0.05

d = results.OA %>%
  filter(Phylum %in% phylumtokeep$Phylum)
dim(d)

p = ggplot(d, aes(x = Phylum, y = log2FoldChange, fill=Phylum, size = baseMean, alpha=Sig))
# establishing our plot is based on the data table d, with our x being the phylum, and the y being log2fold change,
# and the colour being phylum, relative size proportional to mean abundance, and alpha (transparency) based on
# whether or not the adjusted p value is significant.
p = p + geom_jitter(shape=21, width=0.1) # Using shape=21 to get the option that has an outline and a fill colour
p = p + theme_bw()
# sets a theme - adjusts a bunch of aesthetics at once

p = p + theme(strip.text.x = element_text(size = 24),
              strip.text.y = element_text(size = 24),
              axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1, face="italic"),
              axis.title.x = element_text(size = 28),
              axis.text.y = element_text(size=16),
              axis.title.y = element_text(size = 28),
              legend.title = element_text(size=20),
              legend.text = element_text(size = 14),
              #legend.position = "none",
              strip.background = element_blank())
# sets a bunch of visual paramters for the legend (none) and other text

p = p + labs(x = "Phylum")
# sets the label for the x axis.

p = p + labs(y = expression(paste("", log[2]," fold change O vs. A horizon",sep="")))
# sets the label for the y axes.

p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Gets rid of the default gridlines

p = p + geom_hline(yintercept=0)

p

### Which are the significant responders?

Sig.Burned = results[results$Sig=="significant",]
Sig.Burned = Sig.Burned %>%
	arrange(-baseMean)
head(Sig.Burned)
dim(Sig.Burned)
write.csv(Sig.Burned,"SignificantBurnResponders.csv")

Sig.OvsA = results.OA[results.OA$Sig=="significant",]
dim(Sig.OvsA)





### What if more taxa respond in O vs A to burning?

#DESeq2 takes raw reads, not relative abundances,
# So we make a non-normalized no blank ps object
ps.nb = prune_samples(sample_data(ps)$Soil_Type %in% c("A"), ps)

# Making a variable with just the taxonomy
taxonomy = data.frame(tax_table(ps.nb))

# Making the deseq object. The first value is the phyloseq object
# The second value is the variables that we are interested in.
# We are controlling for Plot and for Soil_Type, then testing for Burned
# Could also do this separately for A and O horizons, but I think all together might be better.
dseq = phyloseq_to_deseq2(ps.nb, ~Plot+Burned)

# relevel tells it which value is the baseline/default value
# For us, that is the not burned treatment
dseq$Burned = relevel(dseq$Burned,"N")

# Now we run the deseq fit (Will take a while to process)
dseq = DESeq(dseq, quiet = TRUE, fitType = "local")

# These are the various contrasts we could pull out,
# which were included in the analysis
resultsNames(dseq)

# Creating an object to hold the results
results = results(dseq, contrast=c("Burned","Y","N"), cooksCutoff=TRUE)
# (You can change Cooks Cutoff to control outliers or not)
head(results)


# Join the results back up with the OTU taxonomy 
results = data.frame(results,taxonomy,row.names(taxonomy))
head(results)

threshold = function (thresh){
  dplyr::filter(results, baseMean >= thresh) %>% 
    dplyr::mutate(padj = p.adjust(pvalue,"BH")) %>%
    dplyr::summarize(cutoff=thresh, count=sum(padj<=0.10, na.rm = TRUE))
}

range = seq(0,2,0.05)
# Creates a range of numbers we are interested in for adjusted p values
thresh.s = plyr::ldply(range, threshold)
# Applys the Threshold function we created above to the range of numbers we created above.

plot(thresh.s$count~thresh.s$cutoff)
# We can plot the threshold for base Mean value against the number of samples that will pass under this cutoff.
# We can see the optimum value to use here (here, 0.8 or so)

# Filter out any taxa below the basemean (abundance) cutoff
# Then adjust their p-values using the Benjamini Hochberg correction
cutoff = 0.05
resultsA = results %>%
  filter(baseMean>=0.8) %>% 
  mutate(padj = p.adjust(pvalue,"BH"))%>%
  mutate(Sig = ifelse(padj<cutoff,"significant","not significant"))

# You can see this results in dropping quite a few OTUs
# Also, most p-values are not significant - that makes sense.
dim(results)

# Let's add a categorical column that states whether the adjusted pvalue (padj) is less than a cutoff

cutoff = 0.05

d = results %>%
  filter(Phylum %in% phylumtokeep$Phylum)
dim(d)

p = ggplot(d, aes(x = Phylum, y = log2FoldChange, fill=Phylum, alpha=Sig, size=baseMean))
# establishing our plot is based on the data table d, with our x being the phylum, and the y being log2fold change,
# and the colour being phylum, relative size proportional to mean abundance, and alpha (transparency) based on
# whether or not the adjusted p value is significant.
p = p + geom_jitter(shape=21, width=0.1) # Using shape=21 to get the option that has an outline and a fill colour
p = p + theme_bw()
# sets a theme - adjusts a bunch of aesthetics at once

p = p + theme(strip.text.x = element_text(size = 14),
              strip.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, face="italic"),
              axis.title.x = element_text(size = 14),
              axis.text.y = element_text(size=14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size=14),
              legend.text = element_text(size = 10),
              #legend.position = "none",
              strip.background = element_blank()) + guides(fill=FALSE, size=FALSE,alpha=FALSE)
# sets a bunch of visual paramters for the legend (none) and other text

p = p + labs(x = "Phylum")
# sets the label for the x axis.

p = p + labs(y = expression(paste("", log[2]," fold change vs. unburned",sep="")))
# sets the label for the y axes.

p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Gets rid of the default gridlines

p = p + geom_hline(yintercept=0)

p

# Testing whether the same (and same number of) taxa respond to burn if tested by O vs A
resultsO = resultsO[row.names(resultsO)%in%row.names(resultsA),]
resultsA = resultsA[row.names(resultsA)%in%row.names(resultsO),]
row.names(resultsO)==row.names(resultsA)
l2fcA = resultsA$log2FoldChange
l2fcO = resultsO$log2FoldChange
Phyla = resultsO$Phylum

df = data.frame(Phyla,l2fcA,l2fcO)
p = ggplot(df,aes(x=l2fcA,y=l2fcO))
p = p + geom_point(alpha=0.5) + guides(color=FALSE)
p = p + xlim(-10,10) + ylim(-10,10) + xlab("Burn response in A horizon")+ ylab("Burn response in O horizon")
p


##### What are the relative abundances of the "responders"?

head(Sig.Burned)
colnames(Sig.Burned)
colnames(Sig.Burned)[14]="OTU"
colnames(Sig.Burned)

d.Resp = df %>%
    filter(OTU %in% Sig.Burned$OTU)%>%
    group_by(OTU,Burned,Soil_Type)%>%
    mutate(NonZero = ifelse(Abundance>0,1,0))%>%
    summarize(MeanRelabund=mean(Abundance),NonZero=sum(NonZero))%>%
    arrange(-MeanRelabund)
d.Resp

#write.csv(d.Resp,"Burn_Responsive_Relabund.csv")

### Filtering responders
Merged = merge(d.Resp, Sig.Burned, by="OTU",all.x=TRUE)
head(Merged)

Merged = Merged%>%
    group_by(OTU)%>%
    filter(min(MeanRelabund)>0 | max(NonZero)>3 | mean(MeanRelabund)>0.001)%>%
    group_by(OTU,log2FoldChange,padj,Phylum,Class,Order,Family,Genus,Species,Sig,Burned,Soil_Type)%>%
    summarize(MeanRelabund=mean(MeanRelabund))%>%
    arrange(-log2FoldChange,Burned)
head(Merged)

write.csv(Merged, "Burn_Responsive_Filtered.csv")

d = results %>%
  filter(Phylum %in% phylumtokeep$Phylum)%>%
  mutate(Sig = ifelse(row.names.taxonomy. %in% Merged$OTU, "significant","not significant"))
head(d)

p = ggplot(d, aes(x = Order, y = log2FoldChange, fill=Phylum, alpha=Sig, size=baseMean))
# establishing our plot is based on the data table d, with our x being the phylum, and the y being log2fold change,
# and the colour being phylum, relative size proportional to mean abundance, and alpha (transparency) based on
# whether or not the adjusted p value is significant.
p = p + geom_jitter(shape=21, width=0.1) # Using shape=21 to get the option that has an outline and a fill colour
p = p + theme_bw()
# sets a theme - adjusts a bunch of aesthetics at once

p = p + theme(strip.text.x = element_text(size = 14),
              strip.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, face="italic"),
              axis.title.x = element_text(size = 14),
              axis.text.y = element_text(size=14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size=14),
              legend.text = element_text(size = 10),
              #legend.position = "none",
              strip.background = element_blank()) + guides(fill=FALSE, size=FALSE,alpha=FALSE)
# sets a bunch of visual paramters for the legend (none) and other text

p = p + labs(x = "Phylum")
# sets the label for the x axis.

p = p + labs(y = expression(paste("", log[2]," fold change vs. unburned",sep="")))
# sets the label for the y axes.

p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Gets rid of the default gridlines

p = p + geom_hline(yintercept=0)

p

### Plotting at finer taxonomic level
SigOrders = d %>%
  filter(Sig == "significant")%>%
  group_by(Phylum,Order)%>%
  summarize(N=n())%>%
  arrange(Phylum)
OrderKeep = SigOrders$Order
d.orders = d %>%
  filter(Order %in% OrderKeep)%>%
  filter(!is.na(Order))%>%
  filter(Order != "Unknown Order")%>%
  filter(Order != "")
OrderKeep
d.orders$Order = ordered(d.orders$Order, levels = OrderKeep)

p = ggplot(d.orders, aes(x = Order, y = log2FoldChange, fill=Phylum, alpha=Sig, size=baseMean))
# establishing our plot is based on the data table d, with our x being the phylum, and the y being log2fold change,
# and the colour being phylum, relative size proportional to mean abundance, and alpha (transparency) based on
# whether or not the adjusted p value is significant.
p = p + geom_jitter(shape=21, width=0.1) # Using shape=21 to get the option that has an outline and a fill colour
p = p + theme_bw()
# sets a theme - adjusts a bunch of aesthetics at once

p = p + theme(strip.text.x = element_text(size = 14),
              strip.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, face="italic"),
              axis.title.x = element_text(size = 14),
              axis.text.y = element_text(size=14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size=14),
              legend.text = element_text(size = 10),
              #legend.position = "none",
              strip.background = element_blank()) + guides(size=FALSE,alpha=FALSE)
# sets a bunch of visual paramters for the legend (none) and other text

p = p + labs(x = "Order")
# sets the label for the x axis.

p = p + labs(y = expression(paste("", log[2]," fold change vs. unburned",sep="")))
# sets the label for the y axes.

p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Gets rid of the default gridlines

p = p + geom_hline(yintercept=0)

p



##################### Looking at Archaea ########################
# Looking to find Archaea
data.frame(tax_table(ps.norm.nb))[data.frame(tax_table(ps.norm.nb))$Domain=="Archaea",]
ps.A = subset_taxa(ps,Domain == "Archaea")
ps.A = prune_samples(sample_sums(ps.A)>0,ps.A)
otu_table(ps.A)
ps.A.norm =  transform_sample_counts(ps.A, function(x) x / sum(x) )

ORD =  ordinate(physeq = ps.A.norm, method="PCoA", distance="bray", trymax=1000)

p = plot_ordination(physeq = ps.A.norm, ordination = ORD, type = "samples", axes = 1:2, shape = "Burned",color="Plot")
#p = p + scale_colour_manual(values=c("red", "orange", "green", "blue", "purple", "pink"))
p = p + guides(size=guide_legend(title="Horizon"),colour=guide_legend(title="Site ID"))
p = p + theme_bw()
p = p + theme(panel.grid = element_blank(), strip.background = element_blank())
p = p + theme(strip.text = element_text(size=12), axis.text = element_text(size=12), axis.title = element_text(size=12), axis.text.x = element_text(size=12))
p = p + theme(legend.text = element_text(size = 12))
p = p + theme(legend.title = element_text(size = 12))
p = p + theme(legend.title = element_text(face = "bold"))
p = p + facet_wrap(~sample_data(ps.A.norm)$Soil_Type)
p = p + geom_point(size=3)
p = p + scale_fill_identity(name="Site ID", guide="legend", labels=c("No", "Yes")) + scale_color_manual(name="Site_ID", values=c("red", "orange", "green", "blue", "purple", "pink"), labels=c("A","B", "C", "D", "E", "F"))
p 

p = plot_bar(ps.A, fill = "OTU")
p = p + geom_bar(aes(color=OTU), stat = "identity", position = "stack")
p = p + facet_wrap(~sample_data(ps.A.norm)$Burned)
p
