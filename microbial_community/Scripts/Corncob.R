library("phyloseq")
library("dplyr")
library("ggplot2")
library("vegan")

#devtools::install_github("bryandmartin/corncob")
library(corncob)
library(magrittr)

# Check out package help
help(package = "corncob", help_type = "html")
citation("corncob")

# Bring in phyloseq object
ps = readRDS("ps")
ps = ps %>%
    subset_samples(Burned != "Blank1" & Burned != "Blank2")
ps

# Create pair variable
sample_data(ps)$Pair = paste(sample_data(ps)$Plot,sample_data(ps)$Soil_Type, sep="")
#sample_data(ps)$Pair = as.factor(sample_data(ps)$Pair)
sample_data(ps)

# Add run data - effects on dispersion
sample_data(ps)$Run = c(rep("First",12),rep("Second",12))

colnames(sample_data(ps))
head(otu_table(ps))
head(taxa_names(ps))

# Fix OTU names
OTUIDs = rep(1:dim(otu_table(ps))[1])
OTUIDs = paste("OTU",OTUIDs,sep="")
taxa_names(ps)=OTUIDs


# Differentially variable taxa
# This formula is testing for the effect of burning on variability, controlling for sequencing run and soil horizon (phi.formula vs. phi.formula_null)
da_analysis.dv <- differentialTest(formula = ~ Run+Soil_Type+Burned,
                                phi.formula = ~ Run+Soil_Type+Burned,
                                formula_null = ~ Run+Soil_Type+Burned,,
                                phi.formula_null = ~ Run+Soil_Type
                                test = "LRT", boot = FALSE,
                                data = ps,
                                fdr_cutoff = 0.05)

#saveRDS(da_analysis.dv,"da_analysis.DV.full")
da_analysis.dv = readRDS("da_analysis.DV.full")
                             
# Look at which OTUs are significantly differentially variable with burning
da_analysis.dv$significant_taxa
otu_to_taxonomy(da_analysis.dv$significant_taxa,data=ps)
da_analysis.dv$significant_models[1]

# Plotting taxa with significant burn effect
# OTU3 is less abundant in the burned samples
corncob.3 <- bbdml(formula = OTU600 ~ Run+Soil_Type+Burned,
                 phi.formula = ~ Run+Soil_Type+Burned,
                 data = ps)
plot(corncob.3,shape="Burned", color="Pair")
summary(corncob.3)

# Differentially abundant taxa
# This formula is testing for the effect of burning on abundance,
# controlling for sequencing run and soil horizon
# as well as variability associated with the taxa
da_analysis.da <- differentialTest(formula = ~ Run+Soil_Type+Burned,
                                phi.formula = ~ Run+Soil_Type+Burned,
                                formula_null = ~ Run+Soil_Type,
                                phi.formula_null = ~ Run+Soil_Type+Burned,
                                test = "LRT", boot = FALSE,
                                data = ps,
                                fdr_cutoff = 1)
                                # We set fdr_cutoff to 1 to include all taxa; then post-hoc adjust p
                                # for only subset of interest (abundant taxa)

#saveRDS(da_analysis.da,"da_analysis.DA.full")
da_analysis.da = readRDS("da_analysis.DA.full")

# Melt phyloseq and get mean relative abundances for each OTU
df = psmelt(ps)
d.Abund.OTUs = df %>%
  group_by(Sample) %>%
  mutate(RelAbund = Abundance/sum(Abundance)) %>%
  group_by(OTU) %>%
  summarize(MeanAbund = mean(RelAbund))%>%
  arrange(-MeanAbund)
  
# Optional filter to improve power in testing by
# only testing notus most abundant taxa (10)
notus = 10
da_subset_OTUs = da_analysis.da $significant_taxa[da_analysis.da $significant_taxa %in% d.Abund.OTUs$OTU[1:notus]]
da_subset_p  = da_analysis.da $p[!is.na(da_analysis.da $p)]
da_subset_p = da_subset_p[da_analysis.da $significant_taxa %in% OTUs.abund$OTU[1:notus]]
da_subset_models = da_analysis.da $significant_models[da_analysis.da $significant_taxa %in% OTUs.keep[1:notus]]
# Pull out taxa and p values from only taxa of interest

# Adjust those p values for multiple comparisons
da_subset_padj = p.adjust(da_subset_p, method = "BH", n = length(da_subset_p))

# Choose false discovery rate and filter OTUs above that
FDR=0.05

da_subset_OTUs = da_subset_OTUs[da_subset_padj<FDR]
da_subset_p = da_subset_p[da_subset_padj<FDR]
da_subset_models = da_subset_models[da_subset_padj<FDR]
da_subset_models

# Whith OTUs are significantly differentially abundant with burning?
da_subset_OTUs
otu_to_taxonomy(da_subset_OTUs,data=ps)

# Plotting taxa with significant burn effect
# OTU3 is less abundant in the burned samples
corncob.3 <- bbdml(formula = OTU3 ~ Run+Soil_Type+Burned,
                 phi.formula = ~ Run+Soil_Type+Burned,
                 data = ps)
plot(corncob.3,shape="Burned", color="Pair")
summary(corncob.3)
