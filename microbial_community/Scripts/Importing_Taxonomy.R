library(phyloseq)
# Load library

TaxTab = read.table("taxonomy.tsv",sep="\t", fill=TRUE)
# Gets the taxonomy table separated into the name+size, taxonomy, and two scores.
head(TaxTab)

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
