library(specificity)
library(ape)


# read in hakalau data
  # has 2075 rows (ASVs), 1221 columns (samples - AK1AC1.r, etc.)
	full_otutable <- read.delim("specificity/hakalau_otu_table.csv", sep=",", header=TRUE, row.names=1)
	# has 1323 rows (samples - AK1AC1.r, etc.) and 33 columns (habitat type, etc.)
	metadata <- read.delim("specificity/haka-metadata.tsv", sep="\t", header=TRUE, stringsAsFactors=FALSE)
	# fix rubus 'hawaiensis' spelling so tree tip label is found in metadata
	metadata$host <- sub(pattern = "Rubus hawaiiensis", replacement = "Rubus hawaiensis", 
	                     x =metadata$host)
	
	haka_supertree<- ape::read.tree("specificity/supertree_kacie.tre")
	# get rid of underscores in tree
	haka_supertree$tip.label <- sub(x=haka_supertree$tip.label, pattern="_", replacement=" ")

# filter metadata to only include hosts we want
	# pared down to 1152 samples
	spec_metadata <- metadata[metadata$host %in% haka_supertree$tip.label, ]
	# pared down to 1108 samples
	spec_metadata <- spec_metadata[spec_metadata$sampleid %in% colnames(full_otutable), ]

# filter otutable to match metadata and transpose it so species (ASVs) are on top
	full_otutable <- full_otutable[ , colnames(full_otutable) %in% spec_metadata$sampleid]
	full_otutable <- t(full_otutable)

# make sure otutable and metadata are sorted the same way
	full_otutable <- full_otutable[order(rownames(full_otutable)), ]
	spec_metadata <- spec_metadata[order(spec_metadata$sampleid), ]
	# check:
	all(rownames(full_otutable) == spec_metadata$sampleid)

# prepare otutable for specificity
	
	# get rid of empty samples, so as to not create NAs when prop_abund is run 
	full_otutable <- full_otutable[rowSums(full_otutable) > 0, ]
	# transform to proportional abundance
	otutable_occ <- prop_abund(full_otutable)
	# get rid of super rare otus for which spec can't be calculated
	# brings ASV count down from 2075 to 240
	otutable_occ <- occ_threshold(otutable_occ, 10)

# actually run specificity
	host_spec <- phy_or_env_spec(otutable_occ, hosts=spec_metadata$host, 
		hosts_phylo=haka_supertree, n_sim=100, n_cores=1, p_method="gamma_fit")

	spec_metadata$genus <- sub(x=spec_metadata$host, pattern=" .*", replace="")

# make plots of highly specific OTUs
	host_spec <- host_spec[order(host_spec$Spec), ]
	head(host_spec)


	x <- factor(spec_metadata$genus)
	library(ggplot2)

	# get top 10 otus with strongest specificity
	otus2plot <- rownames(spec_results)[1:10]
	otu <- "ASV542"

	pdf("spec_violin_plots.pdf")
	for(otu in otus2plot){
	  # keeps the single column for the OTU of interest
		y <-  otutable_occ[,colnames(otutable_occ) == otu]
		plotdf <- data.frame(
		  # keep samples with prop_abund values > 0
			propabund = y[y > 0],
			# keep hosts where prop_abund values > 0 ?
			host = x[y > 0]
		)
		p <- ggplot(plotdf, aes(x=host, y=propabund)) +  
		  geom_violin(scale="count", fill="lightblue") + 
		  # shows all hosts, not just those with violins
			scale_x_discrete(drop=FALSE) + 
			ggtitle(paste0(otu, 
			               "; P=", round(spec_results[otu, 'Pval'], 3), 
			               "; Spec=", round(spec_results[otu, 'Spec'], 3))) + 
			ylab("nonzero proportional abundance") + theme_classic()
		print(p)
	}
	
	dev.off()
	
	
# get taxonomy info for ASvs with spec values
	# read in table of ASVs and spec values
	spec_results <- read.delim("specificity/host_specs.txt", row.names = 1)
  
	# make df copy of full taxonomy table
	spec_taxonomy <- as.data.frame(tax_table(unrarefied_physeq))
	# keep only those rows for ASVs existing in spec_results
	spec_taxonomy <- spec_taxonomy[
	  row.names(spec_taxonomy) %in% row.names(spec_results),]

	# append taxonomy info to spec_results
	spec_results <- merge(spec_results, spec_taxonomy, by="row.names")
	# order spec values (low spec value means high specificity)
	spec_results <- spec_results[order(spec_results$Spec),]
	# change ASVs back into row names
	spec_results <- spec_results %>% remove_rownames() %>% column_to_rownames("Row.names")
	
	# export
	write.csv(spec_results, "outputs/specificity_taxonomy.csv")
	
