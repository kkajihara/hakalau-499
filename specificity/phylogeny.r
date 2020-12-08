library(ape)

# read in the tree
	# using Qian and Jin (2016)'s megaphylogeny
	# https://academic.oup.com/jpe/article/9/2/233/2928108
	# "An updated megaphylogeny of plants, a tool for 
	# generating plant phylogenies and an analysis of 
	# phylogenetic community structure"
	supertree <- ape::read.tree("stuff from Jack/PhytoPhylo.tre")


# look at each one, see species
	tips2keep <- list()

	genus <- "Rubus"
	sort(supertree$tip.label[(grepl(x=supertree$tip.label, pattern=genus))])
	# yes, but not hawaiiensis. but that's ok. is rubus monophyletic?
	indices <- sort(which(grepl(x=supertree$tip.label, pattern=genus)))
	n_indices <- length(indices)
	message("is it monophyletic?")
	(min(indices) + n_indices - 1) == max(indices)
	# we looked at Cliff's paper, "Phylogeny of Rubus Subgenus Idaeobatus 
	# (Rosaceae) and its Implications Toward Colonization of the Hawaiian 
	# Islands" and saw that spectabilis is the closet relative of hawaiiensis
	# https://www.jstor.org/stable/2419819?seq=6#metadata_info_tab_contents
	tips2keep$Rubus <- "Rubus_spectabilis"

	genus <- "Pennisetum"
	sort(supertree$tip.label[(grepl(x=supertree$tip.label, pattern=genus))])
	# yes, but not hawaiiensis. but that's ok. is rubus monophyletic?
	indices <- sort(which(grepl(x=supertree$tip.label, pattern=genus)))
	n_indices <- length(indices)
	message("is it monophyletic?")
	(min(indices) + n_indices - 1) == max(indices)
	# no, it's not, but it's OK bc clandestinum is in there!!!!!
	tips2keep$Pennisetum <- "Pennisetum_clandestinum"

	genus <- "Acacia"
	sort(supertree$tip.label[(grepl(x=supertree$tip.label, pattern=genus))])
	# yes, but not hawaiiensis. but that's ok. is rubus monophyletic?
	indices <- sort(which(grepl(x=supertree$tip.label, pattern=genus)))
	n_indices <- length(indices)
	message("is it monophyletic?")
	(min(indices) + n_indices - 1) == max(indices)
	# no, it's not, but it's OK
	tips2keep$Acacia <- "Acacia_koa"

	genus <- c("Myrsine")
	sort(supertree$tip.label[(grepl(x=supertree$tip.label, pattern=genus))])
	# yes, but not hawaiiensis. but that's ok. is rubus monophyletic?
	indices <- sort(which(grepl(x=supertree$tip.label, pattern=genus)))
	n_indices <- length(indices)
	message("is it monophyletic?")
	(min(indices) + n_indices - 1) == max(indices)
	# no, it's not, but it's OK
	# found closest relative in tree is Myrsine_coriacea	
	# ref = "RADseq resolves the phylogeny of Hawaiian Myrsine (Primulaceae)
	# and provides evidence for hybridization"
	# https://onlinelibrary.wiley.com/doi/full/10.1111/jse.12668
	tips2keep$Myrsine <- "Myrsine_coriacea"


	genus <- c("Metrosideros")
	sort(supertree$tip.label[(grepl(x=supertree$tip.label, pattern=genus))])
	# yes, but not hawaiiensis. but that's ok. is rubus monophyletic?
	indices <- sort(which(grepl(x=supertree$tip.label, pattern=genus)))
	n_indices <- length(indices)
	message("is it monophyletic?")
	(min(indices) + n_indices - 1) == max(indices)
	tips2keep$Metrosideros <- "Metrosideros_polymorpha"

	genus <- "Cheirodendron"
	sort(supertree$tip.label[(grepl(x=supertree$tip.label, pattern=genus))])
	# yes, but not hawaiiensis. but that's ok. is rubus monophyletic?
	indices <- sort(which(grepl(x=supertree$tip.label, pattern=genus)))
	n_indices <- length(indices)
	message("is it monophyletic?")
	(min(indices) + n_indices - 1) == max(indices)
	tips2keep$Cheirodendron <- "Cheirodendron_trigynum"


# subset supertree to only contain the tips we want to keep, then rename them by genus
	supertree_kacie <- ape::keep.tip(phy=supertree, tip=unlist(tips2keep))

	supertree_kacie$tip.label[supertree_kacie$tip.label == "Rubus_spectabilis"] <- "Rubus hawaiensis"
	supertree_kacie$tip.label[supertree_kacie$tip.label == "Myrsine_coriacea"] <- "Myrsine lessertiana"

	supertree_hawaiian <- supertree_kacie
	supertree_hawaiian$tip.label[supertree_hawaiian$tip.label == "Rubus hawaiensis"] <- "ʻĀkala (Rubus hawaiensis)"
	supertree_hawaiian$tip.label[supertree_hawaiian$tip.label == "Myrsine lessertiana"] <- "Kōlea (Myrsine lessertiana)"
	supertree_hawaiian$tip.label[supertree_hawaiian$tip.label == "Acacia_koa"] <- "Koa (Acacia koa)"
	supertree_hawaiian$tip.label[supertree_hawaiian$tip.label == "Metrosideros_polymorpha"] <- "ʻŌhiʻa (Metrosideros polymorpha)"
	supertree_hawaiian$tip.label[supertree_hawaiian$tip.label == "Pennisetum_clandestinum"] <- "Mauʻu (Pennisetum clandestinum)"
	supertree_hawaiian$tip.label[supertree_hawaiian$tip.label == "Cheirodendron_trigynum"] <- "ʻŌlapa (Cheirodendron trigynum)"


	cairo_pdf("kacie_plant_phylogeny.pdf")
	plot(supertree_hawaiian, label.offset=10)
	dev.off()

# get tree ready for Kacie's data
	supertree_kacie$tip.label <- sub(pattern="_", replace=" ", x=supertree_kacie$tip.label)
	supertree_kacie$tip.label[supertree_kacie$tip.label == "Pennisetum clandestinum"] <- "Grass"

# save tree for kacie to use
	write.tree(supertree_kacie, file="stuff from Jack/supertree_kacie.tre")
	