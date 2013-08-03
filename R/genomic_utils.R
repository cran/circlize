# == title
# Read cytoband data
#
# == param
# -file path of the uncompressed cytoband file
#
# == details
# The function read the cytoband data, sort the chromosome names and calculate the length of each chromosome.
#
# == values
#
# -df Original data frame for cytoband data
# -chromosome sorted chromosome names
# -chr.len length of chromosomes. Order are same as ``chromosome``
read.cytoband = function(file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep="")) {
	d = read.table(file, colClasses = c("character", "numeric", "numeric", "character", "character"))
	
	chromosome = unique(d[[1]])
	chromosome.ind = gsub("chr", "", chromosome)
	chromosome.num = grep("^\\d+$", chromosome.ind, value = TRUE)
	chromosome.letter = chromosome.ind[!grepl("^\\d+$", chromosome.ind)]
	chromosome.num = sort(as.numeric(chromosome.num))
	chromosome.letter = sort(chromosome.letter)
	chromosome.num = paste("chr", chromosome.num, sep = "")
	chromosome.letter = paste("chr", chromosome.letter, sep = "")

	chromosome = c(chromosome.num, chromosome.letter)
	
	chr.len = NULL
	for(chr in chromosome) {
		d2 = d[d[[1]] == chr, ]
		chr.len = c(chr.len, max(d2[, 3]))
	}
	
	return(list(df = d, chromosome = chromosome, chr.len = chr.len))
}


# == title
# Assign colors to cytogenetic band according to the Giemsa stain results
#
# == param
# -x a vector containing the Giemsa stain results
#
# == details
# The color theme is from http://circos.ca/tutorials/course/slides/session-2.pdf, page 42.
cytoband.col = function(x) {
	x = as.vector(x)
	col.panel = c("gpos100" = rgb(0, 0, 0, maxColorValue = 255), 
                  "gpos"    = rgb(0, 0, 0, maxColorValue = 255),
                  "gpos75"  = rgb(130, 130, 130, maxColorValue = 255),
                  "gpos66"  = rgb(160, 160, 160, maxColorValue = 255),
                  "gpos50"  = rgb(200, 200, 200, maxColorValue = 255),
                  "gpos33"  = rgb(210, 210, 210, maxColorValue = 255),
                  "gpos25"  = rgb(200, 200, 200, maxColorValue = 255),
                  "gvar"    = rgb(220, 220, 220, maxColorValue = 255),
                  "gneg"    = rgb(255, 255, 255, maxColorValue = 255),
                  "acen"    = rgb(217, 47, 39, maxColorValue = 255),
                  "stalk"   = rgb(100, 127, 164, maxColorValue = 255) )
    col = col.panel[x]
    col[is.na(col)] = "#FFFFFF"
    return(col)
}