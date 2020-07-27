library(openxlsx)

cell_phases = c("Interphase", "Prophase", "Prometaphase", "Metaphase", "Anaphase", "Telophase")

out_dir = "./out/"
dir.create(out_dir, showWarnings=FALSE)

xlsx_files = list.files(path="./input/", pattern="*.xlsx", full.names=TRUE)


# ---------------------------
# Cleanup
# ---------------------------

cat("Loading files...\n")
pb = txtProgressBar(0, length(xlsx_files), style=3)
for (i in 1:length(xlsx_files)){
	f = xlsx_files[i]

	# Some students put their charts in Sheets 1-2
	if (i == 73 || i == 81){
		data = read.xlsx(f, rowNames=FALSE, colNames=TRUE, cols=1:8, rows=1:51, sheet=3)
	} else {
		data = read.xlsx(f, rowNames=FALSE, colNames=TRUE, cols=1:8, rows=1:51)
	}

	# Someone included "sums" at the bottom
	if (i == 13){
		data = data[1:49,]
	}

	# Lexicographical ordering
	if (i < 10){
		s = paste('0', i, sep='')

	} else {
		s = i
	}

	fname = paste(out_dir, "BIOL12_LabModule-3_Part-2_Student-", s, ".csv", sep='')
	write.csv(data, file=fname, quote=FALSE)
	setTxtProgressBar(pb, i)
}
cat('\n')
