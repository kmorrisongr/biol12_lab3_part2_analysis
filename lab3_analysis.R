# ---------------------------
# ANALYSIS OF BIOL12_LABMODULE-3_PART-2 ASSIGNMENT SUBMISSIONS
# Questions:
#	1. For each image, what are the average counts (and their standard deviations) for each cell cycle?
#		a. Which images did students vary on the most?
#		b. Include the results from the answer key
#	2. What are the overall average counts (and their standard deviations) for each cell cycle?
#		a. Include the results from the answer key
#	3. How much do students differ from the answer key for each cell cycle?
# ---------------------------


# ---------------------------
# Useful functions
# ---------------------------

clean_image_labels = function(data){
	for (i in 1:length(data$Image.Identity)){
		img_num = as.numeric(strsplit(data$Image.Identity[i], ' ')[[1]][2])

		# Make levels in order
		if (img_num < 10){
			img_num = paste('0', img_num, sep='')
		}

		# Address inconsistencies
		data$Image.Identity[i] = paste("Image ", img_num, sep='')
	}

	return(data)
}

plot_data = function(data, answer_key, results_dir, pdf_name, dowrap){
	g = ggplot(data, aes(x=x, y=count, color=phase, shape=phase)) +
		geom_point(data, mapping=aes(x=x, y=count, color=phase, shape=phase)) +
		geom_line(data, mapping=aes(x=x, y=count, color=phase)) +
		geom_ribbon(aes(x=x, ymin=count-stdev, ymax=count+stdev, alpha=0.6, color=phase),
			    inherit.aes=FALSE) +
		ggtitle("Average Cell Cycle Phase Counts for All Images") +
		xlab("Image #") +
		ylab("Average Counts") +
		guides(alpha="none") +
		# These colors are weird because I haven't figured out ggplot totally...
		geom_point(answer_key, mapping=aes(x=x, y=count, color="Answer Key", shape=phase)) +
		geom_line(answer_key, mapping=aes(x=x, y=count, color="Answer Key"))

	if (dowrap){
		g = g + facet_wrap( ~ phase, ncol=2)
	}

	pdf_name = paste(results_dir, pdf_name, sep='')
	suppressMessages(ggsave(pdf_name, plot=g))
}

plot_resmags = function(data, results_dir, pdf_name){
	pdf_name = paste(results_dir, pdf_name, sep='')

	g = ggplot(data, aes(x=Image, y=count, fill=phase)) +
		geom_bar(stat="identity", color="black", position=position_dodge()) +
		facet_wrap( ~ phase, ncol=2) +
		geom_errorbar(aes(ymin=count-stdev, ymax=count+stdev, width=0.2), position=position_dodge(0.9)) +
		ggtitle("Average Number Off from Answer Key") +
		xlab("Image Number") +
		ylab("Counts") +
		guides(fill=FALSE)

	suppressMessages(ggsave(pdf_name, plot=g))
}


# ---------------------------
# Setup
# ---------------------------

library(openxlsx)
library(ggplot2)
library(reshape)

cell_phases = c("Interphase", "Prophase", "Prometaphase", "Metaphase", "Anaphase", "Telophase")

results_dir = "./results/"
dir.create(results_dir, showWarnings=FALSE)

csv_files = list.files(path="./input/students/", pattern="*.csv", full.names=TRUE)
answer_key = read.xlsx("./input/answer/answer_key.xlsx", rowNames=FALSE, colNames=TRUE, cols=1:8, rows=1:51)

answer_key = clean_image_labels(answer_key)
colnames(answer_key) = c("Image", cell_phases, "Num-Dividing")
answer_key = answer_key[,-which(colnames(answer_key) == "Num-Dividing")]
answer_key[,"Image"] = as.factor(1:50)
answer_key[is.na(answer_key)] = 0

answer_key_long = melt(answer_key, id.vars=c("Image"))
colnames(answer_key_long) = c("x", "phase", "count")
answer_key_long[,"x"] = as.numeric(as.character(answer_key_long[,"x"]))

# Regenerate many plots, or just messing around with cumulative plots?
fresh_run = TRUE


# ---------------------------
# Cleanup, aggregation
# ---------------------------

aggregated = data.frame()

cat("Loading files...\n")
pb = txtProgressBar(0, length(csv_files), style=3)
for (i in 1:length(csv_files)){
	f = csv_files[i]
	data = read.csv(f, header=TRUE)

	# Comes with an extra column
	data = data[,-1]

	data$student = i
	data = clean_image_labels(data)

	aggregated = rbind(aggregated, data)

	setTxtProgressBar(pb, i)
}
cat('\n')

aggregated[is.na(aggregated)] = 0
colnames(aggregated) = c("Image", cell_phases, "Num-Dividing", "Student")
aggregated = aggregated[,-which(colnames(aggregated) == "Num-Dividing")]

for (i in c(1,8)){
	aggregated[,i] = as.factor(aggregated[,i])
}


# ---------------------------
# Analysis
# ---------------------------

# ---------------------------
# 1. Average cell cycle counts for each image

if (fresh_run){
	many_dir = paste(results_dir, "all_images/", sep='')
	dir.create(many_dir, showWarnings=FALSE)

	aggregated_avg = data.frame()

	#print(levels(aggregated$Image))

	cat("Generating plots for each image...\n")
	pb = txtProgressBar(0, length(levels(aggregated[,"Image"])), style=3)
	for (i in 1:length(levels(aggregated[,"Image"]))){
		img = levels(aggregated[,"Image"])[i]
		pdf_name1 = paste(many_dir, gsub(' ', '-', img), ".pdf", sep='')
		pdf_name2 = paste(many_dir, gsub(' ', '-', img), "_mito.pdf", sep='')

		data = aggregated[which(aggregated[,"Image"] == img),]

		avg_data = as.data.frame(matrix(NA, nrow=length(cell_phases), ncol=4))
		colnames(avg_data) = c("x", "count", "stdev", "phase")
		avg_data[,"x"] = 1:nrow(avg_data)

		for (j in 1:length(cell_phases)){
			cp = cell_phases[j]

			counts = data[,cp]
			# Some weirdness in Image 36 for one student
			counts[counts == '`'] = '0'
			counts = as.numeric(counts)

			avg = mean(counts)
			stdev = sd(counts)
			avg_data[j,2:4] = c(avg, stdev, cp)
		}
		for (j in 1:3){
			avg_data[,j] = as.numeric(avg_data[,j])
		}

		avg_data[,"phase"] = factor(as.character(avg_data[,"phase"]), levels=cell_phases)

		g = ggplot(avg_data, aes(x=phase, y=count, fill=phase)) +
			geom_bar(stat="identity", color="black", position=position_dodge()) +
			geom_errorbar(aes(ymin=count-stdev, ymax=count+stdev, width=0.2), position=position_dodge(0.9)) +
			ggtitle(paste("Average Cell Cycle Phase Counts for Image #", i, sep='')) +
			xlab("Cell Phase") +
			ylab("Average Counts") +
			guides(fill=FALSE)

		suppressMessages(ggsave(pdf_name1, plot=g))

		avg_data_mito = avg_data[avg_data[,"phase"] != "Interphase",]

		g = ggplot(avg_data_mito, aes(x=phase, y=count, fill=phase)) +
			geom_bar(stat="identity", color="black", position=position_dodge()) +
			geom_errorbar(aes(ymin=count-stdev, ymax=count+stdev, width=0.2), position=position_dodge(0.9)) +
			ggtitle(paste("Average Mitotic Phase Counts for Image #", i, sep='')) +
			xlab("Stage in Mitosis") +
			ylab("Average Counts") +
			guides(fill=FALSE)

		suppressMessages(ggsave(pdf_name2, plot=g))

		# Will plot by image next
		avg_data[,"x"] = i
		aggregated_avg = rbind(aggregated_avg, avg_data)

		setTxtProgressBar(pb, i)
	}
	cat('\n')

	save(aggregated_avg, file=paste(results_dir, "aggregated_avg_binary", sep=''))

} else {
	load(paste(results_dir, "aggregated_avg_binary", sep=''))
}


# ---------------------------
# 2. Overall results

colnames(aggregated_avg) = c("x", "count", "stdev", "phase")
for (j in 1:3){
	aggregated_avg[,j] = as.numeric(aggregated_avg[,j])
}

# Everyone
aggregated_avg[,"phase"] = factor(as.character(aggregated_avg[,"phase"]), levels=cell_phases)
plot_data(aggregated_avg, answer_key_long, results_dir, "avg_vs_image.pdf", FALSE) 

# Just mitosis
aggregated_avg_mito = aggregated_avg[aggregated_avg[,"phase"] != "Interphase",]
answer_key_long_mito = answer_key_long[answer_key_long[,"phase"] != "Interphase",]
plot_data(aggregated_avg_mito, answer_key_long_mito, results_dir, "avg_vs_image_mito.pdf", TRUE) 


# ---------------------------
# 3. Difference from answer key

resmags = data.frame()

colnames(answer_key) = c("Image", cell_phases)
colnames(aggregated) = c("Image", cell_phases, "Student")

answer_key[,"Image"] = as.character(answer_key[,"Image"])
answer_key[,"Image"][1:9] = c(paste('0', 1:9, sep=''))
answer_key[,"Image"] = as.factor(answer_key[,"Image"])

for (i in 1:length(levels(aggregated[,"Student"]))){
	s = levels(aggregated[,"Student"])[i]

	student_data = aggregated[which(aggregated[,"Student"] == s),]

	answer_counts = answer_key[,2:ncol(answer_key)]
	student_counts = student_data[,2:(ncol(student_data)-1)]

	for (j in 1:ncol(answer_counts)){
		# Fix the weirdness
		student_counts[student_counts == '`'] = '0'

		answer_counts[,j] = as.numeric(answer_counts[,j])
		student_counts[,j] = as.numeric(student_counts[,j])
	}

	# Some students goofed
	if (nrow(answer_counts) > nrow(student_counts)){
		answer_counts = answer_counts[1:nrow(student_counts),]

	} else if (nrow(answer_counts) < nrow(student_counts)){
		student_counts = student_counts[1:nrow(answer_counts),]
	}

	student_resmag = cbind(Image=student_data[,"Image"],
			       abs(answer_counts - student_counts),
			       Student=student_data[,"Student"])

	resmags = rbind(resmags, student_resmag)
}

final_resmags = data.frame()

# Average resmags across students
for (i in 1:length(levels(resmags[,"Image"]))){
	img = levels(resmags[,"Image"])[i]
	img_resmags = resmags[resmags[,"Image"] == img,]

	averages = vector("numeric", length(cell_phases))
	stdevs = vector("numeric", length(cell_phases))
	for (j in 1:length(cell_phases)){
		cp = cell_phases[j]
		averages[j] = mean(img_resmags[,cp])
		stdevs[j] = sd(img_resmags[,cp])
	}

	to_bind = data.frame(Image=rep(i, length(cell_phases)), count=averages, stdev=stdevs, phase=cell_phases)

	final_resmags = rbind(final_resmags, to_bind)
}

final_resmags[,"phase"] = factor(final_resmags[,"phase"], levels=cell_phases)
plot_resmags(final_resmags, results_dir, "avg_resmags_vs_image.pdf")

final_resmags_mito = final_resmags[final_resmags[,"phase"] != "Interphase",]
plot_resmags(final_resmags_mito, results_dir, "avg_resmags_vs_image_mito.pdf")

for (cp in cell_phases){
	this_phase = final_resmags[,"phase"] == cp
	worst = max(final_resmags[this_phase,"count"])
	worst_img = final_resmags[this_phase,"Image"][which(final_resmags[this_phase,"count"] == worst)]
	cat("Cell Phase:", cp, '|', "Largest resmag:", worst, '|', "Image #:", worst_img, '\n',
	    "===================", '\n')
}
