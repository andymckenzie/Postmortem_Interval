library(ggplot2)

cbPalette = c("#DDCC77", "#CC6677", "#117733", "#332288", "#6699CC", "#AA4499", "#44AA99", "#661100", "#888888", "#999933", "#88CCEE")

# Read in data
pmi = read.delim("", sep = "\t", quote = "")

PMI_hours = vector()
for(i in 1:nrow(pmi)){
  PMI_vec = pmi[i, "Time.Point"]
  split_vec = strsplit(PMI_vec, " ")
  if(PMI_vec == ""){
    PMI_hours[i] = NA
    next
  }
  if(split_vec[[1]][2] == "minutes"){
    PMI_val = as.numeric(split_vec[[1]][1]) / 60
  } else {
    PMI_val = split_vec[[1]][1]
  }
  PMI_hours[i] = PMI_val
}

pmi$PMI_hours = as.numeric(PMI_hours)

cor.test(pmi$PMI_hours, pmi$Consensus, method = "spearman")

pmi$Grade = pmi$Consensus
pmi$Grade = gsub("0", "None/Minimal", pmi$Grade)
pmi$Grade = gsub("1", "Partial", pmi$Grade)
pmi$Grade = gsub("2", "Severe",pmi$Grade)
pmi$Grade = gsub("3", "Total/Near-Total", pmi$Grade)
pmi$Grade = factor(pmi$Grade, levels =
  c("None/Minimal", "Partial", "Severe", "Total/Near-Total"))

pmi = pmi[!is.na(pmi$Grade), ]

pmi$Feature = pmi$Structural.Feature
pmi$Feature = gsub("Capillaries", "Blood vessels", pmi$Feature)
pmi$Feature = gsub("Dendrite ", "Dendrite", pmi$Feature)
pmi$Feature = gsub("Myelin ", "Myelin",pmi$Feature)
pmi$Feature = gsub("Neuropil ", "Neuropil", pmi$Feature)
pmi$Feature = gsub("Synapse ", "Synapse", pmi$Feature)
pmi$Feature = gsub("Synapse count", "Synapse", pmi$Feature)
pmi$Feature = gsub("Synapse counts", "Synapse", pmi$Feature)
pmi$Feature = gsub("Synapses", "Synapse", pmi$Feature)
pmi$Feature = gsub("Synapse morphometry", "Synapse", pmi$Feature)

Features = c("Astrocyte process", "Axon", "Blood vessels", "Dendrite", "Myelin", "Neuropil", "Soma", "Synapse")
pmi$Feature = ifelse(pmi$Feature %in% Features, pmi$Feature, "General cell membrane")

pmi_plot = ggplot(pmi, aes(x=PMI_hours, y= Grade, color = Feature)) +
  geom_point(alpha = 0.3, position = position_dodge(width = 0.5), size = 3) + 
    scale_colour_manual(values = cbPalette, name = "Structural Feature") + 
    ylab("Decomposition Severity Grade") + xlab("Postmortem Interval (Log Scale)") + theme_bw() + scale_x_continuous(trans = "log", breaks = c(0.16666, 2.5, 48, 1080), labels = c("10 min", "2.5 h", "2 d", "45 d")) + theme(axis.text=element_text(size=10), axis.title=element_text(size=11), legend.text=element_text(size=10)) + guides(colour = guide_legend(override.aes = list(alpha = 0.6)))

