library(ggplot2)

cbPalette = c("#6699CC", "#661100")

# Read in data
pmi = read.delim("", sep = "\t", quote = "")

pmi$PMI = pmi$Longest.PMI.reported
pmi$SampleSize = pmi$Sample.size
pmi$CorrelationPos = pmi$Any.significant.substantial.correlation.of.a.feature.with.PMI.reported.

pmi$PMI = gsub(" hours", "", pmi$PMI)
pmi$PMI = gsub("Not recorded", NA, pmi$PMI)
pmi$PMI = as.numeric(pmi$PMI)

pmi$SampleSize = gsub("Not recorded", NA, pmi$SampleSize)
pmi$SampleSize = as.numeric(pmi$SampleSize)

pmi$CorrelationPos = gsub("Unclear", NA, pmi$CorrelationPos)

pmi = pmi[!is.na(pmi$CorrelationPos), ]

pmi_plot = ggplot(pmi, aes(x=PMI, y = SampleSize, color = CorrelationPos)) +
  geom_point(alpha = 0.33, position = position_dodge(width = 0.3), size = 3) +
    scale_color_manual(values = cbPalette, name = "Any Correlation of a Structural Change with the PMI Found?") +
    ylab("Sample Size") + xlab("Highest Recorded PMI (Hours)") + theme_bw() +
    theme(legend.position="top") +
    scale_x_continuous(trans = "log", breaks = c(8, 20, 50, 150, 400)) +
    scale_y_continuous(trans = "log", breaks = c(5, 20, 100, 400, 1200))

