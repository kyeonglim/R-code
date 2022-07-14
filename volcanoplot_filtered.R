setwd('C:/lee/MS')
getwd()
df<- read.csv('volcano_filtered.csv', stringsAsFactors = F, header = T)

#load 'ggplot2' packages for plotting
library(ggplot2)

#scatter plot with log2(FC) as x axis and -log10(pvalue) as y axis
plot1 <- ggplot(data=df, aes(x=log2(FC), y=-log10(pvalue))) + geom_point() + theme_minimal()

#add a threshold line with pvalue<0.05 and |foldchnage| >2
plot2 <- plot1 + geom_vline(xintercept=c(-2, 2), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

# In 'df' dataframe, add a new column named 'DEP' and fill the blanks with 'NONE'
df$DEP <- "NONE"

# if FC > 2 and pvalue < 0.05, set as "UP" 
df$DEP[df$FC > 2 & df$pvalue < 0.05] <- "UP"

# if FC < 1/2 and pvalue < 0.05, set as "DOWN"
df$DEP[df$FC < 1/2 & df$pvalue < 0.05] <- "DOWN"

# Volcano plot with customized colors and threshold.
## color seeting
color_setting <- c("blue", "red", "gray")
names(color_setting) <- c("DOWN", "UP", "NONE")
## plot
ggplot(data=df, aes(x=log2(FC), y=-log10(pvalue), col=DEP))+
  geom_point() +
  theme_minimal() +
  geom_hline(yintercept=-log10(0.05), col="red") +
  geom_vline(xintercept=c(-log2(2), log2(2)), col="red") +
  scale_colour_manual(values = color_setting)

#Lable dots with protein nmaes
#create a new column named 'labeling' and fill the blanks with 'NA'
df$labeling <- NA

#Fill 'labeling' columns with the same value of 'GENE_NAME' column if there is not 'NONE' in 'DEP' columns.
df$labeling[df$DEP != "NONE"] <- df$GENE_NAME[df$DEP != "NONE"]

#load 'ggrepel' packages for non-overlapped labeling
library(ggrepel)

#plot
ggplot(data=df, aes(x=log2(FC), y=-log10(pvalue), col=DEP, label=labeling))+
  geom_point() +
  theme_minimal() +
  geom_hline(yintercept=-log10(0.05), col="red") +
  geom_vline(xintercept=c(-log2(2), log2(2)), col="red") +
  scale_colour_manual(values = color_setting) +
  geom_text_repel()