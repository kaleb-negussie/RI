library(DESeq2)
library(ggplot2)
library(devtools)
install_github("vqv/ggbiplot")
count_table <- read.csv("/Users/kalebnegussie/Downloads/quantitation/count_table")
write.csv(count_table,"/Users/kalebnegussie/Desktop/RI data/count_table")

rownames(count_table) <- count_table[,1]
count_table <- count_table[,-1]

treatment_table2<- matrix(c(rep('treated', 30), rep('untreated',30)))
rownames(treatment_table2) <- colnames(count_table) 
colnames(treatment_table2) <- c('condition')

dds<- DESeqDataSetFromMatrix(countData = round(count_table),
                             colData = treatment_table,
                             design = ~ condition)
plot(hclust(as.dist(1-cor(count_table, method="spearman"))))

count_table
log_dds <- vst(dds)
count_table2<- count_table[,-1] 
quant.pca <- prcomp(t(count_table2), center = TRUE, scale. = TRUE)

autoplot(quant.pca, data = sample_info, colour = 'group')

dds<-DESeq(dds)
resLFC <- lfcShrink(dds, coef="condition_untreated_vs_treated", type="apeglm")

count <- read.csv("/Users/kalebnegussie/Desktop/RI data/count_table")
