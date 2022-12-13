markers <- read.csv('/Users/kalebnegussie/Desktop/RI data/Markers.csv')
chrome <- read.csv('/Users/kalebnegussie/Desktop/RI data/Chromesome.csv')
markers <- rename(markers, 'snpID' = 'ACI.SegHsd')

merge<- merge(markers, chrome, by = 'snpID')
merge <- rename(merge, 'cM' = 'X.y')
strain <- select(merge, c('snpID','chr', 'cM', "BXH3","HXB13","BXH12","HXB10", "HXB17", "HXB1", "HXB3","HXB18",
                          "HXB20","BXH8" , "HXB2", "HXB15","HXB21","HXB22",
                          "BXH6", "BXH13","BXH2","HXB23", "HXB25", "HXB27",
                          "HXB4", "HXB5", "HXB29", "HXB31", "BXH5",
                          "HXB7", "BXH10", "HXB24", "BXH9","BXH11"))
outliers( df)
library(qtl)
#Load in pheno/genotype data
df_og <- read.cross("csvr", ".", "gen.csv", na.strings = 'NA')

df_og <- calc.genoprob(df_og, step=1, error.prob=0.01)
#Single QTL using EM 
out.em<- scanone(df_og)
#Single QTL using Hayley-Knott regression
out.hk <- scanone(df_og, method = 'hk')
phenames(df_og)

K_salt <- scanone(df_og, pheno.col = 'K_Salt', model = 'normal', method="em")
Cl_salt <- scanone(df_og, pheno.col = "Cl_Salt", model = 'normal', method="em")
Na_salt <- scanone(df_og, pheno.col = "Na_Salt", model = 'normal', method="hk")
SB_salt <- scanone(df_og, pheno.col = "SB_Salt", model = 'normal', method="em")
K_water <- scanone(df_og, pheno.col = "K_Water", model = 'normal', method="em")
Cl_water <- scanone(df_og, pheno.col = "Cl_Water", model = 'normal', method="em")
Na_water <- scanone(df_og, pheno.col = "Na_Water", model = 'normal', method="em")
SB_water <- scanone(df_og, pheno.col = "SB_Water", model = 'normal', method="em")

#out.perm <- scanone(df, pheno.col = 1, model = 'normal', method="em",
#               n.perm =100)

thresh1 <- summary(out.perm, alpha = c(0.63, 0.10, 0.05))
thresh1

KW.LOD <- plot(K_water, main = 'Mainscan plot of [K⁺] (Water)')
abline(h= thresh1[1],lty = 'dotted', lwd= 1, col='blue')

KS.LOD <- plot(K_salt, main = 'Mainscan plot of [K⁺] (Salt)')
abline(h= thresh1[1],lty = 'dotted', lwd= 1, col='blue')

ClW.LOD <- plot(Cl_water, main = 'Mainscan plot of [Cl⁻] (Water)')
abline(h= thresh1[1],lty = 'dotted', lwd= 1, col='blue')


ClS.LOD <-plot(Cl_salt, main = 'Mainscan plot of [Cl⁻] (Salt)')
abline(h= thresh1[1],lty = 'dotted', lwd= 1, col='blue')


NaW.LOD <-plot(Na_water, main = 'Mainscan plot of [Na⁺] (Water)')
abline(h= thresh1[1],lty = 'dotted', lwd= 1, col='blue')


NaS.LOD <-plot(Na_salt, main = 'Mainscan plot of [Na⁺] (Salt)')
abline(h= thresh1[1],lty = 'dotted', lwd= 1, col='blue')

SBW.LOD <-plot(SB_water, main = 'Mainscan plot of Skin:Body ratio (Water)')
abline(h= thresh1[1],lty = 'dotted', lwd= 1, col='blue')


SBS.LOD <-plot(SB_salt, main = 'Mainscan plot of Skin:Body ratio (Salt)')
abline(h= thresh1[1],lty = 'dotted', lwd= 1, col='blue')


ClW.LOD
ClS.LOD
NaW.LOD
NaS.LOD
SBS.LOD
SBW.LOD


plotPheno(df_og, pheno.col = 1) 
plotMissing(df_og)

          