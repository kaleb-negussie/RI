df <- read.csv('/Users/kalebnegussie/Desktop/RI data/Full_skin_data.csv')

#Finds log of electrolyte concentrations and adds columns to df 
flog <- function(x) {log10(x)}
df <- df %>% mutate(logK = flog(K...mg.kg. ),
                    logCl = flog(Cl...mg.kg.),
                    logNa = flog(Na...mg.kg.))
unique(df$Strain)

#Builds a data frame for each electrolyte with missing values dropped 
Na.df <- 
  df %>% 
  select(Strain, Diet, Na...mg.kg., logNa) %>% 
  drop_na(Na...mg.kg., logNa)

K.df <- 
  df %>% 
  select(Strain, Diet, K...mg.kg., logK) %>% 
  drop_na(K...mg.kg., logK)

Cl.df <- 
  df %>% 
  select(Strain, Diet, Cl...mg.kg., logCl) %>% 
  drop_na(Cl...mg.kg., logCl)

#Cl Histogram
 Cl.dis <- Cl.df %>% ggplot(aes(Cl...mg.kg.)) + 
   labs(title = 'Cl Concentration',
        x= 'Cl Concentration(mg/Kg)',
        y = 'Frequency')+
   stat_bin(bins = 45, fill = '#fc5b5b', color = '#bf4747')+
   ylim(0, 20) + 
   theme_fivethirtyeight()+
   theme(axis.title = element_text(),
         plot.title = element_text(hjust = 0.2))
   Cl.dis
   
#Na Histogram
  Na.dis <- Na.df %>% ggplot(aes(Na...mg.kg.)) + 
  labs(title = 'Na Concentration
       ',
       x= 'Na Concentration(mg/Kg)',
       y = 'Frequency')+
   stat_bin(bins = 50, fill = '#55c974', color = '#3e9455')+
   ylim(0, 20) + 
   theme_fivethirtyeight()+
   theme(axis.title = element_text(),
         plot.title = element_text(hjust = 0.2))
 Na.dis
 
 #K Histogram
    K.dis <- K.df %>% ggplot(aes(x = K...mg.kg.))+ 
      labs(title = 'K Concentration',
           x= 'K Concentration(mg/Kg)',
           y = 'Frequency')+
    stat_bin(bins = 45, fill = "#FFDB6D", color = "#C4961A")+
    ylim(0, 15) + 
    theme_fivethirtyeight()+
      theme(axis.title = element_text(),
            plot.title = element_text(hjust = 0.2))
  K.dis 

#Pre-normalized electrolyte histograms 
 plot_grid(Cl.dis, Na.dis, K.dis,
           labels = c('Cl', 'Na', 'K'),ncol = 2, nrow = 2)
 

#Log of K concentration Hist
 logK.hist <- df %>% ggplot(aes(x = logK))+ 
   stat_bin(bins = 45, fill = "#FFDB6D", color = "#C4961A")+
   ylim(0, 15) + 
   labs(title = 'Log₁₀ of K Concentration',
        x= 'log₁₀(K Concentration(mg/Kg))',
        y = 'Frequency')+
   theme_fivethirtyeight()+
   theme(axis.title = element_text(),
     plot.title = element_text(hjust = 0.2))
 
#Log of Cl concentration Hist
 logCl.hist <- df %>% ggplot(aes(x = logCl))+ 
   stat_bin(bins = 45, fill = "#fc5b5b", color = "#bf4747")+
   ylim(0, 15) + 
   labs(title = 'Log₁₀ of Cl Concentration',
        x= 'log₁₀(Cl Concentration(mg/Kg))',
        y = 'Frequency')+
   theme_fivethirtyeight()+
   theme(axis.title = element_text(),
         plot.title = element_text(hjust = 0.2))
 
 
 #Log of Na concentration Hist
 logNa.hist <- df %>% ggplot(aes(x = logNa))+ 
   stat_bin(bins = 45, fill = "#55c974", color = "#3e9455")+
   ylim(0, 15) + 
   labs(title = '  Log₁₀ of Na
Concentration',
        x= 'log₁₀(Na Concentration(mg/Kg))',
        y = 'Frequency')+
   theme_fivethirtyeight()+
   theme(axis.title = element_text(),
         plot.title = element_text(hjust = 0.2))
 logNa.hist
 
 plot_grid( K.dis, logK.hist,
            ncol = 2, nrow = 1)
 plot_grid( Na.dis, logNa.hist,
            ncol = 2, nrow = 1)
 plot_grid( Cl.dis, logCl.hist,
            ncol = 2, nrow = 1)
 
 

#One Way Anova 

df <- FindReplace(data = df, Var = Diet, replaceData = Replaces, 
                  from = c('Voda', 'Sůl'), to = c('Water','Salt',),
                  exact = FALSE)

#Creates 2 data frames with each respective diet 
df.salt <- filter(df, Diet == 'Sůl')
df.water <- filter(df, Diet == 'Voda')

#Function that takes in df and col and outputs r.squared grouped by Strain
find.rsqu <- function(Data, Column) {
  (summary(lm(Column ~ Strain, data = Data)))$r.squared} 

#Rsquared K Water 
K.W.rsqu <- find.rsqu(df.water, df.water$logK)
#Rsquared Na Water 
Na.W.rsqu <- find.rsqu(df.water, df.water$logNa)
#Rsquared Cl Water 
Cl.W.rsqu <- find.rsqu(df.water, df.water$logCl)
#Rsquared K Salt 
K.S.rsqu <- find.rsqu(df.salt, df.salt$logK)
#Rsquared Na Water 
Na.S.rsqu <-find.rsqu(df.salt, df.salt$logNa)
#Rsquared Cl Water 
Cl.S.rsqu <-find.rsqu(df.salt, df.salt$logCl)

vec <- c(K.S.rsqu, Na.S.rsqu, Cl.S.rsqu,
              K.W.rsqu,Na.W.rsqu, Cl.W.rsqu)

row_names <- c('K⁺','Na⁺','Cl⁻')
col_names <- c('Salt','Water')
Electrolyte_Rsqu =  array(vec,
             dim = c(3,2,2),
             dimnames = list(row_names, col_names))
Electrolyte_Rsqu
K.W.rsqu
Na.W.rsqu
Cl.W.rsqu
K.S.rsqu
Na.S.rsqu
Cl.S.rsqu

#Adds a column to df with skin to body ratio 
df <- df %>% mutate(sb.ratio = (Body.Weight..g. / Skin.Dry.Weight..g. ))
sb <- data.frame(Strain = df$Strain, Diet= df$Diet, sb.ratio = df$sb.ratio)
#Builds 2 data frames separating sb by diet
sb.water<- filter(sb, Diet == 'Voda')
sb.salt<- filter(sb, Diet == 'Sůl')

sb.W.rsqu<- find.rsqu(sb.water, sb.water$sb.ratio)
sb.S.rsqu <- find.rsqu(sb.salt, sb.salt$sb.ratio)

#Builds a data frame with the average of sb.ratio for each strain and diet
sb.ave <- df %>% group_by(Strain, Diet) %>% summarise(mean(sb.ratio)) 


ed <- df %>% group_by(Diet , Strain) %>%  summarize('Na_mg.kg'= mean(Na...mg.kg.), 
                                                    'Cl_mg.kg'= mean(Cl...mg.kg.),
                                                    'K_mg.kg'=  mean(K...mg.kg.))
#Renames Czech diet names 
ed$Diet <- gsub('Voda','Water',ed$Diet)
ed$Diet <- gsub('Sůl','Salt',ed$Diet)

Na_bar <- ed %>% ggplot(aes(x = Strain, y = Na_mg.kg, fill = Diet)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  scale_fill_manual(values=c('#E69F00','#999999'))+
  ylab('Na CONC(mg/Kg)')+
ggtitle('Mean Na Concentration by Strain')

Cl_bar <- ed %>% ggplot(aes(x = Strain, y = Cl_mg.kg, fill = Diet)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  scale_fill_manual(values=c('#3776db','#999999'))+
  ylab('Cl CONC(mg/Kg)')+
  ggtitle('Mean Cl Concentration by Strain')
Cl_bar

K_bar <- ed %>% ggplot(aes(x = Strain, y = K_mg.kg, fill = Diet)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  scale_fill_manual(values=c('#db3737','#999999'))+
  ylab('K CONC(mg/Kg)')+
  ggtitle('Mean K Concentration by Strain')
K_bar

#Builds 2 data frames sorted by diet with added electrolyte conc - average columns 
edSalt <- filter(ed, Diet == 'Salt')
edSalt <- edSalt %>% mutate( Na_from_mean = Na_mg.kg - mean(ed$Na_mg.kg), 
                  Cl_from_mean = Cl_mg.kg - mean(ed$Cl_mg.kg),
                  K_from_mean = K_mg.kg - mean(ed$K_mg.kg))

edWater <- filter(ed, Diet == 'Water')
edWater <- edWater %>% mutate( Na_from_mean = Na_mg.kg - mean(ed$Na_mg.kg), 
                             Cl_from_mean = Cl_mg.kg - mean(ed$Cl_mg.kg),
                             K_from_mean = K_mg.kg - mean(ed$K_mg.kg))
#Building a data frame sorted by each electrolyte's difference from  mean conc 
#and adds a column  based on if the value is above or below the mean 

#Na Salt and Water bar graphs 
Na.S <- edSalt
Na.S$Na_type <- ifelse(Na.S$Na_from_mean < 0, "below", "above")
Na.S <- Na.S[order(Na.S$Na_from_mean),]
Na.S$Strain <- factor(Na.S$Strain, levels = Na.S$Strain)
Na.S_bar <- Na.S %>% ggplot(aes(x = Na_from_mean, y = Strain))+
  geom_bar(stat = 'identity', aes(fill=Na_type), width=.5) +
  scale_fill_manual(name="Na Conc", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  ylab('Strain')+
  xlab('Na Conc.(mg/Kg) difference from mean')+
  ggtitle('Na Concentraiton Relative to Mean by Strain
                     (Salt Diet)')
Na.S_bar

Na.W <- edWater
Na.W$Na_type <- ifelse(Na.W$Na_from_mean < 0, "below", "above")
Na.W <- Na.W[order(Na.W$Na_from_mean),]
Na.W$Strain <- factor(Na.W$Strain, levels = Na.W$Strain)
Na.W_bar <- Na.W %>% ggplot(aes(x = Na_from_mean, y = Strain))+
  geom_bar(stat = 'identity', aes(fill=Na_type), width=.5) +
  scale_fill_manual(name="Na Conc", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  ylab('Strain')+
  xlab('Na Conc.(mg/Kg) difference from mean')+
  ggtitle('Na Concentraiton Relative to Mean by Strain
                     (Water Diet)')
Na.W_bar

#Cl Salt and Water bar graphs 
Cl.S <- edSalt
Cl.S$Cl_type <- ifelse(Cl.S$Cl_from_mean < 0, "below", "above")
Cl.S <- Cl.S[order(Cl.S$Cl_from_mean),]
Cl.S$Strain <- factor(Cl.S$Strain, levels = Cl.S$Strain)
Cl.S_bar <- Cl.S %>% ggplot(aes(x = Cl_from_mean, y = Strain))+
  geom_bar(stat = 'identity', aes(fill=Cl_type), width=.5) +
  scale_fill_manual(name="Cl Conc", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  ylab('Strain')+
  xlab('Cl Conc.(mg/Kg) difference from mean')+
  ggtitle('Cl Concentraiton Relative to Mean by Strain
                     (Salt Diet)')
Cl.S_bar

Cl.W <- edWater
Cl.W$Cl_type <- ifelse(Cl.W$Cl_from_mean < 0, "below", "above")
Cl.W <- Cl.W[order(Cl.W$Cl_from_mean),]
Cl.W$Strain <- factor(Cl.W$Strain, levels = Cl.W$Strain)
Cl.W_bar <- Cl.W %>% ggplot(aes(x = Cl_from_mean, y = Strain))+
  geom_bar(stat = 'identity', aes(fill=Cl_type), width=.5) +
  scale_fill_manual(name="Cl Conc", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  ylab('Strain')+
  xlab('Cl Conc.(mg/Kg) difference from mean')+
  ggtitle('Cl Concentraiton Relative to Mean by Strain
                     (Water Diet)')
Cl.W_bar

K.S <- edSalt
K.S$K_type <- ifelse(K.S$K_from_mean < 0, "below", "above")
K.S <- K.S[order(K.S$K_from_mean),]
K.S$Strain <- factor(K.S$Strain, levels = K.S$Strain)
K.S_bar <- K.S %>% ggplot(aes(x = K_from_mean, y = Strain))+
  geom_bar(stat = 'identity', aes(fill=K_type), width=.5) +
  scale_fill_manual(name="K Conc", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#6459e3", "below"="#f8766d")) + 
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  ylab('Strain')+
  xlab('K Conc.(mg/Kg) difference from mean')+
  ggtitle('K Concentration Relative to Mean by Strain
                     (Salt Diet)')
K.S_bar

K.W <- edWater
K.W$K_type <- ifelse(K.W$K_from_mean < 0, "below", "above")
K.W <- K.W[order(K.W$K_from_mean),]
K.W$Strain <- factor(K.W$Strain, levels = K.W$Strain)
K.W_bar <- K.W %>% ggplot(aes(x = K_from_mean, y = Strain))+
  geom_bar(stat = 'identity', aes(fill=K_type), width=.5) +
  scale_fill_manual(name="K Conc", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#6459e3", "below"="#f8766d")) + 
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.2))+ 
  ylab('Strain')+
  xlab('K Conc.(mg/Kg) difference from mean')+
  ggtitle('K Concentration Relative to Mean by Strain
                     (Water Diet)')
K.W_bar

plot_grid( K.W_bar, K.S_bar,
           ncol = 2, nrow = 1)
summary(df)


#Could be useful 

merge<- merge(markers, chrome, by = 'snpID')
ave_data<- merge(Cl.ed,Na.ed, by=c('Strain','Diet'))

sb1 <- sb %>% group_by(Strain, Diet) %>% 
  summarise('sb.ratio' = mean(sb.ratio))
merge<-merge(ave_data, sb1, by = c('Strain','Diet'))


