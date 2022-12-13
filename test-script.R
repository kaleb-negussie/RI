df <- Skin.Data
#Histogram 
hist(df$Cl...mg.kg.)

df %>% ggplot(aes(Body.Weight..g.,Skin.Dry.Weight..g.))+ 
      geom_point(aes(col = Strain))
sd <- df %>% group_by(Strain, Diet) %>%  summarize(mean(Na...mg.kg.))
sd <- rename(sd, 'Na' = 'mean(Na...mg.kg.)')
sd_plot <- sd %>% ggplot(aes(x = Diet, y = Na)) +
  geom_bar(stat = "identity")+
  ylab('Skin Na+ (mg/Kg)')
sd_plot

ed <- df %>% group_by(Diet , Strain) %>%  summarize(mean(Na...mg.kg.), 
                                             mean(Cl...mg.kg.),
                                             mean(K...mg.kg.))
ed$X <- c(1:12, 1:12)
ed_plot_data <- data.frame(Diet = ed$Diet, 
                           X = ed$X,
                           Electrolytes = c(ed$`mean(Na...mg.kg.)`,
                                            ed$`mean(Cl...mg.kg.)`,
                                            ed$`mean(K...mg.kg.)`),
                           Group = c(rep('Na_salt', 12),
                                     rep('Na_water',12),
                                     rep('Cl_salt',12),
                                     rep('Cl_water',12),
                                     rep('K_salt',12),
                                     rep('K_water',12)))
ggplot(ed_plot_data, aes( X,  Electrolytes, col= Group)) +
  geom_boxplot() + 
  ylab("Electrolyte Concentration (mg/Kg)")+
  xlab("Trial Group") + 
  ggtitle("Electrolyte Concentration by Diet")

r(QTL)
