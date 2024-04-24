library(readxl)
library(readr)
library(dplyr)
library(frontier)
library(stargazer)
library(reshape2)
library(sandwich)
library(car)
library(ggplot2)
library(tidyr)
library(spatialreg)
library(sp)
library(spdep)
library(spgwr)
library(psych)
library(janitor)
library(plm)
library(ggpubr)
library(frontier)
library(DescTools)
library(plm)

cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC0")))
  return(rob)}
data <- read_excel("Фирмы.xlsx")
data$`2018_L` <- as.numeric(as.character(data$`2018_L`))
data$compactness <- data$City_area/(pi*(data$City_radius)^2)
data$compactness_2012 <- data$City_area_2012/(pi*(data$City_radius_2012)^2)
data$Density_2020 <- data$City_pop_2020/data$City_area
data$Density_2012 <- data$City_pop_2012/data$City_area_2012
#####
stage1 <- lm(log(City_area) ~ log(City_pop_2020+0.001) + log(Density_2012+0.001), data = data)
data$Area_pred <- exp(predict(stage1, na.action = na.pass))
predictions <- exp(predict(stage1, newdata = subset(data, is.na(City_area)!=T), na.action = na.pass))
Area_pred <- rep(NA, nrow(data))
Area_pred[!is.na(data$City_area)] <- predictions
data$Area_pred <- Area_pred
summary(stage1)
stargazer(stage1, digits = 2, out = 'stage1.doc', type = 'html')
data$radius_pred <- sqrt(data$Area_pred/pi)
data$compactness_pred <- data$Area_pred/(pi*(data$City_radius)^2)
################
# Инструменты
################
#####
data$Area_growth <- data$City_area/data$City_area_2012
data$Pop_growth <-  data$City_pop_2020/data$City_pop_2012
data[is.infinite(data$Pop_growth),]$Pop_growth <- NA
stage1 <- lm(S ~ City_pop*Industry + City_area*Industry + Density*Industry+Area_growth*Industry+Pop_growth*Industry, data = data)
summary(stage1)
vif(stage1)
predictions <- predict(stage1, newdata = subset(data, is.na(S)!=T), na.action = na.pass)
S_pred <- rep(NA, nrow(data))
S_pred[!is.na(data$S)] <- predictions
data$S_pred <- S_pred
cor(data$S, data$S_pred, use = 'complete')
######
long_data <- pivot_longer(data, cols = starts_with("20"), names_to = c('Year', ".value"), names_pattern = '(\\d+)_(.*)')
long_data$Year <- as.numeric(as.character(long_data$Year))
long_data$Y <-  long_data$VAT/0.2+0.01
long_data$City_age <- long_data$Year-long_data$Year_founded
long_data$Wage <- long_data$W/long_data$L 
summary(subset(long_data, Year == 2022)[c('Age', 'Liquidation_date', 'S', 'North', 'Mono', 'Soviet', 'Caucasus',
                                          'Density', 'L', 'K', 'Reserves', 'Wage', 'City_age')])
desc_tab <- describe(subset(long_data, Year == 2022)[c('Age', 'Liquidation_date',
                                                       'S', 'North', 'Mono', 'Soviet',
                                                       'Caucasus','Density', 'L', 'K',
                                                       'Reserves', 'Wage', 'City_age')], skew = F)
stargazer(desc_tab, summary = F, type = 'html', out = 'desctab.doc')

#df <- long_data%>%filter(is.na(Y)!=TRUE)%>%filter(is.na(L)!=TRUE)%>%filter(is.na(K)!=TRUE)%>%filter(is.na(Reserves)!=TRUE)%>%filter(Year==2022)
################
# Фронтир
################
summary(df)
f1 <- sfa(log(Y) ~ log(K)+log(L)|log(Reserves)+Caucasus, data = subset(df, Industry == 'Продукты'))
v1 <- lm(log(Y) ~ log(K)+log(L), data = subset(df, Industry == 'Продукты'))
summary(f1)

long_data$Age2 <- long_data$Age*long_data$Age
long_data$effi <- NA
stargazer(v1, type = 'html', out = 'C:/Users/User/Desktop/Диссер/tfp_1.doc')

data2 <- data.frame(matrix(NA, nrow = 1463165, ncol = ncol(long_data)))
ggplot(data) + geom_density(aes(x = effi)) + xlab('Эффективность')
pb = txtProgressBar(min = 0, max = length(unique(data$Industry)), initial = 1)
row = 1
i=1
frontiers <- list()
summary(long_data)
for (ind in unique(long_data$Industry)) {
  for (yr in c(2018:2022)) {
    setTxtProgressBar(pb,i)
    subdf <- long_data%>%filter(Industry==ind)%>%filter(Year==yr)%>%
      filter(is.na(Y)!=TRUE)%>%filter(is.na(L)!=TRUE)%>%
      filter(K>0)%>%filter(Reserves>0)
    front <- sfa(log(Y) ~ log(L)+log(K)|Caucasus+log(Reserves), data = subdf)
    frontiers[[i]] <- front
    frontiers[[i]]$ind <- ind
    subdf$effi <- efficiencies(front)
    data2[row:(nrow(subdf)+row-1),] <- subdf
    row = nrow(subdf)+row+1
    i=i+1
    close(pb)
  }
}
data2 <- remove_empty(data2, which = 'rows') 
colnames(data2) <- colnames(long_data)
frontiers[[50]]$ind
summary(frontiers[[50]])
data2 <- subset(data2, Industry!='Парикмахерская')

summary(data2$effi)
data2 <- data2%>%filter(is.na(L)!=TRUE)
data2$Age2 <- data2$Age*data2$Age
data2$S2 <- data2$S*data2$S

################
# Графики
################

ggplot(data2) + geom_point(aes(x=log(K/L), y = log(Y)))

effi_city <- data2%>%filter(Year==2022)%>%filter(Industry == 'Металл')%>%group_by(City)%>%summarise_all(mean, na.rm = T)

ggplot(effi_city) + geom_histogram(aes(x = effi), fill = 'white', col = 'black') + xlab('Средняя эффективность') + ylab('Количество городов')#+
  41ggsave("Efficiency.png", width = 20, height = 9, dpi = 600, device = png, type = "cairo")

for (ind in unique(data2$Industry)) {
  png_name <- paste('Efficiency_', ind, '.png', sep = '')
  effi_city <- data2%>%filter(Year==2022)%>%filter(Industry == ind)%>%group_by(City)%>%summarise_all(mean, na.rm = T)
  try(ggplot(effi_city) + geom_histogram(aes(x = effi), fill = 'white', col = 'black') +
    xlab('Средняя эффективность') + ylab('Количество городов')+
    ggsave(png_name, width = 20, height = 9, dpi = 600, device = png, type = "cairo"))
}
plots <- list()
i=1
for (ind in unique(data2$Industry)) {
  #png_name <- paste('Efficiency_', ind, '.png', sep = '')
  effi_city <- data2%>%filter(Year==2022)%>%filter(Industry == ind)%>%group_by(City)%>%summarise_all(mean, na.rm = T)
  plots[[i]] <- ggplot(effi_city) + geom_histogram(aes(x = effi), fill = 'white', col = 'black') +
        xlab('Средняя эффективность') + ylab('Количество городов')+labs(title = ind)
  i=i+1
}

for (ind in unique(data2$Industry)) {
  #png_name <- paste('Efficiency_', ind, '.png', sep = '')
  effi_city <- data2%>%filter(Year==2022)%>%filter(Industry == ind)%>%group_by(City)%>%summarise_all(mean, na.rm = T)
  plots[[i]] <- ggplot(effi_city) + geom_density(aes(x = effi), fill = 'white', col = 'black') +
    xlab('Средняя эффективность') + ylab('Количество фирм')+labs(title = ind)
  i=i+1
}
library(RColorBrewer)
color_palette <- brewer.pal(9, "Paired")
ggplot(data2 %>% filter(Year == 2022)) +
  geom_density(aes(x = effi, col = Industry)) +
  scale_color_manual(values = color_palette)+theme_bw()+labs(col = 'Отрасль')+xlab('Эффективность')+ylab('Плотность')+
  ggsave('effi_ind.png', width = 15, height = 12, dpi = 600, device = png, type = "cairo")



ggarrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],
          plots[[6]],plots[[7]],plots[[8]],plots[[9]]) #+ 
  ggsave('Eff_9', width = 20, height = 9, dpi = 600, device = png, type = "cairo")

ggplot(effi_city) + geom_histogram(aes(x = effi), fill = 'white', col = 'black') +
  xlab('Средняя эффективность') + ylab('Количество городов')+labs(title = ind)

S_interval = cut(data2$S, breaks=500)
ggplot(data2, aes(x=S_interval, y=effi)) +
  stat_summary(fun=mean, geom="point") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('S') + ylab('Эффективность') #+ 
  ggsave('S_effi.png', width = 15, height = 9, dpi = 600, device = png, type = "cairo")
  
data2$S_interval <- cut(data2$S, breaks=500)
  
data2_summary <- data2 %>%
  group_by(S_interval) %>%
  summarise(mean_effi = mean(effi))
data2_summary <- subset(data2_summary, is.na(S_interval)!=T)
ggplot(data2_summary, aes(x=S_interval, y=mean_effi, group=1)) +
  geom_smooth(col = '#000000') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab('S') +
  ylab('Эффективность')+
  scale_x_discrete(breaks=function(x) x[seq(1, length(x), by=2)])
  
# Корреляционная матрица
#######
corr_data <- na.omit(data[c('effi', 'S', 'Age', 'City_pop', 'Soviet', 'Mono', 'North', 'S_c')])
corr_data$logpop <- log(corr_data$City_pop)
cor_data <- round(cor(corr_data),2)
head(cor_data)
summary(cor_data)
melted_cor <- melt(cor_data)
head(melted_cor)
ggheatmap <- ggplot(melted_cor, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "deepskyblue3", high = "brown2", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(2, 0),
    legend.direction = "vertical")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

##############
# Модели
##############

#data2$area <- cor(data2$City_pop/data2$Density/100, data2$Zone_Area, use = 'pairwise.complete.obs')

data2$S2 <- data2$S*data2$S
data2$S_pred2 <- data2$S_pred*data2$S_pred
data2$Distance_r <- data2$Distance/data2$City_radius
panel_data <- pdata.frame(data2 %>% distinct(INN, Year, .keep_all = T), index = c('INN', 'Year'))
panel_data$Distance_r2 <- panel_data$Distance_r*panel_data$Distance_r
panel_data <- panel_data %>% mutate_at('Density_2020', ~na_if(., 0))
is.pbalanced(panel_data)
panel_data$compactness2 <- panel_data$compactness*panel_data$compactness
m1 <- lm(effi ~ Age+Age2+Industry*S_pred+Industry*S_pred2+North*S_pred+Mono*S_pred+Soviet*S_pred+Caucasus:S_pred+Density_2020+
           log(City_age):S_pred-S_pred-S_pred2+City_area+Density_2020+compactness+compactness2, data = subset(panel_data, Year == 2022))
m2 <- lm(effi ~ Age+Age2+Industry:S_pred+City_area+Density_2020+compactness+compactness2, data = subset(panel_data, Year == 2022))
m3 <- lm(effi ~ Age+Age2+S_pred+City_area+Density_2020+compactness+compactness2, data = subset(panel_data, Year == 2022))
summary(m1)
w1 <- lm(log(W/L) ~ Age+Age2+Industry*S+factor(Region)+North*S+Mono*S+Soviet*S+Caucasus:S+Density+
           log(City_age):S+Industry*S2-S2-S, data = subset(panel_data, Year == 2022))
w2 <- lm(log(W/L) ~ Age+Age2+Industry:S, data = subset(panel_data, Year == 2022))
w3 <- lm(log(W/L) ~ Age+Age2+S, data = subset(panel_data, Year == 2022))
vif(m1)
summary(w1)

full_model <- lm(effi ~ Age + Age2 + Industry * S_pred + Industry*S_pred2 + North*S_pred + Mono*S_pred + Soviet*S_pred + 
                   Caucasus*S_pred + log(Density_2020) + log(City_age):S_pred - S_pred - S_pred2 + log(City_area) + 
                   compactness*Industry + compactness2*Industry + Distance_r*Industry +  Industry*Distance_r2 -
                   compactness - compactness2-Distance_r2-Distance_r + Region, data = subset(panel_data, Year == 2022))
distance_model <- lm(effi ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                       log(City_age) + log(City_area) + Industry*Distance_r + Industry*Distance_r2 -Distance_r2-Distance_r + Region,
                     data = subset(panel_data, Year == 2022))
compact_model <- lm(effi ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                      log(City_age) + log(City_area) + Industry*compactness + 
                      compactness2*Industry - compactness - compactness2 + Region,
                    data = subset(panel_data, Year == 2022))
S_model <- lm(effi ~ Age + Age2 + Industry * S_pred + Industry * S_pred2 + North*S_pred + Mono*S_pred + 
                Soviet*S_pred + Caucasus*S_pred + log(Density_2020) + log(City_age):S_pred - S_pred-S_pred2  + 
                log(City_area) + Region, data = subset(panel_data, Year == 2022))
sum(subset(panel_data, Year == 2022)$Age<18)/nrow(subset(panel_data, Year == 2022))
-distance_model$coefficients[2]/(2*distance_model$coefficients[3])
stargazer(full_model, distance_model, compact_model, S_model, omit = c('[R][e][g][i][o][n]'), 
          column.labels = c('full model', 'distance model', 'compact model', 'S model'),
          #type = 'html', out = 'results0603.doc',
          type = 'text',
          digits = 2,
          se = list(cse(full_model), cse(distance_model), cse(compact_model), cse(S_model)))
full_model$coefficients
full_model_panel <- plm(effi ~ Age + Age2 + Industry * S_pred + Industry*S_pred2 + North*S_pred + Mono*S_pred + Soviet*S_pred + 
                          Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred - S_pred2 + log(City_area) + 
                          compactness*Industry + compactness2*Industry + Distance_r*Industry +  Industry*Distance_r2 -
                          compactness - compactness2-Distance_r2-Distance_r + Region, data = panel_data, model = 'p', effect = 'time')
distance_model_panel <- plm(effi ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                              log(City_age) + log(City_area) + Industry*Distance_r + Industry*Distance_r2 -Distance_r2-Distance_r + Region, 
                      data = panel_data, model = 'p', effect = 'time')
compact_model_panel <- plm(effi ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                             log(City_age) + log(City_area) + Industry*compactness + 
                             compactness2*Industry - compactness - compactness2 + Region,
                           data = panel_data, model = 'p', effect = 'time')
S_model_panel <- plm(effi ~ Age + Age2 + Industry * S_pred + Industry * S_pred2 + North*S_pred + Mono*S_pred + 
                       Soviet*S_pred + Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred-S_pred2  + 
                       log(City_area) + Region, data = panel_data, model = 'p', effect = 'time')
summary(full_model_panel)

stargazer(full_model_panel, 
          distance_model_panel, 
          compact_model_panel, 
          S_model_panel, 
          omit = c('[R][e][g][i][o][n]'), 
          column.labels = c('full model', 
                            'distance model', 
                            'compact model', 
                            'S model'),
          type = 'html', out = 'results_panel.doc',
          #type = 'text',
          digits = 2,
          se = list(cse(full_model_panel), cse(distance_model_panel), cse(compact_model_panel), cse(S_model_panel))
          )

write.xlsx(aggregate(subset(panel_data, Year == 2022)$effi, list(subset(panel_data, Year == 2022)$Region), mean, na.rm = T), 'mean_effi_reg1.xlsx')
panel_data$
panel_data_nomocsow <- subset(panel_data, City != 'Москва')


full_model_nomsc <- lm(effi ~ Age + Age2 + Industry * S_pred + Industry*S_pred2 + North*S_pred + Mono*S_pred + Soviet*S_pred + 
                         Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred - S_pred2 + log(City_area) + 
                         compactness*Industry + compactness2*Industry + Distance_r*Industry +  Industry*Distance_r2 -
                         compactness - compactness2-Distance_r2-Distance_r + Region, data = subset(panel_data_nomocsow, Year == 2022))
distance_model_nomsc <- lm(effi ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                       log(City_age) + log(City_area) + Industry*Distance_r-Distance_r2-Distance_r+ Region,
                     data = subset(panel_data_nomocsow, Year == 2022))
compact_model_nomsc <- lm(effi ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                      log(City_age) + log(City_area) + Industry*compactness + 
                      compactness2*Industry - compactness - compactness2+ Region,
                    data = subset(panel_data_nomocsow, Year == 2022))
S_model_nomsc <- lm(effi ~ Age + Age2 + Industry * S_pred + Industry * S_pred2 + North*S_pred + Mono*S_pred + 
                Soviet*S_pred + Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred - S_pred2+ 
                  log(City_area)+ Region, data = subset(panel_data_nomocsow, Year == 2022))

stargazer(full_model_nomsc, distance_model_nomsc, compact_model_nomsc, S_model_nomsc, omit = c('[R][e][g][i][o][n]'), 
          column.labels = c('full model', 'distance model', 'compact model', 'S model'),
          type = 'html', out = 'results_nomsc0603.doc',
          #type = 'text',
          digits = 2,
          se = list(cse(full_model_nomsc), cse(distance_model_nomsc), cse(compact_model_nomsc), cse(S_model_nomsc)))

full_model_panel_nomsc <- plm(effi ~ Age + Age2 + Industry * S_pred + Industry*S_pred2 + North*S_pred + Mono*S_pred + Soviet*S_pred + 
                                Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred - S_pred2 + log(City_area) + 
                                compactness*Industry + compactness2*Industry + Distance_r*Industry +  Industry*Distance_r2 -
                                compactness - compactness2-Distance_r2-Distance_r + Region, data = panel_data_nomocsow, model = 'p', effect = 'time')
distance_model_panel_nomsc <- plm(effi ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                                    log(City_age) + log(City_area) + Industry*Distance_r + Industry*Distance_r2 -Distance_r2-Distance_r + Region, 
                            data = panel_data_nomocsow, model = 'p', effect = 'time')
compact_model_panel_nomsc <- plm(effi ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                                   log(City_age) + log(City_area) + Industry*compactness + 
                                   compactness2*Industry - compactness - compactness2 + Region, 
                           data = panel_data_nomocsow, model = 'p', effect = 'time')
S_model_panel_nomsc <- plm(effi ~ Age + Age2 + Industry * S_pred + Industry * S_pred2 + North*S_pred + Mono*S_pred + 
                             Soviet*S_pred + Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred-S_pred2  + 
                             log(City_area) + Region, data = panel_data_nomocsow, model = 'p', effect = 'time')
summary(S_model_panel_nomsc)

stargazer(full_model_panel_nomsc, 
          distance_model_panel_nomsc, 
          compact_model_panel_nomsc, 
          S_model_panel_nomsc, 
          omit = c('[R][e][g][i][o][n]'), 
          column.labels = c('full model', 
                            'distance model', 
                            'compact model', 
                            'S model'),
          type = 'html', out = 'results_panel_nomsc.doc',
          #type = 'text',
          digits = 2,
          se = list(cse(full_model_panel), cse(distance_model_panel), cse(compact_model_panel), cse(S_model_panel))
          )
for (ind in unique(aggregated_points$Industry)) {
  print(ind)
  subdf <- subset(aggregated_points, Industry==ind)
  print(paste('w_mean', weighted.mean(subdf$S, w = subdf$count, na.rm = T)))
  print(paste('mean', mean(subdf$S, na.rm = T)))
}
ggplot(aggregated_points%>%filter(Industry %in% )) + geom_density(aes(x=S, group = Industry, col = Industry))+xlim(0,0.2)
#######
m1_city <- lm(effi ~ Age+Age2+Industry*compactness+Industry*compactness2+North*compactness+Mono*compactness+Soviet*compactness+Caucasus:compactness+Density_2020+
           log(City_age):compactness-compactness-compactness2+City_area+Density_2020, data = subset(panel_data, Year == 2022))
m2_city <- lm(effi ~ Age+Age2+Industry:compactness+City_area+Density_2020, data = subset(panel_data, Year == 2022))
m3_city <- lm(effi ~ Age+Age2+compactness+City_area+Density_2020, data = subset(panel_data, Year == 2022))

summary(w1)


stargazer(m1, w1, omit = c('[R][e][g][i][o][n]'), #type = 'text',
          #type = 'html', out = 'C:/Users/User/Desktop/Диссер/results2.doc',
          type = 'text',
          digits = 2,
          se = list(cse(m1), cse(w1)))

stargazer(m1_city, m2_city, m3_city, omit = c('[R][e][g][i][o][n]'), type = 'text',
          #type = 'html',out = 'C:/Users/User/Desktop/Диссер/effi.doc',
          digits = 2,
          add.lines = list(c('Estimation', 'Industry-specific', 'Industry-specific', 'Aggregate'),
                           c('FE', 'Industry', 'No', 'No')), se = list(cse(m1_city), cse(m2_city), cse(m3_city)))
########

stargazer(w1, w2, w3, omit = c('[R][e][g][i][o][n]'),type = 'text', 
          #type = 'html',
          #out = 'C:/Users/User/Desktop/Диссер/w.doc', digits = 2,
          add.lines = list(c('Estimation', 'Industry-specific', 'Industry-specific', 'Aggregate'),
                           c('FE', 'Industry', 'No', 'No')), se = list(cse(w1), cse(w2), cse(w3)))

fe <- plm(effi ~ Age+Age2+Industry*S_pred+Industry*S_pred2+North:S_pred+Mono:S_pred+Soviet:S_pred+Caucasus:S_pred+Density+
           log(City_age):S_pred-S_pred-S_pred2, data = panel_data, model = 'between', effect = 'individual')
summary(fe)
re <- plm(effi ~ Age+Age2+Industry*S+Industry*S2+North:S+Mono:S+Soviet:S+Caucasus:S+Density+
            log(City_age):S-S-S2, data = panel_data, model = 'random', effect = 'individual')
summary(re)
pool <- plm(effi ~ Age+Age2+Industry*S+Industry*S2+North:S+Mono:S+Soviet:S+Caucasus:S+Density+
              log(City_age):S-S-S2, data = panel_data, model = 'p')

stargazer(fe, re, pool, type = 'text', column.labels = c('fe', 're', 'pool'))

stargazer(fe, re, pool, type = 'html', digits = 2, out = 'panels.doc',
          column.labels = c('fe', 're', 'pool'))
panel_data$S2 <- panel_data$S*panel_data$S
waldtest(fe, pool)
bptest(pool, re)
phtest(re, fe)

#######
# Вход(?)
aggregated_data <- panel_data %>%
  group_by(zone_number, Industry, Year) %>%
  summarise(
    count = n(),
    Region = first(Region),
    S = first(S_pred),
    S2 = first(S_pred2),
    City_age = first(City_age),
    North = first(North),
    Mono = first(Mono),
    Year_founded = first(Year_founded),
    Soviet = first(Soviet),
    Caucasus = first(Caucasus),
    City_pop = first(City_pop),
    Density = first(Density),
    Area = first(Zone_Area),
    compactness = first(compactness),
    compactness2 = first(compactness2),
    City_area = first(City_area),
    Area_growth = first(Area_growth),
    Pop_growth = first(Pop_growth)
  ) %>%
  ungroup()
summary(aggregated_data[aggregated_data$Industry=="АЗС",]$count)
summary(aggregated_data[aggregated_data$Industry=="Древообработка",]$count)
summary(aggregated_data[aggregated_data$Industry=="Кафе",]$count)
summary(aggregated_data[aggregated_data$Industry=="Клиника",]$count)
summary(aggregated_data[aggregated_data$Industry=="Мебель",]$count)
summary(aggregated_data[aggregated_data$Industry=="Металл",]$count)
summary(aggregated_data[aggregated_data$Industry=="Отель",]$count)
summary(aggregated_data[aggregated_data$Industry=="Продукты",]$count)
summary(aggregated_data[aggregated_data$Industry=="Шиномонтаж",]$count)
aggregated_data$count_10 <- floor(aggregated_data$count/10)
aggregated_data$count_cat <- case_when(aggregated_data$count == 1 ~ '1', 
                                       aggregated_data$count == 2 ~ '2',
                                       aggregated_data$count == 3 ~ '3',
                                       aggregated_data$count == 4 ~ '4',
                                       aggregated_data$count == 5 ~ '5',
                                       aggregated_data$count == 6 ~ '6',
                                       aggregated_data$count == 7 ~ '7',
                                       aggregated_data$count > 8 ~ '8_more')

aggregated_data$count_10_cat <- case_when(aggregated_data$count_10 == 1 ~ '1', 
                                       aggregated_data$count_10 == 2 ~ '2',
                                       aggregated_data$count_10 == 3 ~ '3',
                                       aggregated_data$count_10 == 4 ~ '4',
                                       aggregated_data$count_10 == 5 ~ '5',
                                       aggregated_data$count_10 > 5 ~ '6_more')
aggregated_data_22 <- subset(aggregated_data, Year == 2022)

point_data <- read.csv('clustered_points_fitlered.csv')
point_data <- subset(point_data, cluster_dbscan3!=-1)
aggregated_points <- point_data %>%
  group_by(cluster_dbscan3, category) %>%
  summarise(
    count = n()
    ) %>%
  ungroup()


connect_data <- read_excel('E:/Диссер/Точки/connectivity_results_dbscan.xlsx')
aggregated_points <- left_join(aggregated_points, connect_data, by = c('category' = 'Category', 'cluster_dbscan3' = 'Zone'))
aggregated_points$Industry <- aggregated_points$category
aggregated_points$Industry <- case_when(aggregated_points$Industry == 'Завод металлических изделий' ~ 'Металл', 
                                        aggregated_points$Industry == 'Обработка дерева' ~ 'Древообработка',
                                        aggregated_points$Industry == 'Мебельный завод' ~ 'Мебель', 
                                        TRUE ~ aggregated_points$Industry)

unique_cities <- unique(aggregated_points$cluster_dbscan3)
unique_industries <- unique(aggregated_points$Industry)

all_combinations <- expand.grid(cluster_dbscan3 = unique_cities, Industry = unique_industries)
merged_data <- merge(all_combinations, aggregated_points, by = c("cluster_dbscan3", "Industry"), all.x = TRUE)
merged_data$count[is.na(merged_data$count)] <- 0

aggregated_points_old <- aggregated_points
aggregated_points <- merged_data
aggregated_data_22_cleaned <- aggregated_data_22 %>%
  group_by(zone_number) %>%
  summarise(across(c(7:14, 16:20), mean, na.rm = TRUE))

#aggregated_points <- left_join(aggregated_points, aggregated_data_22[c(1:2, 6, 7, 16)], by = c('Industry' = 'Industry', 'cluster_dbscan3' = 'zone_number'))
aggregated_points <- left_join(aggregated_points, aggregated_data_22_cleaned, by = c('cluster_dbscan3' = 'zone_number'))
aggregated_points$S <- aggregated_points$Connectivity_Index
aggregated_points$S2 <- aggregated_points$S*aggregated_points$S

aggregated_points$S_pred <- predict(stage1, aggregated_points)

ggplot(aggregated_points) + geom_histogram(aes(x = S), fill = 'white', col = 'black', bins = 60) + xlim(0, 0.2) + ylab('Количество рынков')+
  ggsave("S_hist.png", width = 8, height = 6, dpi = 600, device = png, type = "cairo")
Mode(round(aggregated_points$S, digits = 3), na.rm = T)
weighted.mean(subset(aggregated_points, cluster_dbscan3==557)$S, w = subset(aggregated_points, cluster_dbscan3==557)$count)
summary(aggregated_points[aggregated_points$Industry=="АЗС",]$count)
summary(aggregated_points[aggregated_points$Industry=="Древообработка",]$count)
summary(aggregated_points[aggregated_points$Industry=="Кафе",]$count)
summary(aggregated_points[aggregated_points$Industry=="Клиника",]$count)
summary(aggregated_points[aggregated_points$Industry=="Мебель",]$count)
summary(aggregated_points[aggregated_points$Industry=="Металл",]$count)
summary(aggregated_points[aggregated_points$Industry=="Отель",]$count)
summary(aggregated_points[aggregated_points$Industry=="Продукты",]$count)
summary(aggregated_points[aggregated_points$Industry=="Шиномонтаж",]$count)
summary(aggregated_points[aggregated_points$Industry=="Парикмахерская",]$count)
aggregated_points$count_10 <- floor(aggregated_points$count/10)
aggregated_points$count_cat <- case_when(aggregated_points$count == 0 ~ '0', 
                                         aggregated_points$count == 1 ~ '1', 
                                         aggregated_points$count == 2 ~ '2',
                                         aggregated_points$count == 3 ~ '3',
                                         aggregated_points$count == 4 ~ '4',
                                         aggregated_points$count == 5 ~ '5',
                                         aggregated_points$count == 6 ~ '6',
                                         aggregated_points$count == 7 ~ '7',
                                         aggregated_points$count == 8 ~ '8',
                                         aggregated_points$count == 9 ~ '9',
                                         aggregated_points$count_10 > 9 ~ '10_more')

cat_levels <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10_more')
aggregated_points$count_10_cat <- case_when(aggregated_points$count_10 == 0 ~ '0', 
                                            aggregated_points$count_10 == 1 ~ '1', 
                                            aggregated_points$count_10 == 2 ~ '2',
                                            aggregated_points$count_10 == 3 ~ '3',
                                            aggregated_points$count_10 == 4 ~ '4',
                                            aggregated_points$count_10 == 5 ~ '5',
                                            aggregated_points$count_10 == 6 ~ '6',
                                            aggregated_points$count_10 == 7 ~ '7',
                                            aggregated_points$count_10 == 8 ~ '8',
                                            aggregated_points$count_10 == 9 ~ '9',
                                            aggregated_points$count_10 > 9 ~ '10_more')
aggregated_points$count_cat <- factor(aggregated_points$count_cat, levels = cat_levels)
aggregated_points$count_10_cat <- factor(aggregated_points$count_10_cat, levels = cat_levels)
aggregated_points$S_pred2 <- aggregated_points$S_pred*aggregated_points$S_pred

library(MASS) 
library(ordinal)
aggregated_points_old <- aggregated_points
aggregated_points <- aggregated_points_old 
aggregated_points <- subset(aggregated_points, Density > 0)%>%filter(Density < 1000)%>%filter(count<200)
ol_full_0 <- polr(count_cat ~ log(City_pop) + log(Density) + City_age + S_pred*Industry-S_pred, data = aggregated_points, method = 'logistic')
summary(ol_full_0)

ol_petrol <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred + North + Mono + Soviet + City_age + log(City_area),
                  data = subset(aggregated_points, Industry == 'АЗС'), 
                  method = 'logistic')
#summary(ol_petrol)

ol_clinic <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred  + North + Mono + Soviet + City_age + log(City_area),
            data = subset(aggregated_points, Industry == 'Клиника'), method = 'logistic')
#summary(ol_clinic)

ol_metal <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred  +  North + Mono + Soviet + City_age+ log(City_area),
                 data = subset(aggregated_points, Industry == 'Металл'), 
                 method = 'logistic', Hess = T)
#summary(ol_metal)

ol_tires <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred  + North + Mono + Soviet + City_age+ log(City_area),
                 data = subset(aggregated_points, Industry == 'Шиномонтаж'), 
                 method = 'logistic')
#summary(ol_tires)

ol_wood <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred  + North + Mono + Soviet + City_age+ log(City_area),
                 data = subset(aggregated_points, Industry == 'Древообработка'), 
                 method = 'logistic')
#summary(ol_wood)

ol_furniture <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred  + North + Mono + Soviet + City_age+ log(City_area),
                data = subset(aggregated_points, Industry == 'Мебель'),  Hess = T,
                method = 'logistic')
#summary(ol_furniture)

ol_cafe <- polr(count_10_cat ~ log(City_pop) + log(Density) + S_pred  + North + Mono + Soviet + City_age+ log(City_area),
                     data = subset(aggregated_points, Industry == 'Кафе'),  Hess = T,
                     method = 'logistic')
#summary(ol_cafe)
vif(ol_metal)
ol_hotel <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred  + North + Mono + Soviet + City_age+ log(City_area),
                data = subset(aggregated_points, Industry == 'Отель'),  Hess = T,
                method = 'logistic')
#summary(ol_hotel)

ol_barber <- polr(count_10_cat ~ log(City_pop) + log(Density) + S_pred  + North + Mono + Soviet + City_age+ log(City_area),
                 data = subset(aggregated_points, Industry == 'Парикмахерская'),  Hess = T,
                 method = 'logistic')
#summary(ol_barber)

ol_food <- polr(count_10_cat ~ log(City_pop) + log(Density) + S_pred  + North + Mono + Soviet + City_age+ log(City_area),
                  data = subset(aggregated_points, Industry == 'Продукты'),  Hess = T,
                  method = 'logistic')
#summary(ol_food)

stargazer(ol_barber, ol_cafe, ol_clinic, ol_food, ol_hotel, ol_petrol,  ol_furniture, 
          ol_metal, ol_tires, ol_wood, 
          column.labels = c('Парикмахерская', 'Кафе', 'Клиника', 'Продукты', 'Отель', 'АЗС',
                            'Мебельный завод', 'Металлообработка', 'Шиномонтаж', 'Деревообработка'),
          digits = 2,
          #type = 'text',
          type = 'html', out = 'ol_S_1903'
          )
vif(ol_barber)
###############################
aggregated_points$count_var <- case_when(aggregated_points$Industry == 'АЗС' ~ aggregated_points$count_cat,
                                         aggregated_points$Industry == 'Клиника' ~ aggregated_points$count_cat,
                                         aggregated_points$Industry == 'Металл' ~ aggregated_points$count_cat,
                                         aggregated_points$Industry == 'Шиномонтаж' ~ aggregated_points$count_cat,
                                         aggregated_points$Industry == 'Древообработка' ~ aggregated_points$count_cat,
                                         aggregated_points$Industry == 'Мебель' ~ aggregated_points$count_cat,
                                         aggregated_points$Industry == 'Кафе' ~ aggregated_points$count_10_cat,
                                         aggregated_points$Industry == 'Отель' ~ aggregated_points$count_cat,
                                         aggregated_points$Industry == 'Парикмахерская' ~ aggregated_points$count_10_cat,
                                         aggregated_points$Industry == 'Продукты' ~ aggregated_points$count_10_cat)

ol_comp_petrol <- polr(count_cat ~ log(City_pop)+log(Density) + compactness + compactness2+ North + Mono + Soviet + City_age + log(City_area)+Region,
                  data = subset(aggregated_points, Industry == 'АЗС'), 
                  method = 'logistic')
#summary(ol_comp_petrol)

ol_comp_clinic <- polr(count_cat ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age + log(City_area)+Region,
                  data = subset(aggregated_points, Industry == 'Клиника'), method = 'logistic')
#summary(ol_comp_clinic)

ol_comp_metal <- polr(count_cat ~ log(City_pop)+log(Density) + compactness + compactness2 +  North + Mono + Soviet + City_age+ log(City_area)+Region,
                 data = subset(aggregated_points, Industry == 'Металл'), 
                 method = 'logistic', Hess = T)
#summary(ol_comp_metal)

ol_comp_tires <- polr(count_cat ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age+ log(City_area)+Region,
                 data = subset(aggregated_points, Industry == 'Шиномонтаж'), 
                 method = 'logistic')
#summary(ol_comp_tires)

ol_comp_wood <- polr(count_cat ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age+ log(City_area)+Region,
                data = subset(aggregated_points, Industry == 'Древообработка'), 
                method = 'logistic')
#summary(ol_comp_wood)

ol_comp_furniture <- polr(count_cat ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age+ log(City_area)+Region,
                     data = subset(aggregated_points, Industry == 'Мебель'),  Hess = T,
                     method = 'logistic')
#summary(ol_comp_furniture)

ol_comp_cafe <- polr(count_10_cat ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age+ log(City_area)+Region,
                data = subset(aggregated_points, Industry == 'Кафе'),  Hess = T,
                method = 'logistic')
#summary(ol_comp_cafe)

ol_comp_hotel <- polr(count_cat ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age+ log(City_area)+Region,
                 data = subset(aggregated_points, Industry == 'Отель'),  Hess = T,
                 method = 'logistic')
#summary(ol_comp_hotel)

ol_comp_barber <- polr(count_10_cat ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age+ log(City_area)+Region,
                  data = subset(aggregated_points, Industry == 'Парикмахерская'),  Hess = T,
                  method = 'logistic')
#summary(ol_comp_barber)

ol_comp_food <- polr(count_10_cat ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age+ log(City_area)+Region,
                data = subset(aggregated_points, Industry == 'Продукты'),  Hess = T,
                method = 'logistic')
#summary(ol_comp_food)
stargazer(ol_comp_barber, ol_comp_cafe, ol_comp_clinic, ol_comp_food, ol_comp_hotel, ol_comp_petrol,  ol_comp_furniture, 
          ol_comp_metal, ol_comp_tires, ol_comp_wood, 
          column.labels = c('Парикмахерская', 'Кафе', 'Клиника', 'Продукты', 'Отель', 'АЗС',
                            'Мебельный завод', 'Металлообработка', 'Шиномонтаж', 'Деревообработка'),
          digits = 2,
          
          type = 'text'#,
          #type = 'html', out = 'ol_comp_1903'
          )
vif(ol_comp_food)

summary(ol_comp_petrol)
# Считаем среднее пороговое значение, без населения
mean_city <- sum(ol_comp_petrol$coefficients[c(1:2, 5:9)]*c(mean(log(subset(aggregated_points, Industry == 'АЗС')$City_pop), na.rm = T), 
                                                            mean(log(subset(aggregated_points, Industry == 'АЗС')$Density), na.rm = T),
                                                            mean(subset(aggregated_points, Industry == 'АЗС')$North, na.rm = T),
                                                            mean(subset(aggregated_points, Industry == 'АЗС')$Mono, na.rm = T),
                                                            mean(subset(aggregated_points, Industry == 'АЗС')$Soviet, na.rm = T),
                                                            mean(subset(aggregated_points, Industry == 'АЗС')$City_age, na.rm = T),
                                                            mean(log(subset(aggregated_points, Industry == 'АЗС')$City_area), na.rm = T)))
ol_full <- polr(count_var  ~ log(City_pop)+log(Density) + compactness + compactness2 + North + Mono + Soviet + City_age+ log(City_area), data = aggregated_points,
                method = 'logistic')
mean_city <- sum(ol_full$coefficients[c(1:2, 5:9)]*c(median(log(aggregated_points$City_pop), na.rm = T), 
                                                     median(log(aggregated_points$Density), na.rm = T),
                                                     median(aggregated_points$North, na.rm = T),
                                                     median(aggregated_points$Mono, na.rm = T),
                                                     median(aggregated_points$Soviet, na.rm = T),
                                                     median(aggregated_points$City_age, na.rm = T),
                                                     median(log(aggregated_points$City_area), na.rm = T)))
compactness_deficit <- ol_full$zeta-mean_city

discrim <- ol_full$coefficients[3]^2-4*ol_full$coefficients[4]*(-compactness_deficit)
compactness_needed <- rbind((-ol_full$coefficients[3]+sqrt(discrim))/(2*ol_full$coefficients[4]),
                            (-ol_full$coefficients[3]-sqrt(discrim))/(2*ol_full$coefficients[4]))


######

ol_comp_S_petrol <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2+ North + Mono + Soviet + City_age + log(City_area),
                       data = subset(aggregated_points, Industry == 'АЗС'), 
                       method = 'logistic')
summary(ol_comp_S_petrol)

ol_comp_S_clinic <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 + North + Mono + Soviet + City_age + log(City_area),
                       data = subset(aggregated_points, Industry == 'Клиника'), method = 'logistic')
summary(ol_comp_S_clinic)

ol_comp_S_metal <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 +  North + Mono + Soviet + City_age+ log(City_area),
                      data = subset(aggregated_points, Industry == 'Металл'), 
                      method = 'logistic', Hess = T)
summary(ol_comp_S_metal)

ol_comp_S_tires <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 + North + Mono + Soviet + City_age+ log(City_area),
                      data = subset(aggregated_points, Industry == 'Шиномонтаж'), 
                      method = 'logistic')
summary(ol_comp_S_tires)

ol_comp_S_wood <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 + North + Mono + Soviet + City_age+ log(City_area),
                     data = subset(aggregated_points, Industry == 'Древообработка'), 
                     method = 'logistic')
summary(ol_comp_S_wood)

ol_comp_S_furniture <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 + North + Mono + Soviet + City_age+ log(City_area),
                          data = subset(aggregated_points, Industry == 'Мебель'),  Hess = T,
                          method = 'logistic')
summary(ol_comp_S_furniture)

ol_comp_S_cafe <- polr(count_10_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 + North + Mono + Soviet + City_age+ log(City_area),
                     data = subset(aggregated_points, Industry == 'Кафе'),  Hess = T,
                     method = 'logistic')
summary(ol_comp_S_cafe)

ol_comp_S_hotel <- polr(count_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 + North + Mono + Soviet + City_age+ log(City_area),
                      data = subset(aggregated_points, Industry == 'Отель'),  Hess = T,
                      method = 'logistic')
summary(ol_comp_S_hotel)

ol_comp_S_barber <- polr(count_10_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 + North + Mono + Soviet + City_age+ log(City_area),
                       data = subset(aggregated_points, Industry == 'Парикмахерская'),  Hess = T,
                       method = 'logistic')
summary(ol_comp_S_barber)

ol_comp_S_food <- polr(count_10_cat ~ log(City_pop) + log(Density) + S_pred + S_pred2+compactness+compactness2 + North + Mono + Soviet + City_age+ log(City_area),
                     data = subset(aggregated_points, Industry == 'Продукты'),  Hess = T,
                     method = 'logistic')
summary(ol_comp_S_food)

stargazer(ol_comp_S_barber, ol_comp_S_cafe, ol_comp_S_clinic, ol_comp_S_food, ol_comp_S_hotel, ol_comp_S_petrol,  ol_comp_S_furniture, 
          ol_comp_S_metal, ol_comp_S_tires, ol_comp_S_wood,
          column.labels = c('Парикмахерская', 'Кафе', 'Клиника', 'Продукты', 'Отель', 'АЗС',
                            'Мебельный завод', 'Металлообработка', 'Шиномонтаж', 'Деревообработка'),
          type = 'text')




#####
# Robustness check
W_subset <- subset(panel_data, Year == 2022)%>%
  filter(W/L<500000*12)%>%filter(W>0)

full_model_W <- lm(log(W/L) ~ Age + Age2 + Industry * S_pred + Industry*S_pred2 + North*S_pred + Mono*S_pred + Soviet*S_pred + 
                   Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred - S_pred2 + log(City_area) + 
                   compactness*Industry + compactness2*Industry + Distance_r*Industry +  Industry*Distance_r2 -
                   compactness - compactness2-Distance_r2-Distance_r + Region, data = W_subset)
distance_model_W <- lm(log(W/L) ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                       log(City_age) + log(City_area) + Industry*Distance_r + Industry*Distance_r2 -Distance_r2-Distance_r + Region,
                     data = W_subset)
compact_model_W <- lm(log(W/L) ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                      log(City_age) + log(City_area) + Industry*compactness + 
                      compactness2*Industry - compactness - compactness2 + Region,
                    data = W_subset)
S_model_W <- lm(log(W/L) ~ Age + Age2 + Industry * S_pred + Industry * S_pred2 + North*S_pred + Mono*S_pred + 
                Soviet*S_pred + Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred-S_pred2  + 
                log(City_area) + Region, data = W_subset)
quantile(W_subset$W/W_subset$L/12, 0.8, na.rm = T)

stargazer(full_model_W, distance_model_W, compact_model_W, S_model_W, omit = c('[R][e][g][i][o][n]'), 
          column.labels = c('full model', 'distance model', 'compact model', 'S model'),
          type = 'html', out = 'results_w_1803_2.doc',
          #type = 'text',
          digits = 2,
          se = list(cse(full_model_W), cse(distance_model_W), cse(compact_model_W), cse(S_model_W)))

vif(full_model_panel, type = 'predictor')

full_model_nomsc_W <- lm(log(W/L) ~ Age + Age2 + Industry * S_pred + Industry*S_pred2 + North*S_pred + Mono*S_pred + Soviet*S_pred + 
                         Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred - S_pred2 + log(City_area) +
                         compactness*Industry + compactness2*Industry + Distance_r*Industry +  Industry*Distance_r2 -
                         compactness - compactness2-Distance_r2-Distance_r+ Region, data = subset(W_subset, City != 'Москва'))
distance_model_nomsc_W <- lm(log(W/L) ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                             log(City_age) + log(City_area) + Industry*Distance_r+Industry*Distance_r2-Distance_r2-Distance_r+ Region,
                           data = subset(W_subset, City != 'Москва'))
compact_model_nomsc_W <- lm(log(W/L) ~ Age + Age2 + North + Mono + Soviet + Caucasus + log(Density_2020) + 
                            log(City_age) + log(City_area) + Industry*compactness + 
                            compactness2*Industry - compactness - compactness2+ Region,
                          data = subset(W_subset, City != 'Москва'))
S_model_nomsc_W <- lm(log(W/L) ~ Age + Age2 + Industry * S_pred + Industry * S_pred2 + North*S_pred + Mono*S_pred + 
                      Soviet*S_pred + Caucasus*S_pred + log(Density_2020) + log(City_age)*S_pred - S_pred - S_pred2+ 
                      log(City_area)+ Region, data = subset(W_subset, City != 'Москва'))

stargazer(full_model_nomsc_W, distance_model_nomsc_W, compact_model_nomsc_W, S_model_nomsc_W, omit = c('[R][e][g][i][o][n]'), 
          column.labels = c('full model', 'distance model', 'compact model', 'S model'),
          type = 'html', out = 'results_nomsc_W_1803_2.doc',
          #type = 'text',
          digits = 2,
          se = list(cse(full_model_nomsc_W), cse(distance_model_nomsc_W), cse(compact_model_nomsc_W), cse(S_model_nomsc_W)))

###############################
# Пространственные модели, не пригодились, но было интересно
data3 <- subset(data2, is.na(lat)!=TRUE)
data3$Ind_case <- case_when(data3$Industry == "АЗС" ~ 1,
                            data3$Industry == "Древообработка" ~ 2,
                            data3$Industry == "Кафе" ~ 3,
                            data3$Industry == "Клиника" ~ 4,
                            data3$Industry == "Мебель" ~ 5,
                            data3$Industry == "Металл" ~ 6,
                            data3$Industry == "Отель" ~ 7,
                            data3$Industry == "Парикмахерская" ~ 8,
                            data3$Industry == "Продукты" ~ 9,
                            data3$Industry == "Шиномонтаж" ~ 10)
data3$lon_ind <- data3$lon+data3$Ind_case*1000 #хитрость, чтобы разделить точки по отраслям

coord <- cbind(data3$lon_ind,data3$lat)
knn_neigh<- knn2nb(knearneigh(coord,k=10,longlat=TRUE))
print(is.symmetric.nb(knn_neigh))
knn_nb<-make.sym.nb(knn_neigh)
W.knn <- nb2listw(knn_nb)

summary(W.knn)
m1 <- lm(effi ~ Age+Age2+Industry*S+factor(Region)+North:S+Mono:S+Soviet:S+Caucasus:S+log(Density)+
           log(City_age):S, data = data3)
sp_test <- lm.LMtests(m1, W.knn, zero.policy = T, test = "all") # pvalue низкий для LMerr, LMlag, SARMA, значит у нас модель с пространственной ошибкой и пространственным лагом
moran.test(data3$effi, W.knn)
geary.test(data3$effi, W.knn)


variable<-data3$effi
variable.std<-((variable-mean(variable))/sd(variable))
moran.plot(variable.std, W.knn, labels=as.character(data3$Name), pch=19, quiet=F) # fig.10a

x<-data3$effi 
zx<-scale(x) 
mean(zx) 
sd(zx) 
wzx<-lag.listw(W.knn, zx) 
morlm<-lm(wzx~zx)
slope<-morlm$coefficients[2] 
intercept<-morlm$coefficients[1] 
par(pty="s") 
plot(zx, wzx, xlab="zx",ylab="spatial lag zx", pch="*")
abline(intercept, slope, color="steelblue") 
abline(h=0, lty=2) 
abline(v=0, lty=2)


sp_mod <- sacsarlm(effi ~ Age+Age2+Industry*S+North:S+Mono:S+Soviet:S+Caucasus:S+log(Density)+
                     log(City_age):S, data = data3, listw = W.knn, zero.policy = T)
summary(sp_mod)
sp_mod_sac <- sacsarlm(effi ~ Age+Age2+Industry*S+North:S+Mono:S+Soviet:S+Caucasus:S+log(Density)+
                     log(City_age):S-S, data = data3, listw = W.knn, zero.policy = T)
summary(sp_mod_sac)
trMat<-trW(as(W.knn, 'CsparseMatrix'), type="mult") 
model.imp <- impacts(sp_mod_sac, tr = trMat, R = 5000)
summary(model.imp, zstats=TRUE, short=TRUE)



df2 <- long_data%>%filter(is.na(lat)!=TRUE)%>%filter(Year==2022)
df2$Age2 <- df2$Age*df2$Age

df2$Ind_case <- case_when(df2$Industry == "АЗС" ~ 1,
                          df2$Industry == "Древообработка" ~ 2,
                          df2$Industry == "Кафе" ~ 3,
                          df2$Industry == "Клиника" ~ 4,
                          df2$Industry == "Мебель" ~ 5,
                          df2$Industry == "Металл" ~ 6,
                          df2$Industry == "Отель" ~ 7,
                          df2$Industry == "Парикмахерская" ~ 8,
                          df2$Industry == "Продукты" ~ 9,
                          df2$Industry == "Шиномонтаж" ~ 10)
df2$lon_ind <- df2$lon+df2$Ind_case*1000 #хитрость, чтобы разделить точки по отраслям

coord2 <- cbind(df2$lon_ind,df2$lat)
knn_neigh2<- knn2nb(knearneigh(coord2,k=10,longlat=TRUE))
print(is.symmetric.nb(knn_neigh2))
knn_nb2<-make.sym.nb(knn_neigh2)
W.knn2 <- nb2listw(knn_nb2)

df2$Entered <- case_when(is.na(df2$Liquidation_date) != T ~ 1,
                         df2$Liquidation_date == T ~ 0)
log_mod <- splogit

write.csv(subset(panel_data, Year == 2022), 'dataset_effi.csv') 





effi_city <- aggregate(subset(panel_data, Year == 2022)[c('effi', 'compactness', 'S', 'City_area')], list(subset(panel_data, Year == 2022)$zone_number), mean, na.rm=T)

library(cluster)
effi_city <- subset(effi_city, is.na(compactness)!=T)

wss <- numeric()

# Оценка модели k-средних для разного количества кластеров (например, от 1 до 10)
for (k in 1:10) {
  set.seed(123) # Задаем начальное значение для воспроизводимости результатов
  model <- kmeans(effi_city[c('effi', 'compactness')], centers = k)
  wss[k] <- model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares",
     main = "Elbow Method for Optimal Number of Clusters")


effi_city$cluster <- kmeans(effi_city[c('effi', 'compactness')], 8)$cluster
ggplot(effi_city) + geom_point(aes(x=compactness, y = effi), col = factor(effi_city$cluster))



