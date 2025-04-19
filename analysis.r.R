####
#   Packages 
####
library(dplyr)#Data manipulation
library(magrittr)
library(ggplot2)
library(ggpubr)
library(treemapify)
install.packages("writexl")
library(writexl)
library(psych)#for descriptives
library(vtable)
library(gridExtra)
library(car)  # For VIF
library(nortest)
library(Hmisc)
library(ggExtra)
library(ggmosaic)
library(corrplot)

####
#   Load and Examine the Data 
####




D0<- read.csv("auto_giannis.csv",header = TRUE, sep=";") #D=DATA
D<- read.csv("auto_giannis.csv",header = TRUE, sep=";")

#write.csv(D, file = "cleaned_auto_giannis.csv", row.names = TRUE, col.names = T,sep=',')


######
#   DATA CLEANING
######


?write.csv
dim(D)
head(D)
summary(D)
str(D)

D_no_na<-na.omit(D) # D WITHOUT NAs
summary(D_no_na)
is.na(D_no_na)
na_sum<-length(which(is.na(D))) # NAs COUNT BEFORE

length(which(is.na(D_no_na))) # NAS AFTER
 



Fuel_categ <- unique(DF$Fuel) 
Fuel_categ #7 FUEL CATEGORIES
unique(DF$Fuel)
table(DF$Fuel)

## AIR CONDITION ##

table(D$Air_condition) ## 5 MORE NAs
which(D$Air_condition =="" ) #NA POSITIONS
D<-D_no_na #D BEFORE
D[,11] # AIRCONDITION BEFORE
D <- D[-c(110, 112, 113, 114, 618),] #REMOVED 5 MORE NAs FROM AIR_CONDITION
D[,11] #D AIRCONDITION AFTER
table(D$Air_condition) # 0 NAs


## ACCELERATION ##

table(D$Acceleration)#OBSERV. WITH 0 ACCELER. -> FALSE ENTRY
which(D$Acceleration =="0" ) #POSITION
D[820,]# ROW 820 IS FALSE 
D <- D[-820,] 
table(D$Acceleration)



## HEATED MIRRORS ##

table(D$Heated_mirrors) #MUST HAVE ONLY YES OR NO VALUE
which(D$Heated_mirrors =="" | D$Heated_mirrors =="150" | D$Heated_mirrors =="50") #POSITION
D[c(207,210,283,285,286,480,488,62:66,725:728),] # FALSE AND NA ROWS
D <- D[-c(207,210,283,285,286,480,488,62:66,725:728),] #REMOVED
table(D$Heated_mirrors)

## BACK ELECTRIC WINDOWS ##

table(D$Back_electric_windows) #MUST HAVE ONLY YES OR NO VALUE
which(D$Back_electric_windows =="" | D$Back_electric_windows =="200" | D$Back_electric_windows=="210") #POSITION
D[c(279,292,294,1118),] # FALSE AND NA ROWS
D <- D[-c(279,292,294,1118),] #REMOVED
table(D$Back_electric_windows)

## CLIMA  ##
table(D$Clima)


D$Clima[!(D$Clima %in% c("YES", "NO"))] <- NA # SETS FALSE VALUES TO NA
is.na(D$Clima)
D <-D[!is.na(D$Clima), ] # REMOVES NA's
is.na(D$Clima)



write.csv(D, "cleaned_auto_giannis.csv", row.names = T)#SAVE THE CLEANED DATAFRAME
D<-read.csv('cleaned_auto_giannis.csv',header=T,sep=',')
D<-subset(D,select= - X.1)
D <- D |> 
  rename(Observations= Observ.)
write.csv(D, "cleaned_auto_giannis.csv", row.names = T)

names(D)


#####
## Descriptive Statistics for Numerical Columns
#####

names(DF)

# Summary statistics for numerical variables
summary_stats <- summary(DF[c("CO2", "Price", "Engine", "Horsepower",'Acceleration','Power_Nm','Taxation','Autonomy_klm','Consumption')])


std_devs <- sapply(DF, sd, na.rm = T)
round(std_devs,2)

summary_stats

quantiles <- sapply(DF,quantile,na.rm = T)
quantiles

description_DF <- describe(DF) #!!nice!
description_DF

describe(DF)

quantile(DF, probs = seq(0.75, 0.25), na.rm = T)

# Histogram for each variable
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid

hist(D$CO2, main = "Histogram of CO2 Emissions", xlab = "CO2 Emissions", col = "skyblue", border = "white")
hist(D$Fuel, main = "Histogram of Fuel Consumption", xlab = "Fuel Consumption", col = "lightgreen", border = "white")
hist(D$Engine, main = "Histogram of Engine Size", xlab = "Engine Size", col = "salmon", border = "white")
hist(D$Horsepower, main = "Histogram of Horsepower", xlab = "Horsepower", col = "lightcoral", border = "white")

par(mfrow = c(1, 1))  # Reset layout







###
# PLOTS
###


# Histogram for CO2 Emissions

ggplot(DF, aes(x = CO2,fill = 'yellow')) +
  geom_histogram(binwidth = 5,fill='black', color = "blue") +
  labs(
    title = "Distribution of CO2 Emissions",
    x = "CO2 Emissions (g/km)",
    y = "Frequency"
  
    
    ) + scale_fill_brewer()


ggplot() + geom_histogram(aes(x = DF$CO2), binwidth=4, fill=rainbow(68))








 
 
###
#  HISTOGRAMS
###

CO2_hist <- ggplot(DF, aes(x = CO2, fill = cut(CO2, 300))) +
  geom_histogram(show.legend = FALSE,binwidth = 5) +
  labs(x = "CO2 Emissions (g/km)", y = " ") +
  ggtitle("Distribution of CO2") +
  theme(
    plot.background = element_rect(fill='darkolivegreen2'),
    panel.background = element_rect(fill='black'),
    
    axis.title.x = element_text( hjust = 0.5,vjust = 1 ,color = "black", size = 15, face = "bold"),
    plot.title = element_text(hjust = 0.5,vjust = 1.5,size = 16, face = "bold",color = "black"),
    
  )+ 
  scale_fill_discrete(h = c(90, 210), c = 30, l = 50) +
  geom_vline(aes(xintercept = mean((CO2), na.rm = TRUE)), 
             color = "azure1", size = 1,linetype= 2,)

CO2_hist



Engine_hist <- ggplot(DF, aes(x = Engine, fill = cut(Engine, 300))) +
  geom_histogram(show.legend = FALSE,binwidth = 100) +
  labs(x = "Engine Size(cc)", y = " ") +
  ggtitle("Distribution of Engine") +
  theme(
    plot.background = element_rect(fill='darkseagreen'),
    panel.background = element_rect(fill='darkslategrey'),
    
    axis.title.x = element_text( hjust = 0.5,vjust = 1 ,color = "darkslateblue", size = 15, face = "bold"),
    plot.title = element_text(hjust = 0.5,vjust = 1.5,size = 16, face = "bold",color = "darkslateblue"),
    
  )+ 
  scale_fill_discrete(h = c(938.5, 958), c = 200, l = 27.9) +
  geom_vline(aes(xintercept = mean((Engine), na.rm = TRUE)), 
             color = "aliceblue", size = 1,linetype= 2,)
Engine_hist



Horsepower_hist <- ggplot(DF, aes(x = Horsepower, fill = cut(Horsepower, 300))) +
  geom_histogram(show.legend = FALSE,binwidth = 40) +
  labs(x = "Horsepower (HP)", y = " ") +
  ggtitle("Distribution of Horsepower") +
  theme(
    plot.background = element_rect(fill='antiquewhite3'),
    panel.background = element_rect(fill='darksalmon'),
    
    axis.title.x = element_text( hjust = 0.5,vjust = 1 ,color = "darkred", size = 15, face = "bold"),
    plot.title = element_text(hjust = 0.5,vjust = 1.5,size = 16, face = "bold",color = "darkred"),
    
  )+ 
  scale_fill_discrete(h = c(1760.5, 1898), c = 200, l = 27.9) +
  geom_vline(aes(xintercept = mean((Horsepower), na.rm = TRUE)), 
             color = "white", size = 1,linetype= 2,)
Horsepower_hist


CO2_hist
Engine_hist
Horsepower_hist


###
# barplot 
###

Car_extras<- as.data.frame(DF[,12:15])
Car_extras_rel_freq<-
ggplot(aes(x = Car_extras, fill = evercig)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(y = "Proportion",
       x = "Grade",
       fill = "Lifetime\nCigarette\nUse")



Car_extras



###
# Pies 
###

Fuel_pie <- ggplot(DF, aes(x = "", fill = Fuel)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Fuel Types", fill = "Fuel Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
Fuel_pie




Price_pie <- ggplot(DF, aes(x = "", fill = Price_Range_Label)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Price Ranges", fill = "Price Range (€)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
Price_pie




circlesdata <- data.frame(
  group = c('<10000', '10000-20000', '20000-30000', '30000-40000', '40000-50000', 
            '50000-60000', '60000-70000', '70000-80000', '80000-90000', '>90000'),
  circlesvalues = c(0.54, 27.05, 31.16, 18.93, 8.84, 7.41, 3.39, 1.34, 1.16, 0.18)
)


packing <- circleProgressiveLayout(circlesdata$circlesvalues, sizetype = 'area')


circlesdata <- cbind(circlesdata, packing)


dat.gg <- circleLayoutVertices(packing, npoints = 100) # Increase points for smoother circles


Price_circle_packing <- ggplot() + 
  

  geom_polygon(data = dat.gg, aes(x, y, group = id, fill = as.factor(id)), colour = "black", alpha = 0.6) +

  geom_text(data = circlesdata, aes(x, y, label = group), size = ,fontface = "bold",check_overlap = T) +
  
 
  theme_void() + 
  theme(legend.position = "none",
          plot.background = element_rect(fill="lightgrey"),
          plot.title = element_text(color="#1ABC9C") 
  ) + 
  coord_equal() +
  ggtitle("Proportion of Price Ranges") +
  coord_equal()+
  scale_fill_manual(values = c("#8E44AD", "#1ABC9C", "#FF5733", "#00000F", "#de283f", 
                             "#08ff1d", "#32339f", "#b89c00", "#702e58", "#1a3d2a")) 

Price_circle_packing




treegroup <- Fuel_categ
treevalues <- c(57.86, 32.95, 0.36, 3.57, 1.79, 0.27, 3.21)
treedata <- data.frame(treegroup, treevalues)


Fuel_treemap <- ggplot(treedata, aes(area = treevalues, fill = treegroup, label = paste(treegroup,'(', treevalues, '%',')', sep = " "))) +
  geom_treemap() +
  geom_treemap_text(color = "black", place = "centre", grow = TRUE, reflow = TRUE) +
  theme_minimal() +
 # +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color="#1ABC9C"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="black"),
    
    )+
  scale_fill_brewer(palette = "Greens")+
  ggtitle("Proportion of Fuel Types")

Fuel_treemap
###
# SCATTERS-BOXPLOTS
###


CO2_Engine_scatter <-ggplot(DF, aes(x = Engine, y = CO2)) +
  geom_point(aes(color = Fuel), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Engine Size vs. CO2 Emissions",
    x = "Engine Size (cc)",
    y = "CO2 Emissions (g/km)",
    color = "Fuel Type"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color="#ffff66"),
    legend.position = "right",legend.background = element_rect(fill='lightgrey'),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#d9d2e9"))


CO2_Engine_scatter

CO2_Consumption_scatter <- ggplot(DF, aes(x =Consumption, y = CO2)) +
  geom_point(color = "darkred", alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "Relationship Between Fuel Consumption and CO2 Emissions",
       x = "Consumption (L/100km)",
       y = "CO2 (g/km)") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color="red"),
    legend.position = "none",legend.background = element_rect(fill='lightgrey'),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="yellow"),
      panel.background = element_rect(fill='lightyellow'))
  

CO2_Consumption_scatter

CO2_Fuel_boxplot <- ggplot(DF, aes(x = Fuel, y = CO2, fill = Fuel)) +
  geom_boxplot() +
  labs(title = "CO2 Emissions by Fuel Type",
       x = "Fuel Type",
       y = "CO2 Emissions (g/km)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color="#493267"),
    legend.position = "right",legend.background = element_rect(fill='#7bf5ff'),
    #axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#e86af0"),
    panel.background = element_rect(fill='#7bb3ff'))

CO2_Fuel_boxplot




# Define custom price ranges
DF$Price_Range_Label <- cut(DF$Price, 
                            breaks = c(-Inf, 10000, seq(20000, 90000, by = 10000), Inf), 
                            labels = c("<10,000", 
                                       "10,000-20,000", 
                                       "20,000-30,000", 
                                       "30,000-40,000", 
                                       "40,000-50,000", 
                                       "50,000-60,000", 
                                       "60,000-70,000", 
                                       "70,000-80,000", 
                                       "80,000-90,000", 
                                       ">90,000"),
                            right = FALSE) # Exclude upper bound of intervals



CO2_PriceRange_boxplot <- ggplot(DF, aes(x = Price_Range_Label, y = CO2)) +
  geom_boxplot(fill = "#697d5f", color = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle =25, hjust = 1)) +
  labs(title = "CO2 Emissions by Price Range",
       x = "Price Range (€)",
       y = "CO2 Emissions (g/km)")+
  theme_minimal()+

  theme(
         plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color="white"),
         legend.position = "none",legend.background = element_rect(fill='#7bf5ff'),
         #axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis text (if applicable)
         plot.background = element_rect(fill="#96b389"),
         panel.background = element_rect(fill='#b7b6a8'))




CO2_PriceRange_boxplot





# Density plot 
CO2_Fuel_density<- ggplot(DF, aes(x = CO2, fill = Fuel)) +
  geom_density(alpha = 0.7) + # Alpha controls transparency
  labs(
    title = "Density Plot of CO2 Emissions by Fuel Type",
    x = "CO2 Emissions (g/km)",
    y = "Density",
    fill = "Fuel Type"
  ) +
  theme_dark() + 
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color="#b7c9d4"),
    legend.background = element_rect(fill='#b7c9d4'),
    #axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#65737e")
    #,panel.background = element_rect(fill='#b7b6a8'
  ) +
  scale_fill_brewer(palette = "Set1") # Choose a gradient color palette

CO2_Fuel_density




CO2_Horsepower_scatter <- ggplot(DF, aes(x = Horsepower, y = CO2)) +
  geom_point(aes(color = Fuel), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Horsepower vs. CO2 Emissions",
    x = "Horsepower (HP)",
    y = "CO2 Emissions (g/km)",
    color = "Fuel Type"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color="#eae374"),
    legend.position = "right",legend.background = element_rect(fill='#eae374'),
    #axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#b23638"),
    panel.background = element_rect(fill='#e2f4c7'))


CO2_Horsepower_scatter





CO2_Fuel_violin <- ggplot(DF, aes(x = Fuel, y = CO2, fill = Fuel)) +
  geom_violin(alpha = 0.7, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(
    title = "CO2 Emissions by Fuel Type",
    x = "Fuel Type",
    y = "CO2 Emissions (g/km)",
    fill = "Fuel Type"
  ) +
  theme_minimal()+
  scale_fill_brewer(palette = "Set1")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color='white'
),
    legend.position = "right",legend.background = element_rect(fill='#d4e6fe'),
    #axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#25a9b5"),
    panel.background = element_rect(fill='darkgrey'))

CO2_Fuel_violin

CO2_Fuel_jitter <- ggplot(DF, aes(x = Fuel, y = CO2, color = Fuel)) +
  geom_jitter(alpha = 0.6, width = 0.2) +  # Jitter to avoid overlapping points
  geom_boxplot(outlier.shape =NA, alpha = 0.4) +  # Box plot for comparison
  labs(
    title = "CO2 Emissions by Fuel Type",
    x = "Fuel Type",
    y = "CO2 Emissions (g/km)",
    color = "Fuel Type"
  ) +
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color='black'
    ),
    legend.position = "right",legend.background = element_rect(fill='#c9df8a'),
    #axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#77ab59"),
    panel.background = element_rect(fill='#f0f7da'))

CO2_Fuel_jitter

###
# CORELLATION #####
###




# Select continuous variables, excluding categorical ones
continuous_vars <- DF %>% 
  select(Consumption, Engine, Horsepower, CO2, Price, Acceleration,Power_Nm, Autonomy_klm, Taxation)

# Check for numeric types and convert if necessary
continuous_vars <- continuous_vars %>% 
  mutate(across(everything(), as.numeric))

# Remove rows with missing or invalid data
continuous_vars <- continuous_vars %>% na.omit()

# Compute the correlation matrix
cor_matrix <- cor(continuous_vars, use = "complete.obs")

# Visualize with corrplot

corrplot(cor_matrix, method = "circle", type = "full", tl.col = "darkcyan", tl.srt = 45)



install.packages("Hmisc")
install.packages("PerformanceAnalytics")

rcorr(as.matrix(DF_norm),type = 'pearson')
rcorr(as.matrix(DF[,c(2,4:11)]),type = 'pearson')
chart.Correlation(DF[,c(2,4:11)],histogram=TRUE, pch=19)
chart.Correlation(DF_norm,histogram=TRUE, pch=9)


DF_norm <- as.data.frame((DF[, c(2, 4:11)]))
cor(DF_norm)
cor(DF[,c(2,4:11)], use = "complete.obs")
DF_norm

write.csv(DF, "cleaned_data.csv")


## continuous cor
CO2_Consumption_cor <- cor.test(DF$Consumption, DF$CO2, method = "pearson")
CO2_Consumption_cor


## Categorical cor

# Contingency Table


Fuel_Clima_table <- table(DF$Fuel, DF$Clima)
Fuel_Air_condition_table <- table(DF$Fuel, DF$Air_condition)
Fuel_Back_electric_windows_table <- table(DF$Fuel, DF$Back_electric_windows)
Fuel_Heated_mirrors_table <- table(DF$Fuel, DF$Heated_mirrors)





# Chi-Square Test
Fuel_Clima_xtest <- chisq.test(Fuel_Clima_table)
Fuel_Air_condition_xtest <- chisq.test(Fuel_Air_condition_table)
Fuel_Back_electric_windows_xtest <- chisq.test(Fuel_Back_electric_windows_table)
Fuel_Heated_mirrors_xtest <- chisq.test(Fuel_Heated_mirrors_table)





# install.packages("devtools")
devtools::install_github("haleyjeppson/ggmosaic")

# Mosaic Plot
mosaicplot(cat_table, main = "Mosaic Plot of Fuel Type and Air Conditioning", 
           color = TRUE, las = 1)
  
   ggplot(data= DF) +
      geom_mosaic(aes(x = product(Fuel), fill=Clima), show.legend = F) +
      theme_dark()+
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 36,color='white'
        ),
        axis.text.x = element_text(angle = 30, hjust = 1), # Rotate x-axis text (if applicable)
        plot.background = element_rect(fill="#a2d9ce"),
        panel.background = element_rect(fill='black'))
  


###
## NORMALITY
###

###SHAPIRO WILK    
      
shapiro.test(DF$Engine)  
   
DF$log_CO2 <- log(DF$CO2)
shapiro.test(DF$log_CO2)

DF$log_Price <- log(DF$Price)
shapiro.test(DF$log_Price)
 
DF$log_Consumption <- log(DF$Consumption)
shapiro.test(DF$log_Consumption)   

DF$log_Acceleration <- log(DF$Acceleration)
shapiro.test(DF$log_Acceleration)   


DF$sqrt_CO2 <- sqrt(DF$CO2)
shapiro.test(DF$sqrt_CO2)

DF$log_Price <- log(DF$Price)
shapiro.test(DF$log_Price)

### KOLMOGOROV-SMIRNOV

ks.test(DF$log_Consumption, "pnorm")


###non parametric

#Comparing CO2 emissions across multiple fuel types
kruskal.test(CO2 ~ Fuel, data = DF)



# Histogram with density curve
ggplot(DF, aes(x = CO2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram and Density of CO2", x = "CO2 Emissions")

# Q-Q Plots

CO2_qqplot<- ggplot(DF, aes(sample=CO2)) +
  stat_qq(size=2.5, color='red') + 
  stat_qq_line()+
  labs(
    title = "Normal Q-Q Plot for CO2",
    x = "Theoritical Quantiles",
    y = "Sample Quantiles",

  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color='red'
    ),
    axis.text.x = element_text(angle = 30, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#00ff98"),
    panel.background = element_rect(fill='#a2d9ce'))

CO2_qqplot
Engine_qqplot<- ggplot(DF, aes(sample=Engine)) +
  stat_qq(size=2.5, color='#FFC300') + 
  stat_qq_line()+
  labs(
    title = "Normal Q-Q Plot for Engine",
    x = "Theoritical Quantiles",
    y = "Sample Quantiles",
    
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color='#FFC300'
    ),
    axis.text.x = element_text(angle = 30, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#00c9ff"),
    panel.background = element_rect(fill='#d2b4de'))


Engine_qqplot


Horsepower_qqplot <- ggplot(DF, aes(sample=Horsepower)) +
  stat_qq(size=2.5, color='darkgrey') + 
  stat_qq_line()+
  labs(
    title = "Normal Q-Q Plot for Horsepower",
    x = "Theoritical Quantiles",
    y = "Sample Quantiles",
    
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color='black'
    ),
    axis.text.x = element_text(angle = 30, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#92ff0e"),
    panel.background = element_rect(fill='cyan'))



###
### categorical vs contin.
###


# T-test for Air_condition (binary) and CO2
t_test_air_cond <- t.test(CO2 ~ Air_condition, data = DF)
t_test_clima <- t.test(CO2 ~ Clima, data = DF)
t_test_heated_mir <- t.test(CO2 ~ Heated_mirrors, data = DF)
t_test_Back_elec <- t.test(CO2 ~ Back_electric_windows, data = DF)




###ANOVA
###

# ANOVA 
anova_fuel_engine <- aov(Engine ~ Fuel, data = DF)
summary(anova_fuel_engine)

anova_fuel_CO2 <- aov(CO2 ~ Fuel, data = DF)
summary(anova_fuel_engine)

anova_fuel_Horsepower <- aov(Fuel ~ Fuel, data = DF)
summary(anova_fuel_engine)



# Shapiro-Francia test for CO2
sf_test_CO2 <- sf.test(DF$CO2)
sf_test_CO2


###
###
###

is.matrix(DF[2:11])
## Removed from Price the top 5% 
price_threshold <- quantile(D$Price, 0.95, na.rm = TRUE)

DF <- D %>%
  filter(Price <= price_threshold)


### removed from co2 2 obs##
DFF<-DF

DF<-DFF

DF<-DF %>% filter(CO2 < 280)

### removed from engine >3000 (4 obs)##
DF<-DF %>% filter(Engine < 3000)

### removed from Horsepower >360 (5 obs)##
DF<-DF %>% filter(Horsepower < 361)




## FREQUENCIES ##
#vtable-> sumtable(DF, out = "return") 
Desc_Stats<- as.data.frame(summary(DF))
Desc_Stats
table(DF['Fuel']) # frequencies
n_DF<-length(DF[,'Fuel']) # length
n_DF
str(DF)


## RELATIVE FREQUENCIES (%) ##
rel_freq_fuel <- round(table(DF$Fuel)/n_DF,4)*100
round(table(DF$Clima)/n_DF,4)*100
round(table(DF$Air_condition)/n_DF,4)*100
round(table(DF$Heated_mirrors)/n_DF,4)*100
round(table(DF$Back_electric_windows)/n_DF,4)*100

rel_freq_Price_Range_Labels<-round(table(DF$Price_Range_Label)/n_DF,4)*100

sapply(DF, table)



# Save the summary table to an Excel file
write_xlsx(Desc_Stats, "summary_statistics.xlsx",col_names = T)
write.csv(Desc_Stats,file ='Descriptives DF')



####
##    REGRESSION
####





## fuel dummy transition 
DummyDF <- DF
DummyDF$Fuel <- as.factor(DF$Fuel)
DummyDF

head(dummy_vars)
dummy_vars <- model.matrix(~ Fuel - 1, data = DummyDF)
DummyDF$Fuel <- dummy_vars


DummyDF$Dummy_Diesel <- DummyDF$Fuel[,'Fuel Diesel']
DummyDF$Dummy_Diesel_Mild_Hybird <- DummyDF$Fuel[,2]
DummyDF$Dummy_Gasoline <- DummyDF$Fuel[,3]
DummyDF$Dummy_Hybrid_Plug_in <- DummyDF$Fuel[,4]
DummyDF$Dummy_LPG <- DummyDF$Fuel[,5]
DummyDF$Dummy_Mild_Hybird <- DummyDF$Fuel[,6]
DummyDF$Dummy_Hybrid <- DummyDF$Fuel[,7]


backup_DF <- DF


modeldummy <- lm(CO2 ~ Engine  + Consumption + Horsepower + 
                 Dummy_Diesel +
                 Dummy_Diesel_Mild_Hybird +
                 Dummy_Hybrid_Plug_in  +
                 Dummy_LPG  +
                 Dummy_Mild_Hybird +
                 Dummy_Hybrid  
                 , data = DummyDF)

summary(modeldummy)
summary(model1)





model1 <- modeldummy



finalmodel <- update(model1, .~. -Horsepower)
summary(finalmodel)



# επαληθευση
backward_model <- step(model1, direction = "backward")
summary(backward_model)



# 
identical(modeldummy, model1)

#full  model 

modeldummyfull <- lm(CO2 ~ Engine  + Consumption + Horsepower + Price + Taxation + Power_Nm + Acceleration + Autonomy_klm + 
                       
                   Dummy_Diesel +
                   Dummy_Diesel_Mild_Hybird +
                   Dummy_Hybrid_Plug_in  +
                   Dummy_LPG  +
                   Dummy_Mild_Hybird +
                   Dummy_Hybrid +  
                     Air_condition +
                     Clima+ 
                     Back_electric_windows + Heated_mirrors
                 , data = DummyDF)


backward_modeldummyfull <- step(modeldummyfull, direction = "backward")



summary(modeldummyfull)
summary(backward_modeldummyfull)

# resid plot 
plot(finalmodel)
plot(backward_modeldummyfull)

aug_finalmodel <- augment(finalmodel)
aug_finalmodel


ggplot(aug_finalmodel, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_hline(yintercept = 0)




summary(modeldummy)
class(DF$Fuel)


# Compare AIC
AIC(model1, backward_model)
AIC(modeldummyfull, backward_modeldummyfull)


# Check for multicollinearity
vif(backward_modeldummyfull)


###
# regression plots
###



p1
# Create the individual plots
p1 <- ggplot(data = NULL, aes(x = fitted(backward_modeldummyfull), y = residuals(backward_modeldummyfull))) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")+
  theme(
    plot.background = element_rect(fill="#80ced6"),
    panel.background = element_rect(fill='#3e4444'))

p2 <- ggplot(data = NULL, aes(sample = residuals(backward_modeldummyfull))) +
  geom_qq() +
  geom_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals")+
  theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color='black'
  ),
  legend.position = "right",legend.background = element_rect(fill='#c9df8a'),
  #axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis text (if applicable)
  plot.background = element_rect(fill="#80ced6"),
  panel.background = element_rect(fill='#3e4444'))
p3 <- ggplot(data = NULL, aes(x = fitted(backward_modeldummyfull), y = sqrt(abs(residuals(backward_modeldummyfull))))) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Scale-Location", x = "Fitted Values", y = "Sqrt(|Residuals|)")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16,color='black'
    ),
    legend.position = "right",legend.background = element_rect(fill='#c9df8a'),
    #axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis text (if applicable)
    plot.background = element_rect(fill="#80ced6"),
    panel.background = element_rect(fill='#3e4444'))

p4 <- ggplot(data = NULL, aes(x = seq_along(residuals(backward_modeldummyfull)), y = cooks.distance(backward_modeldummyfull))) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(title = "Cook's Distance", x = "Index", y = "Cook's Distance")+
  theme(
   plot.background = element_rect(fill="#80ced6"),
    panel.background = element_rect(fill='#3e4444'))

# Use grid.arrange to display the plots side by side
grid.arrange(p1, p2, p3, p4, ncol = 2)






