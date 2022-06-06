# University of Nottingham, School of Computer Science
# Data Modelling and Analysis (COMP4030 UNUK) (SPR1 21-22)
# Cheong Beng Chuan, 20218995 
# Khor Yong Teng, Khor Yong Teng

##### Installing Required Packages #####
if (!require("dplyr")) install.packages('dplyr')
if (!require("ggplot2")) install.packages('ggplot2')
if (!require("tidyverse")) install.packages('tidyverse')
if (!require("scales")) install.packages('scales')
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("caret")) install.packages("caret")
if (!require("latentcor")) install.packages("latentcor")
if (!require("reshape2")) install.packages("reshape2")
if (!require("plotly")) install.packages('plotly')
if (!require("FactoMineR")) install.packages("FactoMineR")
if (!require("factoextra")) install.packages("factoextra")
if (!require("ggfortify")) install.packages("ggfortify")
if (!require("plotly")) install.packages("plotly")
if (!require("e1071")) install.packages("e1071")
if (!require("class")) install.packages("class")
if (!require("ggpubr")) install.packages("ggpubr")

###### Loading Packages ######
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(gridExtra)
library(ggpubr)
library(caret)
library(latentcor)
library(reshape2)
library(plotly)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(plotly)
library(e1071)
library(class)
library(ggpubr)

##### Exploratory Data Analysis #####

#Loading Data
heart = read.csv('heart_2020_cleaned.csv', header = TRUE, stringsAsFactors = TRUE)
View(heart)

#Checking Datatype of Columns
sapply(heart, class)

#Checking Columns for Missing Values
sapply(heart, function(x) sum(is.na(x)))

#Boxplot of Numerical Variables
boxplot(heart[, c("BMI", "PhysicalHealth", "MentalHealth", "SleepTime")], main = "Boxplot of Continuous Values")

#Barplot of HeartDisease Class Ratios
ggplot(heart, aes(x=HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  ggtitle("HeartDisease Class Ratio") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none") + 
  scale_y_continuous(labels = comma)

#Piechart of HeartDisease Class Ratios
plot_ly(data.frame(table(heart$HeartDisease)), 
        text = ~Var1, 
        labels = ~Var1, 
        textfont = list(color = "white", size = 20), 
        marker = list(colors = c('#F8766D', '#00BFC4')), 
        height = 550, 
        values = ~Freq) %>% 
  add_pie(hole = 0.5) %>%
  layout(showlegend = F, 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = list(l=0,r=0,b=0,t=0),
         plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)",
         annotations = list(text=paste("HeartDisease\n Ratio"), "showarrow"=F, font=list(size = 25, color = "grey")))

#Kernel Density Plots of Numerical Variables Separated by HeartDisease
plotNumerical = function(df, columnName, groupName) {
  ggplot(df, aes_string(x = columnName, fill = groupName)) +
    geom_density(alpha=0.4)
}

d_list = list()
d_list[[1]] = plotNumerical(heart, "BMI", "HeartDisease")
d_list[[2]] = plotNumerical(heart, "PhysicalHealth", "HeartDisease")
d_list[[3]] = plotNumerical(heart, "MentalHealth", "HeartDisease")
d_list[[4]] = plotNumerical(heart, "SleepTime", "HeartDisease")
plot = ggarrange(plotlist = d_list, ncol = 1  , nrow = 4, common.legend = TRUE, legend="right")
annotate_figure(plot, top = text_grob("Kernel Density Plots separated by HeartDisease", face = "bold", size = 14))

#Histogram Plots of Numerical Variables Separated by HeartDisease
plotNumerical = function(df, columnName, groupName) {
  ggplot(df, aes_string(x = columnName, fill = groupName)) +
    geom_histogram(position = "dodge", binwidth = 1)
}
h_list = list()
h_list[[1]] = plotNumerical(heart, "BMI", "HeartDisease")
h_list[[2]] = plotNumerical(heart, "PhysicalHealth", "HeartDisease")
h_list[[3]] = plotNumerical(heart, "MentalHealth", "HeartDisease")
h_list[[4]] = plotNumerical(heart, "SleepTime", "HeartDisease")
plot = ggarrange(plotlist = h_list, ncol = 1  , nrow = 4, common.legend = TRUE, legend="right")
annotate_figure(plot, top = text_grob("Histogram Plots separated by HeartDisease", face = "bold", size = 14))

#Changing Factor levels of GenHealth to be Ordered Correctly
heart$GenHealth = factor(heart$GenHealth, levels=c("Poor", "Fair", "Good", "Very good", "Excellent"))

#Barplots of Categorical Variables according to HeartDisease
plotCategories = function(df, columnName, groupName, dodge = FALSE){
  tbl = dplyr::arrange_(data.frame(with(df, table(df[,columnName], df[,groupName], dnn = list(columnName, groupName)))),.dots = c(columnName,groupName))
  
  for( i in 1:nrow(tbl)) {
    if(!(i %% 2)) {
      tbl$Percentage[i]=tbl$Freq[i]/(tbl$Freq[i-1] + tbl$Freq[i])
    } else {
      tbl$Percentage[i]=tbl$Freq[i]/(tbl$Freq[i+1] + tbl$Freq[i])
    }
  }
  
  p = ggplot(tbl, aes_string(factor(tbl[,columnName]), "Freq", fill = groupName)) +     
    geom_col(position = 'dodge') +
    geom_text(aes(label = sprintf("%.1f%%", round(Percentage*100, digits = 2))), position=position_dodge(width=0.9), size = 4,vjust=ifelse(tbl$Freq>max(tbl$Freq)*0.9,1.5,-0.5))+
    xlab(columnName) +
    scale_y_continuous(labels = comma)
  
  if (dodge == TRUE){
    p = p + scale_x_discrete(guide = guide_axis(n.dodge=2))
  }
  return(p)
}

b_list = list()
i = 1
for( j in c("Smoking", "AlcoholDrinking", "Stroke", "DiffWalking", "Sex", "PhysicalActivity", "Asthma", "KidneyDisease",  "SkinCancer","AgeCategory", "Race","Diabetic", "GenHealth") ) {
  if(j == "AgeCategory" || j == "Race") {
    b_list[[i]] = plotCategories(heart, j, "HeartDisease", dodge = TRUE)
  } else {
    b_list[[i]] = plotCategories(heart, j, "HeartDisease")
  }
  i = i+1
}
rm(i)
plot = ggarrange(plotlist = b_list[1:9], ncol = 2  , nrow = 5, common.legend = TRUE, legend="right")
annotate_figure(plot, top = text_grob("Barplots of Categorical Variables separated by HeartDisease", face = "bold", size = 14))
plot = ggarrange(plotlist = b_list[10:13], ncol = 1  , nrow = 4, common.legend = TRUE, legend="right")
annotate_figure(plot, top = text_grob("Barplots of Categorical Variables separated by HeartDisease", face = "bold", size = 14))



##### Data Preprocessing #####

#Changing AgeCategory and GenHealth to numerical values
heart$AgeCategory = as.character(heart$AgeCategory)
heart[heart$AgeCategory == "18-24","AgeCategory"] = 1
heart[heart$AgeCategory == "25-29","AgeCategory"] = 2
heart[heart$AgeCategory == "30-34","AgeCategory"] = 3
heart[heart$AgeCategory == "35-39","AgeCategory"] = 4
heart[heart$AgeCategory == "40-44","AgeCategory"] = 5
heart[heart$AgeCategory == "45-49","AgeCategory"] = 6
heart[heart$AgeCategory == "50-54","AgeCategory"] = 7
heart[heart$AgeCategory == "55-59","AgeCategory"] = 8
heart[heart$AgeCategory == "60-64","AgeCategory"] = 9
heart[heart$AgeCategory == "65-69","AgeCategory"] = 10
heart[heart$AgeCategory == "70-74","AgeCategory"] = 11
heart[heart$AgeCategory == "75-79","AgeCategory"] = 12
heart[heart$AgeCategory == "80 or older","AgeCategory"] = 13
heart$AgeCategory = as.integer(heart$AgeCategory)

heart$GenHealth = as.character(heart$GenHealth)
heart[heart$GenHealth == "Poor","GenHealth"] = 1
heart[heart$GenHealth == "Fair","GenHealth"] = 2
heart[heart$GenHealth == "Good","GenHealth"] = 3
heart[heart$GenHealth == "Very good","GenHealth"] = 4
heart[heart$GenHealth == "Excellent","GenHealth"] = 5
heart$GenHealth = as.integer(heart$GenHealth)

#Creating dataframe where Categorical Columns are converted into Numeric Columns
heart2 = heart
#HeartDisease
heart2$HeartDisease = as.character(heart2$HeartDisease)
heart2$HeartDisease = ifelse(heart2$HeartDisease=="Yes", 1, 0)
heart2$HeartDisease = as.integer(heart2$HeartDisease)

#Smoking
heart2$Smoking = as.character(heart2$Smoking)
heart2$Smoking = ifelse(heart2$Smoking=="Yes", 1, 0)
heart2$Smoking = as.integer(heart2$Smoking)

#AlcoholDrinking
heart2$AlcoholDrinking = as.character(heart2$AlcoholDrinking)
heart2$AlcoholDrinking = ifelse(heart2$AlcoholDrinking=="Yes", 1, 0)
heart2$AlcoholDrinking = as.integer(heart2$AlcoholDrinking)

#Stroke
heart2$Stroke = as.character(heart2$Stroke)
heart2$Stroke = ifelse(heart2$Stroke=="Yes", 1, 0)
heart2$Stroke = as.integer(heart2$Stroke)

#DiffWalking
heart2$DiffWalking = as.character(heart2$DiffWalking)
heart2$DiffWalking = ifelse(heart2$DiffWalking=="Yes", 1, 0)
heart2$DiffWalking = as.integer(heart2$DiffWalking)

#PhysicalActivity
heart2$PhysicalActivity = as.character(heart2$PhysicalActivity)
heart2$PhysicalActivity = ifelse(heart2$PhysicalActivity=="Yes", 1, 0)
heart2$PhysicalActivity = as.integer(heart2$PhysicalActivity)

#Asthma
heart2$Asthma = as.character(heart2$Asthma)
heart2$Asthma = ifelse(heart2$Asthma=="Yes", 1, 0)
heart2$Asthma = as.integer(heart2$Asthma)

#KidneyDisease
heart2$KidneyDisease = as.character(heart2$KidneyDisease)
heart2$KidneyDisease = ifelse(heart2$KidneyDisease=="Yes", 1, 0)
heart2$KidneyDisease = as.integer(heart2$KidneyDisease)

#SkinCancer
heart2$SkinCancer = as.character(heart2$SkinCancer)
heart2$SkinCancer = ifelse(heart2$SkinCancer=="Yes", 1, 0)
heart2$SkinCancer = as.integer(heart2$SkinCancer)

#Diabetic
#Added New Variables to make Categories of Diabetic Binary
heart2$Diabetic = as.character(heart2$Diabetic)
heart2 = heart2 %>%
  mutate(Diabetic_Pregnancy = ifelse(heart2$Diabetic == "Yes (during pregnancy)", 1, 0)) %>%
  mutate(Diabetic_Borderline = ifelse(heart2$Diabetic == "No, borderline diabetes", 1, 0))
heart2[heart2$Diabetic == "No","Diabetic"] = 0
heart2[heart2$Diabetic == "Yes","Diabetic"] = 1
heart2[heart2$Diabetic == "No, borderline diabetes","Diabetic"] = 0
heart2[heart2$Diabetic == "Yes (during pregnancy)","Diabetic"] = 0
heart2$Diabetic = as.integer(heart2$Diabetic)
heart2$Diabetic_Pregnancy = as.integer(heart2$Diabetic_Pregnancy)
heart2$Diabetic_Borderline = as.integer(heart2$Diabetic_Borderline)

#Race
heart2$Race = as.character(heart2$Race)
heart2 = heart2 %>%
  mutate(Race_White = ifelse(heart2$Race == "White", 1, 0)) %>%
  mutate(Race_Black = ifelse(heart2$Race == "Black", 1, 0)) %>%
  mutate(Race_Asian = ifelse(heart2$Race == "Asian", 1, 0)) %>%
  mutate(Race_Indian_Native = ifelse(heart2$Race == "American Indian/Alaskan Native", 1, 0)) %>%
  mutate(Race_Hispanic = ifelse(heart2$Race == "Hispanic", 1, 0)) %>%
  mutate(Race_Other = ifelse(heart2$Race == "Other", 1, 0))
heart2$Race_White = as.integer(heart2$Race_White)
heart2$Race_Black = as.integer(heart2$Race_Black)
heart2$Race_Asian = as.integer(heart2$Race_Asian)
heart2$Race_Indian_Native = as.integer(heart2$Race_Indian_Native)
heart2$Race_Hispanic = as.integer(heart2$Race_Hispanic)
heart2$Race_Other = as.integer(heart2$Race_Other)
heart2 = select(heart2,-c(Race))

#Sex
heart2$Sex = as.character(heart2$Sex)
heart2 = heart2 %>%
  mutate(Sex_Male = ifelse(heart2$Sex == "Male", 1, 0)) %>%
  mutate(Sex_Female = ifelse(heart2$Sex == "Female", 1, 0))
heart2 = select(heart2,-c(Sex))
heart2$Sex_Male = as.integer(heart2$Sex_Male)
heart2$Sex_Female = as.integer(heart2$Sex_Female)

###### Pair Plots of Numeric Variables #####
ind = which(sapply(heart, is.numeric))
group = NA
group[heart$HeartDisease == "No"] = 1
group[heart$HeartDisease == "Yes"] = 2
my_cols = c("green", "red")  
pp = pairs(heart[, ind], pch = 20, col = alpha(my_cols[group], 0.4), lower.panel = NULL, main="Paired Scatterplot of Numerical Values")
pp


##### Correlation Plots #####

# Pearson's Correlation
heart_cor = data.frame(sapply(heart, unclass) )
sapply(heart_cor,class)
cal_cor = function(data) {
  cor = data.frame(Attribute = colnames(data))
  
  for( i in colnames(data) ){
    for( j in colnames(dplyr::select(data,-i))){
      cor[cor$Attribute == j,i] = cor(data[,i],data[,j])
    }
  }
  return(cor)
}

pearsons_correlation = cal_cor(heart_cor)

#Heatmap for Pearson's Correlation
cor_hm = function(data,title_) {
  hm_cor = data.matrix(dplyr::select(data,-Attribute))
  row.names(hm_cor) = colnames(hm_cor)
  hm_cor = melt(hm_cor)
  
  suppressWarnings(
    print(
      ggplot(hm_cor, aes(Var1, Var2)) + 
        geom_tile(aes(fill = abs(value))) + 
        geom_text(aes(label = ifelse(is.na(value), "",sprintf("%0.3f", round(value, digits = 2)))),
                  colour="white",
                  na.colour="lightgrey") +
        scale_fill_gradientn(colours=c("LightSalmon","DarkOrange",
                                       "#DC143C", "FireBrick", "DarkRed"),
                             values=scales::rescale(c(0,0.2,0.4,0.6,0.8)),
                             na.value="lightgrey") +
        coord_fixed() +
        scale_y_discrete(limits=rev) +
        scale_x_discrete(position = "top") +
        ggtitle(ifelse(missing(title_),"",title_))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              plot.background = element_rect(fill = "transparent"),
              legend.position="none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
      
    )
  )
}

cor_hm(filter(select(pearsons_correlation,Attribute, HeartDisease,BMI,PhysicalHealth,MentalHealth,SleepTime), 
                     Attribute %in% c("HeartDisease","BMI","PhysicalHealth","MentalHealth","SleepTime")),
              "Heatmap of Pearson's Correlation for Numeric Variables")


# Pearson's Chi square
cal_chi = function(data) {
  chi = data.frame(Attribute = colnames(data))
  suppressWarnings(
    for( i in colnames(data) ){
      for( j in colnames(dplyr::select(data,-i))){
        temp = chisq.test(table(data[,i] ,data[,j]))
        chi[chi$Attribute == j,i] = temp$statistic
      }
    } )
  return(chi)
}

c2_cat = cal_chi(dplyr::select(heart,HeartDisease, Diabetic, Race,AgeCategory,GenHealth, Sex, SleepTime ))
c2_bin = cal_chi(dplyr::select(heart, -Diabetic, -Race,-AgeCategory,-GenHealth, -BMI,-PhysicalHealth,-MentalHealth, -SleepTime ))

#Heatmap for Person's Chisquare
chi_hm = function(data, title_) {
  
  hm_chi = data.matrix(dplyr::select(data,-Attribute))
  row.names(hm_chi) = colnames(hm_chi)
  hm_chi = melt(hm_chi)
  suppressWarnings(
    print(
      ggplot(hm_chi, aes(Var1, Var2)) + 
        geom_tile(aes(fill = value)) + 
        geom_text(aes(label = ifelse(is.na(value), "",sprintf("%.0f", round(value, digits = 2)))),
                  colour="white",
                  na.colour="lightgrey") +
        scale_fill_gradientn(colours=c("LightSalmon","DarkOrange",
                                       "#DC143C", "FireBrick", "DarkRed"),
                             values=rescale(c(0, 500,
                                              5000, 15000, 25000)),
                             na.value="lightgrey") +
        coord_fixed() +
        scale_y_discrete(limits=rev) +
        scale_x_discrete(position = "top") +
        ggtitle(ifelse(missing(title_),"",title_))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              plot.background = element_rect(fill = "transparent"),
              legend.position="none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
      
    )
  )
}

chi_hm(c2_cat,"Heatmap of Chi Square for Categorical Variables")
chi_hm(c2_bin,"Heatmap of Chi Square for Binary Variables")

# Correlation Estimates (Mixed Data)
types = get_types(heart2)
types[types == "tru"] = "con"
corelations = latentcor::latentcor(X = heart2, types = types, showplot = TRUE)
corelations$plotR


##### PCA #####
#Standardizing the Variables
s = scale(heart2[,2:26])
stand_scale = attributes(s)$'scaled:scale'
stand_center = attributes(s)$'scaled:center'
heart2.stand = as.data.frame(s) %>% mutate(HeartDisease = heart$HeartDisease)
pca = prcomp(heart2.stand[,1:25], scale=T)

#PCA summary
summary(pca)

#PCA Scree Plot
fviz_eig(pca, addlabels = TRUE, ncp=15, main = "PCA Scree Plot")

#PCA Biplot
autoplot(pca, data = heart2.stand, colour = 'HeartDisease', loadings = TRUE, loadings.label = TRUE, alpha=0.3, loadings.colour = "blue", loadings.label.colour = "blue") +
  ggtitle("PCA Biplot")

#Transforming Heart Variables into PCA Principal Components
heart2.pca = as.data.frame(predict(pca, heart2.stand[,1:25])) %>% mutate(HeartDisease = heart2.stand$HeartDisease)

#Pair Plot of PCA Principal Components
group = NA
group[heart2.pca$HeartDisease == "No"] = 1
group[heart2.pca$HeartDisease == "Yes"] = 2
my_cols = c("green", "red")  
pcapp = pairs(heart2.pca[,1:6], pch = 20, col = alpha(my_cols[group], 0.1), lower.panel = NULL, main = "Paired Scatterplot of PCA Principal Components")
pcapp

#PCA 3D Scatterplot
plot_ly(heart2.pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~HeartDisease, colors = c('#636EFA','#EF553B')) %>% 
  add_trace(marker = list(size = 2), showlegend=F) %>%
  add_trace(marker = list(size = 15), showlegend=T, visible="legendonly") %>%
  layout(title = 'PCA 3D Scatterplot', legend = list(font = list(size = 15)))


##### FAMD #####

#Standardizing Numerical Variable
heart.famd.s = heart
ind = sapply(heart.famd.s, is.numeric)
heart.famd.s[ind] <- lapply(heart.famd.s[ind], scale)
famd = FAMD(heart.famd.s[,2:18], ncp = 15, graph = FALSE)

#FAMD summary
summary(famd)

#FAMD Scree Plot
fviz_eig(famd, addlabels = TRUE, ncp=15, main = "FAMD Scree Plot")

#Transforming Heart Variables into Dimension Components
heart.famd = data.frame(HeartDisease = heart$HeartDisease, predict(famd, heart.famd.s[,2:18])$coord) %>% `rownames<-`(seq_len(nrow(heart)))

#FAMD Square Loading Plot
fviz_famd_var(famd, 'var',axes = c(1, 2), col.var = 'cos2') +
  labs(title ="FAMD Square Loading Plot")

#FAMD 2D Scatterplot
ggplot(heart.famd, aes(Dim.1, Dim.2, color=HeartDisease)) +
  ggtitle("FAMD Scatterplot") +
  geom_point(alpha = 0.3)

#Pair Plot of FAMD Principal Components
group = NA
group[heart.famd$HeartDisease == "No"] = 1
group[heart.famd$HeartDisease == "Yes"] = 2
my_cols = c("green", "red")  
famdpp = pairs(heart.famd[,2:7], pch = 20, col = alpha(my_cols[group], 0.1), lower.panel = NULL, main = "Paired Scatterplot of FAMD Principal Components")
famdpp

#FAMD 3D Scatterplot
plot_ly(heart.famd, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, color = ~HeartDisease, colors = c('#636EFA','#EF553B')) %>% 
  add_trace(marker = list(size = 2), showlegend=F) %>%
  add_trace(marker = list(size = 15), showlegend=T, visible="legendonly") %>%
  layout(title = 'FAMD 3D Scatterplot', legend = list(font = list(size = 15)))


##### Cross validation of Models #####

# Creating Folds
heart2$HeartDisease = heart$HeartDisease
set.seed(123)
folds = createFolds(heart$HeartDisease, k = 10)

# SVM, KNN and Naive Bayes using Transformed Original Data
results_original_svm = data.frame(HeartDisease = factor(), Prediction = factor())
results_original_knn = data.frame(HeartDisease = factor(), Prediction = factor())
results_original_nb = data.frame(HeartDisease = factor(), Prediction = factor())
for (fold in folds) {
  test_set = heart2[fold, ]
  train_set = heart2[-fold, ]
  
  train_set = as.data.frame(downSample(train_set, train_set$HeartDisease))[,1:26]
  
  s = scale(train_set[,2:26])
  
  stand_scale = attributes(s)$'scaled:scale'
  stand_center = attributes(s)$'scaled:center'
  
  train_set.stand = as.data.frame(s) %>%
    mutate(HeartDisease = train_set$HeartDisease)
  
  test_set.stand = as.data.frame(scale(test_set[,2:26], scale = stand_scale, center = stand_center)) %>%
    mutate(HeartDisease = test_set$HeartDisease)
  
  svmfit = svm(HeartDisease~., data = train_set.stand, kernel = "radial", cost = 10, scale = FALSE)
  results_original_svm = rbind(results_original_svm, test_set.stand %>% mutate(Prediction = predict(svmfit, newdata = test_set.stand)) %>% select(c("HeartDisease", "Prediction")))
  
  results_original_knn = rbind(results_original_knn, test_set.stand %>% 
                                 mutate(Prediction = knn(train_set.stand[1:25], test_set.stand[1:25], cl = train_set.stand$HeartDisease, k=81)) %>% 
                                 select(c("HeartDisease", "Prediction")))
  
  nb_fit = naiveBayes(HeartDisease ~.,data = train_set.stand,laplace=1)
  results_original_nb = rbind(results_original_nb, test_set.stand %>% mutate(Prediction = predict(nb_fit, newdata = test_set.stand,type = "class")) %>% select(c("HeartDisease", "Prediction")))
}

#SVM, KNN and Naive Bayes with PCA
results_pca_svm = data.frame(HeartDisease = factor(), Prediction = factor())
results_pca_knn = data.frame(HeartDisease = factor(), Prediction = factor())
results_pca_nb = data.frame(HeartDisease = factor(), Prediction = factor())


for (fold in folds) {
  test_set = heart2[fold, ]
  train_set = heart2[-fold, ]
  
  train_set = as.data.frame(downSample(train_set, train_set$HeartDisease))[,1:26]
  
  s = scale(train_set[,2:26])
  
  stand_scale = attributes(s)$'scaled:scale'
  stand_center = attributes(s)$'scaled:center'
  
  train_set.stand = as.data.frame(scale(train_set[,2:26]))
  pca = prcomp(train_set.stand,scale=T)
  
  train_set.pca = as.data.frame(predict(pca, train_set.stand)[,1:6]) %>%
    mutate(HeartDisease = train_set$HeartDisease)
  
  test_set.stand = as.data.frame(scale(test_set[,2:26], scale = stand_scale, center = stand_center))
  test_set.pca = as.data.frame(predict(pca, test_set.stand)[,1:6]) %>%
    mutate(HeartDisease = test_set$HeartDisease)
  
  svmfit = svm(HeartDisease~., data = train_set.pca, kernel = "radial", cost = 10, scale = FALSE)
  nb_fit = naiveBayes(HeartDisease ~.,data = train_set.pca,laplace=1)
  results_pca_svm = rbind(results_pca_svm, test_set.pca %>% mutate(Prediction = predict(svmfit, newdata = test_set.pca)) %>% select(c("HeartDisease", "Prediction")))
  
  results_pca_knn = rbind(results_pca_knn, test_set.pca %>% 
                            mutate(Prediction = knn(train_set.pca[1:6], test_set.pca[1:6], cl = train_set.pca$HeartDisease, k=81)) %>% 
                            select(c("HeartDisease", "Prediction")))
  
  results_pca_nb = rbind(results_pca_nb, test_set.pca %>% mutate(Prediction = predict(nb_fit, newdata = test_set.pca,type = "class")) %>% select(c("HeartDisease", "Prediction")))
}

# SVM, KNN and Naive Bayes with FAMD
results_famd_svm = data.frame(HeartDisease = factor(), Prediction = factor())
results_famd_knn = data.frame(HeartDisease = factor(), Prediction = factor())
results_famd_nb = data.frame(HeartDisease = factor(), Prediction = factor())
for (fold in folds) {
  test_set = heart[fold, ]
  train_set = heart[-fold, ]
  
  train_set = as.data.frame(downSample(train_set, train_set$HeartDisease))[,1:18]
  
  ind = sapply(train_set, is.numeric)
  
  train_set[ind] <- lapply(train_set[ind], scale)
  train_set.stand = train_set
  famd = FAMD(train_set.stand[,2:18], ncp = 6, graph = FALSE)
  
  train_set.famd = as.data.frame(predict(famd, train_set.stand)$coord) %>%
    mutate(HeartDisease = train_set$HeartDisease)
  
  test_set[ind] <- lapply(test_set[ind], scale)
  test_set.stand = test_set
  test_set.famd = as.data.frame(predict(famd, test_set.stand)$coord) %>%
    mutate(HeartDisease = test_set$HeartDisease)
  
  svmfit = svm(HeartDisease~., data = train_set.famd, kernel = "radial", cost = 10, scale = FALSE)
  nb_fit = naiveBayes(HeartDisease ~.,data = train_set.famd,laplace=1)
  results_famd_svm = rbind(results_famd_svm, test_set.famd %>% mutate(Prediction = predict(svmfit, newdata = test_set.famd)) %>% select(c("HeartDisease", "Prediction")))
  
  results_famd_knn = rbind(results_famd_knn, test_set.famd %>% 
                             mutate(Prediction = knn(train_set.famd[1:6], test_set.famd[1:6], cl = train_set.famd$HeartDisease, k=81)) %>% 
                             select(c("HeartDisease", "Prediction")))
  
  results_famd_nb = rbind(results_famd_nb, test_set.famd %>% mutate(Prediction = predict(nb_fit, newdata = test_set.famd,type = "class")) %>% select(c("HeartDisease", "Prediction")))
}

##### Result Evaluation #####

#Metrics Tabulation
metricsTabulation = function(data,results) {
    x = confusionMatrix(results$Prediction,results$HeartDisease, mode = "everything",positive = "Yes")
  if(is.null(data)) {
    data = data.frame(Accuracy = x$overall[[1]], Precision = x$byClass[[5]], Recall = x$byClass[[6]], F1 = x$byClass[[7]] )
    rownames(data) = substr(deparse(substitute(results)), 9, nchar(deparse(substitute(results))))
  } else {
    x = data.frame(Accuracy = x$overall[[1]], Precision = x$byClass[[5]], Recall = x$byClass[[6]], F1 = x$byClass[[7]] )
    rownames(x) = substr(deparse(substitute(results)), 9, nchar(deparse(substitute(results))))
    data = rbind(data,x)
  }
  
  return(data)
}

resultMetrics = metricsTabulation(NULL, results_original_svm)
resultMetrics = metricsTabulation(resultMetrics, results_original_knn)
resultMetrics = metricsTabulation(resultMetrics, results_original_nb)
resultMetrics = metricsTabulation(resultMetrics, results_pca_svm)
resultMetrics = metricsTabulation(resultMetrics, results_pca_knn)
resultMetrics = metricsTabulation(resultMetrics, results_pca_nb)
resultMetrics = metricsTabulation(resultMetrics, results_famd_svm)
resultMetrics = metricsTabulation(resultMetrics, results_famd_knn)
resultMetrics = metricsTabulation(resultMetrics, results_famd_nb)


# Result Visualisation
T_os = confusionMatrix(results_original_svm$Prediction,results_original_svm$HeartDisease, mode = "everything",positive = "Yes")$table
T_os = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_os[2,2],T_os[1,1],T_os[2,1],T_os[1,2]))
T_ok = confusionMatrix(results_original_knn$Prediction,results_original_knn$HeartDisease, mode = "everything",positive = "Yes")$table
T_ok = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_ok[2,2],T_ok[1,1],T_ok[2,1],T_ok[1,2]))
T_on = confusionMatrix(results_original_nb$Prediction,results_original_nb$HeartDisease, mode = "everything",positive = "Yes")$table
T_on = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_on[2,2],T_on[1,1],T_on[2,1],T_on[1,2]))

T_ps = confusionMatrix(results_pca_svm$Prediction,results_pca_svm$HeartDisease, mode = "everything",positive = "Yes")$table
T_ps = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_ps[2,2],T_ps[1,1],T_ps[2,1],T_ps[1,2]))
T_pk = confusionMatrix(results_pca_knn$Prediction,results_pca_knn$HeartDisease, mode = "everything",positive = "Yes")$table
T_pk = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_pk[2,2],T_pk[1,1],T_pk[2,1],T_pk[1,2]))
T_pn = confusionMatrix(results_pca_nb$Prediction,results_pca_nb$HeartDisease, mode = "everything",positive = "Yes")$table
T_pn = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_pn[2,2],T_pn[1,1],T_pn[2,1],T_pn[1,2]))

T_fs = confusionMatrix(results_famd_svm$Prediction,results_famd_svm$HeartDisease, mode = "everything",positive = "Yes")$table
T_fs = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_fs[2,2],T_fs[1,1],T_fs[2,1],T_fs[1,2]))
T_fk = confusionMatrix(results_famd_knn$Prediction,results_famd_knn$HeartDisease, mode = "everything",positive = "Yes")$table
T_fk = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_fk[2,2],T_fk[1,1],T_fk[2,1],T_fk[1,2]))
T_fn = confusionMatrix(results_famd_nb$Prediction,results_famd_nb$HeartDisease, mode = "everything",positive = "Yes")$table
T_fn = data.frame(Var1 = c("TP","TN","FP","FN"), Freq =c(T_fn[2,2],T_fn[1,1],T_fn[2,1],T_fn[1,2]))

plot_ly(labels = ~Var1,textfont = list(color = "white", size = 16),outsidetextfont = list(color = "black", size = 14),
        height = 550,values = ~Freq, textinfo = 'percent', 
        marker = list(colors = c('#00BFC4', '#F8766D','#0072BB', '#FF8C00'),
                      line = list(color = '#FFFFFF', width = 0))) %>% 
  add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.385,0.615), y = c(0.5,1)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.135,0.365), y = c(0,0.5)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
  add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.635,0.865), y = c(0,0.5)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
  add_pie(data = T_os, name = "SVM", direction='clockwise', sort=FALSE, domain = list(x=c(0.25,0.75), y = c(0.5,1)),hole = 0.5)%>%
  add_pie(data = T_ok, name = "KNN", direction='clockwise', sort=FALSE, domain = list(x=c(0,0.5), y = c(0,0.5)),hole = 0.5)%>%
  add_pie(data = T_on, name = "NB", direction='clockwise', sort=FALSE, domain = list(x=c(0.5,1), y = c(0,0.5)),hole = 0.5)%>%
  layout(showlegend = T, title=list(text = "Original Transform",font = list(size = 25,color = "black")) ,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)",
         margin = list(t = 50),
         annotations=list(x = c(0.5,0.2,0.785),
                          y = c(0.78,0.215,0.215),
                          text=c("SVM","KNN","NB"), "showarrow"=F, font=list(size = 22, color = "#48594B"))
  )

plot_ly(labels = ~Var1,textfont = list(color = "white", size = 16),outsidetextfont = list(color = "black", size = 14),
        height = 550,values = ~Freq, textinfo = 'percent', 
        marker = list(colors = c('#00BFC4', '#F8766D','#0072BB', '#FF8C00'),
                      line = list(color = '#FFFFFF', width = 0))) %>% 
  add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.385,0.615), y = c(0.5,1)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
  add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.135,0.365), y = c(0,0.5)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
  add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.635,0.865), y = c(0,0.5)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
  add_pie(data = T_ps, name = "SVM", direction='clockwise', sort=FALSE, domain = list(x=c(0.25,0.75), y = c(0.5,1)),hole = 0.5)%>%
  add_pie(data = T_pk, name = "KNN", direction='clockwise', sort=FALSE, domain = list(x=c(0,0.5), y = c(0,0.5)),hole = 0.5)%>%
  add_pie(data = T_pn, name = "NB", direction='clockwise', sort=FALSE, domain = list(x=c(0.5,1), y = c(0,0.5)),hole = 0.5)%>%
  layout(showlegend = T, title=list(text = "PCA Transform",font = list(size = 25,color = "black")) ,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)",
         margin = list(t = 50),
         annotations=list(x = c(0.5,0.2,0.785),
                          y = c(0.78,0.215,0.215),
                          text=c("SVM","KNN","NB"), "showarrow"=F, font=list(size = 22, color = "#48594B"))
  )

plot_ly(labels = ~Var1,textfont = list(color = "white", size = 16),outsidetextfont = list(color = "black", size = 14),
        height = 550,values = ~Freq, textinfo = 'percent', 
        marker = list(colors = c('#00BFC4', '#F8766D','#0072BB', '#FF8C00'),
                      line = list(color = '#FFFFFF', width = 0))) %>% 
  add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.385,0.615), y = c(0.5,1)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
  add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.135,0.365), y = c(0,0.5)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
  add_pie(data = data.frame(Var1 = 1, Freq = 1), domain = list(x=c(0.635,0.865), y = c(0,0.5)),textfont=list(color = "#D6CFC7"),marker=list(colors="#D6CFC7"),showlegend = F)%>%
  add_pie(data = T_fs, name = "SVM", direction='clockwise', sort=FALSE, domain = list(x=c(0.25,0.75), y = c(0.5,1)),hole = 0.5)%>%
  add_pie(data = T_fk, name = "KNN", direction='clockwise', sort=FALSE, domain = list(x=c(0,0.5), y = c(0,0.5)),hole = 0.5)%>%
  add_pie(data = T_fn, name = "NB", direction='clockwise', sort=FALSE, domain = list(x=c(0.5,1), y = c(0,0.5)),hole = 0.5)%>%
  layout(showlegend = T, title=list(text = "FAMD Transform",font = list(size = 25,color = "black")) ,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)",
         margin = list(t = 50),
         annotations=list(x = c(0.5,0.2,0.785),
                          y = c(0.78,0.215,0.215),
                          text=c("SVM","KNN","NB"), "showarrow"=F, font=list(size = 22, color = "#48594B"))
  )


