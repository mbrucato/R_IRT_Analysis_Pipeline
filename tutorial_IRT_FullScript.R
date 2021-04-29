#----------------------------------------------
# Dichotomous Item Response Theory Tutorial
# author: mbrucato@temple.edu
#----------------------------------------------
#set working directory
setwd("C:/Users/maria/Desktop/IRT_Tutorial")

#load in packages
library(ltm)
data(LSAT)
nrow(LSAT)
names(LSAT)

#--Check Unidimensionality----
library(ggplot2)

#APA format theme for ggplot
windowsFonts("TT Arial" = windowsFont("TT Arial"))
apatheme <- theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='TT Arial', size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Run a 2PL model
LSAT_2PL <- ltm(LSAT ~ z1, IRT.param=T) #z1 == 1 latent variable predicted
#Run a 3PL model
LSAT_3PL <- tpm(LSAT, IRT.param=T)
#Check if adding an additional parameter significantly improves fit of data
anova(LSAT_2PL, LSAT_3PL)

#Run the test for unidimensionality
LSAT_2PL_DimTest <- unidimTest(LSAT_2PL)
LSAT_2PL_DimTest

### Create the visualization of unidimensionality test results
#Dataframe of observed values
obs <- data.frame(LSAT_2PL_DimTest$Tobs)
obs$type <- c('Observed Data')
obs$num <- c(row.names(obs))
obs$num <- as.numeric(obs$num)
colnames(obs) <- c('eigenvalue', 'type', 'num')

#Dataframe of average Monte Carlo simulated values
sim <- data.frame(colMeans(LSAT_2PL_DimTest$T.boot))
sim$type <- c('Average Simulated Data')
sim$num <- c(row.names(obs))
sim$num <- as.numeric(sim$num)
colnames(sim) <- c('eigenvalue', 'type', 'num')

#Merge the two data frames (observed and simulated) into data frame
eigendat <- rbind(obs,sim)

#Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis
p <- ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  geom_line()+
  geom_point(size=4)+
  scale_y_continuous(name='Eigenvalue')+
  scale_x_continuous(name='Eigenvalue Number', breaks=min(eigendat$num):max(eigendat$num))+
  scale_shape_manual(values=c(16,1)) +
  labs(title = "Dimensionality of LSAT") +
  annotate("text", x=Inf,y=Inf, hjust=1, vjust=1, 
           label= paste0("p = ",round(LSAT_2PL_DimTest$p.value, digits = 3))) +
  apatheme
p

#--Check Local Dependence using Q3 matrices & plots----
library(sirt)
library(reshape2)
library(ggplot2)

#Run a 2PL model
sirt_LSAT_2PL <- rasch.mml2(LSAT, est.a=1:5)

#estmate weighted likelihood estimators (WLEs)
mod.wle <- wle.rasch(dat=LSAT, b=sirt_LSAT_2PL$item$b)

#get Q3 stats
LSAT_q3 <- Q3(dat=LSAT, theta=mod.wle$theta, b=sirt_LSAT_2PL$item$b)

## arrange df for plotting Q3 statistics
LSAT_cormat <- melt(LSAT_q3$q3.matrix)
LSAT_cormat$Var1 <- factor(LSAT_cormat$Var1, levels = paste("Item",c(seq(1:5))))
LSAT_cormat$Var2 <- factor(LSAT_cormat$Var2, levels = paste("Item",c(seq(1:5))))
#Find 0.2 & 0.3 + average residual correlation
LSAT_Q3crit <- LSAT_q3$Q3.stat[1] + .2
LSAT_Q3critR <- LSAT_q3$Q3.stat[1] + .3
#add colorcodes for different Q3 values
LSAT_cormat$critValCol[LSAT_cormat$value < LSAT_Q3critR & LSAT_cormat$value >= LSAT_Q3crit & LSAT_cormat$value != 1] <- "blue"
LSAT_cormat$critValCol[LSAT_cormat$value >= LSAT_Q3critR & LSAT_cormat$value != 1] <- "red"
LSAT_cormat$critValCol[LSAT_cormat$value < LSAT_Q3crit | LSAT_cormat$value == 1] <- "white"


#Plot Q3 statistics
p_Q3 <- ggplot(data = LSAT_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = LSAT_cormat$critValCol, width=0.9, height=0.9)+
  scale_fill_gradient2(low = "white", high = "black", mid = "grey",
                       midpoint =.5, name="Q3",
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black"))+
  labs(title = "Q3 Correlation Matrix for LSAT",
       x = "Item", y = "Item") +
  theme(
    text=element_text(family='TT Arial', size = 12),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank())
p_Q3

#--Create Item Characteristic Curve (ICC) Plots----
library(ltm)
options(scipen=100)
options(digits=3)

#Store output of 2PL model in a new dataframe
LSAT_2PL_OUT <- as.data.frame(coef(LSAT_2PL))
#create coloumn of item numbers from row names
LSAT_2PL_OUT$item <- rownames(LSAT_2PL)

#Assign line colors based on Discriminibility values for ICC Plot
LSAT_2PL_OUT$lineColor[LSAT_2PL_OUT$Dscrmn <= 0.36] <- "#F85A3E" #Dscrm level = very low
LSAT_2PL_OUT$lineColor[LSAT_2PL_OUT$Dscrmn >= 0.36 & LSAT_2PL_OUT$Dscrmn < 0.65] <- "#F7B05B" #Dscrm level = low
LSAT_2PL_OUT$lineColor[LSAT_2PL_OUT$Dscrmn >= 0.65 & LSAT_2PL_OUT$Dscrmn < 1.34] <- "#F9DC5C" #Dscrm level = moderate
LSAT_2PL_OUT$lineColor[LSAT_2PL_OUT$Dscrmn >= 1.34 & LSAT_2PL_OUT$Dscrmn < 1.69] <- "#7DE300" #Dscrm level = high
LSAT_2PL_OUT$lineColor[LSAT_2PL_OUT$Dscrmn >= 1.7] <- "#19C5FF" #Dscrm level = very high
LSAT_2PL_OUT$lineColor[LSAT_2PL_OUT$item == 6] <- "black" #plot single item

#check your plot and see what level of discrim values there are.
LSAT_2PL_OUT
#this dataset actually only has items with moderate levels of discrim (factor code = 2).
#so, the legend only needs to include that color
#uncomment the coloumns needed for your ICC plot below to color code it based on discrm level
#also add labels on line 154 as needed: labels = c("very low", low", "moderate", "high","very high")

#Factor Levels for Legend
#LSAT_2PL_OUT$DscrmLvl[LSAT_2PL_OUT$Dscrmn <= 0.36] <- "0" #Dscrm level = very low
#LSAT_2PL_OUT$DscrmLvl[LSAT_2PL_OUT$Dscrmn >= 0.36 & LSAT_2PL_OUT$Dscrmn < 0.65] <- "1" #Dscrm level = low
LSAT_2PL_OUT$DscrmLvl[LSAT_2PL_OUT$Dscrmn >= 0.65 & LSAT_2PL_OUT$Dscrmn < 1.34] <- "2" #Dscrm level = moderate
#LSAT_2PL_OUT$DscrmLvl[LSAT_2PL_OUT$Dscrmn >= 1.34 & LSAT_2PL_OUT$Dscrmn < 1.69] <- "3" #Dscrm level = high
#LSAT_2PL_OUT$DscrmLvl[LSAT_2PL_OUT$Dscrmn >= 1.7] <- "4" #Dscrm level = very high
LSAT_2PL_OUT$DscrmLvl.f <- factor(LSAT_2PL_OUT$DscrmLvl, labels = c("moderate"))

#Create legend list according to your needs:
# very low = #F7B05B; low = #F85A3E; moderate = #F9DC5C; high = #7DE300; very high= #19C5FF
colorList = c("#F9DC5C")

plot(LSAT_2PL, type=("ICC"), annot=F, cx="topleft",
     col= LSAT_2PL_OUT$lineColor, lwd = 3,
     items=c(1:5), 
     main="LSAT Item Characteristic Curves",
     xlab="Ability",
     ylab="Probability of Correct Response",
     cex.lab=1.2)
legend("bottomright", inset=.02, title="Item Discriminibility",
       as.character(levels(LSAT_2PL_OUT$DscrmLvl.f)), fill = colorList, horiz=F, cex=1.2)

#Order output by dsicriminibility and then difficulty
LSAT_2PL_table <- LSAT_2PL_OUT[order(-LSAT_2PL_OUT$Dscrmn, LSAT_2PL_OUT$Dffclt),] 
#get numeric output of parameter estimates in a CSV file
write.csv(LSAT_2PL_table, "2PL_IRT_Results.csv")

#--Create Test & Item Information Curve (IIC) Plots----
#To plot all items and get the Test Information Function, set items parameter to 0
plot(LSAT_2PL, type="IIC", items = 0, lwd = 2,
     main="Test Information Function",
     xlab="Ability",
     ylab="Information",
     cex.lab=1.2)

#To plot specific items and get the Item Information Functions, set items to a vector
plot(LSAT_2PL, type="IIC", items = c(1,3), lwd = 2,
     main="Item Information Functions",
     xlab="Within-Task PT Ability",
     ylab="Information",
     cex.lab=1.2)

