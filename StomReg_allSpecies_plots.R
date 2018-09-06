#Krober et al. 2015 Protocol 4.1
#Model gs-vpd response. Begin by plotting species-wise all gs data against vpd, 
#combining all daily courses of infividual leaves into one analysis per species.
#Extract the maximum value observed from the stomatal conductance data by 
#searching for the maximum value.

#instruct R to subset all data from species AE
AE1 = StomReg4$Species == "AE"

#Create vector containing all values pertaining to AE
AE.1 = StomReg4[AE1, ]

#using the plot function
plot(x = AE.1$Mean_VPD_Krober_tree, y = AE.1$Mean_gs_tree,
     xlab = "vpd [hPa]",
     ylab = "gs Conductance [mmol m-2s-1]",
     main = "scatter plot",
     xlim = c(0, 21), ylim = c(0, 600)) 

    
#check for normal distribution although not necessary for logistic regression
qqnorm(AE.1$Mean_VPD_Krober_tree);qqline(AE.1$Mean_VPD_Krober_tree)
plot(density(AE.1$Mean_VPD_Krober_tree))
shapiro.test(AE.1$Mean_VPD_Krober_tree)

qqnorm(AE.1$Mean_gs_tree);qqline(AE.1$Mean_gs_tree)
plot(density(AE.1$Mean_gs_tree))
shapiro.test(AE.1$Mean_gs_tree)

# convert column "Mean_gs_tree" from mmol m-2 s-1 to mol m-2 s-1
AE.1$Mean_gs_mol <- AE.1$Mean_gs_tree / 1000
AE.1$Mean_gs_mol

#preliminary scatterplot for mean_gs_mol as a function of Mean_VPD_Krober_tree
ggplot(AE.1, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_mmol)) +geom_point()
a <- ggplot(AE.1, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_mmol)) +geom_point()

a

# max/min Mean_gs_mol by Species
aggregate(Mean_gs_mol ~ Species, data = AE.1, min)

#determine mean of all gs_mmol values and plot on graph with dashed line
aggregate(Mean_gs_mmol ~ Species, data = AE.1, mean)
a1 <- a + geom_hline(yintercept = 218.2497, linetype = "dashed")
a1


AE.1$Mean_gs_mmol <- AE.1$Mean_gs_mol * 1000
AE.1$Mean_gs_mol

a2 <- a1 + expand_limits(x = 0, y = 0)

gd <- AE.1 %>% summarise(Max_gs_tree = max(Mean_gs_tree), Max_vpd = max(Mean_VPD_Krober_tree))
gd
SRint5.1 <- data.frame(max(AE.1$Mean_gs_tree))
a <- ggplot(AE.1, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_mmol)) +geom_point()
a

#only works to determine row with min and max value
b = which.max(AE.1$Mean_gs_tree)
b
#one above code determines row with max value, save that row as a data.frame
c <- AE.1[22, ]
c

ggplot(AE.1 = aes(Mean_VPD_Krober_tree, Mean_gs_mmol)) + 
  geom_point(data = AE.1) + 
  geom_line(data = c$Mean_gs_mmol) + 
  geom_text(data = c$Mean_gs_mmol, aes(label = "max value"))
#gives output "Error: ggplot2 doesn't know how to deal with data of class numeric"
#ggplot only works with data.frame
#create "c" into a data.frame

d <- data.frame(c)

d
class(d)
class(AE.1)

ggplot(AE.1 = aes(Mean_VPD_Krober_tree, Mean_gs_mmol)) + 
  geom_point(data = AE.1) + 
  geom_line(data = d$Mean_gs_mmol) + 
  geom_text(data = e$Mean_gs_mmol, aes(label = "max value"))

#still no luck. will try to use dplr function to create max value 
#within same data.frame as AE.1 but labelled as AE.2

#make sure dplyr package is activated
require(dplyr)

e <-d %>% summarise(max(AE.1$Mean_gs_mmol))
class(e)

#add a layer to the plot with additional geom_point(), use colour to highlight point
AE.2 <- ggplot(AE.1, aes(x = Mean_VPD_Krober_tree, y = Mean_gs_mmol)) + 
  geom_point(pch=1) +
  geom_point(aes(x=14.7121, y=391.8333), colour="black") 
AE.2

#Adding a mean horizontal line to graph
mean(AE.1$Mean_gs_tree)

AE.3 <- AE.2 + geom_hline(yintercept = 218.2497, linetype="dashed")
AE.3

#Annotate Max point and Mean horizontal line
#SOLUTION 1 = sp2 + geom_text(x=3, y=30, label="Scatter plot")
#SOLUTION 2 = sp2 + annotate(geom="text", x=3, y=30, label="Scatter plot",
#color="red")

AE.4 <- AE.3 + geom_text(x = 30, y = 30, label = "Max") #didn't work for some reason

AE.4 <- AE.3 + annotate(geom="text", x=16.5, y=391.833, label = "Max") + 
  annotate(geom="text", x=22, y=230, label = "Mean")
      
AE.4

#Change theme of plot to more classic
AE.5 <- AE.4 + theme_bw()
AE.5
AE.6 <- AE.5 + theme_bw() + theme(panel.border = element_blank(), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), 
                            axis.line = element_line(colour = "black"))
AE.6

#set limits for axes
AE.7 <- AE.6 + scale_x_continuous(limits = c(0, 25)) + 
  scale_y_continuous(limits = c(0, 400)) 
AE.7

#Fix superscripts on axis labels
AE.8 <- AE.7 + xlab("vpd [hPa]") + ylab(expression(paste("gs Conductance [mmol m"^-2, "s"^-1,"]")))
AE.8                   

#------------------------------------
#create new data.frame with scaled stomatal conductance data (gs/gsmax) in order 
#to extract relative parameters. 

GsMax = max(AE.1$Mean_gs_mmol)

#add column to new data frame containing relative gs values for species-wise comparisons
GsRel <- data.frame(AE.1, "GsRel" = AE.1$Mean_gs_mmol/ GsMax) 
GsRel

GsRel.1 <- ggplot(GsRel, aes(x = Mean_VPD_Krober_tree, y = GsRel)) + 
  geom_point(pch=1)
GsRel.1

#BUILDING THE LOGISTIC REGRESSION MODEL
#check continuous variables
GsRel_cont <- select_if(GsRel, is.numeric)
Names(GsRel)

#add Interval column as categorical variables. 
GsRel_cont$fInterval <- factor(GsRel_cont$Interval,
                             levels = c(1, 5, 2, 3, 4, 6))
GsRel_cont
str(GsRel_cont)

# Histogram with kernel density curve
ggplot(GsRel_cont, aes(x = Mean_VPD_Krober_tree)) +
  geom_density(alpha = .2, fill = "#FF6666")

# Can see that the data set is skewed i.e. not normal but logistic regression does not 
#need normality within its assumptions

#standardize numeric columns to improve performance of regression

AE_rescale <- GsRel_cont %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(AE_rescale)

#remove Season2 column
AE_rescale$Season2 <- NULL
head(AE_rescale)

#plot 

ggplot(GsRel_cont, aes(x = Mean_gs_mmol)) +
  geom_density(aes(color = fInterval), alpha = 0.5) +
  theme_classic()

# Plot gender income
ggplot(recast_data, aes(x = gender, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic()

#-------------------------------------------

library(ggplot2)
#load raw gs and lwp excel table
library(readxl)
Raw_mean_gs_and_lwp <- read_excel("Raw mean gs and lwp.xlsx")
View(Raw_mean_gs_and_lwp_v01)
gslwp <- Raw_mean_gs_and_lwp
str(gslwp)
#check for normal distribution
qqnorm(gslwp$MeanGs);qqline(gslwp$MeanGs)
plot(density(gslwp$MeanGs))
shapiro.test(gslwp$Gs)
shapiro.test(gslwp$MeanGs)
qqnorm(gslwp$MeanLWP);qqline(gslwp$MeanLWP)
plot(density(gslwp$MeanLWP))
shapiro.test(gslwp$MeanLWP)
# convert column "MeanGs" from mmol m-2 s-1 to mol m-2 s-1
gslwp$MeanGs_mol <- gslwp$MeanGs / 1000
gslwp
#convert column "MeanLWP" to negative measurements
gslwp$MeanLWP_neg <- gslwp$MeanLWP * -1
gslwp
#check normality for newly introduced variables
qqnorm(gslwp$MeanLWP_neg);qqline(gslwp$MeanLWP_neg)
qqnorm(gslwp$MeanGs_mol);qqline(gslwp$MeanGs_mol)
shapiro.test(gslwp$MeanGs_mol)
#preliminary scatterplot for raw mean gs as a function of raw mean LWP
ggplot(gslwp, aes(x=MeanLWP_neg, y=MeanGs_mol, color = Species)) +geom_point()
a <- ggplot(gslwp, aes(x=MeanLWP_neg, y=MeanGs_mol, color = Species)) +geom_point()
# reverse order of x axis
a <- a + scale_x_continuous(trans = "reverse", breaks = unique(gslwp$MeanLWP_neg))
a
str(a)
View(a)
head(a)
a <- a + xlim(0, -75)
a <- a + scale_x_continuous(trans = "reverse", breaks = waiver(gslwp$MeanLWP_neg))
#load the package "scales" to access break formatting functions
library(scales)
a <- a + scale_x_continuous(trans = "reverse", breaks = waiver())
a
a <- a + geom_smooth()
a
a <- a + geom_smooth(SE=F)
a <- a + geom_smooth(se=F)
a
a <- a + geom_smooth(se=FALSE)
a
a <- a + ggplot(gslwp, aes(x=MeanLWP_neg, y=MeanGs_mol, color = Species)) +geom_smooth(se=F)
a <- ggplot(gslwp, aes(x=MeanLWP_neg, y=MeanGs_mol, color = Species)) + geom_smooth(se=F)
a
a <- a + scale_x_continuous(trans = "reverse", breaks = waiver())
a
# convert LWP to MPa
gslwp$MeanLWP_negMPa <- gslwp$MeanLWP_neg / 10
a <- ggplot(gslwp, aes(x=MeanLWP_negMPa, y=MeanGs_mol, color = Species)) + geom_smooth(se=F)
a
a <- a + scale_x_continuous(trans = "reverse", breaks = waiver())
a
# change axis labels
a <- a + xlab("Mean_leaf_water_potential_(MPa)")
a
a <- a + xlab("Mean leaf water potential (MPa)")
a
a <- a + ylab("Mean stomatal conductance (mol m-2 s-1)")
a
View(a)
#identify maximum stomatal conductance for each species
aggregate(gslwp$MeanGs, by = list(gslwp$Species), max)
aggregate(MeanGs ~ Species, data = gslwp, max
          aggregate(gslwp$MeanGs, by = list(gslwp$Species), max)
          aggregate(MeanGs ~ Species, data = gslwp, max)
          # max LWP by Species
          View(gslwp)
          aggregate(MeanLWP_neg ~ Species, data = gslwp, max)
          #min LWP by Species
          aggregate(MeanLWP_neg ~ Species, data = gslwp, min)
          aggregate(MeanLWP_neg ~ Species ~ Season, data = gslwp, min)
          create new data.frame with maximum values
          #create new data.frame with maximum values
          install.packages("plyr")
          install.packages("plyr")
          install.packages("plyr")
          install.packages("plyr")
          install.packages("plyr")
          library("plyr")
          ddply(gslwp, c(Species, Season), transform, GsMax,== max(MeanGs))
          ddply(gslwp, c(Species, Season), transform, GsMax = max(MeanGs))
          ddply(gslwp, c(gslwp$Species, gslwp$Season), transform, GsMax==max(MeanGs))
          ddply(gslwp, c(gslwp$Species, gslwp$Season), transform, GsMax==max(gslwp$MeanGs))
          ddply(gslwp, c(gslwp$Species, gslwp$Season), summarize, GsMax==max(gslwp$MeanGs))
          View(gslwp)
          library(readxl)
          Raw_gs_vpd <- read_excel("Raw gs vpd.xlsx")
          View(Raw_gs_vpd)
          b <- Raw_gs_vpd
          gsvpd <- Raw_gs_vpd
          View(b)
          View(b)
          b <- ggplot(gsvpd, aes(x=vpd, y=conductance, color = Species)) +geom_point() + geom_smooth(se=F)
          library(ggplot2)
          b <- ggplot(gsvpd, aes(x=vpd, y=conductance, color = Species)) +geom_point() + geom_smooth(se=F)
          b
          b <- ggplot(gsvpd, aes(x=vpd, y=conductance)) +geom_point() + geom_smooth(se=F)
          b
          b <- ggplot(gsvpd, aes(x=vpd, y=conductance, color = Species)) +geom_point() + geom_smooth(se=F)
          b
          b <- ggplot(gsvpd, aes(x=vpd, y=conductance, color = Species)) +geom_point(color = black) + geom_smooth(se=F)
          b <- ggplot(gsvpd, aes(x=vpd, y=conductance, color = Species)) +geom_point(color = #black) + geom_smooth(se=F)
                                                                                       b
                                                                                     library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
                                                                                     library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
                                                                                     library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
                                                                                     GsRel_cont <- as.factor(GsRel_cont$Interval)
                                                                                     #Krober et al. 2015 Protocol 4.1
                                                                                     #Model gs-vpd response. Begin by plotting species-wise all gs data against vpd,
                                                                                     #combining all daily courses of infividual leaves into one analysis per species.
                                                                                     #Extract the maximum value observed from the stomatal conductance data by
                                                                                     #searching for the maximum value.
                                                                                     #instruct R to subset all data from species AE
                                                                                     AE1 = StomReg4$Species == "AE"
                                                                                     #Create vector containing all values pertaining to AE
                                                                                     AE.1 = StomReg4[AE1, ]
                                                                                     #using the plot function
                                                                                     plot(x = AE.1$Mean_VPD_Krober_tree, y = AE.1$Mean_gs_tree,
                                                                                          xlab = "vpd [hPa]",
                                                                                          ylab = "gs Conductance [mmol m-2s-1]",
                                                                                          main = "scatter plot",
                                                                                          xlim = c(0, 21), ylim = c(0, 600))
                                                                                     #check for normal distribution although not necessary for logistic regression
                                                                                     qqnorm(AE.1$Mean_VPD_Krober_tree);qqline(AE.1$Mean_VPD_Krober_tree)
                                                                                     plot(density(AE.1$Mean_VPD_Krober_tree))
                                                                                     shapiro.test(AE.1$Mean_VPD_Krober_tree)
                                                                                     qqnorm(AE.1$Mean_gs_tree);qqline(AE.1$Mean_gs_tree)
                                                                                     plot(density(AE.1$Mean_gs_tree))
                                                                                     shapiro.test(AE.1$Mean_gs_tree)
                                                                                     # convert column "Mean_gs_tree" from mmol m-2 s-1 to mol m-2 s-1
                                                                                     AE.1$Mean_gs_mol <- AE.1$Mean_gs_tree / 1000
                                                                                     AE.1$Mean_gs_mol
                                                                                     #preliminary scatterplot for mean_gs_mol as a function of Mean_VPD_Krober_tree
                                                                                     ggplot(AE.1, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_mmol)) +geom_point()
                                                                                     a <- ggplot(AE.1, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_mmol)) +geom_point()
                                                                                     a
                                                                                     # max/min Mean_gs_mol by Species
                                                                                     aggregate(Mean_gs_mol ~ Species, data = AE.1, min)
                                                                                     #determine mean of all gs_mmol values and plot on graph with dashed line
                                                                                     aggregate(Mean_gs_mmol ~ Species, data = AE.1, mean)
                                                                                     a1 <- a + geom_hline(yintercept = 218.2497, linetype = "dashed")
                                                                                     a1
                                                                                     AE.1$Mean_gs_mmol <- AE.1$Mean_gs_mol * 1000
                                                                                     AE.1$Mean_gs_mol
                                                                                     a2 <- a1 + expand_limits(x = 0, y = 0)
                                                                                     gd <- AE.1 %>% summarise(Max_gs_tree = max(Mean_gs_tree), Max_vpd = max(Mean_VPD_Krober_tree))
                                                                                     gd
                                                                                     SRint5.1 <- data.frame(max(AE.1$Mean_gs_tree))
                                                                                     a <- ggplot(AE.1, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_mmol)) +geom_point()
                                                                                     a
                                                                                     #only works to determine row with min and max value
                                                                                     b = which.max(AE.1$Mean_gs_tree)
                                                                                     b
                                                                                     #one above code determines row with max value, save that row as a data.frame
                                                                                     c <- AE.1[22, ]
                                                                                     c
                                                                                     ggplot(AE.1 = aes(Mean_VPD_Krober_tree, Mean_gs_mmol)) +
                                                                                       geom_point(data = AE.1) +
                                                                                       geom_line(data = c$Mean_gs_mmol) +
                                                                                       geom_text(data = c$Mean_gs_mmol, aes(label = "max value"))
                                                                                     #gives output "Error: ggplot2 doesn't know how to deal with data of class numeric"
                                                                                     #ggplot only works with data.frame
                                                                                     #create "c" into a data.frame
                                                                                     d <- data.frame(c)
                                                                                     d
                                                                                     class(d)
                                                                                     class(AE.1)
                                                                                     ggplot(AE.1 = aes(Mean_VPD_Krober_tree, Mean_gs_mmol)) +
                                                                                       geom_point(data = AE.1) +
                                                                                       geom_line(data = d$Mean_gs_mmol) +
                                                                                       geom_text(data = e$Mean_gs_mmol, aes(label = "max value"))
                                                                                     #still no luck. will try to use dplr function to create max value
                                                                                     #within same data.frame as AE.1 but labelled as AE.2
                                                                                     #make sure dplyr package is activated
                                                                                     require(dplyr)
                                                                                     e <-d %>% summarise(max(AE.1$Mean_gs_mmol))
                                                                                     class(e)
                                                                                     #add a layer to the plot with additional geom_point(), use colour to highlight point
                                                                                     AE.2 <- ggplot(AE.1, aes(x = Mean_VPD_Krober_tree, y = Mean_gs_mmol)) +
                                                                                       geom_point(pch=1) +
                                                                                       geom_point(aes(x=14.7121, y=391.8333), colour="black")
                                                                                     AE.2
                                                                                     #Adding a mean horizontal line to graph
                                                                                     mean(AE.1$Mean_gs_tree)
                                                                                     AE.3 <- AE.2 + geom_hline(yintercept = 218.2497, linetype="dashed")
                                                                                     AE.3
                                                                                     #Annotate Max point and Mean horizontal line
                                                                                     #SOLUTION 1 = sp2 + geom_text(x=3, y=30, label="Scatter plot")
                                                                                     #SOLUTION 2 = sp2 + annotate(geom="text", x=3, y=30, label="Scatter plot",
                                                                                     #color="red")
                                                                                     AE.4 <- AE.3 + geom_text(x = 30, y = 30, label = "Max") #didn't work for some reason
                                                                                     AE.4 <- AE.3 + annotate(geom="text", x=16.5, y=391.833, label = "Max") +
                                                                                       annotate(geom="text", x=22, y=230, label = "Mean")
                                                                                     AE.4
                                                                                     #Change theme of plot to more classic
                                                                                     AE.5 <- AE.4 + theme_bw()
                                                                                     AE.5
                                                                                     AE.6 <- AE.5 + theme_bw() + theme(panel.border = element_blank(),
                                                                                                                       panel.grid.major = element_blank(),
                                                                                                                       panel.grid.minor = element_blank(),
                                                                                                                       axis.line = element_line(colour = "black"))
                                                                                     AE.6
                                                                                     #set limits for axes
                                                                                     AE.7 <- AE.6 + scale_x_continuous(limits = c(0, 25)) +
                                                                                       scale_y_continuous(limits = c(0, 400))
                                                                                     AE.7
                                                                                     #Fix superscripts on axis labels
                                                                                     AE.8 <- AE.7 + xlab("vpd [hPa]") + ylab(expression(paste("gs Conductance [mmol m"^-2, "s"^-1,"]")))
                                                                                     AE.8
                                                                                     #------------------------------------
                                                                                     #create new data.frame with scaled stomatal conductance data (gs/gsmax) in order
                                                                                     #to extract relative parameters.
                                                                                     GsMax = max(AE.1$Mean_gs_mmol)
                                                                                     #add column to new data frame containing relative gs values for species-wise comparisons
                                                                                     GsRel <- data.frame(AE.1, "GsRel" = AE.1$Mean_gs_mmol/ GsMax)
                                                                                     GsRel
                                                                                     GsRel.1 <- ggplot(GsRel, aes(x = Mean_VPD_Krober_tree, y = GsRel)) +
                                                                                       geom_point(pch=1)
                                                                                     GsRel.1
                                                                                     #BUILDING THE LOGISTIC REGRESSION MODEL
                                                                                     #check continuous variables
                                                                                     GsRel_cont <- select_if(GsRel, is.numeric)
                                                                                     Names(GsRel)
                                                                                     #add Interval column as categorical variables.
                                                                                     GsRel_cont <- as.factor(GsRel_cont$Interval)
                                                                                     GsRel_cont
                                                                                     str(GsRel_cont)
                                                                                     # Histogram with kernel density curve
                                                                                     ggplot(GsRel_cont, aes(x = Mean_VPD_Krober_tree)) +
                                                                                       geom_density(alpha = .2, fill = "#FF6666")
                                                                                     # Can see that the data set is skewed i.e. not normal but logistic regression does not
                                                                                     #need normality within its assumptions
                                                                                     #standardize numeric columns to improve performance of regression
                                                                                     AE_rescale <- GsRel_cont %>%
                                                                                       mutate_if(is.numeric, funs(as.numeric(scale(.))))
                                                                                     head(AE_rescale)
                                                                                     #remove Season2 column
                                                                                     AE_rescale$Season2 <- NULL
                                                                                     head(AE_rescale)
                                                                                     #plot
                                                                                     ggplot(GsRel_cont, aes(x = Mean_gs_mmol)) +
                                                                                       geom_density(aes(color = fInterval), alpha = 0.5) +
                                                                                       theme_classic()
                                                                                     # Plot gender income
                                                                                     ggplot(recast_data, aes(x = gender, fill = income)) +
                                                                                       geom_bar(position = "fill") +
                                                                                       theme_classic()
                                                                                     plot(x = AE.1$Mean_VPD_Krober_tree, y = AE.1$Mean_gs_tree,
                                                                                          xlab = "vpd [hPa]",
                                                                                          ylab = "gs Conductance [mmol m-2s-1]",
                                                                                          main = "scatter plot",
                                                                                          xlim = c(0, 21), ylim = c(0, 600))
                                                                                     ggplot(GsRel_cont, aes(x = Mean_gs_mmol)) +
                                                                                       geom_density(aes(color = fInterval), alpha = 0.5) +
                                                                                       theme_classic()
                                                                                     AE1 = StomReg4$Species == "AE"
                                                                                     library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")