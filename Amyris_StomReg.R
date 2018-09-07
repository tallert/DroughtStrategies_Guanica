#Krober et al. 2015 Protocol 4.1
#Model gs-vpd response. Begin by plotting species-wise all gs data against vpd, 
#combining all daily courses of individual leaves into one analysis per species.
#Extract the maximum value observed from the stomatal conductance data by 
#searching for the maximum value. Working with file StomReg_v01 which you must import first

str(StomReg_v02)
class(AE1)
is.data.frame(AE1)
AE1
rm(AE1)

#convert Interval into a factor variable
StomReg_v02$Interval <- factor(StomReg_v02$Interval)

#instruct R to subset all data from species AE
AE1 = StomReg_v02$Species == "AE"

#Create vector containing all values pertaining to AE
SR.AE = StomReg_v02[AE1, ]

#using the plot function
plot(x = SR.AE$Mean_VPD_Krober_tree, y = SR.AE$Mean_gs_tree,
     xlab = "vpd [hPa]",
     ylab = "gs Conductance [mmol m-2s-1]",
     main = "scatter plot",
     xlim = c(0, 21), ylim = c(0, 600),
     by = "SR.AE$Interval") 

#dixon test to test for outliers
install.packages("outliers")
dixon.test(SR.AE$Mean_gs_tree, type = 0, opposite = FALSE, two.sided = TRUE)
dixon.test(SR.AE$Mean_VPD_Krober_tree, type = 0, opposite = FALSE, two.sided = TRUE)
dixon.test(SR.AE$Mean_lwp_tree, type = 0, opposite = FALSE, two.sided = TRUE)
#no apparent outliers


#check for normal distribution although not necessary for logistic regression
qqnorm(SR.AE$Mean_gs_tree);qqline(SR.AE$Mean_gs_tree)
plot(density(SR.AE$Mean_gs_tree)
     shapiro.test(SR.AE$Mean_gs_tree)
     
     # convert column "Mean_gs_tree" from mmol m-2 s-1 to mol m-2 s-1
     AE.1$Mean_gs_mol <- AE.1$Mean_gs_tree / 1000
     AE.1$Mean_gs_mol
     
     #preliminary scatterplot for mean_gs_mol as a function of Mean_VPD_Krober_tree
     ggplot(SR.AE, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_tree)) +geom_point()
     aeplot <- ggplot(SR.AE, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_tree)) +geom_point()
     
     aeplot
     
     # max/min Mean_gs_mol by Species
     aggregate(Mean_gs_mol ~ Species, data = AE.1, min)
     
     #determine mean of all gs_mmol values and plot on graph with dashed line
     aggregate(Mean_gs_mmol ~ Species, data = AE.1, mean)
     aeplot1 <- aeplot + geom_hline(yintercept = 218.2497, linetype = "dashed")
     aeplot1
     
     
     AE.1$Mean_gs_mmol <- AE.1$Mean_gs_mol * 1000
     AE.1$Mean_gs_mol
     
     aeplot2 <- aeplot1 + expand_limits(x = 0, y = 0)
     aeplot2
     
     #gd <- AE.1 %>% summarise(Max_gs_tree = max(Mean_gs_tree), Max_vpd = max(Mean_VPD_Krober_tree))
     #gd
     #SRint5.1 <- data.frame(max(AE.1$Mean_gs_tree))
     #a <- ggplot(AE.1, aes(x=Mean_VPD_Krober_tree, y=Mean_gs_mmol)) +geom_point()
     #a
     
     #only works to determine row with min and max value
     b = which.max(SR.AE$Mean_gs_tree)
     b
     #one above code determines row with max value, save that row as a data.frame
     c <- SR.AE[22, ]
     c
     
     ggplot(SR.AE = aes(Mean_VPD_Krober_tree, Mean_gs_tree)) + 
       geom_point(data = SR.AE) + 
       geom_line(data = c$Mean_gs_tree) + 
       geom_text(data = c$Mean_gs_tree, aes(label = "max value"))
     #gives output "Error: ggplot2 doesn't know how to deal with data of class numeric"
     #ggplot only works with data.frame
     #create "c" into a data.frame
     
     #d <- data.frame(c)
     
     #d
     #class(d)
     #class(SR.AE)
     
     #ggplot(AE.1 = aes(Mean_VPD_Krober_tree, Mean_gs_mmol)) + 
     #  geom_point(data = AE.1) + 
     #  geom_line(data = d$Mean_gs_mmol) + 
     #  geom_text(data = e$Mean_gs_mmol, aes(label = "max value"))
     
     #still no luck. will try to use dplr function to create max value 
     #within same data.frame as AE.1 but labelled as AE.2
     
     #make sure dplyr package is activated
     #require(dplyr)
     
     #e <-d %>% summarise(max(SR.AE$Mean_gs_tree))
     #class(e)
     
     #forget it, just do it manually
     #add a layer to the plot with additional geom_point(), use colour to highlight point
     aeplot3 <- ggplot(SR.AE, aes(x = Mean_VPD_Krober_tree, y = Mean_gs_tree)) + 
       geom_point(color = "black", fill = "white", shape = 1, size = 3) +
       geom_point(aes(x=14.7121, y=391.8333), colour = "black", size = 3) 
     
     aeplot3
     
     #Adding a mean horizontal line to graph
     mean(SR.AE$Mean_gs_tree)
     
     aeplot4 <- aeplot3 + geom_hline(yintercept = 218.2497, linetype="dashed")
     aeplot4
     
     #Annotate Max point and Mean horizontal line
     #SOLUTION 1 = sp2 + geom_text(x=3, y=30, label="Scatter plot")
     #SOLUTION 2 = sp2 + annotate(geom="text", x=3, y=30, label="Scatter plot",
     #color="red")
     
     #AE.4 <- AE.3 + geom_text(x = 30, y = 30, label = "Max") #didn't work for some reason
     
     aeplot5 <- aeplot4 + annotate(geom="text", x=16, y=391.833, label = "Max") + 
       annotate(geom="text", x=20, y=230, label = "Mean")
     
     aeplot5
     
     #Change theme of plot to more classic
     #AE.5 <- AE.4 + theme_bw()
     #AE.5
     aeplot6 <- aeplot5 + theme_bw() + theme(panel.border = element_blank(), 
                                             panel.grid.major = element_blank(),
                                             panel.grid.minor = element_blank(), 
                                             axis.line = element_line(colour = "black"))
     aeplot6
     
     #set limits for axes
     aeplot7 <- aeplot6 + scale_x_continuous(limits = c(0, 20)) + 
       scale_y_continuous(limits = c(0, 400)) 
     aeplot7
     
     #Fix superscripts on axis labels
     aeplot8 <- aeplot7 + xlab("vpd [hPa]") + ylab(expression(paste("gs Conductance [mmol m"^-2, "s"^-1,"]")))
     aeplot8                   
     
     #------------------------------------
     #create new data.frame with scaled stomatal conductance data (gs/gsmax) in order 
     #to extract relative parameters. 
     
     AE.gsmax = max(SR.AE$Mean_gs_tree)
     
     #add column to new data frame containing relative gs values for species-wise comparisons
     AE.gsrel <- data.frame(SR.AE, "gs_rel" = SR.AE$Mean_gs_tree/ AE.gsmax) 
     AE.gsrel
     
     aeplot.gsrel <- ggplot(AE.gsrel, aes(x = Mean_VPD_Krober_tree, y = gs_rel)) + 
       geom_point(pch=1)
     aeplot.gsrel
     rm(ae.gsrel)
     #hist(ae.gsrel$gs_rel)
     
     AE.gsrel$Interval = as.numeric(AE.gsrel$Interval)
     str(AE.gsrel)
     #BUILDING THE LOGISTIC REGRESSION MODEL
     #subset only continuous variables
     library(dplyr)
     AE.cont <- select_if(AE.gsrel, is.numeric)
     AE.cont
     
     #add Interval column as categorical variables. 
     #GsRel_cont$fInterval <- factor(GsRel_cont$Interval,
     # levels = c(1, 5, 2, 3, 4, 6))
     #GsRel_cont
     #str(GsRel_cont)
     
     # Histogram with kernel density curve
     #ggplot(SR.AE, aes(x = Mean_lwp_tree)) +
     #  geom_density(alpha = .2, fill = "#FF6666")
     #hist(SR.AE$Mean_gs_tree)
     #hist(SR.AE$Mean_VPD_Krober_tree)
     #hist(SR.AE$Mean_lwp_tree)
     
     # Can see that the data set is skewed i.e. not normal but logistic regression does not 
     #need normality within its assumptions
     
     #standardize numeric columns to improve performance of regression
     AE.rescale <- AE.cont %>%
       mutate_if(is.numeric, funs(as.numeric(scale(.))))
     head(AE.rescale)
     
     #remove Tree ID column
     AE.rescale$Tree_ID <- NULL
     head(AE.rescale)
     
     #let's try with a linear model first
     #summary(linear.model)
     AE.lm = lm(AE.rescale$gs_rel ~ AE.rescale$Mean_VPD_Krober_tree)
     summary(AE.lm)
     AE.lm2 = lm(AE.cont$gs_rel ~ AE.rescale$Mean_VPD_Krober_tree)
     summary(AE.lm2)
     
     aeplot.lm = plot(AE.cont$Mean_VPD_Krober_tree, AE.cont$gs_rel, pch=16, ylab = "gs_rel ", cex.lab = 1.3, col = "red" )
     aeplot.lm2 = aeplot.lm + abline(lm(AE.cont$gs_rel ~ AE.cont$Mean_VPD_Krober_tree), col = "blue")
     aeplot.lm2
     
     #Only explains 16% of the variation between gs and vpd
     #let's try with a logistic regression next
     #now we fit a model that is quadratic in vpd
     #create a variable vpd2 which is the square of vpd
     AE.cont <- data.frame(AE.cont, "vpd2" = AE.cont$Mean_VPD_Krober_tree^2)
     head(AE.cont )
     
     #let's attach the data frame to make this easier
     attach(AE.cont)
     AE.qu <- lm(gs_rel ~ Mean_VPD_Krober_tree + vpd2)#quadratic model
     summary (AE.qu)
     
     predictedcounts <- predict(AE.qu, list(vpd = Mean_VPD_Krober_tree, vpdsq = Mean_VPD_Krober_tree^2))
     aeplot.qu = plot(Mean_VPD_Krober_tree, Mean_gs_tree, pch=16, xlab = "vpd (hPa)", ylab = "gs conductance", cex.lab = 1.3, col = "blue")
     
     Now we include the quadratic model to the plot using the lines() command.
     
     lines(Mean_VPD_Krober_tree, predictedcounts, col = "darkgreen", lwd = 3)
     
     #try another procedure
     AE.qu2 <- glm(gs_rel ~ Mean_VPD_Krober_tree, family = binomial("logit"))
     AE.qu2
     summary(AE.qu2)
     AE.
     
     #----------------------------------------
     #gs~lwp relationship (aeplot1a-)
     
     aeplot1a <- ggplot(SR.AE, aes(x=Mean_lwp_tree, y=Mean_gs_tree)) +geom_point()
     aeplot1a
     
     #let's try with a linear model first
     #summary(linear.model)
     AE.lm.1a = lm(AE.cont$gs_rel ~ AE.cont$Mean_lwp_tree)
     summary(AE.lm.1a)
     
     
     aeplot.lm.1a = plot(AE.cont$Mean_lwp_tree, AE.cont$gs_rel, pch=16, xlab = "leaf water potential", ylab = "gs conductance", cex.lab = 1.3, col = "red" )
     aeplot.lm.1b = aeplot.lm.1a + abline(lm(AE.cont$gs_rel ~ AE.cont$Mean_lwp_tree), col = "blue")
     aeplot.lm.1b
     
     #Explains 48% of the variation between gs and lwp
     #let's try with a logistic regression next
     #now we fit a model that is quadratic in lwp
     #create a variable lwp2 which is the square of vpd
     AE.cont <- data.frame(AE.cont, "lwp2" = AE.cont$Mean_lwp_tree^2)
     head(AE.cont )
     
     #let's attach the data frame to make this easier
     attach(AE.cont)
     AE.qu.1a <- lm(gs_rel ~ Mean_lwp_tree + lwp2)#quadratic model
     summary (AE.qu.1a)
     
     predictedcounts2 <- predict(AE.qu.1a, list(lwp = Mean_lwp_tree, lwpsq = Mean_lwp_tree^2))
     aeplot.qu.1a = plot(Mean_lwp_tree, Mean_gs_tree, pch=16, xlab = "lwp (kPa)", ylab = "gs conductance", cex.lab = 1.3, col = "blue")
     
     #set limits for axes
     ?lines
     str(predictedcounts2)
     names(predictedcounts2)
     probs = data.frame()
     
     #Now we include the quadratic model to the plot using the lines() command.
     #logistic model dataframe
     AE.lmdf <- merge(AE.cont, predictedcounts2)
     head(AE.cont)
     ggplot( AE.cont, aes(x=Mean_lwp_tree, y=gs_rel)) +
       geom_point() +
       geom_smooth(method = "glm", 
                   method.args = list(family = "binomial"), 
                   se = FALSE) 
     
     