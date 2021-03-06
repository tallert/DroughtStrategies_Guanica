#Krober et al. 2015 Protocol 4.1
#Model gs-vpd response. Begin by plotting species-wise all gs data against vpd, 
#combining all daily courses of individual leaves into one analysis per species.
#Extract the maximum value observed from the stomatal conductance data by 
#searching for the maximum value. Working with file StomReg_v02 which 
#you must import first

#subset Guettarda K data 
GK1 = StomReg_v02$Species == "GK"
SR.GK = StomReg_v02[GK1, ]
SR.GK

#plot data to assess curves

lo = loess(SR.GK$Mean_gs_tree~SR.GK$Mean_VPD_Krober_tree)
plot(x = SR.GK$Mean_VPD_Krober_tree, y = SR.GK$Mean_gs_tree,
     xlab = "vpd [hPa]",
     ylab = "gs Conductance [mmol m-2s-1]",
     main = "scatter plot",
     xlim = c(0, 20), ylim = c(0, 1200))
lines(predict(lo), col="red", lwd=2)

qplot(SR.GK$Mean_VPD_Krober_tree, SR.GK$Mean_gs_tree, geom = "smooth", span = 0.5)

a = plot(x = SR.ZF$Mean_lwp_tree, y = SR.ZF$Mean_gs_tree)
a

GK.lm = lm(SR.GK$Mean_gs_tree ~ SR.GK$Mean_lwp_tree)
summary(GK.lm)

gkplot.lm = plot(SR.GK$Mean_lwp_tree, SR.GK$Mean_gs_tree, pch=16, ylab = "gs_rel ", cex.lab = 1.3, col = "red" )
gkplot.lm2 = gkplot.lm + abline(lm(SR.GK$Mean_gs_tree ~ SR.GK$Mean_lwp_tree), col = "blue")
gkplot.lm2

SR.GK2 <- data.frame(SR.GK, "lwp2" = SR.GK$Mean_lwp_tree^2)
GK.qu <- lm(SR.GK2$Mean_gs_tree ~ SR.GK2$Mean_lwp_tree + SR.GK2$lwp2)#quadratic model
summary(GK.qu)

rm(GKpredictedcounts)
GKpredictedcounts <- predict(GK.qu, list(lwp = SR.GK$Mean_lwp_tree, lwpsq = SR.GK$Mean_lwp_tree^2))
gkplot.qu = plot(SR.GK2$Mean_lwp_tree, SR.GK2$gs_rel, pch=16, xlab = "lwp (kPa)", ylab = "gs conductance", cex.lab = 1.3, col = "blue")

GKpredictedcounts <- cbind(SR.GK2, predict(GK.qu, interval = 'confidence'))
GKpredictedcounts
SR.GK2
#plot the points (actual observations), regression line, and confidence interval
ggplot(SR.GK3, aes(x=Mean_lwp_tree, y=gs_rel)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


GK.gsmax = max(SR.GK2$Mean_gs_tree)

SR.GK3 <- data.frame(SR.GK2, "gs_rel" = SR.GK2$Mean_gs_tree/ GK.gsmax) 

#Now we include the quadratic model to the plot using the lines() command.
#logistic model dataframe
GK.lmdf <- merge(SR.GK3, GKpredictedcounts)
head(GK.lmdf)
str(GK.lmdf)
GK.lmdf
ggplot(SR.GK3, aes(x=SR.GK3$Mean_lwp_tree, y=SR.GK3$gs_rel)) +
  geom_point() +
  geom_abline(data = GKpredictedcounts)



