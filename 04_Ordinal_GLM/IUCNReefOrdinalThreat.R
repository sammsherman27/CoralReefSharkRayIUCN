### Categorical / ordinal regression using the clmm2 function


library(ggplot2)
library(ordinal)
library(stats)
library(car)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(gbm)
library(beepr)
library(MuMIn)
library(boot)
library(glmmTMB)
library(coefplot)
library(ggcorrplot)
library(dplyr)


# For this you have to compare each individual category with the rest to get the AUC. Eg, CR versus LC+NT+VU+EN

ordinal = read.csv(file = "2100908_CoralReefSpecies.csv")

ordinal$OrdinalStatus=as.factor(ordinal$OrdinalStatus)

####Scale and center predictors by 2SD
ordinal$MaxSize = (log(ordinal$MaxSize)-mean((log(ordinal$MaxSize))))/(2*sd(log(ordinal$MaxSize)))
ordinal$RangeSize = (log(ordinal$RangeSize)-mean((log(ordinal$RangeSize))))/(2*sd(log(ordinal$RangeSize)))
ordinal$LowerDepth = (log(ordinal$LowerDepth)-mean((log(ordinal$LowerDepth))))/(2*sd(log(ordinal$LowerDepth)))
ordinal$Countries = (log(ordinal$Countries)-mean((log(ordinal$Countries))))/(2*sd(log(ordinal$Countries)))

head(ordinal)
################### ORDINAL MODELS #########################
####Single variable testing
ModelResidency <- clmm2(OrdinalStatus~ Residency, data=ordinal, link ='logistic', Hess=TRUE)
summary(ModelResidency) 

ModelOrdinalRes <- clmm2(OrdinalStatus~ ResOrdinal, data=ordinal, link ='logistic', Hess=TRUE)
summary(ModelOrdinalRes)

ModelSize = clmm2(OrdinalStatus ~ SizeMeasure:MaxSize, data =ordinal, link='logistic', Hess=TRUE) 
summary(ModelSize) 

ModelSizeb = clmm2(OrdinalStatus ~ MaxSize, data=ordinal, link='logistic', Hess=TRUE)
summary(ModelSizeb) 

ModelGenLen = clmm2(OrdinalStatus ~ GL, data =ordinal, link = 'logistic', Hess =TRUE)
summary(ModelGenLen) 

ModelSharkRay = clmm2(OrdinalStatus ~ SharkorRay, data =ordinal, link = 'logistic', Hess =TRUE)
summary(ModelSharkRay) 

ModelDepth = clmm2(OrdinalStatus ~ LowerDepth, data=ordinal, link='logistic', Hess=TRUE)
summary(ModelDepth) 

ModelFeeding = clmm2(OrdinalStatus ~ Feeding, data=ordinal, link='logistic', Hess=TRUE)
summary(ModelFeeding) 

ModelClimate = clmm2(OrdinalStatus ~ ThreatsClimateChange, data=ordinal, link='logistic', Hess=TRUE)
summary(ModelClimate) 

ModelCountries = clmm2(OrdinalStatus ~ Countries, data=ordinal, link='logistic', Hess=TRUE)
summary(ModelCountries)

ModelBasin = clmm2(OrdinalStatus ~ OceanBasin, data=ordinal, link='logistic', Hess=TRUE)
summary(ModelBasin) 

ModelRange = clmm2(OrdinalStatus ~ RangeSize, data=ordinal, link='logistic', Hess=TRUE)
Summary(ModelRange)

ModelMulti = clmm2(OrdinalStatus ~ MaxSize + GL + Residency, link = 'logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti) 

ModelMulti2 = clmm2(OrdinalStatus ~ MaxSize + GL, link = 'logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti2) 

ModelMulti3 = clmm2(OrdinalStatus ~ MaxSize + RangeSize, link ='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti3) 

ModelMulti4 = clmm2(OrdinalStatus ~ MaxSize + LowerDepth, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti4)  

ModelMulti5 = clmm2(OrdinalStatus ~ MaxSize + LowerDepth + Residency, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti5) 

ModelMulti6 = clmm2(OrdinalStatus ~ MaxSize + Countries, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti6) 

ModelMulti7 = clmm2(OrdinalStatus ~ MaxSize + Residency, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti7) 

ModelMulti8 = clmm2(OrdinalStatus ~ MaxSize + GL + Countries, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti8) 

ModelMulti9 = clmm2(OrdinalStatus ~ MaxSize + GL + RangeSize, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti9) 

ModelMulti10a = clmm2(OrdinalStatus ~ SizeMeasure:MaxSize + LowerDepth + Countries, link='logistic', Hess=TRUE, data=ordinal)
Summary(ModelMulti10a)

ModelMulti10b = clmm2(OrdinalStatus ~ MaxSize + LowerDepth + Countries, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti10b) 
res=residuals(ModelMulti10b, type = c("pearson"))
res
plot(ModelMulti10b)

ModelMulti11 = clmm2(OrdinalStatus ~ MaxSize + Residency + RangeSize, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti11) 

ModelMulti12 = clmm2(OrdinalStatus ~ MaxSize + Feeding, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti12)

ModelMulti13 = clmm2(OrdinalStatus ~ MaxSize + Feeding + GL, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti13) 

ModelMulti14 = clmm2(OrdinalStatus ~ GL + Countries, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti14)

ModelMulti15 = clmm2(OrdinalStatus ~ GL + RangeSize, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti15) 

ModelMulti16 = clmm2(OrdinalStatus ~ GL + LowerDepth, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti16) 

ModelMulti17 = clmm2(OrdinalStatus ~ Residency + Feeding, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti17) 

ModelMulti18 = clmm2(OrdinalStatus ~ Feeding + Countries, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti18) 

ModelMulti19 = clmm2(OrdinalStatus ~ Feeding + RangeSize, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti19) 

ModelMulti20 = clmm2(OrdinalStatus ~ Residency + LowerDepth, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti20) 

ModelMulti21 = clmm2(OrdinalStatus ~ Residency + GL, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti21) 

ModelMulti22 = clmm2(OrdinalStatus ~ ResOrdinal + MaxSize, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti22) 

ModelMulti23 = clmm2(OrdinalStatus ~ FeedOrdinal + MaxSize, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti23) 

ModelMulti24 = clmm2(OrdinalStatus ~ ResOrdinal + Countries, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti24) 

ModelMulti25 = clmm2(OrdinalStatus ~ FeedOrdinal + Countries, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti25) 

ModelMulti26 = clmm2(OrdinalStatus ~ FeedOrdinal + ResOrdinal + MaxSize + Countries, link='logistic', Hess=TRUE, data=ordinal)
summary(ModelMulti26) 

ModelNull = clmm2(OrdinalStatus ~ (1|GL), link='logistic', Hess=TRUE, data=ordinal)
summary(ModelNull)



##Calculate the AIC values
status.AIC.vals<-rbind(AIC(ModelResidency),AIC(ModelSize),AIC(ModelGenLen),AIC(ModelSharkRay),AIC(ModelDepth),
                       AIC(ModelFeeding),AIC(ModelClimate),AIC(ModelCountries),AIC(ModelBasin),AIC(ModelMulti),
                       AIC(ModelMulti2),AIC(ModelMulti3),AIC(ModelMulti4),AIC(ModelMulti5),AIC(ModelMulti6),
                       AIC(ModelMulti7),AIC(ModelMulti8),AIC(ModelMulti9),
                       #AIC(ModelMulti10a),
                       AIC(ModelMulti10b),
                       AIC(ModelMulti11),AIC(ModelMulti12),AIC(ModelMulti13),AIC(ModelMulti14),AIC(ModelMulti15),
                       AIC(ModelMulti16),AIC(ModelMulti17),AIC(ModelMulti18),AIC(ModelMulti19),AIC(ModelMulti20),
                       AIC(ModelMulti21),AIC(ModelMulti22),AIC(ModelMulti23),
                       AIC(ModelMulti24),AIC(ModelMulti25),AIC(ModelMulti26),AIC(ModelNull))
##Calculating the AIC differences and weights 
status.AIC.table<-cbind(round(status.AIC.vals,2),round(status.AIC.vals-min(status.AIC.vals),2),
                        round(exp(-0.5*as.numeric(status.AIC.vals-min(status.AIC.vals)))/sum(exp(-0.5*as.numeric(status.AIC.vals-min(status.AIC.vals)))),2))
##Naming the columns of the table and combining the columns
status.AIC.table<-cbind(c("Residency","Size","GL","SharkRay","Depth","Feeding","Climate","Countries","Basin",
                          "Multi","M2","M3","M4","M5","M6","M7","M8","M9",
                          "M10b",
                          "M11","M12","M13","M14","M15","M16",
                          "M17","M18","M19","M20","M21","M22","M23","M24",
                          "M25","M26","Null"),status.AIC.table)
colnames(status.AIC.table)<-c("Model","AIC","AIC diff","Weight") ##Assigning column names

status.AIC.table
beep()

### Check VIF on top model
vifmulti10b = vif(lm(GL ~ Countries + ResOrdinal, data=ordinal))
vifmulti10b

###### Coefficient Plot ############### 

# Extracting coefficients:
coef<-ModelMulti10b$beta

# Extracting SE:
SE<-coef(summary(ModelMulti10b))[4:6, "Std. Error"]

# Wes Anderson's Moonrise Kingdom colour palette:
cols<-c("#e76071", "#2dadc4", "#f5b409") # pink, blue, yellow

x<-c(-4,-3,-2,-1,0,1,2,3,4)
y<-c(0.2,0.3,0.4,0.6,0.7,0.8,1,1.1,1.2)

# Plotting all three models together, this should give a blank, transparent bg plot
par(oma=c(0,0,0,0),mar=c(7,1,1,1), xpd=FALSE, bg="transparent",mfrow=c(1,1))
plot(x, y, xlim=c(-2.3,3.5), ylim=c(0,0.6), pch=16, cex=2, col=cols, 
     bty="l", las=1, yaxt="n", xaxt="n", xlab=NA, ylab=NA, fg="transparent", type="n", xpd=FALSE, 
     yaxs="i")

# Abline:
abline(v=0, col="gray35", lwd=5)

# Custom axes
axis(2, las=1, at=seq(0,0.6,0.6), col="gray35", col.lab="transparent", col.axis="transparent",
     lwd=0.5, col.ticks="transparent")
axis(1, las=1, at=seq(-2,3,1), col = "transparent", col.lab="transparent", col.axis="gray35",
     lwd=0.5, lwd.ticks=0.5, cex.axis=2, col.ticks="gray35")

# Custom Y labels:
text(1.35, 0.5, labels="Maximum size", cex=2.5, col="#e76071", adj=c(1,NA))
text(0.25, 0.3, labels="Lower depth", cex=2.5, col="#2dadc4", adj=c(0,NA))
text(-0.05, 0.1, labels="Number of nations", cex=2.5, col="#f5b409", adj=c(1,NA))

# Add points:
points(coef, c(0.5,0.3,0.1), pch = 19, cex=5, col=cols)

# Add error bars:
l_ci<- coef-SE*1.96
u_ci<- coef+SE*1.96

l1<- c(l_ci[1],u_ci[1])
l2<- c(l_ci[2],u_ci[2])
l3<- c(l_ci[3],u_ci[3]) 

lines(c(0.5,0.5)~l1, col="#e76071", lwd=4)
lines(c(0.3,0.3)~l2, col="#2dadc4", lwd=4)
lines(c(0.1,0.1)~l3, col="#f5b409", lwd=4)

# Custom X axis labels:
mtext(side=1, text="LOW", at=-2.3, line=3.25, col="gray35", 
      las=1, cex=2, font=2)
mtext(side=1, text="HIGH", at=3.5, line=3.25, col="gray35", 
      las=1, cex=2, font=2)
mtext(side=1, text="Threat level (standard deviation)", at=0.5, line=3.25, col="gray35", 
      las=1, cex=2, font=2)

box(col="gray35")


######## RangeSize and Residency
aootest = aov(RangeSize ~ Residency, data=ordinal)
summary(aootest)
TukeyHSD(aootest)



############################################### Correlation Matrix ###########################################
head(ordinal)
correlationordinal = ordinal[,c(5,7,13,14,15,16,17)] ### only including numerical variables
head(correlationordinal)

corrordinal <- correlationordinal %>%
  rename(
    FeedingType = FeedOrdinal,
    ResidencyType = ResOrdinal,
    Nations = Countries,
    MaximumSize = MaxSize,
    GenerationLength = GL
  )
head(corrordinal)

#### Create Matrix
matrix = cor(corrordinal, method = c("pearson"))
round(matrix,5)

head(matrix)


matrix2 = rcorr(as.matrix(corrordinal))
matrix2

### Extract Correlation Coefficients
matrix2$r
###Extract P-values 
matrix2$P

### Create plots
corrplot <- corrplot(matrix2$r, type="upper", order="hclust", addCoef.col='black',
                  p.mat =matrix2$P, sig.level=0.01, insig="blank", tl.col='black') 


chart.Correlation(corrordinal,histogram=TRUE, pch=19)

heatmap(x=matrix, symm=TRUE)

#### Correlation plot to Match BRT #### 
othercorrplot <- ggcorrplot(matrix, hc.order=TRUE, type ="lower", lab=TRUE, show.legend=FALSE,
                            show.diag=TRUE)
othercorrplot



#### Checking residency and trophic level significance ####
ModelResidency = clmm2(OrdinalStatus~ ResOrdinal, data=ordinal, link ='logistic', Hess=TRUE)
summary(ModelResidency)


ModelFeedOrd = clmm2(OrdinalStatus~ FeedOrdinal, data=ordinal, link ='logistic', Hess=TRUE)
summary(ModelFeedOrd)


