### Load processed data 
# 10/1518 Chris Weber

#clear workspace
rm(list=ls())

#Load used packages packages
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(stringr)
library(zoo)
library(dplyr)
library(tidyr)
library(openxlsx)
library(magrittr)

# filepath to TROPICS data sets
f_datamap <- '/Users/weber/OneDrive - World Wildlife Fund/1.5stuff/SR15_Scenarios/TROPICS/TROPICS_dataset_mapping.xlsx'

#Load data output from 200406_scenario_filter_TROPICS.R
datamapping <-  (read.xlsx(f_datamap, sheet='deduplicated'))

#create dataframe for regression output
regoutputfinal <- data.frame(samplesize = numeric(1),model = character(1),variable = character(1),slope = character(1),param = numeric(1),intercept = numeric(1),r2 = numeric(1))          # Specify empty vectors in data.frame


#loop through relevant files from deduplicated list of scenario sets
for (counter in 1:nrow(datamapping)) {

#read in scenario data
scens <- read.csv(paste("TROPICS_dataset-",as.character(datamapping$V8[counter]),".csv",sep=""),stringsAsFactors=FALSE,strip.white=TRUE)


##########Process scenario data and make diagnostic plots
#add model family variable
models <- read.csv("/Users/weber/OneDrive - World Wildlife Fund/1.5stuff/SR15_Scenarios/models.csv")
scens <- merge(scens,models,by = "Model")

#remove baseline scenarios, as they should not take part in modeling of mitigation targets
baselines <- read.csv("/Users/weber/OneDrive - World Wildlife Fund/1.5stuff/SR15_Scenarios/baselines.csv")
scens <- scens[!scens$Scenario %in% baselines$baseline,]
names(scens)[2] <- "concscen2"

#add 2100 66% MAGICC scores
temp66 <- read.csv("/Users/weber/OneDrive - World Wildlife Fund/1.5stuff/SR15_Scenarios/temp66.csv")
names(temp66)[2] <- "Warm2100MAG66"
scens <- merge(scens,temp66,by = "concscen2",all.x=TRUE,all.y=FALSE)
  
#downselect to potential target benchmarks
benchdown <- c("Emissions|Kyoto Gases","Emissions|CO2|Energy and Industrial Processes","INT.emKyoto_gdp","INT.emCO2Elec_elecGen","INT.emCO2EI_PE","INT.emCO2EI_elecGen","INT.emCO2Transport_gdp","median.warming.at.peak..MAGICC6.","median.warming.in.2100..MAGICC6.") 
benchdata <- subset(scens,variable %in% benchdown ,select = c(category,concscen2,modelshort,Scenario,variable, slope5, slope10, slope15, slope20, slope25, slope30, Warm2100MAG66,median.warming.in.2100..MAGICC6.,median.warming.at.peak..MAGICC6.))
benchdownsub <- c("Emissions|Kyoto Gases","Emissions|CO2|Energy and Industrial Processes","INT.emKyoto_gdp","INT.emCO2Elec_elecGen","INT.emCO2EI_PE","INT.emCO2EI_elecGen","INT.emCO2Transport_gdp") 
names(benchdata)[names(benchdata) %in% "median.warming.in.2100..MAGICC6."] <- "Warm2100MAG"
names(benchdata)[names(benchdata) %in% "median.warming.at.peak..MAGICC6."] <- "PeakwarmMAG"
#reshape data for plotting
benchdata <- melt(benchdata, id.vars = c("category","concscen2","Scenario","modelshort","Warm2100MAG","PeakwarmMAG","Warm2100MAG66","variable"))
names(benchdata)[length(names(benchdata))-1] <- "slope"
slopes <- unique(benchdata$slope)

#flip sign for target reductions to be positive
benchdata$value <- benchdata$value*-1

# add overshoot level for plot labelling
benchdata$osratio <- benchdata$PeakwarmMAG/benchdata$Warm2100MAG
benchdata$oslabel <- "zerolow"
benchdata$oslabel[benchdata$osratio > 1 & benchdata$osratio < 1.15] <- "mid"
benchdata$oslabel[benchdata$osratio > 1.15] <- "high"

#create shorter scenario/model name for plotting
benchdata$scenshort <- substr(benchdata$Scenario,1,15)

#function for plotting equation and r2 on graph
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

#Outliers removal; concscen2s picked from visual inspection of Cook's D leverage
outliers <- c("GCAM 4.2 SSP5-26","GCAM 4.2 SSP2-26","AIM/CGE 2.1 EMF33_tax_lo_none","AIM/CGE 2.1 EMF33_tax_lo_full","AIM/CGE 2.1 EMF33_tax_hi_none","AIM/CGE 2.1 EMF33_tax_hi_full")
benchdata <- benchdata[!benchdata$concscen2 %in% outliers,]

#plot regressions by variable and horizon in a single PDF for each scenario set
pdf(paste("Regressions_byvariables_byhorizon_temp66","constraint_set",datamapping$V8[counter],".pdf",sep="_"))
for(i in 1:length(benchdownsub)){
  for (j in 1:length(slopes)) {
    bdata <- subset(benchdata,variable %in% benchdown[i] & slope %in% slopes[j])
    bdata2 <- subset(bdata,select = c("value","Warm2100MAG66"))
    names(bdata2) <- c("x","y")
    templot <- ggplot(data = bdata) +
      geom_point(aes(x = value,y=Warm2100MAG66,color=oslabel),size = 2) +  #add color by model
      #data labels
      geom_text(aes(x = value,y=Warm2100MAG66,label = scenshort),size= 2) +
      geom_text(x=5,y=3.5, label = lm_eqn(bdata2), parse = TRUE) +
      ggtitle(paste(benchdown[i]," ",slopes[j])) +
      geom_smooth(method = lm, aes( x = value,y=Warm2100MAG66)) + 
      #stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
      ylab("Warming in 2100, 66% chance (degC)") +
      xlab("Linear annual reduction rate (%)") +
      scale_x_continuous(limits = c(-5,7.5)) +
      scale_y_continuous(limits = c(0.5,4)) +
      theme(legend.position = "bottom")
    print(templot)
  }
}
#close pdf
dev.off()


###########Part 3: Create and store regression models 

#initialize regression lists
nrow = length(benchdownsub)*length(slopes)
regoutput <-  data.frame(samplesize = numeric(nrow),variable = character(nrow),slope = character(nrow),param = numeric(nrow),intercept = numeric(nrow),r2 = numeric(nrow))          # Specify empty vectors in data.frame
regoutput$variable <- as.character(regoutput$variable)
regoutput$slope <- as.character(regoutput$slope)

#plot regression diagnostic plots
pdf(paste("Regression_diagnostics_","constraint_set",datamapping$V8[counter],".pdf"))
for(i in 1:length(benchdownsub)){
  for (j in 1:length(slopes)) {
    bdata <- subset(benchdata,variable %in% benchdown[i] & slope %in% slopes[j])  
    rownames(bdata) <- bdata$concscen2
    lmtemp <- lm(Warm2100MAG ~ value, data = bdata)
    regoutput$model[(i-1)*(length(benchdownsub)-1)+j] <- datamapping$V8[counter]
    regoutput$samplesize[(i-1)*(length(benchdownsub)-1)+j] <- length(lmtemp$residuals)
    regoutput$variable[(i-1)*(length(benchdownsub)-1)+j] <- benchdownsub[i]
    regoutput$slope[(i-1)*(length(benchdownsub)-1)+j] <- levels(slopes)[j]
    regoutput$intercept[(i-1)*(length(benchdownsub)-1)+j] <- lmtemp$coefficients[1]
    regoutput$param[(i-1)*(length(benchdownsub)-1)+j] <- lmtemp$coefficients[2]
    regoutput$r2[(i-1)*(length(benchdownsub)-1)+j] <- summary(lmtemp)$r.squared
    
    par(mfrow = c(2,2))
    plottemp <- plot(lmtemp,main = paste(benchdown[i]," ",slopes[j]))
    print(plottemp)
  }
}
#close pdf
dev.off()

regoutputfinal <- rbind(regoutputfinal,regoutput)
#end loop
}

#remove initialization row
regoutputfinal <- subset(regoutputfinal,samplesize>0)


############Save final output

write.csv(regoutputfinal,"Regressions_Summary.csv",row.names = FALSE)

#summarize regression output
sumreg <- ddply(regoutputfinal,.(variable,slope),summarize,size = median(samplesize),medregslope = median(param),regslopep10 = quantile(param,probs = 0.1),regslopep90 = quantile(param,probs = 0.9),medregint = median(intercept),regintp10 = quantile(intercept,probs = 0.1),regintp90 = quantile(intercept,probs = 0.9),medregr2 = median(r2),regr2p10 = quantile(r2,probs = 0.1),regr2p90 = quantile(r2,probs = 0.9))
write.csv(sumreg,"Regressions_crossmodel_summary.csv",row.names = FALSE)

#create rank by r2 for GHGs and GHG intensities/GDP
regoutwide <- dcast(regoutputfinal,model+slope ~ variable,value.var = "r2")
regoutwide15 <- subset(regoutwide,slope %in% "slope15")
regoutwide15$avgr215 <- rowMeans(regoutwide15[,names(regoutwide15) %in% c("Emissions|Kyoto Gases","INT.emKyoto_gdp","INT.emCO2EI_elecGen")])
regoutwide15 <- regoutwide15[order(-regoutwide15$avgr215),]
regoutwide15$rank15 <- NA
regoutwide15$rank15 <- 1:nrow(regoutwide15)
regoutwide30 <- subset(regoutwide,slope %in% "slope30")
regoutwide30$avgr230 <- rowMeans(regoutwide30[,names(regoutwide30) %in% c("Emissions|Kyoto Gases","INT.emKyoto_gdp","INT.emCO2EI_elecGen")])
regoutwide30 <- regoutwide30[order(-regoutwide30$avgr230),]
regoutwide30$rank30 <- NA
regoutwide30$rank30 <- 1:nrow(regoutwide30)
regoutwide2 <- regoutwide15[,names(regoutwide15) %in% c("model","avgr215","rank15")]
regoutwide3 <- regoutwide30[,names(regoutwide30) %in% c("model","avgr230","rank30")]
rankreg <- merge(regoutwide2,regoutwide3, by = c("model"))

#combine with data mappping
rankregfinal <- merge(rankreg,datamapping,by.x = "model",by.y = "V8")
names(rankregfinal) <- c("Scenario Set","Average R2, 15 year", "Rank R2, 15 year","Average R2, 30 year", "Rank R2, 30 year", "concat_Descript","Peak Emissions year","Peak Emissions variable","Peak filter applied to","CDR filter variable","CDR Limit (Gt CO2/yr)","Number scenarios in set")
write.csv(rankregfinal,"Rank_r2_regressions.csv",row.names = FALSE)

#visualize R2 by sample size
x <- ggplot(data=subset(regoutputfinal,slope %in% "slope15")) + geom_point(aes(x=samplesize,y=r2,color=variable),size = 2) + theme_classic() + theme(legend.position = "bottom") 
ggsave("scatterplot_r2bysamplesize.pdf",x,width = 5, height = 4, units = "in",scale=1.8, dpi = 300,)


