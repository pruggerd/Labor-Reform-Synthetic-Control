#Do-File for the Bachelor Thesis: Comparing labour market outcomes across OECD Countries using synthetic control methods

#Dominik Prugger

#Studentnumber:567214


#Part One: Reading in the data
#1. balsic pre-requirements
require(xlsx)
require(reshape2)
require(tidyr)
require(dplyr)
require(Synth)
library(foreign)
library(Synth)
library(xtable)
library(gridExtra)

rm(list = ls())
setwd("C:/Users/domin/Dropbox/Bachelorarbeit/Data/New Data (14.06)")

# reading in the oecd data 

#1. balsic pre-requirements
require(xlsx)
require(reshape2)
require(tidyr)
require(dplyr)

rm(list = ls())
setwd("C:/Users/domin/Dropbox/Bachelorarbeit/Data/New Data (14.06)")



#writing a function for oecd data read ins

readoecd = function(x,y){
  a = read.csv(y, header = T, sep = ",")
  columnanmes = c("Country", "TIME", "Value") 
  a = a [, columnanmes]
}

#example
gdpc = readoecd("gdpc", "gdp_capita_2006.csv")

#1 reading in unemp
unemp = readoecd("unemp", "unemp_2006.csv")
colnames(unemp)[3] = "unemp"

#2 reading in gdpc
gdpc = readoecd("gdpc", "gdp_capita_2006.csv")
colnames(gdpc)[3] = "gdpc"

#3 reading in gdp per hour
#gdph = readoecd("gdppr", "gdp_productivity_2006.csv")
#colnames(gdph)[3] = "gdph"

#4 reading in account_bip
bipacc = readoecd("bipacc", "account_bip_1806.csv")
colnames(bipacc)[3] = "bipacc"

#5 reading in Export quota/not sure about data

#6 reading in gini
#gini = readoecd("gini", "gini_workingpop_2006.csv")
#colnames(gini)[3] = "gini"

#7 reading in R&D benchmark STILL DO DO!
#rd = readoecd("rd", "net_rd_1806.csv")

#8 reading in social Spending 
socsp = read.csv("socialspending_2106.csv")
socsp = socsp[, c("Country", "Year", "Value")]
colnames(socsp) = c("Country", "TIME", "socsp")

#9 reading in inflation 
infl = readoecd("infl", "inflation_1806.csv")
colnames(infl)[3] = "infl"

#reading in minwage
minwage = readoecd("minwage", "minwage_2706.csv")
colnames(minwage)[3] = "minwage"

#10. Reading in exports to GDP ratio
#expq = readoecd("expq", "Exports_GDP_1406.csv")

#11 Reading in Change of GDP 
loggdp = readoecd("loggdp", "gdp_change_2906.csv")
colnames(loggdp)[3] = "loggdp"

#12 Reading in youth_unemp
#youth_unemp = readoecd("youth_unemp", "youth_unemp_0107.csv")
#colnames(youth_unemp)[3] = "youth_unemp"

#13 Reading in employment protection
emp_protect = readoecd("emp_protect", "emp_protect_1207.csv")
colnames(emp_protect)[3] = "emp_protect"

#14 Reading in school levels (Percentage at least upper secondary) 
schooling = read.csv("schooling_secondary_1607.csv")
schooling = schooling[, -c(1:2, 4)]
schooling = schooling[-c(29:33),]
schooling$id = as.numeric(as.factor(schooling$Country.Name))
colnames(schooling) = c("Country", 1990 : 2016, "id")
#checking if colum header are factor
schooling$Country = factor(schooling$Country)

#changing from wide to long format
schooling = reshape(schooling, direction="long", varying=list(names(schooling)[2:28]), v.names="schooling", 
                    idvar=c("id","Country"), timevar="TIME", times=1990:2016)

schooling = schooling[ order(schooling$Country), ]
rownames(schooling) = c()
schooling = schooling[, c(1, 3, 4)]
schooling[schooling == ".."] = NA

#15 reading in Labour Force Participation
particip = readoecd("particip", "participation_1907.csv" )
colnames(particip)[3] = "particip"

#16 reading in Exports
library(tidyr)
exports = read.csv("Exports_GDP_1907.csv")
exports = exports[, -c(1:2, 4)]
exports = exports[-c(28:32),]
colnames(exports) = c("Country", 1992 : 2016)
#checking if colum header are factor
exports$Country = factor(exports$Country)

#changing from wide to long format
keycol = "TIME"
valuecol = "exports"
gathercols = c(1992:2016)

exports = gather_(exports, keycol, valuecol, gathercols)
#dropping some variables is not crucial

exports = exports[ order(exports$Country), ]
rownames(exports) = c()
exports[exports == ".."] = NA
#10merging the data sets

#. merge all different data sets
#merge1 = merge(unemp, gini,  by=c("Country","TIME"),all  = T)
merge1 = merge(unemp, gdpc, by=c("Country","TIME"),all  = T)
#merge1 = merge(merge1, gdph, by=c("Country","TIME"),all  = T)
merge1 = merge(merge1, infl, by=c("Country","TIME"),all  = T)
merge1 = merge(merge1, socsp, by=c("Country","TIME"),all  = T)
merge1 = merge(merge1, minwage, by= c("Country", "TIME"), all = T)
merge1 = merge(merge1, bipacc, by = c("Country", "TIME"), all = T)
merge1 = merge(merge1, loggdp, by = c("Country", "TIME"), all = T)
#merge1 = merge(merge1, youth_unemp, by = c("Country", "TIME"), all = T)
merge1 = merge(merge1, schooling, by = c("Country", "TIME"), all = T)
merge1 = merge(merge1, emp_protect, by = c("Country", "TIME"), all = T)
merge1 = merge(merge1, particip, by = c("Country", "TIME"), all = T)
merge1 = merge(merge1, exports, by = c("Country", "TIME"), all = T)

#subsetting the data without unwanted countries
subsetv = c("Iceland","European Union (27 countries)", "Croatia", "Cyprus", "United Arab Emirates",  "Japan","Lithuania","New Zealand", "OECD - Total", "Latvia")
merge1 = subset(merge1, !(Country %in% subsetv))

#check class of data frame
sapply(merge1, class)

# changing all except country to numeric
cols.num = c("TIME", "bipacc", "unemp", "gdpc", "infl", "socsp", "schooling", "emp_protect", "exports", "particip")    
merge1[cols.num] = sapply(merge1[cols.num],as.numeric)
sapply(merge1, class)

#test if numeric
mean(merge1$unemp, na.rm = T)
#adding unique ID to countries
merge1$id = as.numeric(as.factor(merge1$Country))


#11 save merged data
write.csv(merge1, "MergedData.csv", ,row.names=FALSE)
#


##################################################################################################################
#Part TWO: Calculating the Effect of the introduction of minimum wages in Germany and doing informative graphics

#create a new data frame with countries that don't have minimum wages
nominwage = c( "Austria", "Denmark", "Finland", "Germany", "Italy", "Norway", "Sweden", "Switzerland")
merge_na = subset(merge1, (Country %in% nominwage))
merge_na$Country = as.character(merge_na$Country)

#dropping minwages
merge_na = subset(merge_na, select = -minwage)
merge_na$id = as.numeric(as.factor(merge_na$Country))

#check for balancy
rownames(merge_na) = seq(length=nrow(merge_na))

balcheck = table(    merge_na[c(79:104,1:78, 105:180),"id"],
                     merge_na[c(79:104,1:78, 105:180),"TIME"])        

print(balcheck)
unique(balcheck)
length(unique(balcheck)) != 1 
unique(balcheck)!= 1

#checking which years to select for schooling, containing least NAs

help_min = merge_na[, c("Country", "TIME", "schooling")]
help_min = reshape(help_min, idvar = "Country", timevar = "TIME", direction = "wide")
help_min = help_min[, -which(colMeans(is.na(help_min)) > 0.2)]
#help_min = help_min[ , colSums(is.na(help_min)) == 0]
# The Years with just 1 NA are 2007,2008,2009,2010,2011,2012,2014,2015 

# Step 1: Unemployment Germany Minwage UNEMP
prep_min_u=
  dataprep(
    foo = merge_na,
    predictors = c( "infl", "loggdp", "emp_protect","bipacc", "exports", "particip"),
    predictors.op = "mean",
    dependent = "unemp",
    special.predictors = list(
      list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), "mean"), 
      list("socsp", seq(1995,2013,2), "mean")
    ),
    unit.variable = "id",
    time.variable = "TIME",
    
    treatment.identifier = 4,
    controls.identifier = c(1,2,3,5,6,7),
    time.predictors.prior = c(1995:2015),
    time.optimize.ssr = c(1995:2014),
    unit.names.variable = "Country",
    time.plot = 1995:2016
  )


#calculating synth command
synth_min_u = synth(prep_min_u)

#calculating tables
synth.table_min_u = synth.tab(
  dataprep.res = prep_min_u,
  synth.res = synth_min_u)

print(synth.table_min_u)


# Step 2: GDP per Capita Germany Minwage UNEMP
prep_min_gdp=
  dataprep(
    foo = merge_na,
    predictors = c("infl", "loggdp", "emp_protect","bipacc", "exports", "particip"),
    predictors.op = "mean",
    special.predictors = list(
      list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), c("mean")),
      list("socsp", seq(1995,2013, 2), "mean")
    ),
    dependent = "gdpc",
    unit.variable = "id",
    time.variable = "TIME",
    
    treatment.identifier = 4,
    controls.identifier = c(1,2,3,5,6,7),
    time.predictors.prior = c(2002:2015),
    time.optimize.ssr = c(2002:2014),
    unit.names.variable = "Country",
    time.plot = 2002:2016
  )


#calculating synth command
synth_min_gdp = synth(prep_min_gdp)

#calculating tables
synth.table_min_gdp = synth.tab(
  dataprep.res = prep_min_gdp,
  synth.res = synth_min_gdp)

print(synth.table_min_gdp)

min_gdp_gaps = prep_min_gdp$Y1plot-(
  prep_min_gdp$Y0plot%*%synth_min_gdp$solution.w
)
min_gdp_gaps

# Doing Germany Graphics for the Unemployment Rate
#defining working directory 
setwd("C:/Users/domin/Dropbox/Bachelorarbeit/Latex/NewGraphics")

#1. table that creates the differences in explanatory variables: Synthetic vs. Real Germany
min_tab1_u = data.frame(synth.table_min_u$tab.w)
min_tab1_u[,1] = round(min_tab1_u[,1],2) 

table1_min_u = xtable(cbind(min_tab1_u))
colnames(table1_min_u) = c("Weights", "Country", "Unit Number")

pdf(file = "Germany-Unemp-Weights.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(table1_min_u, rows = NULL)
dev.off()


#2. Creating a graph: Synthetic vs Real Germany -GDP -Pathplot
pdf(file = "Germany-Synthetic-Pathplot-unemp.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
synth_min_u_YO = (prep_min_u$Y0plot%*%synth_min_u$solution.w)
plot(1995:2016, prep_min_u$Y1plot, 
     type = "l", ylim = c(3,14), col = "black", lty = "solid", 
     ylab = "Harmonised Unemployment Rate", 
     xlab = "year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
lines(1995:2016, synth_min_u_YO , col = "black", lty = "dashed", lwd = 2)
abline(v=2015, lty = "dotted")
legend(x ="bottomright", 
       legend = c("Germany", "Synthetic Germany"), 
       lty = c("solid", "dashed"), col = c("black", "black"), 
       cex = .8, bg = "white", lwd = c(2,2))
arrows(2013.8,12,2014.5,12,col="black",length=.1)
text(2010.5,12,"Minimum Wage",cex=0.9)
dev.off()


#3. gap_east 
pdf(file = "Germany-Unemp-Synthetic-Gapsplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
gap_min_u = prep_min_u$Y1plot - (prep_min_u$Y0plot%*%synth_min_u$solution.w)
plot(1995:2016, gap_min_u, 
     type = "l", ylim = c(-8, 8), col = "black", lty = "solid", 
     ylab = c("Gap in the harmonised unemployment rate)"), 
     xlab = "Year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
abline(v = 2015, lty = "dotted") 
abline(h = 0, lty = "dotted")
arrows(2013.8,6,2014.5,6,col="black",length=.1)
text(2011,6,"Minimum Wage",cex=0.9)
dev.off()
#weights
pred_table_min_u = xtable(round(synth.table_min_u$tab.pred , 1),digits=1)

pdf(file = "Germany-Unemp-Dependent-Variables-Fit.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(pred_table_min_u)
dev.off()

#Doing Graphics for the GDP per capita
#1. table that creates the differences in explanatory variables: Synthetic vs. Real Germany
min_tab1_gdp = data.frame(synth.table_min_gdp$tab.w)
min_tab1_gdp[,1] = round(min_tab1_gdp[,1],2) 

table1_min_gdp = xtable(cbind(min_tab1_gdp))
colnames(table1_min_gdp) = c("Weights", "Country", "Unit Number")

pdf(file = "Germany-GDP-Weights.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(table1_min_gdp, rows = NULL)
dev.off()


#2. Creating a graph: Synthetic vs Real Germany -GDP -Pathplot
pdf(file = "Germany-Synthetic-Pathplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
synth_min_gdp_YO = (prep_min_gdp$Y0plot%*%synth_min_gdp$solution.w)
plot(2002:2016, prep_min_gdp$Y1plot, 
     type = "l", ylim = c(20000,50000), col = "black", lty = "solid", 
     ylab = "Per-Capita GDP (PPP, USD)", 
     xlab = "year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
lines(2002:2016, synth_min_gdp_YO , col = "black", lty = "dashed", lwd = 2)
abline(v=2015, lty = "dotted")
legend(x ="bottomright", 
       legend = c("Germany", "Synthetic Germany"), 
       lty = c("solid", "dashed"), col = c("black", "black"), 
       cex = .8, bg = "white", lwd = c(2,2))
arrows(2013.8,30000,2014.5,30000,col="black",length=.1)
text(2011.5,30000,"Minimum Wage",cex=0.9)
dev.off()

#3. gap_east 
pdf(file = "Germany-GDP-Synthetic-Gapsplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
gap_min_gdp = prep_min_gdp$Y1plot - (prep_min_gdp$Y0plot%*%synth_min_gdp$solution.w)
plot(2002:2016, gap_min_gdp, 
     type = "l", ylim = c(-4000, 4000), col = "black", lty = "solid", 
     ylab = c("Gap in per-capita GDP (PPP, USD)"), 
     xlab = "Year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
abline(v = 2015, lty = "dotted") 
abline(h = 0, lty = "dotted")
arrows(2013.8,3000,2014.5,3000,col="black",length=.1)
text(2011,3000,"Minimum Wage",cex=0.9)
dev.off()

#4. Table of the Different averaged variables

pred_table_min_gdp = xtable(round(synth.table_min_gdp$tab.pred , 1),digits=1)

pdf(file = "Germany-GDP-Dependent-Variables-Fit.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(pred_table_min_gdp)
dev.off()

#Plot the country paths for Unemployment 
# Plot for total unemployment data 
p1 = ggplot(data = merge1, aes(x = TIME, y = unemp, group = Country, colour = Country)) + geom_line()+xlab("Year") + ylab ("Harmonised Unemployment Rate")+ ggtitle("Harmonized Unemployment Rate; 1991 - 2017")
p2 = ggplot(data = merge_na, aes(x = TIME, y = unemp, group = Country, colour = Country)) + geom_line(size = 1.2)+xlab("Year") + ylab ("Harmonised Unemployment Rate")+ ggtitle("Harmonized Unemployment Rate; 1991 - 2017")

#introducing points in the graph and different y -  and x Label
p3 = p2 + geom_point(size = 1.5) + scale_y_continuous(breaks=seq(0,20,5)) + scale_x_continuous(breaks = seq(1990, 2016, 5))


pdf("Unemploymentrate_Countries.pdf")
p1
p2
p3
dev.off()


#############################################################################################################
#Part three: Calculation and Grpahics for the Effect of the New Labor Code 2012 in Hungary

#defining new "Hungary" dataset
#merge_na = merge1[is.na(merge1$minwage),]
eastcountries= c("Germany", "Hungary", "Austria", "Czech Republic", "Slovenia", "Slovak Republic", "Poland", "Greece", "Estonia")
merge_east = subset(merge1, (Country %in% eastcountries))
merge_east$Country = as.character(merge_east$Country)

#Re-creating an id
merge_east$id = as.numeric(as.factor(merge_east$Country))


# S1. SCM concerning UNEMP in Hungary
prep_east_u=
  dataprep(
    foo = merge_east,
    predictors = c( "schooling", "emp_protect", "particip", "bipacc", "exports", "infl"),
    predictors.op = "mean",
    dependent = "unemp",
    unit.variable = "id",
    time.variable = "TIME",
    
    treatment.identifier = 6,
    controls.identifier = c(1,2,3,4,5,7,8,9),
    time.predictors.prior = c(2002:2012),
    time.optimize.ssr = c(2002:2012),
    unit.names.variable = "Country",
    time.plot = 2002:2016
  )


#calculating synth command
synth_east_u = synth(prep_east_u)

#calculating tables
table_east_u = synth.tab(
  dataprep.res = prep_east_u,
  synth.res = synth_east_u)

#2. Performing SCM Concerning log GDP

prep_east_gdp=
  dataprep(
    foo = merge_east,
    predictors = c( "schooling", "emp_protect", "particip","unemp",  "bipacc", "loggdp", "infl"),
    predictors.op = "mean",
    dependent = "gdpc",
    unit.variable = "id",
    time.variable = "TIME",
    
    treatment.identifier = 6,
    controls.identifier = c(1,2,3,4,5,7,8,9),
    time.predictors.prior = c(2002:2012),
    time.optimize.ssr = c(2002:2012),
    unit.names.variable = "Country",
    time.plot = 2002:2016
  )

#calculating synth command
synth_east_gdp = synth(prep_east_gdp)

#calculating tables
table_east_gdp = synth.tab(
  dataprep.res = prep_east_gdp,
  synth.res = synth_east_gdp)

# Doing Hungary Graphics for the unemployment rate
setwd("C:/Users/domin/Dropbox/Bachelorarbeit/Latex/NewGraphics")
#1. table that creates the differences in explanatory variables: Synthetic vs. Real
east_tab1_u = data.frame(table_east_u$tab.w)
east_tab1_u[,1] = round(east_tab1_u[,1],2) 

table1_east_u = xtable(cbind(east_tab1_u))
colnames(table1_east_u) = c("Weights", "Country", "Unit Number")

pdf(file = "Hungary-Unemp-Weights.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(table1_east_u, rows = NULL)
dev.off()

#table that creates the importance in explanatory variables: Synthetic vs. Real
east_tab2_u = data.frame(table_east_u$tab.v)
east_tab2_u[,2] = round(east_tab2_u[,2]) 

table2_east_u = xtable(cbind(east_tab2_u))
colnames(table2_east_u) = c("Variable Weights")
xtable(table2_east_u)


#2. Creating a graph: Synthetic vs Real Hungary -Pathplot
pdf(file = "Hungary-Synthetic-Unemp-Pathplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
synth_east_u_YO = (prep_east_u$Y0plot%*%synth_east_u$solution.w)
plot(2002:2016, prep_east_u$Y1plot, 
     type = "l", ylim = c(2,15), col = "black", lty = "solid", 
     ylab = "Harmonized Unemployment rate", 
     xlab = "year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
lines(2002:2016, synth_east_u_YO , col = "black", lty = "dashed", lwd = 2)
abline(v=2012, lty = "dotted")
legend(x ="bottomright", 
       legend = c("Hungary", "Synthetic Hungary"), 
       lty = c("solid", "dashed"), col = c("black", "black"), 
       cex = .8, bg = "white", lwd = c(2,2))
arrows(2010.4,14,2011.5,14,col="black",length=.1)
text(2008.5,14,"New Labour Code",cex=0.8)
dev.off()

#3. gap_east 
pdf(file = "Hungary-Unemp-Synthetic-Gapsplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
gap_east_u = prep_east_u$Y1plot - (prep_east_u$Y0plot%*%synth_east_u$solution.w)
plot(2002:2016, gap_east_u, 
     type = "l", ylim = c(-5, 5), col = "black", lty = "solid", 
     ylab = c("Gap in per-capita GDP (PPP, USD)"), 
     xlab = "Year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
abline(v = 2012, lty = "dotted") 
abline(h = 0, lty = "dotted")
arrows(2010.4,3.5,2011.5,3.5,col="black",length=.1)
text(2008.5,3.5,"New Labour Code",cex=0.8)
dev.off()

#4. Table of the Different averaged variables

pred_table_east_u = xtable(round(table_east_u$tab.pred , 1),digits=1)

pdf(file = "Hungary-Unemp-Dependent-Variables-Fit.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(pred_table_east_u)
dev.off()


# Now doing Graphics for the GDP per Capita 
#1. table that creates the differences in explanatory variables: Synthetic vs. Real
east_tab1_gdp = data.frame(table_east_gdp$tab.w)
east_tab1_gdp[,1] = round(east_tab1_gdp[,1],2) 

table1_east_gdp = xtable(cbind(east_tab1_gdp))
colnames(table1_east_gdp) = c("Weights", "Country", "Unit Number")

pdf(file = "Hungary-GDP-Weights.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(table1_east_gdp, rows = NULL)
dev.off()
#creating a latex table that provides the predictive weights
east_tab2_gdp = data.frame(table_east_gdp$tab.v)
east_tab2_gdp[,1] = round(east_tab2_gdp[,1]) 

table2_east_gdp = xtable(cbind(east_tab2_gdp))
colnames(table2_east_gdp) = c("Variable Weights")
xtable(table2_east_gdp)

#2. Creating a graph: Synthetic vs Real Hungary -GDP -Pathplot
pdf(file = "Hungary-Synthetic-Pathplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
synth_east_gdp_YO = (prep_east_gdp$Y0plot%*%synth_east_gdp$solution.w)#keine ahnung was das sein soll// was $YO ist 
plot(2002:2016, prep_east_gdp$Y1plot, 
     type = "l", ylim = c(10000,30000), col = "black", lty = "solid", 
     ylab = "Per-Capita GDP (PPP, USD)", 
     xlab = "year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
lines(2002:2016, synth_east_gdp_YO , col = "black", lty = "dashed", lwd = 2)
abline(v=2012, lty = "dotted")
legend(x ="bottomright", 
       legend = c("Hungary", "Synthetic Hungary"), 
       lty = c("solid", "dashed"), col = c("black", "black"), 
       cex = .8, bg = "white", lwd = c(2,2))
arrows(2010,25000,2011.5,25000,col="black",length=.1)
text(2008,25000,"New Labour Code",cex=0.8)
dev.off()

#3. gap_east 
pdf(file = "Hungary-GDP-Synthetic-Gapsplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
gap_east_gdp = prep_east_gdp$Y1plot - (prep_east_gdp$Y0plot%*%synth_east_gdp$solution.w)
plot(2002:2016, gap_east_gdp, 
     type = "l", ylim = c(-4000, 4000), col = "black", lty = "solid", 
     ylab = c("Gap in per-capita GDP (PPP, USD)"), 
     xlab = "Year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
abline(v = 2012, lty = "dotted") 
abline(h = 0, lty = "dotted")
arrows(2010,2000,2011.5,2000,col="black",length=.1)
text(2008,2000,"New Labour Code",cex=0.8)
dev.off()

#4. Table of the Different averaged variables

pred_table_east_gdp = xtable(round(table_east_gdp$tab.pred , 1),digits=1)

pdf(file = "Hungary-GDP-Dependent-Variables-Fit.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(pred_table_east_gdp)
dev.off()


###############################################################################################################
#Part Four: Calculations and Graphics for the effect of the Ley 3/2012 in Spain


#defining new "CRISIS" dataset
#merge_na = merge1[is.na(merge1$minwage),]
crisiscountries = c( "Ireland","Spain",  "Greece", "Italy", "Portugal", "Estonia", "Slovak Republic")
merge_crisis = subset(merge1, (Country %in% crisiscountries))
merge_crisis$Country = as.character(merge_crisis$Country)

#create new id
merge_crisis$id = as.numeric(as.factor(merge_crisis$Country))

#check for balancy
rownames(merge_crisis) = seq(length=nrow(merge_crisis))

#check which years to choose for schooling
help_spain = merge_crisis[, c("Country", "TIME", "schooling")]
help_spain = reshape(help_spain, idvar = "Country", timevar = "TIME", direction = "wide")
help_spain = help_spain[, -which(colMeans(is.na(help_spain)) > 0.2)]
#help_spain = help_spain[, colSums(is.na(help_spain)) == 0]

#1. Spain Unemployment: SCM
prep_spain_u = dataprep(
  foo = merge_crisis,
  predictors = c( "infl", "loggdp", "emp_protect", "exports", "particip"),
  predictors.op = "mean",
  dependent = "unemp",
  special.predictors = list(
    list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), "mean"), 
    list("socsp", seq(1995,2013,2), "mean")
  ),
  unit.variable = "id",
  time.variable = "TIME",
  
  treatment.identifier = 7,
  controls.identifier = c(1,2,3,4,5,6),
  time.predictors.prior = c(2002:2012),
  time.optimize.ssr = c(2002:2011),
  unit.names.variable = "Country",
  time.plot = 2002:2016
)


#calculating synth command
synth_spain_u = synth(prep_spain_u)

#calculating tables
synth.table_spain_u = synth.tab(
  dataprep.res = prep_spain_u,
  synth.res = synth_spain_u)

print(synth.table_spain_u)

#calculating gaps
spain_unemp_gaps = prep_spain_u$Y1plot-(
  prep_spain_u$Y0plot%*%synth_spain_u$solution.w
)

#1. Spain GDP (Per Capita or logged): SCM
prep_spain_gdp = dataprep(
  foo = merge_crisis,
  predictors = c("infl", "loggdp", "emp_protect", "exports", "particip"),
  predictors.op = "mean",
  special.predictors = list(
    list("socsp", seq(1995,2013, 4), "mean"), 
    list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), c("mean"))
  ),
  dependent = "gdpc",
  unit.variable = "id",
  time.variable = "TIME",
  
  treatment.identifier = 7,
  controls.identifier = c(1,2,3,4,5,6),
  time.predictors.prior = c(2002:2012),
  time.optimize.ssr = c(2002:2011),
  unit.names.variable = "Country",
  time.plot = 2002:2016
)


#calculating synth command
synth_spain_gdp = synth(prep_spain_gdp)

#calculating tables
synth.table_spain_gdp = synth.tab(
  dataprep.res = prep_spain_gdp,
  synth.res = synth_spain_gdp)

print(synth.table_spain_gdp)

#calculating gaps
spain_gdp_gaps = prep_spain_gdp$Y1plot-(
  prep_spain_gdp$Y0plot%*%synth_spain_gdp$solution.w
)
xtable(spain_gdp_gaps)

#creating graphics for Spains Unemployment Effect
setwd("C:/Users/domin/Dropbox/Bachelorarbeit/Latex/NewGraphics")
#1. table that creates the differences in explanatory variables: Synthetic vs. Real
spain_tab1_u = data.frame(synth.table_spain_u$tab.w)
spain_tab1_u[,1] = round(spain_tab1_u[,1],2) 

table1_spain_u = xtable(cbind(spain_tab1_u))
colnames(table1_spain_u) = c("Weights", "Country", "Unit Number")

pdf(file = "Spain-Unemp-Weights.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(table1_spain_u, rows = NULL)
dev.off()

#creating a latex table that provides the predictive weights
spain_tab2_u = data.frame(synth.table_spain_u$tab.v)
spain_tab2_u[,1] = round(spain_tab2_u[,1]) 

table2_spain_u = xtable(cbind(spain_tab2_u))
colnames(table2_spain_u) = c("Variable Weights")
xtable(table2_spain_u)

#2. Creating a graph: Synthetic vs Real Spain -Unemp -Pathplot
pdf(file = "Spain-Synthetic-Pathplot-UNEMP.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
synth_spain_u_YO = (prep_spain_u$Y0plot%*%synth_spain_u$solution.w)
plot(2002:2016, prep_spain_u$Y1plot, 
     type = "l", ylim = c(5,30), col = "black", lty = "solid", 
     ylab = "Harmonised Unemployment Rate", 
     xlab = "year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
lines(2002:2016, synth_spain_u_YO, col = "black", lty = "dashed", lwd = 2)
abline(v=2012, lty = "dotted")
legend(x ="bottomright", 
       legend = c("Spain", "Synthetic Spain"), 
       lty = c("solid", "dashed"), col = c("black", "black"), 
       cex = .8, bg = "white", lwd = c(2,2))
arrows(2010,28000,2011.8,28000,col="black",length=.1)
text(2008.8,28000,"Labor Reform 2012",cex=0.8)

dev.off()

#3. gap_spain 
pdf(file = "Spain-Unemp-Synthetic-Gapsplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
gap_spain_u = prep_spain_u$Y1plot - (prep_spain_u$Y0plot%*%synth_spain_u$solution.w)
plot(2002:2016, gap_spain_u, 
     type = "l",
     yaxt = "n",
     ylim = c(-10, 10),
     col = "black", lty = "solid", 
     ylab = c("Harmonised Unemployment Rate"), 
     xlab = "Year", 
     xaxs = "i", 
     yaxs = "i",
     lwd = 2
)
ylabel = seq(-10, 10, by = 2)
axis(1)
axis(2, at = ylabel, las = 1)
abline(v = 2012, lty = "dotted") 
abline(h = 0, lty = "dotted")
arrows(2010,-2000,2011.8,-2000,col="black",length=.1)
text(2008.8,-2000,"Labor Reform 2012",cex=0.8)
dev.off()

#4. Table of the Different averaged variables

pred_table_spain_u = xtable(round(synth.table_spain_u$tab.pred , 1),digits=1)

pdf(file = "Spain-Unemp-Dependent-Variables-Fit.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(pred_table_spain_u)
dev.off()

#creating graphics for Spains Effect on GDP per Capita
#1. table that creates the differences in explanatory variables: Synthetic vs. Real
spain_tab1_gdp = data.frame(synth.table_spain_gdp$tab.w)
spain_tab1_gdp[,1] = round(spain_tab1_gdp[,1],2) 

table1_spain_gdp = xtable(cbind(spain_tab1_gdp))
colnames(table1_spain_gdp) = c("Weights", "Country", "Unit Number")

pdf(file = "Spain-GDP-Weights.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(table1_spain_gdp, rows = NULL)
dev.off()

#creating a latex table that provides the predictive weights
spain_tab2_gdp = data.frame(synth.table_spain_gdp$tab.v)
spain_tab2_gdp[,1] = round(spain_tab2_gdp[,1]) 

table2_spain_gdp = xtable(cbind(spain_tab2_gdp))
colnames(table2_spain_gdp) = c("Variable Weights")
xtable(table2_spain_gdp)
#2. Creating a graph: Synthetic vs Real Spain -GDP -Pathplot
pdf(file = "Spain-Synthetic-Pathplot-GDPC.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
synth_spain_gdp_YO = (prep_spain_gdp$Y0plot%*%synth_spain_gdp$solution.w)#keine ahnung was das sein soll// was $YO ist 
plot(2002:2016, prep_spain_gdp$Y1plot, 
     type = "l", ylim = c(20000,45000), col = "black", lty = "solid", 
     ylab = "Per-Capita GDP Spain (PPP, USD)", 
     xlab = "year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
lines(2002:2016, synth_spain_gdp_YO, col = "black", lty = "dashed", lwd = 2)
abline(v=2012, lty = "dotted")
legend(x ="bottomright", 
       legend = c("Spain", "Synthetic Spain"), 
       lty = c("solid", "dashed"), col = c("black", "black"), 
       cex = .8, bg = "white", lwd = c(2,2))
arrows(2010.8,28000,2011.8,28000,col="black",length=.1)
text(2008.8,28000,"Labor Reform 2012",cex=0.8)

dev.off()

#3. gap_spain 
pdf(file = "Spain-GDPC-Synthetic-Gapsplot.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
gap_spain_gdp = prep_spain_gdp$Y1plot - (prep_spain_gdp$Y0plot%*%synth_spain_gdp$solution.w)
plot(2002:2016, gap_spain_gdp, 
     type = "l",
     yaxt = "n",
     ylim = c(-6000, 6000),
     col = "black", lty = "solid", 
     ylab = c("Gap in per-capita GDP (PPP, USD)"), 
     xlab = "Year", 
     xaxs = "i", 
     yaxs = "i",
     lwd = 2
)
ylabel = seq(-6000, 6000, by = 2000)
axis(1)
axis(2, at = ylabel, las = 1)
abline(v = 2012, lty = "dotted") 
abline(h = 0, lty = "dotted")
arrows(2010.8,-2000,2011.8,-2000,col="black",length=.1)
text(2008.8,-2000,"Labor Reform 2012",cex=0.8)
dev.off()

#4. Table of the Different averaged variables

pred_table_spain_gdp = xtable(round(synth.table_spain_gdp$tab.pred , 1),digits=1)

pdf(file = "Spain-GDP-Dependent-Variables-Fit.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(pred_table_spain_gdp)
dev.off()

#graph of all export shares

library(ggplot2)
require(foreign)
require(ggplot2)
require(stargazer)
#load basic data
# Plot for total unemployment data 
s1 = ggplot(data = merge1, aes(x = TIME, y = exports, group = Country, colour = Country)) + geom_line()+xlab("Year") + ylab ("Share of Exports/GDP")+ ggtitle("Exports/GDP in percent 1991 - 2017")
s2 = ggplot(data =merge_crisis , aes(x = TIME, y = exports, group = Country, 
                                     colour = grey.colors(Country, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))) + geom_line(size = 1.2)+xlab("Year") + ylab ("Share of Exports/GDP")+ ggtitle("Exports/GDP in percent: 1991 - 2017")

#introducing points in the graph and different y -  and x Label
s3 = s2 + geom_point(size = 1.5) + scale_y_continuous(breaks=seq(0,100,10)) + scale_x_continuous(breaks = seq(1991, 2016, 5))


pdf(file="Spain-Exports.pdf", family = "Times")
print(s3)
dev.off()

##########################################################################################################################

#Part Five: Doing inference tests


#Run again Synth but for year 2000
#exclude Estonia for Data issues 
# data prep for main model

prep_spain_gdp_test =dataprep(
  foo = merge_crisis,
  predictors = c("infl", "loggdp", "emp_protect", "exports", "particip"),
  predictors.op = "mean",
  special.predictors = list(
    list("socsp", seq(1995,2013, 4), "mean"), 
    list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), c("mean"))
  ),
  dependent = "gdpc",
  unit.variable = "id",
  time.variable = "TIME",
  
  treatment.identifier = 7,
  controls.identifier = c(1,2,3,5,6),
  time.predictors.prior = c(2005:2011),
  time.optimize.ssr = c(2005:2010),
  unit.names.variable = "Country",
  time.plot = 2005:2016
)
# synth calc with weights from the main model
synth_spain_gdp_test = synth(
  data.prep.obj=prep_spain_gdp_test
)


#performing plots
pathplot_spain_gdp_test = path.plot(dataprep.res = prep_spain_gdp_test, synth.res = synth_spain_gdp_test, 
                                    tr.intake = 2011, Xlab = c("Year"), 
                                    Ylab = "GDP per Capita Spain in Dollar, PPP")



#2. Creating a graph: Synthetic vs Real Spain -GDP -Pathplot
pdf(file = "Spain-Test-2011-GDP.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
synth_spain_gdp_YO_test = (prep_spain_gdp_test$Y0plot%*%synth_spain_gdp_test$solution.w)
plot(2005:2016, prep_spain_gdp_test$Y1plot, 
     type = "l", ylim = c(20000,45000), col = "black", lty = "solid", 
     ylab = "Per-Capita GDP Spain (PPP, USD)", 
     xlab = "year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
lines(2005:2016, synth_spain_gdp_YO_test, col = "black", lty = "dashed", lwd = 2)
abline(v=2011, lty = "dotted")
legend(x ="bottomright", 
       legend = c("Spain", "Synthetic Spain"), 
       lty = c("solid", "dashed"), col = c("black", "black"), 
       cex = .8, bg = "white", lwd = c(2,2))
arrows(2010,40000,2010.9,40000,col="black",length=.1)
text(2008,40000,"Placebo Labor Market Reform 2002",cex=0.6)


dev.off()

#Calculating Post MSPE/PRE MSPE

#Ratio of post-reunification RMSPE to pre-reunification RMSPE: West Germany and control countries.

# loop across control units
storegaps_spain = matrix(NA,
                         length(2002:2016),
                         length(unique(merge_crisis$id))-1
)
rownames(storegaps_spain) = 2002:2016
i = 1
co = unique(merge_crisis$id)

for(k in unique(merge_crisis$id)[-7]){
  
  #Training Model
  prep_spain_gdp_mspe = dataprep(
    foo = merge_crisis,
    predictors = c("infl", "loggdp", "emp_protect", "exports", "particip"),
    predictors.op = "mean",
    special.predictors = list(
      list("socsp", seq(1995,2013, 4), "mean"), 
      list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), c("mean"))
    ),
    dependent = "gdpc",
    unit.variable = "id",
    time.variable = "TIME",
    
    treatment.identifier = 7,
    controls.identifier = c(1,2,3,5,6),
    time.predictors.prior = c(2002:2012),
    time.optimize.ssr = c(2002:2011),
    unit.names.variable = "Country",
    time.plot = 2002:2016
  )
  
  #calculating synth command
  synth_spain_gdp_mspe = synth(prep_spain_gdp_mspe)
  
  # Prep Command
  prep_spain_gdp_mspe = dataprep(
    foo = merge_crisis,
    predictors = c("infl", "loggdp", "emp_protect", "exports", "particip"),
    predictors.op = "mean",
    special.predictors = list(
      list("socsp", seq(1995,2013, 4), "mean"), 
      list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), c("mean"))
    ),
    dependent = "gdpc",
    unit.variable = "id",
    time.variable = "TIME",
    
    treatment.identifier = k,
    controls.identifier = co[-which(co==k)],
    time.predictors.prior = 2002:2012,
    time.optimize.ssr = 2002:2011,
    unit.names.variable = "Country",
    time.plot = 2002:2016
  )
  
  # Synth Command
  synth_spain_gdp_mspe = synth(
    data.prep.obj= prep_spain_gdp_mspe,
    custom.v=as.numeric(synth_spain_gdp_mspe$solution.v)
  )
  
  storegaps_spain[,i] =  
    prep_spain_gdp_mspe$Y1-
    (prep_spain_gdp_mspe$Y0%*%synth_spain_gdp_mspe$solution.w)
  i = i + 1
} # close loop over control units
merge_crisis = merge_crisis[order(merge_crisis$id,merge_crisis$TIME),]
colnames(storegaps_spain) = unique(merge_crisis$country)[-7]
storegaps_spain = cbind(gap_spain_gdp,storegaps_spain)
colnames(storegaps_spain)[1] = c("Spain")

# compute ratio of post-reunification RMSPE 
# to pre-reunification RMSPE                                                  
rmse_spain = function(x){sqrt(mean(x^2))}
preloss_spain = apply(storegaps_spain[1:10,],2,rmse_spain)
postloss_spain = apply(storegaps_spain[11:14,],2,rmse_spain)

#create a data frame with the numbers
ratio_spain = postloss_spain/preloss_spain
ratio_df = matrix(ratio_spain)
ratio_df = as.data.frame(ratio_df)
ratio_df[, 2] = c("Spain", "Estonia", "Greece", "Ireland", "Italy", "Portugal", "Slovak Republic")
colnames(ratio_df) = c("postpre", "Country")
ratio_df = ratio_df[order(ratio_df$postpre),]

ratio_df$Country = as.(ratio_df$Country)
#plot the Distribution
pdf("ratio_post_to_preperiod_Spain.pdf")
plot(ratio_df$postpre, 
     type = "p", 
     xlab = "Countries", 
     ylab = "Post-Period RMSE / Pre-Period RMSE", 
     pch = 15
)
arrows(6.5,5,6.9,5.6,col="black",length=.1, lwd = 1.2)
text(6.2,5,"Spain",cex=1)

dev.off()
help(arrows)

#Last one: Doing a Placebo Study
# loop across control units
storegaps_placebo = matrix(NA,
                           length(2002:2016),
                           length(unique(merge_crisis$id))
)
rownames(storegaps_placebo) = 2002:2016
i = 1
co = unique(merge_crisis$id)

for(k in unique(merge_crisis$id)){
  
  #Training Model
  prep_spain_gdp_mspe = dataprep(
    foo = merge_crisis,
    predictors = c("infl", "loggdp", "emp_protect", "exports", "particip"),
    predictors.op = "mean",
    special.predictors = list(
      list("socsp", seq(1995,2013, 4), "mean"), 
      list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), c("mean"))
    ),
    dependent = "gdpc",
    unit.variable = "id",
    time.variable = "TIME",
    
    treatment.identifier = 7,
    controls.identifier = c(1,2,3,5,6),
    time.predictors.prior = c(2002:2012),
    time.optimize.ssr = c(2002:2011),
    unit.names.variable = "Country",
    time.plot = 2002:2016
  )
  
  #calculating synth command
  synth_spain_gdp_mspe = synth(prep_spain_gdp_mspe)
  
  # Prep Command
  prep_spain_gdp_mspe = dataprep(
    foo = merge_crisis,
    predictors = c("infl", "loggdp", "emp_protect", "exports", "particip"),
    predictors.op = "mean",
    special.predictors = list(
      list("socsp", seq(1995,2013, 4), "mean"), 
      list("schooling", c(2007,2008,2009,2010,2011,2012,2014,2015), c("mean"))
    ),
    dependent = "gdpc",
    unit.variable = "id",
    time.variable = "TIME",
    
    treatment.identifier = k,
    controls.identifier = co[-which(co==k)],
    time.predictors.prior = 2002:2012,
    time.optimize.ssr = 2002:2011,
    unit.names.variable = "Country",
    time.plot = 2002:2016
  )
  
  # Synth Command
  synth_spain_gdp_mspe = synth(
    custom.v=as.numeric(synth_spain_gdp_mspe$solution.v),#soll das rein oder nicht? Warum die alte Gewichtung beibehalten??
    data.prep.obj= prep_spain_gdp_mspe
  )
  
  storegaps_placebo[,i] =  
    prep_spain_gdp_mspe$Y1plot-
    (prep_spain_gdp_mspe$Y0plot%*%synth_spain_gdp_mspe$solution.w)
  i = i + 1
}
#making a data frame
storegaps_placebo = as.data.frame(storegaps_placebo)
#making the final superduper graph, yiha!
pdf(file = "Placebo-Test-Spain.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
gap_spain_gdp = prep_spain_gdp$Y1plot - (prep_spain_gdp$Y0plot%*%synth_spain_gdp$solution.w)
plot(2002:2016, storegaps_placebo$V7, 
     type = "l",
     yaxt = "n",
     ylim = c(-15000, 40000),
     col = "black", lty = "solid", 
     ylab = c("Gap in per-capita GDP (PPP, USD)"), 
     xlab = "Year", 
     xaxs = "i", 
     yaxs = "i",
     lwd = 6
)
ylabel = seq(-15000, 40000, by = 4000)
axis(1)
axis(2, at = ylabel, las = 1)
abline(v = 2012, lty = "dotted") 
abline(v = 2012, lty = "dotted")
abline(h = 0, lty = "dotted")
lines(2002:2016, storegaps_placebo$V1, col = "grey", lwd = 2)
lines(2002:2016, storegaps_placebo$V2, col = "grey", lwd = 2)
lines(2002:2016, storegaps_placebo$V3, col = "grey", lwd = 2)
lines(2002:2016, storegaps_placebo$V4, col = "grey", lwd = 2)
lines(2002:2016, storegaps_placebo$V5, col = "grey", lwd = 2)
lines(2002:2016, storegaps_placebo$V6, col = "grey", lwd = 2)

dev.off()

#Part Two: doing an Synth for all Countries, except the ones who implemented structural labour market reforms. 
#redefining merge1 Dataset
#merge_na = merge1[is.na(merge1$minwage),]

merge1$Country = as.character(merge1$Country)

#defining countries that had big labor reforms
noreforms = c("Spain","Belgium", "Canada", "Denmark", "Finland", "France", "Luxembourg", "Netherlands", "Norway", "Poland", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States")

merge2 = subset(merge1, (Country %in% noreforms))
#check for balancy

#check which years to choose for schooling
help_merge1 = merge1[, c("Country", "TIME", "schooling")]
help_merge1 = reshape(help_merge1, idvar = "Country", timevar = "TIME", direction = "wide")
help_merge1 = help_merge1[, -which(colMeans(is.na(help_merge1)) > 0.2)]
#help_spain = help_spain[, colSums(is.na(help_spain)) == 0]

#check for unemp

#set new id
merge2$Country = as.character(merge2$Country)
merge2$id = as.numeric(as.factor(merge2$Country))

#1. Spain Unemployment: SCM
prep_spain_all = dataprep(
  foo = merge2,
  predictors = c( "infl", "loggdp","exports", "emp_protect", "particip"),
  predictors.op = "mean",
  dependent = "gdpc",
  special.predictors = list(
    list("schooling", c(2005,2009,2010,2011,2012,2014), "mean"), 
    list("socsp", seq(2003,2013,2), "mean")
  ),
  unit.variable = "id",
  time.variable = "TIME",
  
  treatment.identifier = 10,
  controls.identifier = c(1,2,3,4,5,6,7,8,9,11,12,13,14),
  time.predictors.prior = c(2002:2012),
  time.optimize.ssr = c(2002:2011),
  time.plot = 2002:2016,
  unit.names.variable = "Country"
)


#calculating synth command
synth_spain_all = synth(prep_spain_all)

#calculating tables
synth.table_spain_all = synth.tab(
  dataprep.res = prep_spain_all,
  synth.res = synth_spain_all)

print(synth.table_spain_all)

spain_all_gaps = prep_spain_all$Y1plot-(
  prep_spain_all$Y0plot%*%synth_spain_all$solution.w
)
#performing plots
pathplot_spain_all = path.plot(dataprep.res = prep_spain_all, synth.res = synth_spain_all, 
                               tr.intake = 2012, Xlab = c("Year"), 
                               Ylab = "GDP per Capita Spain in Dollar, PPP")

gapsplot_spain_all = gaps.plot(dataprep.res = prep_spain_all, 
                               synth.res = synth_spain_all, tr.intake = 2012, 
                               Xlab = c("Year"), Ylab = "Harmonized Unemployment Rate Spain")


setwd("C:/Users/domin/Dropbox/Bachelorarbeit/Latex/NewGraphics")
#1. table that creates the differences in explanatory variables: Synthetic vs. Real
spain_tab1_all = data.frame(synth.table_spain_all$tab.w)
spain_tab1_all[,1] = round(spain_tab1_all[,1],2) 

table1_spain_all = xtable(cbind(spain_tab1_all))
colnames(table1_spain_all) = c("Weights", "Country", "Unit Number")

pdf(file = "Spain-GDP-ALL-Countries-Weights.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(table1_spain_all, rows = NULL)
dev.off()


#2. Creating a graph: Synthetic vs Real Spain -GDP -Pathplot
pdf(file = "Spain-Synthetic-Pathplot-GDPC-ALL.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
synth_spain_all_YO = (prep_spain_all$Y0plot%*%synth_spain_all$solution.w)#keine ahnung was das sein soll// was $YO ist 
plot(2002:2016, prep_spain_all$Y1plot, 
     type = "l", ylim = c(0,45000), col = "black", lty = "solid", 
     ylab = "Per-Capita GDP Spain (PPP, USD)", 
     xlab = "year", 
     xaxs = "i", yaxs = "i", 
     lwd = 2
)
lines(2002:2016, synth_spain_all_YO, col = "black", lty = "dashed", lwd = 2)
abline(v=2012, lty = "dotted")
legend(x ="bottomright", 
       legend = c("Spain", "Synthetic Spain"), 
       lty = c("solid", "dashed"), col = c("black", "black"), 
       cex = .8, bg = "white", lwd = c(2,2))
arrows(2011,20000,2011.8,20000,col="black",length=.1)
text(2008.8,20000,"Labor Reform 2012",cex=0.8)

dev.off()

#3. gap_spain 
pdf(file = "Spain-GDPC-Synthetic-Gapsplot-ALL.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
gap_spain_all = prep_spain_all$Y1plot - (prep_spain_all$Y0plot%*%synth_spain_all$solution.w)
plot(2002:2016, gap_spain_all, 
     type = "l",
     yaxt = "n",
     ylim = c(-6000, 6000),
     col = "black", lty = "solid", 
     ylab = c("Gap in per-capita GDP (PPP, USD)"), 
     xlab = "Year", 
     xaxs = "i", 
     yaxs = "i",
     lwd = 2
)
ylabel = seq(-6000, 6000, by = 2000)
axis(1)
axis(2, at = ylabel, las = 1)
abline(v = 2012, lty = "dotted")
abline(h = 0, lty = "dotted")
arrows(2011,4000,2011.8,4000,col="black",length=.1)
text(2008.8,4000,"Labor Reform 2012",cex=0.8)
dev.off()

#4. Table of the Different averaged variables

pred_table_spain_all = xtable(round(synth.table_spain_all$tab.pred , 1),digits=1)

pdf(file = "Spain-GDP-Dependent-Variables-Fit.pdf", width = 5.5, height = 5.5, family = "Times", pointsize = 12)
grid.table(pred_table_spain_all)
dev.off()