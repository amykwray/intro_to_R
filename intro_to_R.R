######GETTING STARTED######
# what are these hashtags?? this lets you write text in an R script but
# it doesn't actually run as a function. that will make sense later but 
# basically, putting a hashtag in front of text lets you take notes and 
# annotate your R scripts, which is a good practice so you don't forget 
# why or what you did...

###### if you want to create a heading you need hashtags on both sides ########

# download R and R studio at http://www.r-project.org/ and http://www.rstudio.org/

# to run commands from your script, hit command + enter or the run button (top right corner)
# you can also type commands directly into the console, for example, if you don't want to
# save it in a script.

# create a folder called "intro_to_R" on your desktop and set that as your working directory.
# (a working directory is a nice way to keep your files organized)
# assuming your working directory is a file named "intro_to_R" on your desktop,
# with all of the .csv files for this tutorial in it,
# go to the top toolbar and choose session->set working directory->choose directory 
# and then select whatever folder you want to use OR use this command:

setwd("~/Desktop/intro_to_R")

##on a pc:
setwd("C:/User/name/Desktop/intro_to_R")

# install packages that we will be using (including dependencies -- might take a few minutes)
# you only need to install packages once, then call them later using "library"()
# (re-install packages after you update R/RStudio)

#find out which version of R you have:
version
#you should have R version 4.1.0 & RStudio 1.4.1717. if not, update.

install.packages("ggpubr")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("devtools")
install.packages("maps")
install.packages("praise")
install.packages("DataCombine")
install.packages("wesanderson")
install.packages("unikn")
install.packages("ggridges")

# now use "library" to load/activate those packages:

library(tidyverse)
library(reshape2)
library(devtools)
library(maps)
library(praise)
library(DataCombine)
library(wesanderson)
library(ggpubr)
library(unikn)
library(ggridges)

# now, one *key* package is NOT a CRAN package so we install from github (using devtools):

install_github("Gibbsdavidl/CatterPlots")
library(CatterPlots)

#Viewing citations:
citation()
citation("ggplot2")

# cool. ok so get familiar with how R works. it can be a calculator e.g.
4+5
10^2

# naming a variable:
x<-5
x

# renaming that variable:
x<-7
x

# or you can write your own vectors using the "concatenate" function e.g.
a<-c(1,2,3,4)
b<-c(5,6,7,8)
sum(a)
mean(b)

#creating a data matrix: cbind: binds lists by columns, rbind: binds lists by rows
m<-cbind(a,b)
m2<-rbind(a,b)

# use "View" to look:
View(m)
View(m2)

#you can also look at your new matrix in the console:
m
m2

#in R, columns and rows can also be selected following the syntax of [rows,columns]
#so to select the second row of m:
m[2,]

#and the second column of m2:
m2[,2]

# you can also do simple stats very easily. here's an example from our mosquito paper.
# these are values representing little brown (mylu) and big brown (epfu) bat diet samples 
# (23 and 7) with at least one mosquito species detected in a guano sample vs 
# guano samples with no mosquito detections (9 and 14):

a<-c(23,7)
b<-c(9,14)
c<-cbind(a,b)
chisq.test(c)

# these are values showing the average number of mosquito species detected in guano samples at each site:
epfu_site_avg<-c(0,1,2,1,0,0,4,2,1,0)
mylu_site_avg<-c(8,1,7,13,7,7,5,1,1,5,4,4)
t.test(mylu_site_avg,epfu_site_avg,paired=FALSE)

# if you forget the notation for a function you can run:
?t.test
?chisq.test

# and it will give you more information. there are a ton of different functions built
# into R even before you add any libraries or packages or whatever.

# you're doing so well! how about some praise?
praise()

# need more praise? hit the "up" arrow in the console to go to your previous command
# you can keep using the up or down arrows to navigate to previously entered commands. 

####UPLOADING A DATA FILE#####
# be careful with your data files!!! R can get cranky with spaces, special characters, excel formulas,
# or anything that confuses it. also, computers are very literal so if you have two things
# with different spellings, it doesn't know that those are different. so check that your variables
# are spelled correctly with the same formatting -- ie site "Yellowstone" will be treated
# differently than site "yellowstone", "yellowston", and "Yellowstone ". 

# upload a file. you can name it whatever you want, but try to give a meaningful label.
# or use "df" for "dataframe", which is a common name scheme.
# this is a dataframe of some passive acoustic monitoring for bat calls from 2 years.
# "high calls" is high frequency calls ≥35 kHz (corresponds to little brown bats and some other smaller bats)
# "low calls" is low frequency calls <35 kHz (correspond to big brown bats & some other larger bats),
# and "all calls" is the combination of both high & low.

df<-data.frame(read.csv(file="calls.csv"))
#check out the structure of your data, there are several helpful functions:
str(df)
summary(df)
names(df)
#how many rows are there?
nrow(df)
#how many columns are there?
length(df)

#rows & columns all at once:
dim(df)

#you can also look at selected parts of your dataset (e.g., columns 2-4 & column 7):
View(df[,c(2:4, 7)])

# you can use "$" to select variables in your data frame:
hist(df$low_calls)
hist(df$high_calls)

# peek at potential correlated variables (numeric only)
pairs(df[,c(3,5,6,7)])

# you can do the same looking at transformed data:
# (use an offset of +1 because log(0) is -Inf)
# note: in R, "log" is natural log, while "log10" is log base 10.
hist(log(df$low_calls+1))

# rename your data frame:
calls<-df

# take a subset of that dataset:
nobats<-subset(calls,type=="bug", drop=T)
epfu<-subset(calls, sp=="epfu", drop=T)
mylu<-subset(calls, sp=="mylu", drop=T)

# summarize calls for each site:
calls%>%
  group_by(site)%>%
  summarize(total=sum(all_calls),
              median=median(all_calls),
              mean=mean(all_calls),
              sd=sd(all_calls))

# note on summary stats: typically, report mean if data are normally distributed
# report the median if the data are non-normally distributed (or if there are extremes)

# change plot window to have 1 row and 3 columns:
par(mfrow=c(1,3))
boxplot(log(mylu$high_calls+1),log(mylu$low_calls+1), main="MYLU Roosts")
boxplot(log(nobats$high_calls+1),log(nobats$low_calls+1), main="Control")
boxplot(log(epfu$high_calls+1),log(epfu$low_calls+1), main="EPFU Roosts")

# change it back:
par(mfrow=c(1,1))

# make a better boxplot with ggplot2.
# first, convert to long format:

calls_melt<-reshape2::melt(calls[,c(1:7,9,11)], id.vars=c("sample_no","site","week","year","type","sp"))
View(calls_melt)

calls_grouped<-subset(calls_melt, calls_melt$variable != "all_calls")
p<-ggplot(aes(x=variable, y=log(value+1), color=variable), data=calls_grouped)+geom_boxplot()
p


# there are all kinds of different point types. see:
?pch

# a few more improvements, including adding group comparison stats with ggpubr: 
pp<-ggplot(aes(x=variable, y=log(value+1), color=variable), data=calls_grouped)+
  geom_boxplot(outlier.shape=NA, width=0.4, lwd=1)+
  geom_jitter(width=0.1, alpha=0.8, color="gray70", pch=1)+facet_wrap(~year)+
  stat_compare_means(method="wilcox.test")+
  xlab("frequency group")+
  ylab("average weekly pulses (log +1)")+
  theme_classic()+
  theme(legend.position="none")
pp

#much better!

# confirm that the answer from stat_compare_means() is correct:
wilcox.test(value~variable, data=calls_grouped, subset = year %in% 2015)
wilcox.test(value~variable, data=calls_grouped, subset = year %in% 2016)

# stats note: wilcoxon rank sum tests are also the same for transformed data:

wilcox.test(log(value+1)~variable, data=calls_grouped, subset = year %in% 2015)
wilcox.test(log(value+1)~variable, data=calls_grouped, subset = year %in% 2016)


## you can also make maps in R!
# see http://www.molecularecologist.com/2012/09/making-maps-with-r/ -- has a nice tutorial/ more info.

#### MAPS ########

### state maps USA
###upload gps points (lat/lon) e.g.
gps<-data.frame(read.csv(file="2014_guano_gps_pts.csv"))
names(gps)
##subset if u want different types of points
mylu<-subset (gps, sp=="mylu")
epfu<-subset (gps, sp=="epfu")

#open a new window (on a mac)
quartz()

#or on a pc:
windows()

# using the "map" function from the "maps" package
# (to call a function within a package, use the package name, then "::", then the function):

maps::map("county", "Wisconsin")

points(mylu$lon,mylu$lat,pch=17,col="#203731", cex=2)
points(epfu$lon,epfu$lat,pch=21,col="#9B6B00", cex=2)

# side note: do those colors look familiar? ;)

### usa map with wyoming shaded

quartz()
#windows()

maps::map(database="state")
maps::map(database="state", regions="wyoming", col="gray40", fill=T, add=TRUE)


# making maps with your own color palette:

#make the palette:
gg_main<-c("#CFB06F","#437B88","#EA93A0","#9F3537","#97BC50","#FE5D2B","#ADA9A4")
oc_main<-c("#07486D","#4796B5","#ABCBC8","#F8E77E","#EEA94A","#F56848","#AF2817")

# View palettes:
unikn::seecol(oc_main)
unikn::seecol(gg_main)

quartz()
maps::map(database="state")
maps::map(database="state", regions=c("arizona","california","colorado","idaho","nevada","oregon","utah"),
    col=gg_main, fill=TRUE, add=TRUE)


## more figures with your custom palettes:
# create some fake data: 

df<-data.frame(ID=c(rep("a",10),rep("b",10),rep("c",10),rep("d",10),
                    rep("e",10),rep("f",10),rep("g",10)),
               v1=rnorm(70,0,100),
               v2=runif(70, 0, 1))

### figures:

ggplot(df, aes(x=ID, y=v1, fill=factor(ID)))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(pch=1)+
  scale_fill_manual(values=gg_main)+
  theme_minimal()

ggplot(df, aes(x=v2, y=ID, fill=ID))+
  geom_density_ridges(alpha=0.8)+
  scale_fill_manual(values=oc_main)+
  scale_y_discrete(limits=rev(df$ID))+
  theme_classic()

# and you can find more colors here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# other resources for colors: 
# http://tools.medialab.sciences-po.fr/iwanthue/
# http://colorbrewer2.org
# you can put color names or any hexadecimal code that you want. there are many
# themed color packages, or you can make a plot with cats (so far there 
# is not a "batterplots" package yet). 
# this next bit of code is adapted from https://github.com/Gibbsdavidl/CatterPlots
# (more examples there too!)

# for fun
x <- -10:10
y <- -x^2 + 10
purr <- CatterPlots::catplot(xs=x, ys=y, cat=3, catcolor='steelblue1')
CatterPlots::cats(purr, -x, -y, cat=4, catcolor='springgreen')

# random cats
meow <- CatterPlots::multicat(xs=x, ys=rnorm(21),
                 cat=c(1,2,3,4,5,6,7,8,9,10),
                 catcolor=list('#33FCFF',"dodgerblue","gray70","deeppink"),
                 canvas=c(-0.1,1.1, -0.1, 1.1),
                 xlab="some cats", ylab="other cats", main="Random Cats")

##### back to serious work!!!! cleaning up messy data #####
## this example is for processing raw data from a real-time quaking induced conversion assay (RT-QuIC).
## this assay can be used to detect prions, so basically over time/cycles there are changes in values
## of fluorescence which can indicate presence of prions in a sample. this data set is from an experiment
## testing some blinded ("unknown") samples as well as different concentrations of known positive samples.

df<-data.frame(read.csv(file="06032021_run5_raw_results.csv"))
meta<-data.frame(read.csv(file="RTQ_samples_meta.csv"))

names(df)[1]<-"Well"
names(df)[2]<-"SampleID"
length(df)

# I personally do not like numbers in the labels for a column, so we are 
# pasting a "t" for timepoint, even though it will be removed later
names(df)[3:102]<-paste("t", c(1:100), sep="")

# you have to run this command twice because of the negative control name.
df$SampleID <- sub(" ", "_", df$SampleID)
df$SampleID <- sub(" ", "_", df$SampleID)

# I also find single digit sample IDs to be extremely distressing. Use
# find & replace to fix it so they can be in order:

replaces<-data.frame(from=c("Sample_X1", "Sample_X2", "Sample_X3","Sample_X4",
                            "Sample_X5","Sample_X6","Sample_X7","Sample_X8",
                            "Sample_X9"), 
                     to=c("Sample_X01","Sample_X02","Sample_X03","Sample_X04",
                          "Sample_X05","Sample_X06","Sample_X07","Sample_X08",
                          "Sample_X09"))
df_renamed<-FindReplace(data=df, Var="SampleID", replaceData=replaces, 
                        from="from", to="to", exact=TRUE)

# change back to df for simplicity:
df2<-df_renamed
head(df2)

# you can also plot the regular output type curves, but to do this, you need
# to aggregate data (take the mean from each SampleID):
df_ag<-aggregate(.~SampleID, data=df2[,2:102], mean)

# then, it needs to be in "long" format:
# to call a function directly from within a package, use "::" (recommended for reshape2):
df_melt<-reshape2::melt(df_ag, id.vars="SampleID")

# find the maximum value:
max_fl<-max(df2[,3:102])

# create a new variable for the percentage of the total:
df_melt$percent_total<-df_melt$value/max_fl

df_merged<-merge(meta, df_melt, by="SampleID", all=TRUE)
df_merged2<-subset(df_merged, df_merged$variable !="t1" & df_merged$type!="NA")
df_merged2$cycle<-str_remove(df_merged2$variable, "t")
df_merged2$cycle<-as.integer(df_merged2$cycle)

# simple plotting with ggplot2:

p1<-ggplot(aes(x=cycle, y=percent_total, color=type), data=df_merged2)+
  geom_point()
p1

# that is one rather unfortunate plot my friend. add a few easy fixes:

p2<-ggplot(aes(x=cycle, y=percent_total, group=SampleID, color=type, 
              shape=as.factor(concentration)), data=df_merged2)+
  geom_point()+
  theme_classic()+geom_line()+
  ylab("percent total fluorescence")
p2

# a few more aesthetic improvements:

p2+facet_wrap(~type, ncol=1)+
  scale_color_manual(values=wes_palette("IsleofDogs1"))+
  geom_hline(yintercept=0.2, lty=2)+labs(shape="concentration")

# beautiful!!!! now, the rest of using R is pretty much just advanced googling!!!
praise()

#####END TUTORIAL#####