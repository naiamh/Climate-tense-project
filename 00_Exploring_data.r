# Preliminary analyses of climate-tense data
# May 8 2015 v1
# Naia, Carrie and Andrew

rm(list=ls())

library(ggplot2)

# Read in the data 

# NOTE: DOWNLOAD COPY FROM GOOGLE DRIVE AND CHANGE FILE NAME HERE ACCORDINGLY
#rawData = read.csv('Data/Test_data.csv', as.is=T) 
rawData = read.csv('Data/Data_CC_tense_28Sept2015.csv', as.is=T) 

#----------#
# CLEANING #
#----------#
# note: several of these clean-ups should be irrelevant once we have the full dataset

# Filter papers that have actually been checked
filledData = rawData[which(rawData$OK_for_analysis != ''),]
table(filledData$OK_for_analysis)

# Select papers that are OK for analysis, and the relevant columns
df = filledData[which(filledData$OK_for_analysis == 'yes'),
                    c('ID_old','Publication_year','Journal','paper_focus',
                      'tense','confidence_in_tense','climate_date_range',
                      'climate_date_min','climate_date_max',
                      'climate_data_source','citation',
                      'geographic_extent_of_study','Data_collector_name')]

# Filter papers where climate_data_range is filled correctly
table(df$climate_date_range)
df = subset(df, climate_date_range == T | df$climate_date_range == F)

# Lump the classes "implicit past" and "past", and "implicit present" and "present" in a new column
df$lump_tense = df$tense
df$lump_tense[which(df$tense=='implicit past' | df$tense=='past')] = 'past'
df$lump_tense[which(df$tense=='implicit present' | df$tense=='present')] = 'present'

# Table with impact factor for each journal (from www.citefactor.org")
IF_lookup = data.frame(Journal = sort(unique(df$Journal)), 
                       IF2014=c(4.454, 5.469, 5.000, 13.042, 4.248, 5.694, 42.351, 9.809))

#-----------------#
# THE FUN BEGINS! #
#-----------------#

#............
## Differences across journals?
# Plot the distribution of all tenses by journal
hist_cut = ggplot(df, aes(x=Journal, fill=tense))
hist_cut + geom_bar()

# Plot the distribution of lumped tenses by journal
hist_cut = ggplot(df, aes(x=Journal, fill=lump_tense))
hist_cut + geom_bar(position="fill")

#..............
## Differences across time?
#Distribution of papers over time --> use to decide whether to work with proportions or raw counts
hist(rawData$Publication_year, breaks=seq(1979,2020,10), main="Papers per time period", ylim=c(0,250)) #all papers
hist(df$Publication_year, breaks=seq(1979,2020,10), add=T, col="grey")
legend(x="topright", legend=c("All", "In analysis"), fill=c("white", "grey"))

# Plot % tense == past as a function of time period, with one line per journal, and boxplot of means
rownames(df)=NULL
tyear = do.call(rbind, by(df, df$Journal, function(x) {
  prop = tapply(x$tense, floor(x$Publication_year/10)*10, function(a){length(which(a=='past'))/length(a)}) 
  prop2 = tapply(x$tense, floor(x$Publication_year/10)*10, function(a){length(which(a=='present'))/length(a)})
  data.frame(Journal=x$Journal[1], decade = as.numeric(names(prop)), percent_past=as.vector(prop), percent_present=as.vector(prop2))
}))
# Add impact factor to tyear table
tyear$IF = IF_lookup$IF2014[match(tyear$Journal, IF_lookup$Journal)]



prop_ave = tapply(df$tense, floor(df$Publication_year/10)*10, function(a){length(which(a=='past'))/length(a)})  
prop_ave2 = tapply(df$tense, floor(df$Publication_year/10)*10, function(a){length(which(a=='present'))/length(a)})  

tyear_ave = data.frame(decade = as.numeric(names(prop_ave)), percent_past=as.vector(prop_ave), percent_present=as.vector(prop_ave2))

ggplot() +
  geom_line(data=tyear, 
            mapping=aes(x=decade, y=percent_past, group=Journal, 
                        color=Journal),
            size=1) +
  scale_color_brewer(palette="Set3") +
  geom_line(data=tyear_ave,
            mapping=aes(x=decade, y=percent_past),
            linetype="dashed")

#..............
## Differences across impact factor?
# Weight the colors in journal plot by impact factor of journal (warmer color higher IF)
mid=mean(log10(tyear$IF))
ggplot() +
  geom_line(data=tyear, 
            mapping=aes(x=decade, y=percent_present, group=Journal, 
                        color=log10(tyear$IF)),
            size=1.5) +
  scale_color_gradient(low="blue",high="red") +
  geom_line(data=tyear_ave,
            mapping=aes(x=decade, y=percent_present),
            linetype="dashed")


#..............
## Differences depending on focus of the paper?
hist_cut = ggplot(df, aes(x=geographic_extent_of_study, fill=lump_tense))
hist_cut + geom_bar(position="fill")

hist_cut = ggplot(df, aes(x=paper_focus, fill=climate_date_range))
hist_cut + geom_bar(position="fill")

# Difference in present vs past tense depending on focus
hist_cut = ggplot(df, aes(x=paper_focus, fill=lump_tense))
hist_cut + geom_bar(position="fill")

hist_cut = ggplot(df, aes(x=lump_tense, fill=climate_date_range))
hist_cut + geom_bar(position="fill")


### "Behind the scenes" analyses
### (to check that our results are not affected by collector or confidence in tense)

## Are there differences in the classification of tense depending on who checked the paper (extractor)?
hist_cut = ggplot(df, aes(x=Data_collector_name, fill=lump_tense))
hist_cut + geom_bar(position="fill")

# Do anova test!

## Are results affected by category "confidence_in_tense"?
# Test results including and excluding "confidence_in_tense" == "no" --> if no difference, include all
# if difference: check "no"'s with everyone to assign to "yes" or ditch paper and pick a new one
table(df$confidence_in_tense, df$tense)



