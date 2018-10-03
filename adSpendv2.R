library(stringr)
library(ggplot2)
library(plyr)


opts_string = {theme(axis.text.x=element_text(size=rel(2),angle=0),
                     axis.text.y=element_text(size=rel(2)),
                     legend.text=element_text(size=rel(2)),
                     title=element_text(size=rel(2)),
                     panel.background=element_blank(),
                     panel.border=element_rect(color='black',fill=NA),
                     panel.grid.major=element_line(colour='grey20',linetype='dotted'),
                     panel.grid.minor=element_line(color='grey',linetype='dotted'))}


adSpending <- read.csv('google-political-ads-advertiser-stats.csv')

adClass <- read.csv('Advertiser Party Affiliation V2.csv')

#geoInfo <- read.csv('google-political-ads-geo-spend.csv')

adData <- merge(adClass, adSpending, by='Advertiser_Name')

#geoInfo <- merge(geoInfo, adData, by='Advertiser')
#geoInfo <- geoInfo[geoInfo$Office != "State", ]


# Violin Plot of COntributions
twoParties <- adData[adData$party=='R' | adData$party=='D', ]
twoParties <- twoParties[is.na(twoParties$party)==FALSE, ]
options(scipen=3) 

ggplot(twoParties, aes(y=Total_Spend_USD, x=party, fill=party)) +
  geom_violin(trim=FALSE) + scale_y_log10(labels=scales::dollar) + 
  xlab('Party\n') + ylab('Spending by Advertiser') + 
  scale_fill_manual(values=c("blue","red")) + 
  guides(fill=FALSE) +
  opts_string + 
  ggtitle('Spending Distribution on Google Ads for 2018 Midterm Election\nBreakdown by Party')
ggsave('SpendDistribute.png',units=c('cm'),width=50,height=50)

# Contributions over time
weekAds <- read.csv('weeklyAds.csv')
weekAds$Advertiser <- weekAds$Advertiser_Name
weekAdData <- merge(weekAds, adData, by='Advertiser')

dWeek<-weekAdData[weekAdData$party=='D', ]
rWeek<-weekAdData[weekAdData$party=='R', ]

rWeek <- aggregate(rWeek$Spend_USD, by=list(Week=rWeek$Week_Start_Date), FUN=sum)
dWeek <- aggregate(dWeek$Spend_USD, by=list(Week=dWeek$Week_Start_Date), FUN=sum)

weeklySpending <- merge(rWeek, dWeek, by='Week')
colnames(weeklySpending) <- c('Week', 'R', 'D')
weeklySpending$Week <- str_sub(weeklySpending$Week, 1, str_length(weeklySpending$Week)-5)

plot(weeklySpending$R, type='o', col='red', axes=FALSE, xlab='Week Start Date', ylab='Total Spending ($)')
lines(weeklySpending$D, type='o', col='blue')
axis(1, at=1:11, lab=weeklySpending$Week)
axis(2)


# National vs. Local Group
adData$groupType <- 'Race Specific'
adData <- within(adData, groupType[Race.Type == 'State'] <- 'Not Race Specific')
adData <- within(adData, groupType[Race.Type == 'National'] <- 'Not Race Specific')

groupType <- adData

groupType <- groupType[groupType$party=='R' | groupType$party=='D', ]
groupType$party <- as.character(groupType$party)
groupType$Race.Type<- as.character(groupType$Race.Type)

groupTypeAgg <- aggregate(groupType$Total_Spend_USD, by=list(Type=groupType$groupType, Party=groupType$party), FUN=sum)

ggplot(groupTypeAgg, aes(x=Type, y=x, fill = Party)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + scale_fill_manual(values=c('blue','red') ) + xlab('Group Type\n') + 
  ylab('Total Spent') +ggtitle('Spending Distribution on Google Ads for 2018 Midterm Election\nBreakdown by Group Type and Party') + 
  opts_string + scale_y_continuous(labels=scales::dollar)
ggsave('SpendByType.png',units=c('cm'),width=50,height=50)


# By race type
adData2 <- adData[adData$groupType=='Race Specific', ]
adData2 <- adData2[adData2$Race.Type != 'State House', ]
adData2 <- adData2[adData2$party=='R' | adData2$party=='D', ]
adData2<- aggregate(adData2$Total_Spend_USD, by=list(Office=adData2$Race.Type, Party=adData2$party), FUN=sum)

ggplot(adData2, aes(x=Office, y=x, fill = Party)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + scale_fill_manual(values=c('blue','red') ) + xlab('Office\n') +
  ylab('Total Spent') +ggtitle('Spending Distribution on Google Ads for 2018 Midterm Election\nBreakdown by Office Type and Party') +
  opts_string + scale_y_continuous(labels=scales::dollar)
ggsave('SpendByOffice.png',units=c('cm'),width=50,height=50)
