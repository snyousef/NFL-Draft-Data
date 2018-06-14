#Objective: Find associations in NFL draft data selections. Analysis done spearately by conference and by school. The rule data would be skewed if school and conference data was in same transaction dataset as a school is only a part of one conference.

library(arules)
library(tidyverse)

####Exploratory Data Analysis & ggplot2####
#Uploaded using read.csv
DraftListFull <- read.csv('C:/Users/Saad/Desktop/Additional R Work/DraftListFull.csv') #Loaded again but as table so that exploratory data analysis can be done
View(DraftListFull)
str(DraftListFull)
complete.cases(DraftListFull)
sum(is.na(DraftListFull))
DraftListFull <- DraftListFull[-c(385,468),]
DraftListFull$Year.Drafted <- as.factor(DraftListFull$Year.Drafted)



#Picks by Conference
ggplot(data = DraftListFull) + geom_bar(mapping = aes(x=DraftListFull$Conference, fill = DraftListFull$Year.Drafted)) +  ggtitle("Picks by NCAAF Conference") + labs(x="NCAAF Conference", y = "Quantity") + labs(fill='Year Drafted') + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1))


#Picks by NFL Team
ggplot(data = DraftListFull) + geom_bar(mapping = aes(x=DraftListFull$Team, fill = DraftListFull$Year.Drafted)) + ggtitle("Picks by NFL Team") + labs(x="NFL Teams", y = "Quantity") + labs(fill='Year Drafted') + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1))


#Picks by Player Position
ggplot(data = DraftListFull) + geom_bar(mapping = aes(x=DraftListFull$Position, fill = DraftListFull$Position)) + ggtitle("Picks by Player Position") + labs(x="Player Position", y = "Quantity") + labs(fill='Player Position') + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1))



####Transaction Data by School####
DraftListSchool <- read.transactions('C:/Users/Saad/Desktop/Additional R Work/DraftListSchool.txt', sep ="\t")
summary(DraftListSchool)
DraftListSchool

##Scratch work to determine appropriate support levels
#32 NFL Teams, 207 Schools, 21 Player Positions = 260 unique values
#DraftListSchool: Denisty=.01143243; 1276 (rows)*262(columns)*(.01143243) = 3822 cells with values
#3822/(32+207+21) = 14.7; if each item was represented equally, each would show up 14.7 times
#14.7/3822 = .003846155; any item with support higher than ~.003 shows up more often than the expected average

itemFrequency(DraftListSchool[,1:10])
itemFrequencyPlot(DraftListSchool, support = .10) #Returns items with support .10
itemFrequencyPlot(DraftListSchool, topN = 5) #Returns items with top support

DraftRuleSchool <- apriori(DraftListSchool, parameter = list(support=0.004, confidence=.10, minlen=2))
DraftRuleSchool      
summary(DraftRuleSchool)
inspect(DraftRuleSchool[1:39])
inspect(sort(DraftRuleSchool, by="lift"))



####Transaction Data by Conference####
DraftListConference <- read.transactions('C:/Users/Saad/Desktop/Additional R Work/DraftListConference.txt', sep ="\t")
summary(DraftListConference)
DraftListConference

####Scratch work to determine appropriate support levels
#32 NFL Teams, 43 Conferences, 21 Player Positions = 96 unique values
#DraftListConference: Density= .03087128; 1276(rows)*97(columns)*(.03087128) = 3821
#3821/(32+43+21) = 39.80208; if each item was represented equally, each would show up 39.80208 times
#39.80208/3821 = .01041667; any item with support hifher than ~.01 shows up more often than the expected average

itemFrequency(DraftListConference[,1:10])
itemFrequencyPlot(DraftListConference, support = .10) #Returns items with support .10
itemFrequencyPlot(DraftListConference, topN = 5) #Returns items with top support

DraftRuleConference <- apriori(DraftListConference, parameter = list(support=0.011, confidence=.10, minlen=2))
DraftRuleConference      
summary(DraftRuleConference)
inspect(DraftRuleConference[1:33])
inspect(sort(DraftRuleConference, by="lift"))

