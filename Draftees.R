#Objective: Find associations in NFL draft data selections. Analysis done spearately by conference and by school. The rule data would be skewed if school and conference data was in same transaction dataset as schools only belong to one collegiate conference.

library(arules)
library(tidyverse)

####Exploratory Data Analysis & ggplot2####
#Uploaded using read.csv
DraftListFull <- read.csv('C:/Users/Saad/Desktop/Additional R Work/DraftListFull.csv', header = TRUE, na.strings = c("", "NA")) #na.strings puts an NA value in any cell that is blank
DraftListFull <- DraftListFull[complete.cases(DraftListFull), ] #Removes rows with any number of NA's in it
View(DraftListFull)
str(DraftListFull)
DraftListFull$Year.Drafted <- as.factor(DraftListFull$Year.Drafted)



#Picks by Conference
ggplot(data = DraftListFull) + geom_bar(mapping = aes(x=DraftListFull$Conference, fill = DraftListFull$Year.Drafted)) +  ggtitle("Picks by NCAAF Conference") + labs(x="NCAAF Conference", y = "Quantity") + labs(fill='Year Drafted') + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1))


#Picks by NFL Team
ggplot(data = DraftListFull) + geom_bar(mapping = aes(x=DraftListFull$Team, fill = DraftListFull$Year.Drafted)) + ggtitle("Picks by NFL Team") + labs(x="NFL Teams", y = "Quantity") + labs(fill='Year Drafted') + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1))


#Picks by Player Position
ggplot(data = DraftListFull) + geom_bar(mapping = aes(x=DraftListFull$Position, fill = DraftListFull$Position)) + ggtitle("Picks by Player Position") + labs(x="Player Position", y = "Quantity") + labs(fill='Player Position') + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1))



####Transaction Data by School####
#Again, I am not including school and conference in the same dataset since schools automatically belong to a conference and thus no significant rules can come out of associations between schools and conferences
#Need to prepare data from cleaned dataset above in order to re-upload data as .txt into a transaction dataset
DraftListPared <- DraftListFull[,-c(1,2,4,7,8)] #Remove columns 1,2,4,7 and 8 to prepare for school-based associations
write.table(DraftListPared, 'C:/Users/Saad/Desktop/Additional R Work/DraftListSchool.txt', sep ="\t")
DraftListSchool <- read.transactions('C:/Users/Saad/Desktop/Additional R Work/DraftListSchool.txt', sep ="\t")
summary(DraftListSchool)
DraftListSchool

itemFrequency(DraftListSchool[,1:10])
itemFrequencyPlot(DraftListSchool, support = .10) #Returns items with support .10 (suppport indicates the number of times an item shows up in a dataset relative to all the other items)
itemFrequencyPlot(DraftListSchool, topN = 5) #Returns items with top support

DraftRuleSchool <- apriori(DraftListSchool, parameter = list(support=0.005, confidence=.10, minlen=2))
DraftRuleSchool
summary(DraftRuleSchool)
inspect(DraftRuleSchool)
inspect(sort(DraftRuleSchool, by="lift"))



####Transaction Data by Conference####
#Removing same columns as above but keeping conference in and removing school column 
DraftListPared2 <- DraftListFull[,-c(1,2,4,6,8)] #Remove columns 1,2,4,6 and 8 to prepare for conference-based associations
write.table(DraftListPared2, 'C:/Users/Saad/Desktop/Additional R Work/DraftListConference.txt', sep ="\t")
DraftListConference <- read.transactions('C:/Users/Saad/Desktop/Additional R Work/DraftListConference.txt', sep ="\t")
summary(DraftListConference)
DraftListConference

itemFrequency(DraftListConference[,1:10])
itemFrequencyPlot(DraftListConference, support = .10) #Returns items with support .10
itemFrequencyPlot(DraftListConference, topN = 5) #Returns items with top support

DraftRuleConference <- apriori(DraftListConference, parameter = list(support=0.011, confidence=.15, minlen=2))
DraftRuleConference
summary(DraftRuleConference)
inspect(DraftRuleConference)
inspect(sort(DraftRuleConference, by="lift"))

