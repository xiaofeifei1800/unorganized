library(RSQLite)

setwd("I:/R Data")
db = dbConnect(SQLite(), "database.sqlite")
dbListTables(db)


library(ggplot2)
x = dbGetQuery(db, "SELECT Year, COUNT(Id) NumSchools FROM Scorecard GROUP BY Year")
ggplot(x, aes(x = Year, y = NumSchools)) + geom_bar(stat="identity", fill="lightblue")


earnings <- dbGetQuery(db, "
SELECT s11.INSTNM College,
       s11.CONTROL CollegeType,
       s11.md_earn_wne_p10 e50,
       s11.pct10_earn_wne_p10 e10,
       s11.pct25_earn_wne_p10 e25,
       s11.pct75_earn_wne_p10 e75,
       s11.pct90_earn_wne_p10 e90
FROM Scorecard s11

INNER JOIN Scorecard s13 ON s11.UNITID=s13.UNITID
WHERE s11.Year=2011
  AND s13.Year=2013
  AND s11.pct75_earn_wne_p10 IS NOT NULL
  AND s11.pct75_earn_wne_p10 != 'PrivacySuppressed'
  AND s11.PREDDEG = 'Predominantly bachelor''s-degree granting'
  AND s13.CCBASIC NOT LIKE '%Special%'
ORDER BY s11.pct75_earn_wne_p10 DESC")
earnings <- cbind(Rank=1:nrow(earnings), earnings)
earnings$College <- paste(earnings$Rank, earnings$College, sep=". ")
earnings$College <- factor(earnings$College, levels=rev(earnings$College))

ggplot(earnings, aes(x=e50, color=CollegeType, fill=CollegeType)) +
  geom_density(alpha=0.5) +
  theme_light(base_size=15) +
  xlab("Median Earnings 10 Years after Matriculation") + ylab("")

ggplot(earnings[1:20,], aes(x=College, ymin=e10, lower=e25, middle=e50, upper=e75, ymax=e90)) +
  geom_boxplot(stat="identity", fill="lightblue") + 
  geom_text(aes(x=College, y=e75-2000, hjust=0.95, label=paste0("$", e75)), size=4) + 
  theme(axis.text.y = element_text(hjust=0, color="black")) +
  theme_light(base_size=15) +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("Top Quartile Earnings 10 Years After Matriculation ($)")


sat <- dbGetQuery(db, "
SELECT INSTNM College,
       SATMTMID Math,
       SATVRMID Verbal,
       SATWRMID Writing
FROM Scorecard
WHERE Year=2013
  AND SATMTMID IS NOT NULL
  AND SATMTMID != 'PrivacySuppressed'
  AND SATVRMID IS NOT NULL
  AND SATVRMID != 'PrivacySuppressed'
  AND SATWRMID IS NOT NULL
  AND SATWRMID != 'PrivacySuppressed'")

head(sat)

library(tidyr)
ggplot(sat %>% gather(Section, Score, -College), aes(x=Score, color=Section, fill=Section, group=Section)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("SAT Score") +
  ylab("")

topmath = sat[order(sat[,2], decreasing = T)[1:20],1:2]
topmath <- cbind(Rank=1:nrow(topmath), topmath)
topmath$College <- paste(topmath$Rank, topmath$College, sep=". ")
topmath$College <- factor(topmath$College, levels=rev(topmath$College))

ggplot(topmath, aes(x = College, y = Math)) +

  geom_bar(stat = "identity", fill = "#53cfff") +
  geom_text(aes(x = College, y = Math-50,label = Math)) + 

  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  coord_flip() +
  xlab("") + ylab("")+
 ggtitle("Top SAT Math Scores")


cost <- dbGetQuery(db, "
SELECT INSTNM College,
       COSTT4_A Cost,
       CONTROL CollegeType
FROM Scorecard
WHERE Year=2013
  AND PREDDEG='Predominantly bachelor''s-degree granting'
  AND CCBASIC NOT LIKE '%Special Focus%'
  AND COSTT4_A IS NOT NULL
ORDER BY COSTT4_A DESC")
cost <- cbind(Rank=1:nrow(cost), cost)
cost$College <- paste(cost$Rank, cost$College, sep=". ")

ggplot(cost, aes(x=Cost, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("") + ylab("Cost of Attendance")

ggplot(cost[1:20,], aes(x=College, y=Cost)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=Cost-500, ymax=Cost, hjust=0.95, label=paste0("$", Cost)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("Most Expensive Colleges")
cost$College <- factor(cost$College, levels=rev(cost$College))


lowestAdmissionRates <- dbGetQuery(db, "
SELECT INSTNM College,
       ADM_RATE*100.0 AdmissionRate
FROM Scorecard
WHERE Year=2013
  AND ADM_RATE IS NOT NULL
  AND ADM_RATE != 0.0
  AND PREDDEG='Predominantly bachelor''s-degree granting'
  AND CCBASIC NOT LIKE '%Special Focus%'
ORDER BY ADM_RATE
LIMIT 20")
lowestAdmissionRates <- cbind(Rank=1:nrow(lowestAdmissionRates), lowestAdmissionRates)
lowestAdmissionRates$College <- paste(lowestAdmissionRates$Rank, lowestAdmissionRates$College, sep=". ")
lowestAdmissionRates$College <- factor(lowestAdmissionRates$College, levels=rev(lowestAdmissionRates$College))

ggplot(lowestAdmissionRates, aes(x=College, y=AdmissionRate)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=AdmissionRate-0.55, ymax=AdmissionRate, hjust=0.95, label=paste0(AdmissionRate, "%")), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("Lowest Admission Rates")
