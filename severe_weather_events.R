# storm_data <- read.csv("repdata-data-StormData.csv")

severe <- function() {
head(storm_data)

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# remove rows with missing fatalies or injuries data
print(nrow(storm_data))
# storm_data_by_event <- subset(storm_data, !is.na(c(storm_data$FATALITIES, storm_data$INJURIES)))
# print(nrow(storm_data))
# 
# storm_data_by_event <- storm_data_by_event %>%
#   group_by(EVTYPE) %>%
#     summarise(FATALITIES_TOTAL = sum(FATALITIES), INJURIES_TOTAL = sum(INJURIES), BOTH_TOTAL = sum(FATALITIES + INJURIES))
# 
# print(nrow(storm_data_by_event))
# 
top_limit <- 5
# 
# storm_data_by_event_fatalities <- arrange(storm_data_by_event, desc(FATALITIES_TOTAL))[1:top_limit, ]
# # print(head(storm_data_by_event_fatalities))
# # print(storm_data_by_event_fatalities[1:top_limit, ]$FATALITIES_TOTAL)
# p <- with(storm_data_by_event_fatalities, 
#           qplot(reorder(EVTYPE, -FATALITIES_TOTAL), 
#                 FATALITIES_TOTAL,
#                 main = paste("Top", top_limit, "Fatality-Causing Weather Events"), 
#                 xlab = "Event Type", 
#                 ylab = "Total Fatalities") + 
#           geom_bar(stat="identity") + 
#           theme(axis.text.x = element_text(angle = 90)))
# 
# rm(storm_data_by_event_fatalities)
# 
# storm_data_by_event_injuries <- arrange(storm_data_by_event, desc(INJURIES_TOTAL))[1:top_limit, ]
# # print(head(storm_data_by_event_injuries))
# q <- with(storm_data_by_event_injuries, 
#           qplot(reorder(EVTYPE, -INJURIES_TOTAL), 
#                 INJURIES_TOTAL, 
#                 main = paste("Top", top_limit, "Injury-Causing Weather Events"),
#                 xlab = "Event Type", 
#                 ylab = "Total Injuries") + 
#           geom_bar(stat="identity") + 
#           theme(axis.text.x = element_text(angle = 90)))
# 
# rm(storm_data_by_event_injuries)
# 
# storm_data_by_event_total <- arrange(storm_data_by_event, desc(BOTH_TOTAL))[1:top_limit, ]
# # print(head(storm_data_by_event_injuries))
# r <- with(storm_data_by_event_total,
#           qplot(reorder(EVTYPE, -BOTH_TOTAL),
#                 BOTH_TOTAL,
#                 main = paste("Top", top_limit, "Fatality and Injury-Causing Weather Events"),
#                 xlab = "Event Type", 
#                 ylab = "Total Fatalities and Injuries") + 
#           geom_bar(stat="identity") + 
#           theme(axis.text.x = element_text(angle = 90)))
# 
# rm(storm_data_by_event_total)

# grid.arrange(p, q, r)

# group by economic impact
storm_data_by_impact <- storm_data
print(head(storm_data_by_impact))
print(nrow(storm_data_by_impact))
print(levels(storm_data_by_impact$PROPDMGEX))
print(levels(storm_data_by_impact$CROPDMGEXP))
print(unique(storm_data_by_impact$PROPDMGEXP))
print(unique(storm_data_by_impact$CROPDMGEXP))
print(head(storm_data_by_impact[storm_data_by_impact$PROPDMGEXP == "2", ]))
complete_rows <- complete.cases(storm_data_by_impact[, c("PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")])
storm_data_by_impact <- storm_data_by_impact[complete_rows, ]
storm_data_by_impact <- storm_data_by_impact[(storm_data_by_impact$PROPDMG > 0 & storm_data_by_impact$CROPDMG > 0), ]
# storm_data_by_impact <- storm_data_by_impact[(!(storm_data_by_impact$PROPDMGEXP %in% c("", "-", "?", "+")) & !(storm_data_by_impact$CROPDMGEXP %in% c("", "-", "?", "+"))), ]
storm_data_by_impact$PROPDMGEXP <- as.character(storm_data_by_impact$PROPDMGEXP)
storm_data_by_impact$CROPDMGEXP <- as.character(storm_data_by_impact$CROPDMGEXP)

storm_data_by_impact$PROPDMG[storm_data_by_impact$PROPDMGEXP %in% c("", "-", "?", "+")] <- 0  
storm_data_by_impact$PROPDMGEXP[storm_data_by_impact$PROPDMGEXP %in% c("", "-", "?", "+")] <- 0  
storm_data_by_impact$PROPDMGEXP[storm_data_by_impact$PROPDMGEXP %in% c("k", "K")] <- 3  
storm_data_by_impact$PROPDMGEXP[storm_data_by_impact$PROPDMGEXP %in% c("m", "M")] <- 6  
storm_data_by_impact$PROPDMGEXP[storm_data_by_impact$PROPDMGEXP %in% c("b", "B")] <- 9  

storm_data_by_impact$CROPDMG[storm_data_by_impact$CROPDMGEXP %in% c("", "-", "?", "+")] <- 0  
storm_data_by_impact$CROPDMGEXP[storm_data_by_impact$PROPDMGEXP %in% c("", "-", "?", "+")] <- 0  
storm_data_by_impact$CROPDMGEXP[storm_data_by_impact$CROPDMGEXP %in% c("k", "K")] <- 3  
storm_data_by_impact$CROPDMGEXP[storm_data_by_impact$CROPDMGEXP %in% c("m", "M")] <- 6  
storm_data_by_impact$CROPDMGEXP[storm_data_by_impact$CROPDMGEXP %in% c("b", "B")] <- 9  

storm_data_by_impact <- storm_data_by_impact %>%
  group_by(EVTYPE, PROPDMGEXP, CROPDMGEXP) %>%
  summarise(PROPDMG_UNSCALED_TOTAL = sum(PROPDMG) / 1000000, 
            CROPDMG_UNSCALED_TOTAL = sum(CROPDMG) / 1000000)

print(storm_data_by_impact)

storm_data_by_impact$PROPDMG_SCALED_TOTAL = storm_data_by_impact$PROPDMG_UNSCALED_TOTAL * (10 ^ as.integer(storm_data_by_impact$PROPDMGEXP))
storm_data_by_impact$CROPDMG_SCALED_TOTAL = storm_data_by_impact$CROPDMG_UNSCALED_TOTAL * (10 ^ as.integer(storm_data_by_impact$CROPDMGEXP))

storm_data_by_impact <- storm_data_by_impact %>%
  group_by(EVTYPE) %>%
  summarise(PROPDMG_TOTAL = sum(PROPDMG_SCALED_TOTAL), 
            CROPDMG_TOTAL = sum(CROPDMG_SCALED_TOTAL))

storm_data_by_impact$PROPDMG_TOTAL[is.na(storm_data_by_impact$PROPDMG_TOTAL)] <- 0
storm_data_by_impact$CROPDMG_TOTAL[is.na(storm_data_by_impact$CROPDMG_TOTAL)] <- 0
storm_data_by_impact$BOTH_TOTAL = storm_data_by_impact$PROPDMG_TOTAL + storm_data_by_impact$CROPDMG_TOTAL
# print(storm_data_by_impact)

storm_data_by_impact_propdmg <- arrange(storm_data_by_impact, desc(PROPDMG_TOTAL))[1:top_limit, ]
# print(head(storm_data_by_event_fatalities))
# print(storm_data_by_event_fatalities[1:top_limit, ]$FATALITIES_TOTAL)
t <- with(storm_data_by_impact_propdmg, 
          qplot(reorder(EVTYPE, -PROPDMG_TOTAL), 
                PROPDMG_TOTAL,
                main = paste("Top", top_limit, "Property Damage-Causing Weather Events"),
                xlab = "", 
                ylab = "Total Property") + 
          geom_bar(stat="identity") + 
          theme(axis.text.x = element_text(angle = 90)))

rm(storm_data_by_impact_propdmg)

storm_data_by_impact_cropdmg <- arrange(storm_data_by_impact, desc(CROPDMG_TOTAL))[1:top_limit, ]
# print(head(storm_data_by_event_injuries))
u <- with(storm_data_by_impact_cropdmg, 
          qplot(reorder(EVTYPE, -CROPDMG_TOTAL), 
                CROPDMG_TOTAL, 
                main = paste("Top", top_limit, "Crop Damage-Causing Weather Events"),
                xlab = "", 
                ylab = "Total Crop") + 
          geom_bar(stat="identity") + 
          theme(axis.text.x = element_text(angle = 90)))

rm(storm_data_by_impact_cropdmg)

storm_data_by_impact_both <- arrange(storm_data_by_impact, desc(BOTH_TOTAL))[1:top_limit, ]
# print(head(storm_data_by_event_injuries))
v <- with(storm_data_by_impact_both,
          qplot(reorder(EVTYPE, -BOTH_TOTAL),
                BOTH_TOTAL,
                main = paste("Top", top_limit, "Property and Crop Damage-Causing Weather Events"),
                xlab = "", 
                ylab = "Total Property and Crops") + 
          geom_bar(stat="identity") + 
          theme(axis.text.x = element_text(angle = 90)))

rm(storm_data_by_impact_both)

grid.arrange(t, u, v)
}