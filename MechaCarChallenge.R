#MechaCar_StatisticalAnalysis

# Segment 1
library(dbplyr)
mechafile <- read.csv('/Users/MariaCastellanos/Desktop/Analytics_Bootcamp/MechaCar_StatisticalAnalysis/MechaCar_mpg.csv')
View(MechaCar_mpg)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechafile)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechafile))

## Segment 2
suspensionfile <- read.csv('/Users/MariaCastellanos/Desktop/Analytics_Bootcamp/MechaCar_StatisticalAnalysis/Suspension_Coil.csv')
View(suspensionfile)
total_summary <- suspensionfile %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI),.groups = 'keep')
lot_summary <- suspensionfile %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI, na.rm = TRUE),.group ='keep')

## Segment 3
test_table <- suspensionfile %>% sample_n(50)
t.test((test_table$PSI), mu=mean(suspensionfile$PSI))
t.test(subset(suspensionfile$PSI, suspensionfile$Manufacturing_Lot == "Lot1"),suspensionfile$PSI)
t.test(subset(suspensionfile$PSI, suspensionfile$Manufacturing_Lot == "Lot2"),suspensionfile$PSI)
t.test(subset(suspensionfile$PSI, suspensionfile$Manufacturing_Lot == "Lot3"),suspensionfile$PSI)
