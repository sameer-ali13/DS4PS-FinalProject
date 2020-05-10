## Final Project ## 


WorldFreedom <- read.csv("ObservationData_hyeckfe.csv")
dim(WorldFreedom)
View(WorldFreedom)

SierraLeone<- subset(WorldFreedom, location == "Sierra Leone")
View(SierraLeone)

SierraLeone$War <- ifelse(SierraLeone$Date >= 1991 & 
                            SierraLeone$Date <= 2002, 1, 0)

sierraleonePR.inwar <- subset(SierraLeone, War == 1 & 
                                variable == "Political Rights")
sierraleoneCL.inwar <- subset(SierraLeone, War == 1 & 
                               variable == "Civil Liberties")

sierraleonePR <- subset(SierraLeone, variable == "Political Rights")
sierraleoneCL <- subset(SierraLeone, variable == "Civil Liberties")

plot(x = sierraleonePR$Date, 
     y = sierraleonePR$Value, 
     type = "l", 
     col = "maroon", 
     xlab = "Year",
     ylab = "Index", 
     main = "Political Rights and Civil Liberties Indecis 
     for Sierra Leone Over Time", 
     las = 1)
points(x = sierraleoneCL$Date, 
       y = sierraleoneCL$Value, 
       type = "l",
       col = "navy")
legend("bottomleft",
       legend = c("Political Rights", "Civil Liberties"), 
       col = c("maroon", "navy"), lty = 1:1, cex = 0.6
       )

plot(x = sierraleonePR.inwar$Date,
     y = sierraleonePR.inwar$Value,
     type = "o", 
     main = "Political Rights and Civil Liberties Indecis 
     for Sierra Leone In War Time", 
     xlab = "Year", 
     ylab = "Index", 
     ylim = c(1,7), 
     xlim = c(1985, 2005),
     col = "maroon")
points(x = sierraleonCL.inwar$Date, 
       y = sierraleonCL.inwar$Value, 
       type = "o",
       col = "navy")
legend("bottomleft",
       legend = c("Political Rights", "Civil Liberties"), 
       col = c("maroon", "navy"), lty = 1:1, cex = 0.6
)

sierraleonePR.notinwar <- subset(SierraLeone, SierraLeone$War != 1)
sierraleoneCL.notinwar <- subset(SierraLeone, SierraLeone$War != 1)

t.test(sierraleonePR.inwar$Value, sierraleonePR.notinwar$Value)
t.test(sierraleoneCL.inwar$Value, sierraleoneCL.notinwar$Value)

## Algeria

Algeria <- subset(WorldFreedom, location == "Algeria")
View(Algeria)

Algeria$War <- ifelse(Algeria$Date >= 1991 & 
                            Algeria$Date <= 2002, 1, 0)

algeriaPR.inwar <- subset(Algeria, War == 1 & 
                                variable == "Political Rights")
algeriaCL.inwar <- subset(Algeria, War == 1 & 
                                variable == "Civil Liberties")

algeriaPR <- subset(Algeria, variable == "Political Rights")
algeriaCL <- subset(Algeria, variable == "Civil Liberties")


plot(x = algeriaPR$Date, 
     y = algeriaPR$Value, 
     type = "l", 
     col = "maroon", 
     xlab = "Year",
     ylab = "Index", 
     main = "Political Rights and Civil Liberties Indecis 
     for Algeria Over Time", 
     las = 1)
points(x = algeriaCL$Date, 
       y = algeriaCL$Value, 
       type = "l",
       col = "navy")
legend("bottomleft",
       legend = c("Political Rights", "Civil Liberties"), 
       col = c("maroon", "navy"), lty = 1:1, cex = 0.6
)

plot(x = algeriaPR.inwar$Date,
     y = algeriaPR.inwar$Value,
     type = "o", 
     las = 1,
     main = "Political Rights and Civil Liberties Indecis 
     for Algeria In War Time", 
     xlab = "Year", 
     ylab = "Index", 
     ylim = c(1,7), 
     xlim = c(1985, 2005),
     col = "maroon")
points(x = algeriaCL.inwar$Date, 
       y = algeriaCL.inwar$Value, 
       type = "o",
       col = "navy")
legend("bottomleft",
       legend = c("Political Rights", "Civil Liberties"), 
       col = c("maroon", "navy"), lty = 1:1, cex = 0.6
)

algeriaPR.notinwar <- subset(Algeria, Algeria$War != 1)
algeriaCL.notinwar <- subset(Algeria, Algeria$War != 1)

t.test(algeriaPR.inwar$Value, algeriaPR.notinwar$Value)
t.test(algeriaCL.inwar$Value, algeriaCL.notinwar$Value)

## Iran-Iraq 

Iran <- subset(WorldFreedom, location == "Iran")
Iraq <- subset(WorldFreedom, location == "Iraq")

Iraq$War <- ifelse(Iraq$Date >= 1980 & 
                        Iraq$Date <= 1988, 1, 0)
Iran$War <- ifelse(Iran$Date >= 1980 &
                     Iran$Date <= 1988, 1, 0)

iranPR <- subset(Iran, variable == "Political Rights")
iranCL <- subset(Iran, variable == "Civil Liberties")

iraqPR <- subset(Iraq, variable == "Political Rights")
iraqCL <- subset(Iraq, variable == "Civil Liberties")

iraqPR.inwar <- subset(Iraq, War == 1 & 
                            variable == "Political Rights")
iraqCL.inwar <- subset(Iraq, War == 1 & 
                            variable == "Civil Liberties")

iranPR.inwar <- subset(Iran, War == 1 & 
                            variable == "Political Rights")
iranCL.inwar <- subset(Iran, War == 1 & 
                            variable == "Civil Liberties")


plot(x = iraqPR.inwar$Date,
     y = iraqPR.inwar$Value,
     type = "l", 
     las = 1,
     main = "Political Rights Index of Iraq and Iran In Around War Time", 
     cex.main = 0.8,
     xlab = "Year", 
     ylab = "Index", 
     ylim = c(4,7), 
     xlim = c(1975, 1990),
     col = "maroon")
points(x = iranPR.inwar$Date, 
       y = iranPR.inwar$Value, 
       type = "l",
       col = "navy")
legend("bottomleft",
       legend = c("Iraq", "Iran"), 
       col = c("maroon", "navy"), lty = 1:1, cex = 0.6
)

plot(x = iraqPR$Date, 
     y = iraqPR$Value, 
     type = "l", 
     col = "maroon", 
     xlab = "Year",
     ylab = "Index", 
     main = "Political Rights Index of Iran and Iraq Over Time", 
     ylim = c(4,7),
     las = 1)
points(x = iranPR$Date, 
       y = iranPR$Value, 
       type = "l",
       col = "navy")
legend("bottomleft",
       legend = c("Iraq", "Iran"), 
       col = c("maroon", "navy"), lty = 1:1, cex = 0.6)

iraqPR.notinwar <- subset(Iraq, Iraq$War != 1)
iranPR.notinwar <- subset(Iran, Iran$War != 1)


t.test(iraqPR.inwar$Value, iraqPR.notinwar$Value)
t.test(iranPR.inwar$Value, iranPR.notinwar$Value) 


