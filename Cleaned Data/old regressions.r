#Parity Regressions
setwd("~/GSCHOOL/Job Market Preparation/Self Teaching/Projects/Football/Teams/Cleaned Data")
paritydata <- read.csv(file="paritydata.csv", head=TRUE, sep=",")
attach(paritydata)
post_cap_trend_diff <- year * salary_cap_era

#Approach 1: iid Normal errors

#####Win Percentage Regressions
#Model 1
winpct1 <- lm(formula = normed_winpct_parity ~
                year, data = paritydata)

#Model 2
winpct2 <- lm(formula = normed_winpct_parity ~ 
                year + salary_cap_era, data = paritydata)

#Model 3
winpct3 <- lm(formula = normed_winpct_parity ~ 
                year + post_cap_trend_diff, 
              data = paritydata)

#Model 4
winpct4 <- lm(formula = normed_winpct_parity ~ 
                year + salary_cap_era + post_cap_trend_diff, 
              data = paritydata)

#####Playoff Rating Regressions
#Model 1
playoff1 <- lm(formula = normed_pr_parity ~ year, 
               data = paritydata)

#Model 2
playoff2 <- lm(formula = normed_pr_parity ~ 
                 year + salary_cap_era, data = paritydata)

#Model 3
playoff3 <- lm(formula = normed_pr_parity ~ 
                 year + post_cap_trend_diff, 
               data = paritydata)

#Model 4
playoff4 <- lm(formula = normed_pr_parity ~ 
                 year + salary_cap_era + post_cap_trend_diff, 
               data = paritydata)

#####Gini Coefficient Regressions 
#Model 1
gini1 <- lm(formula = adapted_gini_coefficient ~ 
              year, data = paritydata)

#Model 2
gini2 <- lm(formula = adapted_gini_coefficient ~ 
              year + salary_cap_era, data = paritydata)

#Model 3
gini3 <- lm(formula = adapted_gini_coefficient ~ 
              year + post_cap_trend_diff, 
            data = paritydata)

#Model 4
gini4 <- lm(formula = adapted_gini_coefficient ~ 
              year + salary_cap_era + post_cap_trend_diff, 
            data = paritydata)

sink("OLSresults.txt")
print("Win Percentage Parity: Model 1")
summary(winpct1)
print("Win Percentage Parity: Model 2")
summary(winpct2)
print("Win Percentage Parity: Model 3")
summary(winpct3)
print("Win Percentage Parity: Model 4")
summary(winpct4)
print("Playoff Rating Parity: Model 1")
summary(playoff1)
print("Playoff Rating Parity: Model 2")
summary(playoff2)
print("Playoff Rating Parity: Model 3")
summary(playoff3)
print("Playoff Rating Parity: Model 4")
summary(playoff4)
print("Adapted Gini Coefficient: Model 1")
summary(gini1)
print("Adapted Gini Coefficient: Model 2")
summary(gini2)
print("Adapted Gini Coefficient: Model 3")
summary(gini3)
print("Adapted Gini Coefficient: Model 4")
summary(gini4)
gini4$newse <- vcovHC(gini4)
coeftest(gini4, gini4$newse)

sink()

#Approach 2: White's estimator
#####Win Percentage Regressions
#Model 1
winpct1 <- lm(formula = normed_winpct_parity ~
                year, data = paritydata)
#Model 2
winpct2 <- lm(formula = normed_winpct_parity ~ 
                year + salary_cap_era, data = paritydata)

#Model 3
winpct3 <- lm(formula = normed_winpct_parity ~ year + post_cap_trend_diff, 
              data = paritydata)

#Model 4
winpct4 <- lm(formula = normed_winpct_parity ~ year + salary_cap_era + 
                post_cap_trend_diff, data = paritydata)

#####Playoff Rating Regressions
#Model 1
playoff1 <- lm(formula = normed_pr_parity ~ year, data = paritydata)

#Model 2
playoff2 <- lm(formula = normed_pr_parity ~ 
                 year + salary_cap_era, data = paritydata)

#Model 3
playoff3 <- lm(formula = normed_pr_parity ~ year + post_cap_trend_diff, 
               data = paritydata)

#Model 4
playoff4 <- lm(formula = normed_pr_parity ~ year + salary_cap_era + 
                 post_cap_trend_diff, data = paritydata)

#####Gini Coefficient Regressions 
#Model 1
gini1 <- lm(formula = adapted_gini_coefficient ~ year, data = paritydata)

#Model 2
gini2 <- lm(formula = adapted_gini_coefficient  ~ 
              year + salary_cap_era, data = paritydata)

#Model 3
gini3 <- lm(formula = adapted_gini_coefficient  ~ year + 
              post_cap_trend_diff, data = paritydata)

#Model 4
gini4 <- lm(formula = adapted_gini_coefficient  ~ year + 
              salary_cap_era + post_cap_trend_diff, data = paritydata)

#Approach 2: White's estimator
require("sandwich")
require("lmtest")

winpct1$newse <- vcovHC(winpct1)
coeftest(winpct1, winpct1$newse)

winpct2$newse <- vcovHC(winpct2)
coeftest(winpct2, winpct2$newse)

winpct3$newse <- vcovHC(winpct3)
coeftest(winpct3, winpct3$newse)

winpct4$newse <- vcovHC(winpct4)
coeftest(winpct4, winpct4$newse)

parity1$newse <- vcovHC(parity1)
coeftest(parity1, parity1$newse)

parity2$newse <- vcovHC(parity2)
coeftest(parity2, parity2$newse)

parity3$newse <- vcovHC(parity3)
coeftest(parity3, parity3$newse)

parity4$newse <- vcovHC(parity4)
coeftest(parity4, parity4$newse)

gini1$newse <- vcovHC(gini1)
coeftest(gini1, gini1$newse)

gini2$newse <- vcovHC(gini2)
coeftest(gini2, gini2$newse)

gini3$newse <- vcovHC(gini3)
coeftest(gini3, gini3$newse)



# Dickey-Fuller test
sink("AugDickeyFuller.txt")
adf.test(normed_winpct_parity)
adf.test(normed_pr_parity)
adf.test(adapted_gini_coefficient)
sink()

#HP Filter
hpwin <- hpfilter(normed_winpct_parity, freq=6.25, type=c("lambda"))
hpplayoff <- hpfilter(normed_pr_parity, freq=6.25, type=c("lambda"))
hpgini <- hpfilter(adapted_gini_coefficient, 
                   freq=6.25, type=c("lambda"))

year <- 2014:1967
windf = melt(data.frame(hpwin[2], hpwin[10], year), id.var="year")
playoffdf = melt(data.frame(hpplayoff[2], 
                            hpplayoff[10], year), id.var="year")
ginidf = melt(data.frame(hpgini[2], hpgini[10], year), id.var="year")

windf$Legend[windf$variable == "trend"] <- "Filtered Win Percentage Parity"
windf$Legend[windf$variable == "x"] <- "Raw Win Percentage Parity"
playoffdf$Legend[windf$variable == "trend"] <- "Filtered Playoff Rating Parity"
playoffdf$Legend[windf$variable == "x"] <- "Raw Playoff Rating Parity"
ginidf$Legend[windf$variable == "trend"] <- "Filtered Adapted Gini Coefficient"
ginidf$Legend[windf$variable == "x"] <- "Raw Adapted Gini Coefficient"

ggplot(windf, aes(x=year, y=value)) +
  geom_line(aes(colour=Legend, group=Legend)) +
  geom_point(aes(colour=Legend, shape=Legend, group=Legend),
             size = 2) +
  ylab(label = "Win Percentage Parity") + 
  xlab(label = "Year") + 
  ggtitle("Hodrick-Prescott Filter: Win Percentage Parity")

ggplot(playoffdf, aes(x=year, y=value)) +
  geom_line(aes(colour=Legend, group=Legend)) +
  geom_point(aes(colour=Legend, shape=Legend, group=Legend),
             size = 2) +
  ylab(label = "Playoff Rating Parity") + 
  xlab(label = "Year") + 
  ggtitle("Hodrick-Prescott Filter: Playoff Rating Parity")

ggplot(windf, aes(x=year, y=value)) +
  geom_line(aes(colour=Legend, group=Legend)) +
  geom_point(aes(colour=Legend, shape=Legend, group=Legend),
             size = 2) +
  ylab(label = "Adapted Gini Coefficient") + 
  xlab(label = "Year") + 
  ggtitle("Hodrick-Prescott Filter: Adapted Gini Coefficient")




















ggplot(playoffdf, aes(x = year)) + 
  geom_line(aes(y = trend), colour = "#CC33FF") + 
  geom_line(aes(y = x), colour = "#00CCFF") + 
  ylab(label = "Playoff Rating Parity") + 
  xlab(label = "Year") + 
  ggtitle("Hodrick-Prescott Filter: Playoff Rating Parity")

ggplot(ginidf, aes(x = year)) + 
  geom_line(aes(y = trend), colour="#CC33FF") + 
  geom_line(aes(y = x), colour = "#00CCFF") + 
  ylab(label = "Adapted Gini Coefficient") + 
  xlab(label = "Year") + 
  ggtitle("Hodrick-Prescott Filter: Adapted Gini Coefficient")
