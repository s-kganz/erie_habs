library(tidyverse)
library(lubridate)
library(GGally)
library(psych)
library(GPArotation)
library(heplots)

s2 <- read_csv("05_data_working/s2_observations.csv") %>%
    filter(year(measure_date) %in% c(2014:2018)) %>%
    filter(month(measure_date) %in% c(5:11))

# Run EFA first. Factors are probably not orthogonal so
# PCA may not be a good choice.
s2.cor <- cor(s2[, 3:15])
corrplot::corrplot(s2.cor)

# Judge factorability of dataset. It's pretty good.
cortest.bartlett(s2.cor, n=259)
KMO(s2.cor)

# How many factors? 2
fa.parallel(s2.cor, n.obs=259, fa='fa')

# Only 1 factor is sufficient, probably
s2.fa <- fa(s2[, 3:15], n.factors=2, rotate='varimax')
summary(s2.fa)

s2.fa$loadings

s2.plot <- cbind(s2, s2.fa$scores)

ggplot(filter(s2.plot, MR1 < 3), aes(x=MR1, fill=station)) + geom_histogram()
ggplot(s2.plot, aes(x=MR1, fill=station)) + geom_histogram()

bright <- filter(s2.plot, MR1 > 2)

s2 <- arrange(s2, B3)

image(1:nrow(s2), 1, as.matrix(1:nrow(s2)), 
      col=rgb(s2$B4, s2$B3, s2$B2),
      xlab="", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
