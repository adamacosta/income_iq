#! /usr/bin/Rscript

library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)

df        <- read.csv("default.csv") %>%
             filter(R0618301 > 0 & T4112300 > 0) %>%
             mutate(iq=((R0618301 - mean(R0618301)) / sd(R0618301)) * 
                                 15 + 100) %>%
             mutate(dec=cut(iq, unique(quantile(iq, seq(0, 1, 0.1))),
                            include.lowest=TRUE)) %>%
             select(-R0618301)
names(df) <- c("id", "income", "iq", "dec")

g1        <- ggplot(data=df, aes(iq, income)) +
             geom_point(cex=0.8) +
             stat_smooth(method="lm", lwd=1) +
             scale_y_continuous(labels=comma) +
             theme_few()
png(filename="income_iq.png", height=600, width=480)
g1
dev.off()

means     <- summarise(group_by(df, dec), mean_income=mean(income))

g2        <- ggplot(means, aes(dec, mean_income)) +
             geom_bar(stat="identity", fill="slategrey") +
             theme_few()
png(filename="mean_income.png", height=600, width=480)
g2
dev.off()

sds       <- summarise(group_by(df, dec), sd_income=sd(income))

g3        <- ggplot(sds, aes(dec, sd_income)) +
             geom_bar(stat="identity", fill="slategrey") +
             scale_y_continuous(labels=comma) +
             theme_few()
png(filename="sd_income.png", height=600, width=480)
g3
dev.off()
