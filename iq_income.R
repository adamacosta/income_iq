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
g1

grouped   <- group_by(df, dec)
means     <- summarise(grouped, mean_income=mean(income))

g2        <- ggplot(means, aes(dec, mean_income)) +
             geom_bar(stat="identity", fill="slategrey") +
             theme_few()
g2