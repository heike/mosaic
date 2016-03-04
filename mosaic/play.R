library(ggplot2)
library(plyr)
library(wesanderson)
setwd("~/Documents/mosaic")
load("happy2.RData")
happy[happy=="NA"] <- NA



head(happy)
prodplot(happy, ~ happy, "hbar")
prodplot(happy, ~ happy, "hspine")

prodplot(happy, ~ sex + happy, c("vspine", "hbar"))
prodplot(happy, ~ sex + happy, stacked()) # only displays the relative frequencies - not the percentages?

# The levels argument can be used to extract a given level of the plot
prodplot(happy, ~ sex + happy, stacked(), level = 1)
prodplot(happy, ~ sex + happy, stacked(), level = 2)


prodplot(happy, ~ happy + sex, mosaic())
prodplot(happy, ~ happy + finrela + health, mosaic())
prodplot(happy, ~ happy + marital + health, mosaic())

prodplot(happy, ~ happy + finrela + health, c("vspine", "fluct"))
prodplot(happy, ~ happy | finrela + health, c("vspine", "fluct"))
prodplot(happy, ~ happy | finrela + health, c("hspine", "fluct"))

prodplot(data=happy, ~ happy+sex, c("vspine", "hspine"))
prodplot(happy, ~happy+finrela, c("vspine","hspine"), subset=level==2)

# interaction between financial relation and sex
# emphasize difference between men/women
prodplot(happy, ~happy+sex+finrela, c("vspine","hspine","hspine"), na.rm=T) + 
  aes(fill=happy) +scale_fill_manual(values=wes_palette(n=3, name="Moonrise2"))

prodplot(happy, ~ happy | degree, na.rm=T) + aes(fill = happy)+scale_fill_manual(values=wes_palette(n=3, name="Moonrise2"))
prodplot(happy, ~ happy | degree, na.rm=T) + aes(fill = degree)+scale_fill_manual(values=wes_palette(n=5, name="Cavalcanti"))


# emphasize different pattern 
prodplot(happy, ~happy+finrela+sex, c("vspine","hspine","hspine"), na.rm=T)+ 
  aes(fill = happy, alpha=finrela)+ scale_alpha_discrete(range=c(0.4,0.8))+scale_fill_manual(values=wes_palette(n=3, name="Moonrise2"))


prodplot(happy, ~happy+finrela+sex, c("vspine","hspine","hspine"), na.rm=T)+ aes(fill = sex)+scale_fill_manual(values=wes_palette(n=3, name="Moonrise2"))
prodplot(happy, ~happy+finrela+sex, c("vspine","hspine","hspine"), na.rm=T)+ aes(fill = finrela)+scale_fill_manual(values=wes_palette(n=5, name="Cavalcanti"))

prodplot(data=happy, ~ happy+marital, c("vspine", "hspine"), na.rm=T, subset=level==2)

prodplot(data=happy, ~ happy+degree, c("vspine", "hspine"), na.rm=T, subset=level==2)

prodplot(data=happy, ~ happy+health, c("vspine", "hspine"), na.rm=T, subset=level==2)








library(dplyr)
library(lubridate)
library(YaleToolkit)
library(ggvis)
library(ggplot2)
library(tidyr)
library(Hmisc)


intro.data <- get("diamonds")
head(intro.data)

intro.plot <- intro.data %>% mutate(id = 1:n(), intro_x_cat = factor(carat), intro_x_num = as.numeric(carat), 
                                    intro_y_cat = factor(depth), intro_y_num = as.numeric(depth))

intro.mosaic <- table(intro.data[, c("cut", "color")]) %>% prop.table %>% as.data.frame %>% 
  mutate(margin_x = rep(prop.table(table(intro.data$cut)), times = length(unique(intro.data$color))), 
         y_height = Freq/margin_x, x_center = rep(c(0, cumsum(margin_x)[1:length(levels(intro.data$cut)) - 
                                                                          1]), times = length(unique(intro.data$color))) + margin_x/2)
ggplot(intro.mosaic, aes(x_center, y_height)) + geom_bar(stat = "identity", aes(width = margin_x, 
                                                                                fill = color), col = "black") + geom_text(aes(label = cut, x = x_center, y = 1.05), 
                                                                                                                          angle = 45, hjust = 0) + xlim(c(0, 1.2)) + ylim(c(0, 1.2)) + xlab("cut") + ylab("color") + 
  theme_bw() + coord_fixed()


cat("X Variable: cut; Y Variable: color")

prodplot(intro.data, ~ cut + color, mosaic())
prodplot(happy, ~ sex + happy, mosaic())

