library(mirt)
library(ggmirt)
library(tidyverse)
theme_set(theme_bw())

taia <- read_csv("https://github.com/angelgardt/taia/raw/master/data/taia.csv")
taia %>% select(starts_with(c("pr", "co", "ut", "fa", "de", "un"))) -> taia_items
colnames(taia_items)

model <- "
PR = pr01-pr10
CO = co01-co10
UT = ut01-ut12
FA = fa01-fa10
DE = de01-de11
UN = un01-un12
"
fit3PL <- mirt(data = taia_items, 
               model = model,
               itemtype = "graded", 
               verbose = FALSE)
beepr::beep()
fit3PL
M2(fit3PL, type = "C2")
beepr::beep()
summary(fit3PL)
params3PL <- coef(fit3PL, IRTpars = TRUE, simplify = TRUE)


# plot(fit3PL, type='trace', which.item = c(1,2,3,4,5,6), facet_items=T, 
#      as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8), 
#      theta_lim = c(-3, 3), 
#      main = "")

# itempersonMap(fit3PL)
# 
# tracePlot(fit3PL)
# tracePlot(fit3PL, facet = F, legend = T) + scale_color_brewer(palette = "Set3")
# tracePlot(fit3PL, items = 1:3, facet = F, legend = T) + scale_color_brewer(palette = "Set2")
# 
# itemInfoPlot(fit3PL) + scale_color_brewer(palette = "Set3")
# itemInfoPlot(fit3PL, facet = T)
# testInfoPlot(fit3PL, adj_factor = 2)
# 
# scaleCharPlot(fit3PL)
# 
# tu_data <- read_csv2('https://raw.githubusercontent.com/angelgardt/hseuxlab-wlm2021/master/data/tolerance_uncertainty.csv')
# tu_data %>% select(-sex, -age) -> tu
# 
# model_tu <- 'F1 = 1-22'
# fitRSM <- mirt(data = tu, 
#                model = model_tu,
#                itemtype = "rsm", 
#                verbose = FALSE)
# fitRSM
# summary(fitRSM)
# coef(fitRSM, IRTpars = TRUE, simplify = TRUE)
# M2(fitRSM)
# itemfit(fitRSM)
# itemfit(fitRSM, fit_stats = "infit")
# 
# itemfitPlot(fitRSM)
# itempersonMap(fitRSM)
# 
# 
# 
# model.pcm <- 'F1 = 1-22' 
# results.pcm <- mirt(data=tu, model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE)
# coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
# 
# 
# plot(results.pcm, type = 'trace', 
#      main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
#      auto.key=list(points=FALSE,lines=TRUE, columns=4)) 
# 
# plot(results.pcm, type = 'infotrace', 
#      main = "", par.settings = simpleTheme(lwd=2))
