# Read data from Quality of government
dat <- read.csv("http://www.qogdata.pol.gu.se/data/qog_soc_tsl_4apr12.csv", sep=";")
# Read data on countries and continents from my github repository
library(RCurl)
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/world/countriesContinents.csv")
datCont <- read.csv(text = GHurl)
dat <- merge(dat,datCont,by.x="ccode",by.y="cNr")
save(dat, file="data/social_policy_cont.RData")