#
# Mijn aanname (die ik graag onderzocht zie): driepunters zijn een grotere 
# factor in het aantal gewonnen wedstrijden dan alleen zichtbaar is in het Efg%. 
# Met andere woorden: als ik mijn verdediging zo inricht dat ik het 
# driepuntspercentage naar beneden breng (significant, maar dat lijkt me 
# duidelijk hoewel ook interessant is wat dan significant is), vergroot dit dan
# mijn kansen om te winnen?
#

efgPctPlot <- ggplot(gmStats, aes(plg_ShortName, EFGpct)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(EFGpct)), linetype="dotted") +
  opts(title ="Effective Field Goal % (EFG%)") +
  xlab("") + 
  ylim(c(0.2,0.8))
print(efgPctPlot)

fg2Plot <- ggplot(gmStats, aes(plg_ShortName, FG2pct)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(FG2pct)), linetype="dotted") +
  opts(title ="2pt Pct per game") +
  xlab("") + 
  ylim(c(0.2,0.8))
print(fg2Plot)

fg3Plot <- ggplot(gmStats, aes(plg_ShortName, FG3pct)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(FG3pct)), linetype="dotted") +
  opts(title ="3pt Pct per game") +
  xlab("") + 
  ylim(c(0.0,0.8))
print(fg3Plot)


fgPtsOf3Plot <- ggplot(gmStats, aes(plg_ShortName, ContrFG3pts )) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(FG3pts / pts)), linetype="dotted") +
  opts(title ="3pt contribution - pct of points scored from 3s") +
  xlab("") 
print(fgPtsOf3Plot)


forPlot <- gmStats[c("wed_ID","Nrtg","EFGpct","ContrFG3pts","ContrFG2pts","ContrFTpts",
                     "plg_ShortName","Home")] 
forPlot.m <- melt(forPlot, id=c("wed_ID", "plg_ShortName", "Home","Nrtg"))

p <- ggplot(forPlot.m, aes(value, Nrtg)) +
  geom_point(aes(shape=plg_ShortName, colour=plg_ShortName)) + 
  scale_shape_manual(values=as.numeric(forPlot.m$plg_ShortName)) +
  stat_smooth(method="lm") +
  facet_wrap(~variable,scales="free")

print(p)

# correlation with Nrtg

d = data.frame(gmStats$Nrtg, 
               gmStats$EFGpct,
               #gmStats$ORpct, 
               #gmStats$TOpct, gmStats$FTTpct,
               gmStats$ContrFG2pts, gmStats$ContrFG3pts, gmStats$ContrFTpts,
               gmStats$FG2pct, gmStats$FG3pct, gmStats$FTpct               
               )
names(d) <- sub("^gmStats.", "", names(d))
corComp <- cor(d)

print(corComp,2)

corComp.m <- melt(corComp)
corPlot <- ggplot(corComp.m, aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "red",  high = "blue") +
  opts(title="Correlation matrix for complete competition")
print(corPlot)

forPlot <- gmStats[c("wed_ID","Nrtg","EFGpct","FG2pct", "FG3pct","FTpct",
                     "plg_ShortName","Home")] 
forPlot.m <- melt(forPlot, id=c("wed_ID", "plg_ShortName", "Home","Nrtg"))

#corForPlot <- c(cor(forPlot$Nrtg, forPlot$EFGpct),
#                cor(forPlot$Nrtg, forPlot$FG2pct ), 
#                cor(forPlot$Nrtg, forPlot$FG3pct ), 
#                cor(forPlot$Nrtg, forPlot$FTpct ))

p <- ggplot(forPlot.m, aes(value, Nrtg)) +
  geom_point(aes(shape=plg_ShortName, colour=plg_ShortName)) + 
  scale_shape_manual(values=as.numeric(forPlot.m$plg_ShortName)) +
  stat_smooth(method="lm") +
  facet_wrap(~variable,scales="free")
  
print(p)
