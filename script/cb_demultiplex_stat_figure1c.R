# tmp demultiplex stat
setwd("~/Dropbox/research/sc_longread/manuscript/analysis")
library(ggplot2)
library(dplyr)
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

experiment = c("scmixology1","scmixology1","scmixology2","scmixology2",
               "MuSC","MuSC","CLL2","CLL2","CLL152","CLL152","CLL153","CLL153","CLL318","CLL318","CLL267","CLL267")
demultiplex = rep(c("unassigned","demultiplexed"),length(experiment)/2)

count = c(20511067,30450558,18213595,18713951,25858384,19619406,55034293,41366055,22785456,17448466,23799647,44904180,17283929,24263740,60025545,56945198)

plot_df=data.frame(experiment=experiment,
                   demultiplex=demultiplex,
                   count=count)
tot_cnt = plot_df %>% group_by(experiment) %>% summarise(s=sum(count))
tmp = plot_df[plot_df$demultiplex=="demultiplexed",]
tmp = tmp %>% left_join(tot_cnt,by=c("experiment"="experiment"))
tmp$pct = tmp$count/tmp$s
tmp$pct = percent(tmp$pct,0)
tmp = tmp[,c("experiment","demultiplex","pct")]
plot_df = plot_df %>% left_join(tmp)
plot_df$pct[is.na(plot_df$pct)] = ""
plot_df$experiment <- factor(plot_df$experiment,
                         levels = c('scmixology1',"scmixology2","MuSC","CLL2","CLL267","CLL152","CLL153","CLL318"),ordered = TRUE)

plot_df$demultiplex <- factor(plot_df$demultiplex,
                             levels = c("unassigned","demultiplexed"),ordered = TRUE)

plot_df = plot_df[plot_df$experiment %in% c("scmixology1","scmixology2","MuSC","CLL2"),]
ggplot(data=plot_df,aes(x=experiment,y=count,fill=demultiplex))+
  geom_bar(stat="identity")+
  geom_text(aes(label=pct),position = position_stack(vjust = 0.95),col="white")+
  labs(y="number of reads",fill="")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+
  theme(legend.position="top",axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="figs/demultiplex_stat.pdf",height = 3,width = 2.7)
