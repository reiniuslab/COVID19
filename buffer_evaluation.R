library(data.table)
df.buffer <- fread("buffer/buffer_data_merged.tsv")
df.buffer.melt <- melt(df.buffer, id.vars = c("condition","control","replicate") )
df.buffer.melt[,c("day","date","vol","temp") := tstrsplit(variable, "_") ]
df.buffer.melt[,vol := as.numeric(gsub("ul","",vol))]

##########
## Plot ##
##########
pal <- c("#8b2c6a","#82b14e","#5858bc","#bdb33b","#7283e9","#c9772a","#553483","#4c9c52","#b850ae","#48c49c","#d8598d","#a58d3f","#bd83d6","#9b4a2a","#628dd4","#d55d47","#da7bb9","#c44d55","#8a2543","#d25972")
lvls <- df.buffer.melt[,mean(value),by=c("condition","day","temp", "control")][temp=="NA" & is.na(control),condition[order(V1)]]

library(ggplot2)
library(cowplot)
library(ggrepel)
p.buff.d0 <- 
ggplot(df.buffer.melt[temp == "NA" & is.na(control)], aes(y=value, x=vol, col=condition )) +
  geom_point() +
  stat_summary(fun.y="median",geom = "line") +
  facet_wrap(~factor(condition, levels=lvls)) +
  labs(x="Volume (µL)", y="Ct") +
  coord_cartesian(ylim=c(10,50) ) +
  scale_y_continuous(labels = c(seq(10,40,10),"NA") ) +
  scale_x_reverse() +
  scale_color_manual(values = pal) +
  theme_cowplot() +
  theme(strip.background = element_blank())

p.buff.time <-
ggplot(df.buffer.melt[temp != "NA" & is.na(control)], aes(y=value, x=as.numeric(gsub("Day","",day)), col=condition, shape=temp, group=temp )) +
  geom_point() +
  stat_summary(fun.y="median",geom = "line") +
  facet_wrap(~factor(condition, levels=lvls)) +
  labs(x="Time (days)", y="Ct") +
  coord_cartesian(ylim=c(10,50),xlim=c(0,8)) +
  scale_y_continuous(labels = c(seq(10,40,10),"NA") ) +
  scale_color_manual(values = pal) +
  scale_shape_manual(values=c(15,17)) +
  theme_cowplot() +
  theme(strip.background = element_blank())

ggsave2("plots/buffer_test.pdf",plot_grid(p.buff.d0,p.buff.time),width = 14)