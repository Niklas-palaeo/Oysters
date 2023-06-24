Eskilsø  <- Clean %>%
  na.omit() %>%
  filter(Site == "Eskilsø") %>%
  filter(!grepl("F|G", Layer)) %>% # removes layers outside sample column
  mutate(Layer = factor(
    Layer,
    levels = c("A layer",
               "E layer",
               "D layer 12",
               "C layer 11",
               "B layer 10+14")
  )) %>%
  arrange(Layer)

EskHinges <- Eskilsø %>% 
  ggplot(aes(x=Layer,y=Hinge, colour=Periods, fill=Periods))+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1)+
  geom_smooth(aes(x=Layer,y=Hinge,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  geom_point()+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot() + ggtitle('Eskilsø', subtitle = "Hinge Size") + ylim(0, 20)+xlab("")+gg_("hide legend")

EskAges <- Eskilsø %>% 
  ggplot(aes(x=Layer,y=AgeSeasoned, colour=Periods, fill=Periods))+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1)+
  geom_smooth(aes(x=Layer,y=AgeSeasoned,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  geom_point()+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot() + ggtitle('', subtitle = "Biological Age") + ylim(0, 25)+xlab("")+
  gg_("hide legend")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

MeanResEskilsø <- Eskilsø %>% 
  group_by(Site) %>% 
  summarise(mean=mean(Residuals)) %>% 
  pull(mean)




EskRes <- Eskilsø %>% 
  ggplot(aes(x=Layer,y=Residuals, colour=Periods, fill=Periods))+
  geom_hline(yintercept = 0, col = "grey80") +
  geom_hline(yintercept = MeanResEskilsø, col = "black",linetype="dashed") +
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1)+
  geom_smooth(aes(x=Layer,y=Residuals,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  geom_point()+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot() + ggtitle("",subtitle='Residuals') + ylim(-5, 5)+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# EskGraphs <- EskHinges+EskRes+plot_layout(guides = 'collect')

# 
# TT_Res_Eskilsø <- t.test(Residuals~Periods, data=Eskilsø, var.equal=TRUE)
# TT_Hin_Eskilsø <- t.test(Residuals~Periods, data=Eskilsø, var.equal=TRUE)
