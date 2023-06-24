Bjørnsholm <- Clean %>%
  na.omit() %>%
  filter(Site == "FHM 2911 Bjørnsholm") %>%
  mutate(Layer = factor(Layer,
                        levels = c("16 (level or sample)",
                                   "15 (level or sample)",
                                  "14 (level or sample)",
                                  "13 (level or sample)",
                                  "11 (level or sample)",
                                  "10 (level or sample)",
                                  "9 (level or sample)",
                                  "8 (level or sample)",
                                  "7 (level or sample)",
                                  "6 (level or sample)",
                                  "5 (level or sample)",
                                  "4 (level or sample)",
                                  "3 (level or sample)",
                                  "2 (level or sample)",
                                  "1 (level or sample)",
                                  "Level 7",
                                  "Level 6",
                                  "Level 3",
                                  "Level 2",
                                  "Level 1"
                                  )
                              
                        )
    ) %>%
  arrange(Layer)




BjørnsholmHinges <- 
  Bjørnsholm %>% 
  ggplot(aes(x=Layer,y=Hinge, colour=Periods, fill=Periods))+
  geom_point()+
  geom_smooth(aes(x=Layer,y=Hinge,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1,outlier.alpha = 0)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot()+ggtitle('Bjørnsholm',subtitle="Hinge size")+ylim(0,15)+
  theme(legend.position = "none")+
  xlab("")+
  ylab("")

BjørnsholmAges <- 
  Bjørnsholm %>% 
  ggplot(aes(x=Layer,y=AgeSeasoned, colour=Periods, fill=Periods))+
  geom_point()+
  geom_smooth(aes(x=Layer,y=AgeSeasoned,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1,outlier.alpha = 0)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot()+ggtitle('',subtitle="Biological Age")+ylim(0,15)+
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


MeanResBjørnsholm <- Bjørnsholm %>% 
  group_by(Site) %>% 
  summarise(mean=mean(Residuals)) %>% 
  pull(mean)


BjørnsholmResiduals <-
  Bjørnsholm %>% 
  ggplot(aes(x=Layer,y=Residuals, colour=Periods, fill=Periods))+
  geom_hline(yintercept = 0, col = "grey80") +
  geom_hline(yintercept = MeanResBjørnsholm, col = "black",linetype="dashed") +
  geom_point()+
  geom_smooth(aes(x=Layer,y=Residuals,group=1),alpha=0.2,size=0, inherit.aes = FALSE)+
  coord_flip()+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1,outlier.alpha = 0)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot() + 
  ggtitle("",subtitle='Residuals')+
  xlab("")+
  ylim(-5,5)+
  ylab("")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )





# 
# 
# TT_Res_Norsminde <- t.test(Residuals~Periods, data=Norsminde, var.equal=TRUE)
# TT_Hin_Norsminde <- t.test(Residuals~Periods, data=Norsminde, var.equal=TRUE)
