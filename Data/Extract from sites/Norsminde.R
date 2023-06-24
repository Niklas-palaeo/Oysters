Norsminde <- Clean %>%
  na.omit() %>%
  filter(Site == "FHM 1734 Norsminde") %>%
  mutate(Layer = factor(Layer,
                        levels = c(
                          "10", "9",
                          "8",
                          "7",
                          "6",
                          "5",
                          "4",
                          "3",
                          "2", "1"
                        ))) %>%
  arrange(Layer)



NorsmindeHinges <-
  Norsminde %>% 
  ggplot(aes(x=Layer,y=Hinge, colour=Periods, fill=Periods))+
  geom_point()+
  geom_smooth(aes(x=Layer,y=Hinge,group=1),alpha=0.2, inherit.aes = FALSE,linetype=0)+
  coord_flip()+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1,outlier.alpha = 0)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot()+ggtitle('Norsminde',subtitle="Hinge size")+ylim(0,15)+
  theme(legend.position = "none")+
  xlab("")+
  ylab("")

NorsmindeAges <- 
  Norsminde %>% 
  ggplot(aes(x=Layer,y=AgeSeasoned, colour=Periods, fill=Periods))+
  geom_point()+
  geom_smooth(aes(x=Layer,y=AgeSeasoned,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1,outlier.alpha = 0)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot()+ggtitle('',subtitle="Biological Age")+
  ylim(0,10)+
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

MeanResNorsminde <- Norsminde %>% 
  group_by(Site) %>% 
  summarise(mean=mean(Residuals)) %>% 
  pull(mean)

NorsmindeResiduals <-
  Norsminde %>% 
  ggplot(aes(x=Layer,y=Residuals, colour=Periods, fill=Periods))+
  geom_hline(yintercept = 0, col = "grey80") +
  geom_hline(yintercept = MeanResNorsminde, col = "black",linetype="dashed") +
  geom_point()+
  geom_smooth(aes(x=Layer,y=Residuals,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
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


Norsm_layer_graphs <- NorsmindeHinges+NorsmindeResiduals+plot_layout(guides = 'collect')





# 
# 
# TT_Res_Norsminde <- t.test(Residuals~Periods, data=Norsminde, var.equal=TRUE)
# TT_Hin_Norsminde <- t.test(Residuals~Periods, data=Norsminde, var.equal=TRUE)
