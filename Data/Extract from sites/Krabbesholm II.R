Krabbesholm <- Clean %>%
  na.omit() %>%
  filter(Site == "FHM 4383 Krabbesholm II") %>%
  mutate(Level = factor(Level,
                        levels = c("16",
                                   "15",
                                   "14",
                                   "13",
                                   "12",
                                   "11",
                                   "10",
                                   "8",
                                   "7",
                                   "6",
                                   "5",
                                   "4",
                                   "3",
                                   "2"
                                )
                        )
         ) %>%
  arrange(AssignedFindsOrSampleNum)


KrabbesholmHinges <- 
  Krabbesholm %>% 
  filter(ContextFeature=="Column 7737") %>% 
  ggplot(aes(x=Level,y=Hinge, colour=Periods, fill=Periods))+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1)+
  # geom_flat_violin(position = position_nudge(x = .1),alpha=0.5,adjust=0.8)+
  coord_flip()+
  geom_point()+
  geom_smooth(aes(x=Level,y=Hinge,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot()+xlab("Column 7737")+ggtitle('Krabbesholm',subtitle='Hinge Size')+
  theme(legend.position = "none")+
  ylim(0,15)+
  xlab("")

KrabbesholmAges <- 
  Krabbesholm %>% 
  filter(ContextFeature=="Column 7737") %>% 
  ggplot(aes(x=Level,y=AgeSeasoned, colour=Periods, fill=Periods))+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1)+
  # geom_flat_violin(position = position_nudge(x = .1),alpha=0.5,adjust=0.8)+
  coord_flip()+
  geom_point()+
  geom_smooth(aes(x=Level,y=AgeSeasoned,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot()+xlab("Column 7737")+ggtitle('',subtitle='Biological Age')+
  theme(legend.position = "none")+
  ylim(0,15)+
  xlab("")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

MeanResKrabbesholm <- Krabbesholm %>% 
  group_by(Site) %>% 
  summarise(mean=mean(Residuals)) %>% 
  pull(mean)



KrabbesholmResiduals <- 
Krabbesholm %>% 
ggplot(aes(x=Level,y=Residuals, colour=Periods, fill=Periods))+
  geom_hline(yintercept = 0, col = "grey80") +
  geom_hline(yintercept = MeanResKrabbesholm, col = "black",linetype="dashed") +
  geom_point()+
  # geom_flat_violin(position = position_nudge(x = .1),alpha=0.5,adjust=0.8)+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1,outlier.alpha = 0)+
  geom_smooth(aes(x=Level,y=Residuals,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot() +
  xlab("") +
  ylim(-5,5)+
  coord_flip()+
  ggtitle("",subtitle='Residuals')+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


KrabbesholmHinges|KrabbesholmResiduals

# TT_Res_Krabbesholm <- t.test(Residuals~Periods, data=Krabbesholm, var.equal=TRUE)
# TT_Hin_Krabbesholm <- t.test(Residuals~Periods, data=Krabbesholm, var.equal=TRUE)







#