Havnø <- Clean %>% 
  na.omit() %>% 
  filter(Site == "FHM 4014 Havnø") %>% 
  mutate(AssignedFindsOrSampleNum = factor(
    AssignedFindsOrSampleNum,
    levels = c("Random",
               "2",
               "3",
               "THU",
               "THR",
               "1",
               "SQG",
               "UED",
               "UEC",
               "POL")
    )
  ) %>%
  arrange(AssignedFindsOrSampleNum)

Havnø[Havnø$GridSquare=="98/101",]$AssignedFindsOrSampleNum <- "1"

HavnøHinges <- 
  Havnø %>% 
  ggplot(aes(x=AssignedFindsOrSampleNum,y=Hinge, colour=Periods, fill=Periods))+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1)+
  geom_smooth(aes(x=AssignedFindsOrSampleNum,y=Hinge,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  geom_point()+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot()+xlab("Unclear Sequence")+ggtitle('Havnø',subtitle='Hinge Size')+
  ylim(0,20)+gg_("hide legend")+
  xlab("")

HavnøAges <- 
  Havnø %>% 
  ggplot(aes(x=AssignedFindsOrSampleNum,y=AgeSeasoned, colour=Periods, fill=Periods))+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1)+
  geom_smooth(aes(x=AssignedFindsOrSampleNum,y=AgeSeasoned,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  geom_point()+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot()+xlab("Unclear Sequence")+ggtitle('',subtitle='Biological Age')+
  ylim(0,15)+
  gg_("hide legend")+
  xlab("")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )



MeanResHavnø <- Havnø %>% 
  group_by(Site) %>% 
  summarise(mean=mean(Residuals)) %>% 
  pull(mean)


HavnøResiduals <- 
  Havnø %>% 
ggplot(aes(x=AssignedFindsOrSampleNum,y=Residuals, colour=Periods, fill=Periods))+
  geom_hline(yintercept = 0, col = "grey80") +
  geom_hline(yintercept = MeanResHavnø, col = "black",linetype="dashed") +
  geom_point()+
  geom_smooth(aes(x=AssignedFindsOrSampleNum,y=Residuals,group=1),alpha=0.2,linetype=0, inherit.aes = FALSE)+
  coord_flip()+
  # geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1,outlier.alpha = 0)+
  scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
  theme_cowplot() +xlab("") +ylim(-5,5)+
  ggtitle("",subtitle='Residuals')+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )




# 
# TT_Res_Havnø <- t.test(Residuals~Periods, data=Havnø, var.equal=TRUE)
# TT_Hin_Havnø <- t.test(Residuals~Periods, data=Havnø, var.equal=TRUE)
