Ertebølle  <- Clean %>%
  na.omit() %>%
  filter(Site == "FHM 2168 Ertebølle") %>% 
  filter(grepl("J", GridSquare))  %>% # removes layers outside sample column
  mutate(Layer = factor(
    Layer,
    levels = c("26","25","24","23","22","21","20","19","18","15","14","12","11","10","8","7")
)) %>%
  arrange(Layer)

# 
# 
# ErtebølleHinges <- 
#   Ertebølle %>% 
#   ggplot(aes(x=Layer,y=Hinge, colour=Periods, fill=Periods))+
#   geom_point()+
#   geom_flat_violin(position = position_nudge(x = .1),alpha=0.5,adjust=0.8)+coord_flip()+
#   geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1, col=c("black"))+
#   scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
#   scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
#   theme_cowplot()+ggtitle('Hinges')+ylim(0,15)+theme(legend.position = "none")
# 
# 
# 
# ErtebølleResiduals <- 
#   Ertebølle %>% 
#   ggplot(aes(x=Layer,y=Residuals, colour=Periods, fill=Periods))+
#   geom_hline(yintercept = 0, col = "grey80") +
#   geom_point()+
#   geom_flat_violin(position = position_nudge(x = .1),alpha=0.5,adjust=0.8)+coord_flip()+
#   geom_boxplot(alpha=0.1,position = position_nudge(x = .1), width=0.1, col=c("black"))+
#   scale_colour_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
#   scale_fill_manual(values = c(viridis_pal()(4)[2],viridis_pal()(4)[3]))+
#   theme_cowplot() + ggtitle('Residuals')
# 
# 
# Ertebølle_layer_graphs <- ErtebølleHinges+ErtebølleResiduals+plot_layout(guides = 'collect')
