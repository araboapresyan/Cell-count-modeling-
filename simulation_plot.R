### Load libraries ###
library(ggplot2)
library(ggrepel)

## original TBI cell proportions ##
orig_t <- c(0.5205479 , 0.5750000  ,0.6070288 , 0.6283422  ,0.6598174 , 0.6240000,  0.6725146 , 0.7318436 , 0.5243902,
            0.3271028 , 0.3740053,  0.5422741 , 0.3841060)


## Correlation between original and predicted TBI cell proportions
#vvet_t - from similation_code.R
cor_plot <-  ggplot() + geom_point(aes(x=vvet_t,y = orig_t, color = "black")) +
  guides(color = guide_legend(override.aes = list(size = 10,shape = 95) ) ) +
  geom_abline(slope = coef(lm(orig_t~vvet_t))[["vvet_t"]],
              intercept = coef(lm(orig_t~vvet_t))[["(Intercept)"]],
              color = "blue") + 
  labs(title = "Correlation between original and predicted proportion of TBI cell numbers",
       subtitle = paste0("Correlation = ",cor_score%>%round(digits= 3)),
       x = "Predicted",y = "Original") +
  geom_text_repel(aes(x=vvet_t,y = orig_t,label = astro_small@active.ident%>%levels),
            nudge_x = 0,nudge_y = -0.01) +
  geom_abline(slope = 1,intercept = 0,color = "red")  +
   scale_color_manual(name = NULL,labels = c("regression line","diagonal line"),
                     values = c(blue = "blue",red = "red"))+ 
  theme(rect = element_blank(),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.background = element_rect(colour = "black",fill = NA),
        legend.justification = c("right", "bottom"),
        legend.position = c(1, 0),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,size = 9),
        plot.subtitle = element_text(hjust = 0.5,face = "italic"))


# Original proportion of control ant TBI cells
#cell.numb - from similation_code.R
origin <- data.frame(cell.numb[,c(1,3)],prop= c(0.4794521 , 0.4250000 , 0.3929712 , 0.3716578 , 0.3401826 , 0.3760000 , 0.3274854 , 0.2681564 , 0.4756098,
                                                0.6728972 , 0.6259947 , 0.4577259 , 0.6158940,
                                                0.5205479 , 0.5750000  ,0.6070288 , 0.6283422  ,0.6598174 , 0.6240000,  0.6725146 , 0.7318436 , 0.5243902,
                                                0.3271028 , 0.3740053,  0.5422741 , 0.3841060))

#Barplot of original proportion of control ant TBI cells
p_o <- ggbarplot(origin, x = "name", y = "prop",xlab = "",ylab = "Origin %",
                 fill = "exp",colour = c("#F35E5A ","#17B3B7"),  
                 position = position_stack()) + labs(fill = "") + 
  scale_y_continuous(breaks = c(0,0.5767,1),labels = c("0.00","57.67","100.00")) +
  scale_x_discrete(labels = c("NSC-stage1","NSC-stage2","RG-like","N-stage1",
                              "N-stage2","N-stage3","N-stage4","N-stage5","N-stage6",
                              "A-stage1","A-stage2","A-stage3","A-stage4")) + 
  geom_abline(slope = 0,intercept = 0.5767,size =1.5,linetype="dashed" ) + 
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(colour = "black"))



#Original and predicted barplots
bar_plots <- ggarrange(p_o + theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.x = element_blank() )
          ,pp,ncol = 1,common.legend = T,heights = c(1,1.75))   



# Final plot #  
final_plot <- ggarrange(bar_plots,cor_plot) 
annotate_figure(final_plot,bottom = " ",right = " ",left = " ",
  top = text_grob("Priliferation rate (NSC-stage1 = 1, NSC-stage2 = 1, RG-like = 1), corr.sigma = 0.07, seed = 1",size = 15,face = "bold"))

final_plot


