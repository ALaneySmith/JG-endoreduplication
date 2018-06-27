# Plots for herbicide stress

library(ggplot2)
##############################
#PLot of ploidy increase vs herbicide

herb_plotting_data<-test.herb.endo.lsm
herb_plotting_data<-herb_plotting_data[1:5,]

ploidy_v_herb<-ggplot(data=herb_plotting_data ,aes(y=herb_plotting_data$Estimate, x=herbicide_fac))+geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=herb_plotting_data$Estimate+herb_plotting_data$`Std. Error`,
                    ymax=herb_plotting_data$Estimate+herb_plotting_data$`Std. Error`),width=.25)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+ylab("Cell ploidy (% increase)")+xlab("Herbicide")+
    scale_y_continuous(expand = c(0,0)) 


#########################      
#Plot of cuberoot biomass vs herbicide

test.herb.bio<- lmer(formula = HerbEndoData$cuberoot.biomass ~ 
                        HerbEndoData$HERBICIDE +
                        HerbEndoData$HABITAT +
                        HerbEndoData$initial.height + 
                        (1|HerbEndoData$Round) +
                        (1|HerbEndoData$HABITAT:HerbEndoData$accession)+
                       HerbEndoData$HERBICIDE*HerbEndoData$HABITAT)                                   
library(ggplot2)
library(lsmeans)
library(multcomp)
anova(test.herb.bio)
anova(test.herb.bio, ddf = "Kenward-Roger")
ls_means(test.herb.bio)
ls_means_out_endo_bio<-ls_means(test.herb.bio)
View(ls_means_out_endo_bio)
glht(model = test.herb.bio)
diff_ls_means_herb_bio<-difflsmeans(model = test.herb.bio,which = "HerbEndoData$HERBICIDE")
write.csv(x = diff_ls_means_herb_bio,file = "C:/Users/Brian/Desktop/diff_ls_means_herb_bio.csv")
write.csv(x = ls_means_out_endo_bio,file="C:/Users/Brian/Desktop/ls_means_output_endo_bio.csv")

ls_means_out_endo_bio
bio_plotting_data<-ls_means_out_endo_bio[9:18,]
bio_plotting_data$Habitat_fac<-c("Ag","Ag","Ag","Ag","Ag","Non-ag","Non-ag","Non-ag","Non-ag","Non-ag")
bio_plotting_data$Herbicide_fac<-c("Beacon","Control","Glyphosate","Poast Plus","Pursuit","Beacon","Control","Glyphosate","Poast Plus","Pursuit")


biomass_v_herb<-ggplot(data=bio_plotting_data ,aes(y=bio_plotting_data$Estimate, x=Herbicide_fac,fill=Habitat_fac))+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=bio_plotting_data$Estimate-bio_plotting_data$`Std. Error`,
                    ymax=bio_plotting_data$Estimate+bio_plotting_data$`Std. Error`),width=.25,position=position_dodge(.9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),legend.title=element_blank())+
  ylab("Aboveground biomass (g)")+xlab("Herbicide")+
  scale_y_continuous(expand = c(0,0),limits = c(0,1))+scale_fill_manual(values=c('darkgrey','lightgray'))
  
biomass_v_herb

library(ggpubr)
ggpubr::ggarrange(ploidy_v_herb,biomass_v_herb,ncol = 1,nrow = 2)
