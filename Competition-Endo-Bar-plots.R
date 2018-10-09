********
#Assign factors
  
  CompetitionEndoData$TISSUE<-  as.factor(CompetitionEndoData$TISSUE)

  CompetitionEndoData$HABITAT<- as.factor(CompetitionEndoData$HABITAT)

  CompetitionEndoData$COMPETITION<- as.factor(CompetitionEndoData$COMPETITION)

  CompetitionEndoData$ACCESSION <- as.factor(CompetitionEndoData$ACCESSION)
  CompetitionEndoData$population<-unlist(lapply(X = CompetitionEndoData$ACCESSION,FUN = function(x){paste(strsplit(as.character(x),split = "-")[[1]][1:2],collapse = "-")}  ))
  CompetitionEndoData$population<-as.factor(CompetitionEndoData$population)

*********
# Rename 
  initial height
  names(CompetitionEndoData)[10] <- "initial_height"  

  final height
  names(CompetitionEndoData)[11] <- "final_height"

  log_endo
  names(CompetitionEndoData)[13] <- "log_endo"

  cuberoot_biomass
  names(CompetitionEndoData)[12] <- "cuberoot_biomass"

  
*********
  
# Split up data by tissue type
  
  CompetitionEndoDataRoots<-CompetitionEndoData[which(CompetitionEndoData$TISSUE=="ROOTS"),]
  CompetitionEndoDataShoots<-CompetitionEndoData[which(CompetitionEndoData$TISSUE=="SHOOTS"),] 
  
**********
# Test lm models 

  library(lme4)



  test_glm <- glm(formula = CompetitionEndoData$log_endo ~ 
                  CompetitionEndoData$COMPETITION +
                  CompetitionEndoData$HABITAT +
                  CompetitionEndoData$initial_height + 
                  CompetitionEndoData$(population) +
                  CompetitionEndoData$COMPETITION * CompetitionEndoData$HABITAT
                  )
**********
# Delete residual column
  CompetitionEndoData2<-CompetitionEndoData[- 14]
  CompetitionEndoData2<-na.omit(CompetitionEndoData2)
  CompetitionEndoData<-CompetitionEndoData2
  rm(CompetitionEndoData2,CompetitionEndoData_original)
**********
# Roots_endo_test_model <- lmer(formula = CompetitionEndoDataRoots$log_endo ~ 
    CompetitionEndoDataRoots$COMPETITION + CompetitionEndoDataRoots$HABITAT + 
    CompetitionEndoDataRoots$initial_height + (1|CompetitionEndoDataRoots$population) + 
    CompetitionEndoDataRoots$COMPETITION * CompetitionEndoDataRoots$HABITAT)
    summary(Roots_endo_test_model)

    library("lme4")
    library("languageR")
    model=lmer(...)
    pvals.fnc(Roots_endo_test_model)
    anova(Roots_endo_test_model, ddf="Kenward-Roger")
    
    anova(Roots_endo_test_model, ddf = "Kenward-Roger")
    
    df.lsmass <- lsmeans(m.mass, test.effs = "agfac:treatfac")[[1]]
    df.lsmass$se <- df.lsmass[, "Standard Error"]
    
    difflsmeans(m.mass, test.effs = "agfac:pop.x")
    
  #save ls_means output  
    ls_means_out_Roots<-ls_means(Roots_endo_test_model)  
    ls_means_hab_comp<-ls_means_out_Roots[7:12,]
    rownames(ls_means_hab_comp)
    agfac<-c("Ag","Ag","Ag","Non-ag","Non-ag","Non-ag")
    compfac<-c("Corn","Alone","Conspecific","Corn","Alone","Conspecific")    
********
library(ggplot2)
# Bar plot for endo Roots
     rootplot <- ggplot(data = ls_means_hab_comp, aes(x = compfac, y = ls_means_hab_comp$Estimate, fill = agfac)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      geom_errorbar(aes(ymin = ls_means_hab_comp$Estimate - ls_means_hab_comp$`Std. Error`, ymax = ls_means_hab_comp$Estimate + ls_means_hab_comp$`Std. Error`), position = position_dodge(0.9), width = 0.25) +
      scale_fill_manual(values = c("white", "grey"), labels = c("Ag", "Non-Ag")) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5),breaks = c(0,0.5,1,1.5,2,2.5,3,3.5)) +
      scale_x_discrete(labels = c("Alone", "Conspecific", "Corn")) +
      ylab("Roots") + xlab("Competition") + 
       theme(
        axis.text = element_text(size = 10, color = "black"),
         axis.title.y = element_text(size = 10),
         axis.title.x = element_blank(),
         axis.line.x = element_line(color = "black"),
         axis.line.y = element_line(color = "black"),
         axis.ticks.x = element_blank(),
         axis.ticks.y = element_line(color = "black"),
         panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank()
       )
*********
# Repeat lmer model by REML for shoots endo 
      Shoots_endo_test_model <- lmer(formula = CompetitionEndoDataShoots$log_endo ~ CompetitionEndoDataShoots$COMPETITION + CompetitionEndoDataShoots$HABITAT + 
     CompetitionEndoDataShoots$initial_height + (1|CompetitionEndoDataShoots$population) + CompetitionEndoDataShoots$COMPETITION * CompetitionEndoDataShoots$HABITAT)
     summary(Shoots_endo_test_model)
     
# P-values, least square means table
     anova(Shoots_endo_test_model, ddf="Kenward-Roger")
     ls_means(Shoots_endo_test_model)
     ls_means_out_Shoots<-ls_means(Shoots_endo_test_model)
     View(ls_means_out_Shoots)
     
# Save ls_means output  
     ls_means_out_Shoots<-ls_means(Shoots_endo_test_model) 
# naming rows of ls means output
     View(ls_means_out_Shoots)
     row.names(ls_means_out_Shoots)
     
     ls_means_hab_comp_Shoots<-ls_means_out_Shoots[7:12,]
     View(ls_means_hab_comp_Shoots)
     rownames(ls_means_hab_comp_Shoots)
 *******
# Bar plot for Shoots endo Competition
     agfac<-c("Ag","Ag","Ag","Non-ag","Non-ag","Non-ag")
     compfac<-c("Corn","Alone","Conspecific","Corn","Alone","Conspecific")
     shoot_ploidy_plot<-ggplot(data = ls_means_hab_comp_Shoots, aes(x = compfac, y = ls_means_hab_comp_Shoots$Estimate, fill = agfac)) +
             geom_bar(stat = "identity", position = "dodge", color = "black") +
              geom_errorbar(aes(ymin = ls_means_hab_comp_Shoots$Estimate - ls_means_hab_comp_Shoots$`Std. Error`, ymax = ls_means_hab_comp_Shoots$Estimate + ls_means_hab_comp_Shoots$`Std. Error`), position = position_dodge(0.9), width = 0.25) +
              scale_fill_manual(values = c("white", "grey"), labels = c("Ag", "Non-Ag")) + 
              scale_y_continuous(expand = c(0, 0), limits = c(0, 2.0),breaks = c(0,0.5,1,1.5,2.0)) +
              scale_x_discrete(labels = c("Alone", "Conspecific", "Corn")) +
              ylab("Shoots") + xlab("Competition") +
               theme(
                  axis.text = element_text(size = 10, color = "black"),
                  axis.title.y = element_text(size = 10),
                   axis.title.x = element_blank(),
                   axis.line.x = element_line(color = "black"),
                   axis.line.y = element_line(color = "black"),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line(color = "black"),legend.title = element_blank(), panel.background = element_blank()    )
     
library(ggpubr) #used to arrange ggplot stuff
pct_increase_plots<-ggarrange(rootplot,shoot_ploidy_plot,nrow=2,ncol = 1,common.legend = T) #add xlabel saying "Cell ploidy (% increase)"
?ggarrange


         
   ggplot(data = ls_means_hab_comp_Shoots, aes(x = compfac, y = ls_means_hab_comp_Shoots$Estimate, fill = agfac)) +
              geom_bar(stat = "identity", position = "dodge", color = "black") +
              geom_errorbar(aes(ymin = ls_means_hab_comp_Shoots$Estimate - ls_means_hab_comp_Shoots$`Std. Error`, ymax = ls_means_hab_comp_Shoots$Estimate + ls_means_hab_comp_Shoots$`Std. Error`), position = position_dodge(0.9), width = 0.25)
     
     df.lsmass <- lsmeans(m.mass, test.effs = "agfac:treatfac")[[1]]
     df.lsmass$se <- df.lsmass[, "Standard Error"]
     
     difflsmeans(m.mass, test.effs = "agfac:pop.x")
     
     #save ls_means output  
     ls_means_out_Shoots<-ls_means(Shoots_endo_test_model)  
     ls_means_hab_comp_Shoots<-ls_means_out_Shoots[7:12,]
     rownames(ls_means_hab_comp_Shoots)
     agfac<-c("Ag","Ag","Ag","Non-ag","Non-ag","Non-ag")
     compfac<-c("Corn","Alone","Conspecific","Corn","Alone","Conspecific")  
      

    
    