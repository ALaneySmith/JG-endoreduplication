*****
# Import herbicide data

# Assign name to data frame

  "HerbEndoData"

# Assign factors and rename variables
  str(HerbEndoData)
names(HerbEndoData)[2]<-"accession"
HerbEndoData$HERBICIDE<-as.factor(HerbEndoData$HERBICIDE)
HerbEndoData$HABITAT<-as.factor(HerbEndoData$HABITAT)
HerbEndoData$accession <- as.factor(HerbEndoData$accession)
HerbEndoData$population<-unlist(lapply(X = HerbEndoData$accession,FUN = function(x){paste(strsplit(as.character(x),split = "-")[[1]][1:2],collapse = "-")}  ))
HerbEndoData$population<-as.factor(HerbEndoData$population)
HerbEndoData$Round<-as.factor(HerbEndoData$Round)

# Delete residuals columns
HerbEndoData<-HerbEndoData[,-13:-14]

# Mixed model ANOVA, with endoreduplication as response variable

 #Remove missing data, which seems to be causing issues in model fitting

test.herb.endo<- lmer(formula = HerbEndoData$log.endo ~ 
                  HerbEndoData$HERBICIDE +
                  HerbEndoData$HABITAT +
                  HerbEndoData$initial.height + 
                  (1|HerbEndoData$Round) +
                 (1|HerbEndoData$HABITAT:HerbEndoData$accession)                                   
                

test.herb.endo.int<- lmer(formula = HerbEndoData$log.endo ~ 
                      
                        HerbEndoData$initial.height + 
                        (1|HerbEndoData$Round) +
                        (1|HerbEndoData$HABITAT:HerbEndoData$accession)+
                          HerbEndoData$HERBICIDE*HerbEndoData$HABITAT)
                

library(bbmle)
AICtab(test.herb.endo,test.herb.endo.int)



# Step-wise removal of interaction term

#take se lsmeans, means come from lsmeans
#### 

library(lsmeans)
anova(test.herb.endo)
anova(test.herb.endo, ddf = "Kenward-Roger")
ls_means(test.herb.endo)
ls_means_out_endo_herb<-ls_means(test.herb.endo)
View(ls_means_out_endo_herb)

#### naming rows of ls means output
View(ls_means_out_endo_herb)
row.names(ls_means_out_endo_herb)

ls_means_out_endo_herb<-ls_means_out_endo_herb[1:5,]

ls_means_out_endo_herb
test$se <- df.lsmass[, "Standard Error"]

difflsmeans(m.mass, test.effs = "agfac:pop.x")
####### First run at herbicide endo violin plots

aov_out<-aov(formula = HerbEndoData$`Cube Root[A Mass]`~HerbEndoData$HABITAT )
summary(aov_out)
aov_out<-aov(formula = HerbEndoData$`Log[Endo]`~
               HerbEndoData$HABITAT 
             + HerbEndoData$HERBICIDE 
             + HerbEndoData$HABITAT *HerbEndoData$HERBICIDE
             + (1| HerbEndoData$Round)
             + HerbEndoData$`HGT 1`
             
                )


#Trying to run with either population or accession as a random effect gives the error:
#"contrasts can be applied only to factors with 2 or more levels"

str
summary(test_glm)
summary(test_glm2)

library(ggplot2)

ggplot(data = HerbEndoData,aes(x=HERBICIDE, y=`Log[Endo]`, fill=HABITAT )) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci),
                width=.2, 
                position=position_dodge(.9))

ggplot(data = HerbEndoData, aes(x=HERBICIDE, y=`Log[Endo]`, fill=HABITAT)) +
  geom_violin()+ theme_bw()+ylab("Endopolyploidy")+ scale_color_grey() + scale_fill_grey()

ggplot(data = HerbEndoData, aes(x=HABITAT, y=`Log[Endo]`)) +
  geom_violin()+ theme_bw()+ylab("Endopolyploidy")

###### Herbicide endo bar plots with lsmeans estimates#########
ls_means_out_endo_herb$Estimate
herbicide_fac<-c("Beacon","Control","Glyphosate","Poast Plus","Pursuit")
herb_est<-ls_means_out_endo_herb$Estimate[1:5]
scale_fill_manual(values = c("black", "black", "black","black","black"), labels = c("Beacon","Control","Glyphosate","Poast Plus","Pursuit") +
 scale_x_discrete(labels=c(Beacon, Control, Glyphosate, Poast Plus, Pursuit), breaks= c(Beacon, Control, Glyphosate, Poast Plus, Pursuit)) + theme(legend.title = element_blank()) +
  theme(axis.text.x  = element_text(angle = 45, vjust=1, hjust=1),
   axis.text.y = element_text("Endopolyploidy", size = 16, color = "black"),
xlab(x="Herbicide") + ylab(y="Endopolyploidy") +
axis.line.y = element_line(color = "black"),
theme(axis.ticks.x = element_text(angle = 45, vjust = 1, hjust = 1),
axis.ticks.x = element_line(color = "black"),
legend.position = "none",
panel.grid = element_blank(),
panel.background = element_rect(fill = "white"))  
herbicide.endo.plot + theme(axis.title.x = "Herbicide", axis.text.x = element_text(angle = 45, vjust=1, hjust=1)


herbicide.endo.plot + labs(x = "Herbicide", y = "Endopolyploidy")
scale_fill_manual(values = c("black"), labels = c("Beacon","Control", "Glyphosate", "Poast Plust", "Pursuit")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5),breaks = c(0,0.5,1,1.5,2,2.5,3,3.5)) +
  
