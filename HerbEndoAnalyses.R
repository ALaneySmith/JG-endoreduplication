str(HerbEndoData)

#log(endo)~ Round(RE) + Height1 + Habitat + Accession(re within habitat) + herbicide + accession*herbicide

names(HerbEndoData)[2]<-"accession"
HerbEndoData$HERBICIDE<-as.factor(HerbEndoData$HERBICIDE)
HerbEndoData$HABITAT<-as.factor(HerbEndoData$HABITAT)
HerbEndoData$accession <- as.factor(HerbEndoData$accession)
HerbEndoData$population<-unlist(lapply(X = HerbEndoData$accession,FUN = function(x){paste(strsplit(as.character(x),split = "-")[[1]][1:2],collapse = "-")}  ))
HerbEndoData$population<-as.factor(HerbEndoData$population)

library(lme4)



test_glm <- glm(formula = HerbEndoData$`Log[Endo]` ~ 
                  HerbEndoData$HERBICIDE +
                  HerbEndoData$HABITAT +
                  HerbEndoData$`HGT 1` + 
  
                )

test_glm2 <- glm(formula = HerbEndoData$`Log[Endo]` ~ 
                  HerbEndoData$HERBICIDE +
                  HerbEndoData$`HGT 1` +
                  HerbEndoData$accession 
                )

plot(HerbEndoData$`HGT 2`~HerbEndoData$HABITAT)
?ks.test
ks.test(x =HerbEndoData$`HGT 1`[which(HerbEndoData$HABITAT=="AG")],y = HerbEndoData$`HGT 1`[which(HerbEndoData$HABITAT=="NON-AG")] )
ks.test(x =HerbEndoData$`HGT 2`[which(HerbEndoData$HABITAT=="AG")],y = HerbEndoData$`HGT 2`[which(HerbEndoData$HABITAT=="NON-AG")] )
ks.test(x =HerbEndoData$`HGT 1`[which(HerbEndoData$HABITAT=="AG" & HerbEndoData$HERBICIDE!="CONTROL")],y = HerbEndoData$`HGT 1`[which(HerbEndoData$HABITAT=="NON-AG" & HerbEndoData$HERBICIDE!="CONTROL")] )
ks.test(x =HerbEndoData$`HGT 2`[which(HerbEndoData$HABITAT=="AG" & HerbEndoData$HERBICIDE!="CONTROL")],y = HerbEndoData$`HGT 2`[which(HerbEndoData$HABITAT=="NON-AG" & HerbEndoData$HERBICIDE!="CONTROL")] )
ks.test(x =HerbEndoData$`HGT 1`[which(HerbEndoData$HABITAT=="AG" & HerbEndoData$HERBICIDE=="CONTROL")],y = HerbEndoData$`HGT 1`[which(HerbEndoData$HABITAT=="NON-AG" & HerbEndoData$HERBICIDE=="CONTROL")] )
ks.test(x =HerbEndoData$`HGT 2`[which(HerbEndoData$HABITAT=="AG" & HerbEndoData$HERBICIDE=="CONTROL")],y = HerbEndoData$`HGT 2`[which(HerbEndoData$HABITAT=="NON-AG" & HerbEndoData$HERBICIDE=="CONTROL")] )

ks.test(x =HerbEndoData$`Cube Root[A Mass]`[which(HerbEndoData$HABITAT=="AG")],y = HerbEndoData$`HGT 1`[which(HerbEndoData$HABITAT=="NON-AG")] )
ks.test(x =HerbEndoData$`Cube Root[A Mass]`[which(HerbEndoData$HABITAT=="AG")],y = HerbEndoData$`HGT 2`[which(HerbEndoData$HABITAT=="NON-AG")] )

aov_out<-aov(formula = HerbEndoData$`Cube Root[A Mass]`~HerbEndoData$HABITAT )
summary(aov_out)
aov_out<-aov(formula = HerbEndoData$`Log[Endo]`~
               HerbEndoData$HABITAT 
             + HerbEndoData$HERBICIDE 
             + HerbEndoData$HABITAT *HerbEndoData$HERBICIDE
             + (1| HerbEndoData$Round)
             + HerbEndoData$`HGT 1`
             
                )

TukeyHSD(aov_out,which = "HerbEndoData$HERBICIDE")
?TukeyHSD


HerbEndoData$HERBICIDE

HerbEndoData$`HGT 1`

ano
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

library(BIEN)