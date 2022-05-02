library(dplyr)
library(tidyr)
library(ggplot2)
gc=read.csv("C:/Users/garim/Desktop/dissertation/primary data/gc_data.csv")

morpho=read.csv("C:/Users/garim/Desktop/dissertation/primary data/morphometric_data.csv")
morpho%>%
  filter(Shark.Species =="Scoliodon laticaudus" | Shark.Species =="Carcharhinus limbatus" | Shark.Species =="Chiloscyllium griseum" | Shark.Species =="Chiloscyllium arabicum" | Shark.Species == "Sphyrna lewini")%>%
  group_by(Shark.Species)%>%
  summarise(N = n(),
            n=sum((Gut.content)=="Yes"),
            n_male=sum((Gut.content)=="Yes" & Sex=="M"),
            n_female=sum((Gut.content)=="Yes" & Sex=="F"),
            prop.gc=n/N)

#prey samples collected
gc%>%
  group_by(Shark.Species)%>%
  summarise(N=length(unique(gc_ID)))

#PROPORTION OF UNIDENTIFIED SPECIEMENS

gc%>%
  summarise(N_sp = length(unique(Prey.Species)), # number of prey species
            N_fam = length(unique(Prey.Family)), # number of prey families
            # proportion of unidentitified specimens
            unid_sp = sum(Prey.Species == "Unidentified" | Prey.Species == "", na.rm = T)*100/n(), ## species
            unid_fam = sum(Prey.Family == "Unidentified" | Prey.Family == "", na.rm = T)*100/n())%>% ## family
  # creating clean table
  gather()%>%
  mutate(Metric = ifelse(substr(key, 1, 1) == "N", "Richness", "% Unidentified"),
         Unit = ifelse(grepl(key, pattern = "sp", fixed = T), "Prey Species", "Prey Family"))%>%
  dplyr::select(Unit, Metric, value)%>%
  spread(Metric, value)

#number of sharks with more than one prey specimen
gc%>%
  filter(gc_ID != "")%>%
  count(gc_ID,name = "Prey")%>%
  count(Prey > 1)

#IRI

IRI<-gc%>%
  filter(Prey.Family !="")%>%
  filter(Shark.Species =="Scoliodon laticaudus" | Shark.Species =="Carcharhinus limbatus" | Shark.Species =="Chiloscyllium griseum" | Shark.Species =="Chiloscyllium arabicum" | Shark.Species == "Sphyrna lewini")%>%
  group_by(Shark.Species)%>%
  mutate(Fr=length(unique(Shark_ID)),
         W=sum(Weight_gms),
         N=n())%>%
  group_by(Shark.Species,Prey.Family)%>%
  summarise(f=length(unique(Shark_ID)),
            Fr=last(Fr),
            w=sum(Weight_gms),
            W=last(W),
            n=n(),
            N=last(N))%>%
  group_by(Shark.Species,Prey.Family)%>%
  #percentages
  summarise(per.F=f*100/Fr,
            per.W=w*100/W,
            per.N=n*100/N,
            #formula IRI
            IRI=(per.N+per.W)*per.F)

#clean table
IRI%>%
  filter(IRI > 0)

#plotting prey preference

IRI%>%
  ggplot(aes(reorder(Prey.Family, IRI), IRI, fill = Shark.Species))+
  geom_col(col = "black", position = position_dodge(preserve = "single"))+
  scale_y_sqrt(name = "IRI (sq.rt.)")+
  labs(x = "Prey Family")+
  scale_fill_brewer(palette = "Accent", name = "Species")+
  theme(axis.text.x = element_text(hjust = 1, angle = 60),
        legend.text = element_text(face = "italic"))
