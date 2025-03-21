########## Load Packages ----------------------
library(tidyverse)
library(openxlsx)
library(survey)
options("survey.lonely.psu" = "adjust")

########## Data Importation -------------------
#Importation de la dernière table générée par Florent
conso_compo <- read.csv2("../../TABLES INCA3 PRIX AGB/conso_compo_prix_impact_recette_FEV_2025.csv", sep =  ",",dec=".",
                          encoding="UTF-8")
description_indiv <- read.csv2("../../TABLES INCA3 PRIX AGB/description-indiv.csv", 
                               sep = ";",dec=".")

code_gpe_INCA3 <- read.csv2("in/gpe_INCA3.csv")

list_indiv_lait_mat=conso_compo%>%filter(is.na(qte_conso_pond))%>%distinct(NOIND)%>%pull()#17 individus, dt seulement 3 de 1-3 ans
#Préparation table conso_compo
conso_compo <- conso_compo%>%
  left_join(code_gpe_INCA3, by="gpe_INCA3")%>%
  #suppression des indiv ac lait maternel (pas de compo)
  filter(!NOIND%in%list_indiv_lait_mat)%>%
  #Suppression des matières grasses ac énergie =0  
  filter(!(aet == 0 & eau == 0 & lipides == 0))%>%
  #supprimer les aliments qui ont des valeurs de compo manquantes (aspartame, cyclamate, réglisse)
  filter(!aliment_code_INCA3%in%c(3049, 3393,3724))%>%
  left_join(description_indiv,by="NOIND")%>%
  #Add "0" variables aa qui n'ont pas de prot
  mutate(across(c(PROT_DIG:Isoflavonoids,fer_heminique:phytate_100g),~ifelse(is.na(.x),0,.x)))%>%
  #Ajout colonne ag laurique myristique palmitique
  mutate(ag_lau_myr_pal=ag_12_0+ag_14_0+ag_16_0)

#Recommandations enfants et adultes : tableau issu du projet IFIP, mis à jours pour considérer les recos par classe d'âge

#Note : reco en eau, fibres = EFSA ; reco protéines en kg de poids corporel et sucres libres : WHO
#pas de reco sur les AGS chz les enfants. Reco en fibre en g/kcal
#Interprétation pr les glucides et lipides de l'augmentation progressive
#Adaptation des recos en fonction des classes d'âges dispo ds INCA. Par ex la reco en eau pr les 9-14 a été appliquée pour les 7-10 et 11-14
reco_enft=read.xlsx("in/reco_nut.xlsx",sheet="ENFANTS")

#Note : 
reco_adu=read.xlsx("in/reco_nut.xlsx",sheet="ADULTES")

########## Apports nutritionnels - coût - impacts environnementaux - Consommations variables PNNS individuels -------------------
#Liste Variables
list_var=colnames(conso_compo)[c(20:73,75,146:163,169:171,173:226,232:244,248:253,257,258,260,261,263,449)]
list_var_pct=list_var[c(3,4,5,8,10,22:24,32,34,74,75,155)]

#Hors boissons alcoolisées ------

# Variables non % NRJ
app_nut=conso_compo%>%
  dplyr::select(NOIND,pond_indiv_adu_pop3,pond_indiv_enf_pop3,pop3,code_gpe_INCA,qte_conso_pond,R24_nombre,list_var)%>%
  filter(pop3==1)%>%
  filter(code_gpe_INCA!=40)%>%#exclusion boissons alcoolisées
  mutate(across(list_var,~.x/100*qte_conso_pond))%>%
  #On conserve uniquement les individus qui ont rep à au moins 2 interview
  group_by(NOIND,R24_nombre,pond_indiv_adu_pop3,pond_indiv_enf_pop3)%>%
  dplyr::summarise(across(list_var,~sum(.x)))%>%
  #Calcul des apports journaliers
  mutate(across(list_var,~.x/R24_nombre))%>%
  ungroup()

# Variables en % NRJ
app_pct=app_nut%>%
  select(NOIND,pond_indiv_adu_pop3,pond_indiv_enf_pop3,aet,all_of(list_var_pct))%>%
  #Expression des variables en kcal
  mutate(across(c("proteines","glucides","sucres","sucres_aj","sucres_libres","fibres"),~4*.x))%>%
  #Expression des variables en kcal
  mutate(across(c("lipides","agpi","ag_18_2_lino","ag_18_3_a_lino","ag_lau_myr_pal"),~9*.x))%>%
  mutate(across(list_var_pct,~.x/aet*100,.names = "{paste0(col,'_pctNRJ')}"),.keep="unused")

app_nut=app_nut%>%left_join(app_pct)%>%
  mutate(type="app_hors_alcool")

#Avec boissons alcoolisées ------

# Variables non % NRJ
app_nut_alc=conso_compo%>%
  dplyr::select(NOIND,pond_indiv_adu_pop3,pond_indiv_enf_pop3,pop3,code_gpe_INCA,qte_conso_pond,R24_nombre,list_var)%>%
  filter(pop3==1)%>%
  mutate(across(list_var,~.x/100*qte_conso_pond))%>%
  #On conserve uniquement les individus qui ont rep à au moins 2 interview
  group_by(NOIND,R24_nombre,pond_indiv_adu_pop3,pond_indiv_enf_pop3)%>%
  dplyr::summarise(across(list_var,~sum(.x)))%>%
  #Calcul des apports journaliers
  mutate(across(list_var,~.x/R24_nombre))%>%
  ungroup()

# Variables en % NRJ
app_pct_alc=app_nut_alc%>%
  select(NOIND,pond_indiv_adu_pop3,pond_indiv_enf_pop3,aet,all_of(list_var_pct))%>%
  #Expression des variables en kcal
  mutate(across(c("proteines","glucides","sucres","sucres_aj","sucres_libres","fibres"),~4*.x))%>%
  #Expression des variables en kcal
  mutate(across(c("lipides","agpi","ag_18_2_lino","ag_18_3_a_lino","ag_lau_myr_pal"),~9*.x))%>%
  mutate(across(list_var_pct,~.x/aet*100,.names = "{paste0(col,'_pctNRJ')}"),.keep="unused")

app_nut_alc=app_nut_alc%>%left_join(app_pct_alc)%>%
  mutate(type="app_with_alcool")

app_nut=bind_rows(app_nut,app_nut_alc)

#EXPORT
openxlsx::write.xlsx(app_nut,file="out/xlsx/app_nut_indiv.xlsx")
write.csv2(app_nut,file="out/csv/app_nut_indiv.csv")

########## Apports en % de la recommandation ------------------
#app_nut=read.csv2("out/csv/app_nut_indiv.csv")

reco_enftok <- reco_enft%>%
  #filter(is.na(SOUS_POP)|SOUS_POP=="pertes menstruelles faibles à modérées ou non menstruées")%>%#on considère pertes moyennes ou faibles
  #filter(!(NUT_inca=="proteines"&UNITE=="% AET"))%>%
  pivot_longer(cols=c(BNM,RNP,AS,LSS),names_to = "type",values_to = "reco")%>%
  pivot_longer(cols=c(class1_3:class15_17))%>%filter(value=="X")%>%select(-value)%>%
  mutate(tage_PS=ifelse(name=="class1_3",2,
                        ifelse(name=="class4_6",3,
                               ifelse(name=="class7_10",4,
                                      ifelse(name=="class11_14",5,
                                             ifelse(name=="class15_17",6,NA))))))%>%filter(!is.na(reco))%>%
  pivot_wider(names_from = "type",values_from = "reco")%>%
  mutate(sex_PS=ifelse(SEXE=="femme",2,1))%>%
  distinct(NUTRIMENT,UNITE,SOUS_POP,tage_PS,sex_PS,conversion,NUT_inca,AS,LSS,BNM,RNP)%>%
  mutate(class="enft")

reco_aduok <- reco_adu%>%
  #filter(is.na(SOUS_POP)|SOUS_POP=="pertes menstruelles faibles à modérées ou non menstruées")%>%#on considère pertes moyennes ou faibles
  #filter(!(NUT_inca=="proteines"&UNITE=="% AET"))%>%
  pivot_longer(cols=c(BNM,RNP,AS,LSS),names_to = "type",values_to = "reco")%>%
  pivot_longer(cols=c(class18_44:class65_79))%>%filter(value=="X")%>%select(-value)%>%
  mutate(tage_PS=ifelse(name=="class18_44",7,
                        ifelse(name=="class45_64",8,
                               ifelse(name=="class65_79",9,NA))))%>%filter(!is.na(reco))%>%
  pivot_wider(names_from = "type",values_from = "reco")%>%
  mutate(sex_PS=ifelse(SEXE=="femme",2,1))%>%
  distinct(NUTRIMENT,UNITE,SOUS_POP,tage_PS,sex_PS,conversion,NUT_inca,AS,LSS,BNM,RNP)%>%
  mutate(class="adu")

reco=bind_rows(reco_aduok,reco_enftok)

#13 individus n'ont pas de poids : ajout d'un poids moyen par age/sexe
poids_mean=description_indiv%>%
  filter(!(is.na(poids)))%>%
  group_by(tage_PS,sex_PS)%>%
  filter(pop3==1)%>%
  mutate(weight=ifelse(is.na(pond_indiv_adu_pop3),pond_indiv_enf_pop3,pond_indiv_adu_pop3))%>%
  nest()%>%
  mutate(data_pond=map(.x=data,.f=~svydesign(id=~zae+NOMEN+NOIND,strata = ~strate,data = .x,
                                             fpc=~fpc1+fpc2+fpc3, weights = ~weight)))%>%
  mutate(
    mean=map_dbl(.x=data_pond,~svymean(~poids,design = .x)[[1]]))%>%
  select(sex_PS,tage_PS,poids_mean=mean)

app_pct_reco=
  #on fait que pr les enfants de 1 ans et plus
app_nut%>%
  filter(type=="app_hors_alcool")%>%
  left_join(description_indiv%>%select(NOIND,poids,tage_PS,sex_PS),by="NOIND")%>%
  filter(tage_PS!=1)%>%
  left_join(poids_mean,by=c("sex_PS","tage_PS"))%>%
  mutate(proteines_kg=ifelse(is.na(poids),proteines/poids_mean,proteines/poids))%>%
  #conserve le choix pour les recos en zinc
  mutate(zinc300=zinc,zinc600=zinc,zinc900=zinc)%>%select(-zinc)%>%
  #conserve les diff reco en fer
  mutate(ferlow=fer,ferhigh=fer)%>%select(-fer)%>%
  pivot_longer(cols=c(aet:ag_lau_myr_pal_pctNRJ,proteines_kg:ferhigh))%>%
  mutate(class=ifelse(is.na(pond_indiv_adu_pop3),"enft","adu"))%>%
  left_join(reco,by=c("name"="NUT_inca","tage_PS","sex_PS","class"))%>%
  filter(!(is.na(AS)&is.na(LSS)&is.na(BNM)&is.na(RNP)))%>%
  mutate(pct_AS=
           #warning pr les fibres, l'unité est g/kcal dc on récupère la variable en % de l'énergie et on divise par 4
           ifelse(class=="enft"&name=="fibres_pctNRJ",((value/4)/100)/AS*100,
           value/AS*100),
         pct_BNM=value/BNM*100,
         pct_RNP=value/RNP*100,
         pct_LSS=value/LSS*100)

#EXPORT
openxlsx::write.xlsx(app_pct_reco,file="out/xlsx/app_pct_reco.xlsx")
write.csv2(app_pct_reco,file="out/csv/app_pct_reco.csv")

########## Indicateurs nut ---------------------

#Densité énergétique -------------

#MAR----------

#MAR 2000kcal --------

#MER -----------

#PanDiet -------------

########## Indicateurs alim ---------------------
#PNNS-GS ------------------