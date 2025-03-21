########## Load Packages ----------------------
library(tidyverse)
library(openxlsx)

########## Data Importation -------------------
#Importation de la dernière table générée par Florent
conso_compo <- read.csv2("../../TABLES INCA3 PRIX AGB/conso_compo_prix_impact_recette_FEV_2025.csv", sep =  ",",dec=".",
                          encoding="UTF-8")
description_indiv <- read.csv2("../../TABLES INCA3 PRIX AGB/description-indiv.csv", 
                               sep = ";",dec=".")
code_gpe_INCA3 <- read.csv2("in/gpe_INCA3.csv")

#Préparation table conso_compo
conso_compo <- conso_compo%>%
  left_join(code_gpe_INCA3, by="gpe_INCA3")%>%
  #suppression du lait maternel sans compo
  filter(!is.na(aet))%>%
  #Suppression des matières grasses ac énergie =0  
  filter(!(aet == 0 & eau == 0 & lipides == 0))%>%
  #supprimer les aliments qui ont des valeurs de compo manquantes (aspartame, cyclamate, réglisse)
  filter(!aliment_code_INCA3%in%c(3049, 3393,3724))%>%
  left_join(description_indiv,by="NOIND")%>%
  #Add "0" variables aa qui n'ont pas de prot
  mutate(across(c(PROT_DIG:Isoflavonoids,fer_heminique:phytate_100g),~ifelse(is.na(.x),0,.x)))

########## Apports nutritionnels - coût - impacts environnementaux - Consommations variables PNNS individuels -------------------
#Liste Variables
list_var=colnames(conso_compo)[c(20:73,75,146:163,169:171,173:226,232:244,248:253,257,258,260,261,263)]
list_var_pct=list_var[c(3,4,5,10,22:24,32,34,74,75)]

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
  mutate(across(c("proteines","glucides","sucres","sucres_aj","sucres_libres"),~4*.x))%>%
  #Expression des variables en kcal
  mutate(across(c("lipides","agpi","ag_18_2_lino","ag_18_3_a_lino"),~9*.x))%>%
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
  mutate(across(c("proteines","glucides","sucres","sucres_aj","sucres_libres"),~4*.x))%>%
  #Expression des variables en kcal
  mutate(across(c("lipides","agpi","ag_18_2_lino","ag_18_3_a_lino"),~9*.x))%>%
  mutate(across(list_var_pct,~.x/aet*100,.names = "{paste0(col,'_pctNRJ')}"),.keep="unused")

app_nut_alc=app_nut_alc%>%left_join(app_pct_alc)%>%
  mutate(type="app_with_alcool")

app_nut=bind_rows(app_nut,app_nut_alc)

#EXPORT
openxlsx::write.xlsx(app_nut,file="out/xlsx/app_nut_indiv.xlsx")
write.csv2(app_nut,file="out/csv/app_nut_indiv.csv")

########## Apports en % de la recommandation ------------------


########## Indicateurs nut ---------------------

#Densité énergétique -------------

#MAR----------

#MAR 2000kcal --------

#MER -----------

#PanDiet -------------

########## Indicateurs alim ---------------------
#PNNS-GS ------------------