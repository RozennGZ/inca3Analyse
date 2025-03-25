########## Load Packages ----------------------
library(tidyverse)
library(openxlsx)
library(survey)
options("survey.lonely.psu" = "adjust")
source("fonctions/mar_mer.R")

#Indicateurs nutritionnels et alimentaires pour les individus d'INCA3 de plus de 1 an

########## Data Importation -------------------
#Importation de la dernière table générée par Florent
conso_compo <- read.csv2("../../TABLES INCA3 PRIX AGB/conso_compo_prix_impact_recette_FEV_2025.csv", sep =  ",",dec=".",
                          encoding="UTF-8")
description_indiv <- read.csv2("../../TABLES INCA3 PRIX AGB/description-indiv.csv", 
                               sep = ";",dec=".")
activite_phys <- read.csv2("../../TABLES INCA3 PRIX AGB/actphys-sedent.csv", 
                               sep = ";",dec=".")


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

description_indiv=description_indiv%>%
  left_join(poids_mean,by=c("sex_PS","tage_PS"))%>%
  mutate(poidsOK=ifelse(is.na(poids),poids_mean,poids))%>%
  left_join(activite_phys%>%select(NOIND,nap),by="NOIND")

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
  filter(pop3==1)%>%
  #Add "0" variables aa qui n'ont pas de prot
  mutate(across(c(PROT_DIG:Isoflavonoids,fer_heminique:phytate_100g),~ifelse(is.na(.x),0,.x)))%>%
  #Ajout colonne ag laurique myristique palmitique
  mutate(ag_lau_myr_pal=ag_12_0+ag_14_0+ag_16_0)%>%
  #on filtre sur les individus de plus de 1 an
  filter(tage_PS>1)

#Recommandations enfants et adultes : tableau issu du projet IFIP, mis à jours pour considérer les recos par classe d'âge

#Note : reco en eau, fibres = EFSA ; reco protéines en kg de poids corporel et sucres libres : WHO
#pas de reco sur les AGS chz les enfants. Reco en fibre en g/kcal
#Interprétation pr les glucides et lipides de l'augmentation progressive
#Adaptation des recos en fonction des classes d'âges dispo ds INCA. Par ex la reco en eau pr les 9-14 a été appliquée pour les 7-10 et 11-14
reco_enft=read.xlsx("in/reco_nut.xlsx",sheet="ENFANTS")

#Note : 
reco_adu=read.xlsx("in/reco_nut.xlsx",sheet="ADULTES")

#Tableau avec une ligne = une reco par classe d'âge et sexe
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

########## Apports nutritionnels - coût - impacts environnementaux - Consommations variables PNNS individuels -------------------
#Liste Variables
list_var=colnames(conso_compo)[c(20:73,75,146:163,169:171,173:226,232:244,248:253,257,258,260,261,263,452)]
list_var_pct=list_var[c(3,4,5,8,10,11,22:24,32,34,74,75,155)]

#Hors boissons alcoolisées ------

# Variables non % NRJ
app_nut=conso_compo%>%
  left_join(description_indiv%>%select(NOIND,tage_PS,sex_PS,poidsOK))%>%
  dplyr::select(NOIND,pond_indiv_adu_pop3,pond_indiv_enf_pop3,pop3,tage_PS,sex_PS,poidsOK,code_gpe_INCA,qte_conso_pond,R24_nombre,list_var)%>%
  filter(code_gpe_INCA!=40)%>%#exclusion boissons alcoolisées
  mutate(across(list_var,~.x/100*qte_conso_pond))%>%
  #On conserve uniquement les individus qui ont rep à au moins 2 interview
  group_by(NOIND,R24_nombre,pond_indiv_adu_pop3,pond_indiv_enf_pop3,tage_PS,sex_PS,poidsOK,)%>%
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
  mutate(across(c("lipides","agpi","ag_18_2_lino","ag_18_3_a_lino","ags","ag_lau_myr_pal"),~9*.x))%>%
  mutate(across(list_var_pct,~.x/aet*100,.names = "{paste0(col,'_pctNRJ')}"),.keep="unused")

app_nut=app_nut%>%left_join(app_pct)%>%
  mutate(type="app_hors_alcool")

#Avec boissons alcoolisées ------

# Variables non % NRJ
app_nut_alc=conso_compo%>%
  left_join(description_indiv%>%select(NOIND,tage_PS,sex_PS,poidsOK))%>%
  dplyr::select(NOIND,pond_indiv_adu_pop3,pond_indiv_enf_pop3,pop3,tage_PS,sex_PS,poidsOK,code_gpe_INCA,qte_conso_pond,R24_nombre,list_var)%>%
  filter(pop3==1)%>%
  mutate(across(list_var,~.x/100*qte_conso_pond))%>%
  #On conserve uniquement les individus qui ont rep à au moins 2 interview
  group_by(NOIND,R24_nombre,pond_indiv_adu_pop3,pond_indiv_enf_pop3,tage_PS,sex_PS,poidsOK)%>%
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
  mutate(across(c("lipides","agpi","ag_18_2_lino","ag_18_3_a_lino","ags","ag_lau_myr_pal"),~9*.x))%>%
  mutate(across(list_var_pct,~.x/aet*100,.names = "{paste0(col,'_pctNRJ')}"),.keep="unused")

app_nut_alc=app_nut_alc%>%left_join(app_pct_alc)%>%
  mutate(type="app_with_alcool")

app_nut=bind_rows(app_nut,app_nut_alc)

#EXPORT
openxlsx::write.xlsx(app_nut,file="out/xlsx/app_nut_indiv.xlsx")
write.csv2(app_nut,file="out/csv/app_nut_indiv.csv")

########## Apports en % de la recommandation ------------------
#app_nut=read.csv2("out/csv/app_nut_indiv.csv")
app_pct_reco=
  #on fait que pr les enfants de 1 ans et plus
app_nut%>%
  filter(type=="app_hors_alcool")%>%
  filter(tage_PS!=1)%>%
  mutate(proteines_kg=proteines/poidsOK)%>%
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

data_de<-conso_compo%>%
  select(NOIND,gpe_INCA3,R24_nombre,code_gpe_INCA,qte_conso_pond,aet)%>%
  #lait inclus ds les liquides
  dplyr::mutate(TYPE = ifelse(code_gpe_INCA%in%c(12,35,36,37,38,39,40,44),"LIQUIDE","SOLIDE"))%>%
  dplyr::mutate(NRJ_CONSO = (qte_conso_pond*aet)/100)%>%
  dplyr::group_by(NOIND,TYPE,R24_nombre)%>%
  dplyr::summarise(tot_NRJ = sum(NRJ_CONSO),tot_QTE= sum(qte_conso_pond))%>%
  tidyr::pivot_wider(names_from = TYPE,values_from = c("tot_QTE","tot_NRJ"))%>%
  dplyr::mutate(MOY_NRJ_J = tot_NRJ_SOLIDE/R24_nombre, MOY_QTE_J = tot_QTE_SOLIDE/R24_nombre)%>%
  dplyr::mutate(DE = (MOY_NRJ_J/MOY_QTE_J)*100)

#20 individus ont une DE nulle car aucune conso d'aliments solides (laits infantiles)
View(conso_compo%>%filter(NOIND%in%c(data_de%>%filter(is.na(DE))%>%pull(NOIND))))

#MAR----------
#app_nut=read.csv2("out/csv/app_nut_indiv.csv")

res_MAR_MER=MAR_MER(apport_nut_indiv=
app_nut%>%#filter(!is.na(pond_indiv_adu_pop3))%>%
  filter(type=="app_hors_alcool")%>%
  #attention pr les enft il faut récupérer les fibres en g par kcal (pas en % aet)
  mutate(fibres_pctNRJ=((fibres_pctNRJ/4)/100)),

reco_table=bind_rows(reco_enftok,reco_aduok%>%
                       #selection d'une reco unique pr le zinc -> zinc 600
                       filter(!NUT_inca%in%c("zinc300","zinc900"))%>%
                       mutate(NUT_inca=ifelse(NUT_inca=="zinc600","zinc",NUT_inca)))%>%
  #sélection des reco high en fer pr les femmes de 11 à 65 ans, et suppression des low pr ces classes d'âge
  filter(!((tage_PS%in%c(5,6,7,8)&NUT_inca=="ferlow"&sex_PS==2)|(tage_PS==9&NUT_inca=="ferhigh")))%>%
  mutate(NUT_inca=ifelse(NUT_inca%in%c("ferlow","ferhigh"),"fer",NUT_inca))%>%
  #remove AS du sodium
  mutate(AS=ifelse(NUT_inca=="sodium",NA,AS))
  
  )
#note : calcium à 900mg pour tous les adultes)

#PanDiet -------------

#CODE TYPHAINE
# PANDIET <- TABLE_INCA_CONSO%>%
#   left_join(TABLE_INFO_INDIV%>%select(NOIND,sex_PS,poids,tage_PS,taille), on="NOIND")%>%
#   filter(gpe_INCA3!=33)%>% # le groupe 33 correspond aux boissons alcolisées
#   #calcul d'un poids moyen pour les manquant en fonction de la age, du sexe et de la taille
#   mutate(taille_test = as.factor(round(taille,0)))%>% #arondir nombre de chiffre 
#   group_by(sex_PS,tage_PS,taille_test)%>%
#   mutate(poids_calc=mean(poids, na.rm = T))%>% 
#   ungroup()%>%
#   mutate(poids=if_else(is.na(poids),poids_calc,poids))%>%
#   select(NOIND, c(aet:iode), aliment_code_INCA3,qte_conso_pond, R24_nombre,R24_num,sex_PS,poids)%>%
#   filter(!(aliment_code_INCA3 %in% c(3049,3393,3724)))%>% 
#   #enlève les aliments qui n'ont aucune valeur (cyclamate, aspartame et réglisse)
#   mutate(prot_NRJ = proteines*4, 
#          lip_NRJ = lipides*9, 
#          glu_NRJ = glucides*4,
#          sucres_NRJ = sucres*4,
#          ags=ags*9,
#          ag_18_2_lino=ag_18_2_lino*9,
#          ag_18_3_a_lino=ag_18_3_a_lino*9)%>%
#   pivot_longer(cols = c(aet:iode,prot_NRJ:sucres_NRJ), names_to = "nutriment")%>%
#   mutate(apport=value * qte_conso_pond/100)%>% 
#   mutate(NOIND = as.factor(NOIND),nutriment = as.factor(nutriment))%>%
#   group_by(NOIND,nutriment,sex_PS,poids,R24_num, R24_nombre)%>%
#   dplyr::summarise(apportj=sum(apport))%>% 
#   # mutate(apportj = apportj/R24_nombre)%>%
#   pivot_wider(names_from = nutriment,values_from = apportj)%>%
#   mutate(prot_NRJ = prot_NRJ/aet*100,
#          lip_NRJ = lip_NRJ/aet*100, 
#          glu_NRJ = glu_NRJ/aet*100,
#          sucres_NRJ = sucres_NRJ/aet*100,
#          ag_18_2_lino= ag_18_2_lino/aet*100,
#          ag_18_3_a_lino= ag_18_3_a_lino/aet*100,
#          proteines= proteines/poids,
#          vitamine_a=retinol+beta_carotene/6,
#          epa_dha = sum(ag_20_5_epa,ag_20_6_dha),
#          vitamine_b1=vitamine_b1*1000/aet,
#          vitamine_b3 = vitamine_b3*1000/aet, 
#          ags = ags/aet*100,
#          magnesium=magnesium/poids)%>%
#   pivot_longer(cols = c(acides_organiques:epa_dha), names_to = "nutriment")%>%
#   group_by(NOIND,nutriment,sex_PS,poids,R24_nombre)%>%
#   summarise(meanapp=mean(value, na.rm = T), S_dev = sd(value, na.rm = T))%>%
#   left_join(references%>%select(sex_PS:TUIL_PANDIET),on=c("nutriment"))%>%
#   filter(!is.na(label))%>%
#   ###old version  
#   mutate(
#     Ascore=pnorm((meanapp-Ref_A_PANDIET)/sqrt((Ref_A_PANDIET*CV_ref_PANDIET)**2 + S_dev**2/R24_nombre)),
#     Mscore=1-pnorm((meanapp-Ref_M_PANDIET)/sqrt((Ref_M_PANDIET*CV_ref_PANDIET)**2 + S_dev**2/R24_nombre)),
#     TUILp= if_else(meanapp>TUIL_PANDIET,1,0))%>%
#   # dha vaut que 1/2 sur le coef
#   mutate(Ascore=if_else(nutriment%in% c("ag_20_6_dha","epa_dha"),Ascore*0.5,Ascore))%>%
#   group_by(NOIND)%>%
#   dplyr::summarise(Ascore_tot=sum(Ascore,na.rm = TRUE)/26*100,
#                    Mscore_tot=sum(Mscore,na.rm = TRUE),
#                    TUILp=sum(TUILp, na.rm=TRUE))%>%
#   mutate(Mscore_tot=(Mscore_tot/(TUILp+6))*100)%>%
#   #moyenne de A et M
#   mutate(PANDiet=(Ascore_tot+Mscore_tot)/2)%>%
#   select(NOIND,PANDiet)
# 
# 
# moy_pandiet <- mean(PANDIET$PANDiet)

#EXPORT
write.xlsx(res_MAR_MER%>%left_join(data_de%>%select(NOIND,tot_QTE_SOLIDE,tot_NRJ_SOLIDE,DE)),
           file="out/xlsx/indic_nut.xlsx")
write.csv2(res_MAR_MER%>%left_join(data_de%>%select(NOIND,tot_QTE_SOLIDE,tot_NRJ_SOLIDE,DE)),
           file="out/csv/indic_nut.csv")
########## Indicateurs alim ---------------------

#PNNS-GS ------------------

#Calcul spécifique pr les produits céréaliers complet en portion vs petit dej 
portion_cereal_compl=conso_compo%>%
  left_join(description_indiv%>%select(NOIND,tage_PS,sex_PS,poidsOK))%>%
  dplyr::select(NOIND,pond_indiv_adu_pop3,pond_indiv_enf_pop3,pop3,tage_PS,sex_PS,poidsOK,code_gpe_INCA,qte_conso_pond,R24_nombre,sg_PNNS_prodcerealcomplets)%>%
  filter(code_gpe_INCA!=40)%>%#exclusion boissons alcoolisées
  #estimation portion produits céréaliers complet en fct du gpe INCA
  mutate(port_PNNS_prodcerealcomplets=ifelse(
    #céréales du petit dej
    sg_PNNS_prodcerealcomplets>0&code_gpe_INCA==18,sg_PNNS_prodcerealcomplets/30,
    ifelse(sg_PNNS_prodcerealcomplets>0&code_gpe_INCA!=18,sg_PNNS_prodcerealcomplets/200,sg_PNNS_prodcerealcomplets)))%>%
  mutate(port_PNNS_prodcerealcomplets=port_PNNS_prodcerealcomplets/100*qte_conso_pond)%>%
  #On conserve uniquement les individus qui ont rep à au moins 2 interview
  group_by(NOIND,R24_nombre,pond_indiv_adu_pop3,pond_indiv_enf_pop3,tage_PS,sex_PS,poidsOK,)%>%
  dplyr::summarise(port_PNNS_prodcerealcomplets=sum(port_PNNS_prodcerealcomplets))%>%
  #Calcul des apports journaliers
  mutate(port_PNNS_prodcerealcomplets=port_PNNS_prodcerealcomplets/R24_nombre)%>%
  ungroup()%>%select(NOIND,port_PNNS_prodcerealcomplets)


app_nut_pnns=app_nut%>%
  filter(type=="app_hors_alcool")%>%
  select(NOIND,aet,sex_PS,tage_PS,poidsOK,sodium,sg_PNNS_fruitssecs,sg_PNNS_jusdefruits100_,sg_PNNS_legumes,sg_PNNS_fruits,
         sg_PNNS_noixfruitsacoque,sg_PNNS_legsecs,
         sg_PNNS_prodpancomplets,sg_PNNS_prodcerealcomplets,
         sg_PNNS_fromage,sg_PNNS_lait,sg_PNNS_yaourtfromageblanc,
         sg_PNNS_viandeshorsvolaille,g_PNNS_charcu,g_PNNS_prodpeche,
         g_PNNS_matgrasse,sucres_aj_pctNRJ,sg_PNNS_bsucrees
         )%>%
  #Ajout de la variable alcool avec conservation boissons alcoolisées
  left_join(app_nut%>%
              filter(type=="app_with_alcool")%>%select(NOIND,sg_PNNS_boissonsalcool))%>%
  
  #ajout de la variable des céréales complètes en portion en fct pdj ou non
  left_join(portion_cereal_compl,by="NOIND")%>%
  
  #Fruits et légumes : en servings
  mutate(
    
    #FRUITS LEG, FRUITS SEC JUS EN PORTION
    #dried fruits 30g portion, 1 max
    port_PNNS_fruitssecs=ifelse(sg_PNNS_fruitssecs/30>1,1,sg_PNNS_fruitssecs/30),
    #jus de fruit 150g portion, 1 max
    port_PNNS_jusdefruits100=ifelse(sg_PNNS_jusdefruits100_/150>1,1,sg_PNNS_jusdefruits100_/150),
    PNNS_FLEG=sg_PNNS_fruits/80+sg_PNNS_legumes/80+port_PNNS_fruitssecs+port_PNNS_jusdefruits100,
    
    score_FLEG=ifelse(PNNS_FLEG<3.5,0,
                      ifelse(PNNS_FLEG>=3.5&PNNS_FLEG<5,0.5,
                             ifelse(PNNS_FLEG>=5&PNNS_FLEG<7.5,1,
                                    ifelse(PNNS_FLEG>=7.5,2,NA)))),
      
    #NUTS en portion
    port_PNNS_nuts=sg_PNNS_noixfruitsacoque/30,
    
    score_NUTS=ifelse(port_PNNS_nuts==0,0,
                      ifelse(port_PNNS_nuts>0&port_PNNS_nuts<0.5,0.5,
                             ifelse(port_PNNS_nuts>=0.5&port_PNNS_nuts<1.5,1,
                                    ifelse(port_PNNS_nuts>=1.5,0,NA)))),
    
    #LEGUMES SEC en portion
    port_PNNS_LGS=(sg_PNNS_legsecs*7)/200,
    
    score_LGS=ifelse(port_PNNS_LGS==0,0,
                     ifelse(port_PNNS_LGS>0&port_PNNS_LGS<2,0.5,
                            ifelse(port_PNNS_LGS>=2,1,NA))),
         
    #WHOLE GRAINS 200 pâtes/riz, 30 céréales, 50 pain
    port_PNNS_pain=sg_PNNS_prodpancomplets/50,
    
    PNNS_FEC=port_PNNS_pain+port_PNNS_prodcerealcomplets,
    
    score_FEC=ifelse(PNNS_FEC==0,0,
                     ifelse(PNNS_FEC>0&PNNS_FEC<1,0.5,
                            ifelse(PNNS_FEC>=1&PNNS_FEC<2,1,
                                   ifelse(PNNS_FEC>=2,1.5,NA)))),
    
    #MILK DAIRY EN PORTION 150 milk 125 yoghurt 30g cheese
    port_PNNS_fromage=sg_PNNS_fromage/30,
    port_PNNS_lait=sg_PNNS_lait/150,
    port_PNNS_yaourt=sg_PNNS_yaourtfromageblanc/125,
    
    PNNS_DAIRY = port_PNNS_fromage+port_PNNS_lait+port_PNNS_yaourt,
    
    score_DAIRY=ifelse(PNNS_DAIRY<0.5,0,
                       ifelse(PNNS_DAIRY>=0.5&PNNS_DAIRY<1.5,0.5,
                              ifelse(PNNS_DAIRY>=1.5&PNNS_DAIRY<2.5,1,
                                     ifelse(PNNS_DAIRY>=2.5,0,NA)))),
    
    #RED MEAT
    sg_PNNS_viandeshorsvolaille=sg_PNNS_viandeshorsvolaille*7,
    
    score_REDMEAT=ifelse(sg_PNNS_viandeshorsvolaille<500,0,
                         ifelse(sg_PNNS_viandeshorsvolaille>=500&sg_PNNS_viandeshorsvolaille<750,-1,
                                ifelse(sg_PNNS_viandeshorsvolaille>=750,-2,NA))),
    
    #PROCESSED MEAT
    g_PNNS_charcu=g_PNNS_charcu*7,
    
    score_PROCESSED=ifelse(g_PNNS_charcu<150,0,
                           ifelse(g_PNNS_charcu>=150&g_PNNS_charcu<300,-1,
                                  ifelse(g_PNNS_charcu>=300,-2,NA))),
    
    #FISH and SEAFOODS 100g per week
    port_PNNS_prodpeche=(g_PNNS_prodpeche*7)/100,
    
    score_FISH=ifelse(port_PNNS_prodpeche<1.5,0,
                      ifelse(port_PNNS_prodpeche>=1.5&port_PNNS_prodpeche<2.5,1,
                             ifelse(port_PNNS_prodpeche>=2.5&port_PNNS_prodpeche<3.5,0.5,
                                    ifelse(port_PNNS_prodpeche>=3.5,0,NA)))),
    
    #ADDED FATS
    LIPpct_PNNS_matgrasse=g_PNNS_matgrasse*9/aet*100,
    
    score_ADDEDFAT=ifelse(LIPpct_PNNS_matgrasse>16,0,
                          ifelse(LIPpct_PNNS_matgrasse<=16,1.5,NA)),
    
    #SUGARY FOODS
    score_SUGARY=ifelse(sucres_aj_pctNRJ<10,0,
                        ifelse(sucres_aj_pctNRJ>=10&sucres_aj_pctNRJ<15,-1,
                               ifelse(sucres_aj_pctNRJ>=15,-2,NA))),
    
    #SWEET BEVERAGES
    score_BEV=ifelse(sg_PNNS_bsucrees==0,0,
                     ifelse(sg_PNNS_bsucrees>0&sg_PNNS_bsucrees<250,-0.5,
                            ifelse(sg_PNNS_bsucrees>=250&sg_PNNS_bsucrees<750,-1,
                                   ifelse(sg_PNNS_bsucrees>=750,-2,NA)))),
    
    #ALCOHOLIC BEVERAGES
    sg_PNNS_boissonsalcool=sg_PNNS_boissonsalcool*7,

    score_ALCOOL=ifelse(sg_PNNS_boissonsalcool==0,0.5,
                        ifelse(sg_PNNS_boissonsalcool>0&sg_PNNS_boissonsalcool<=100,0,
                               ifelse(sg_PNNS_boissonsalcool>100&sg_PNNS_boissonsalcool<=200,-1,
                                      ifelse(sg_PNNS_boissonsalcool>200,-2,NA)))),
  
    #SALT
    SEL=sodium*2.54/1000,
    
    score_SALT=ifelse(SEL<=6,1,
                      ifelse(SEL>6&SEL<=8,0,
                             ifelse(SEL>8&SEL<=10,-0.5,
                                    ifelse(SEL>10&SEL<=12,-1,
                                           ifelse(SEL>12,-2,NA))))),
    
    #weights : 3 FLEG, PROCESSED, SUGARY FOODS, ALCOOHOL, SWEET BEV, SALT, 
    #2 for WHOLE grains, red meat, fish, added fats, 
    #1 for nuts, legumes, milk
    score_PNNS=score_FLEG*(3/2)+
      score_NUTS*(1/1)+
      score_LGS*(1/1)+
      score_FEC*(2/1.5)+
      score_DAIRY*(1/1)+
      score_REDMEAT*(2/(2))+
      score_PROCESSED*(3/2)+
      score_FISH*(2/1)+
      score_ADDEDFAT*(2/1.5)+
      score_SUGARY*(3/2)+
      score_BEV*(3/2)+
      score_ALCOOL*(3/2)+
      score_SALT*(3/2))%>%
  left_join(description_indiv%>%select(NOIND,nap))%>%
  mutate(BMR=ifelse(sex_PS==1,
                    ifelse(tage_PS==2,60.9*poidsOK-54,
                           ifelse(tage_PS%in%c(3,4),22.7*poidsOK+495,
                                  ifelse(tage_PS%in%c(5,6),17.5*poidsOK+651,
                                         ifelse(tage_PS==7,15.3*poidsOK+679,
                                                ifelse(tage_PS==8,11.6*poidsOK+879,
                                                       ifelse(tage_PS==9,13.5*poidsOK+487,NA)))))),
                    ifelse(sex_PS==2,
                           ifelse(tage_PS==2,61.0*poidsOK-51,
                                  ifelse(tage_PS%in%c(3,4),22.5*poidsOK+499,
                                         ifelse(tage_PS%in%c(5,6),12.2*poidsOK+746,
                                                ifelse(tage_PS==7,14.7*poidsOK+496,
                                                       ifelse(tage_PS==8,8.7*poidsOK+829,
                                                              ifelse(tage_PS==9,10.5*poidsOK+596,NA)))))),NA)))%>%
  #pr les valeurs manquantes d'activité physique, on applique activité physique moyen
  mutate(nap=ifelse(is.na(nap),2,nap))%>%
  mutate(EER=ifelse(sex_PS==1,
                    ifelse(nap==1,BMR*1.55,
                           ifelse(nap==2,BMR*1.78,
                                  ifelse(nap==3,BMR*2.10,NA))),
                    ifelse(sex_PS==2,
                           ifelse(nap==1,BMR*1.56,
                                  ifelse(nap==2,BMR*1.64,
                                         ifelse(nap==3,BMR*1.82,NA))),NA)))%>%
  mutate(ratio_EER=aet/EER)%>%
  mutate(score_PNNS_adj=ifelse(ratio_EER>1.05,score_PNNS-score_PNNS*(ratio_EER-1),score_PNNS))%>%
  select(NOIND:poidsOK,starts_with("score_"),nap,BMR,EER,ratio_EER,score_PNNS_adj)

#EXPORT
write.xlsx(app_nut_pnns,
           file="out/xlsx/indic_alim_PNNS.xlsx")
write.csv2(app_nut_pnns,"out/csv/indic_alim_PNNS.xlsx")
