#' Fonction pour calculer le MAR/MER : chz adultes et enfant possible !
#' apport_nut_indiv table d'apports nutritionnels qui doit contenir : NOIND, aet (pr mar2000), tage_PS pour la classe d'âge, sex_PS pour le sexe (et doivent correspondre à la table des apports), et les variables suivantes
#' "ag_18_3_a_lino_pctNRJ",
#' "ag_18_2_lino_pctNRJ",
#' "proteines_pctNRJ",
#' "epa_dha","fibres_pctNRJ",
#' "fibres","calcium","vitamine_b1_pctNRJ","vitamine_b2","vitamine_b3_pctNRJ","vitamine_c",
#' "vitamine_b9","iode","phosphore","potassium","selenium","vitamine_b12","vitamine_d","cuivre","fer",
#' "magnesium","zinc","vitamine_a","vitamine_b6","vitamine_e","sucres_libres_pctNRJ","ags_pctNRJ","sodium"
#' reco_table doit contenir une variable RNP, AS et LSS (pr le MER) pour chaque classe d'âge et de sexe pour les mêmes nutriments cités au dessus

MAR_MER=function(apport_nut_indiv=NULL,
                 reco_table=NULL){
  
  liste_var_MAR=c("ag_18_3_a_lino_pctNRJ",
                  "ag_18_2_lino_pctNRJ",
                  "proteines_pctNRJ",
                  "epa_dha","fibres_pctNRJ",
                  "fibres","calcium","vitamine_b1_pctNRJ","vitamine_b2","vitamine_b3_pctNRJ","vitamine_c",
                  "vitamine_b9","iode","phosphore","potassium","selenium","vitamine_b12","vitamine_d","cuivre","fer",
                  "magnesium","zinc","vitamine_a","vitamine_b6","vitamine_e","sucres_libres_pctNRJ","ags_pctNRJ","sodium")
  
  #Check all requested nutrients are available in both input data and reco database
  if(!all(liste_var_MAR%in%colnames(apport_nut_indiv))){
    print("All requested nutrients are not in apport_nut_indiv")
    stop()
  }
  if(!all(liste_var_MAR%in%unique(reco_table$NUT_inca))){
    print("All requested recommended values are not in reco_table")
    stop()
  }
  
  MAR=
    apport_nut_indiv%>%
    select(NOIND,tage_PS,sex_PS,aet,all_of(liste_var_MAR))%>%
    pivot_longer(cols=c(liste_var_MAR))%>%
    left_join(reco_table,by=c("name"="NUT_inca","sex_PS","tage_PS"))%>%
    mutate(recook=ifelse(is.na(AS)&is.na(RNP),NA,
                         ifelse(is.na(AS)&!is.na(RNP),RNP,AS)))%>%
    filter(!is.na(recook))%>%
    mutate(ratio=value/recook*100)%>%
    mutate(ratio2000=((value/aet)*2000)/recook*100)%>%
    #cap les ratios
    mutate(ratio=ifelse(ratio>100,100,ratio))%>%
    mutate(ratio2000=ifelse(ratio2000>100,100,ratio2000))%>%
    group_by(NOIND,tage_PS,sex_PS)%>%
    summarise(MAR=mean(ratio),
              MAR_2000=mean(ratio2000),n_mar=n())#pr les 1-3 ans, pas epa dha
  
  MER=apport_nut_indiv%>%
    select(NOIND,sex_PS,tage_PS,sodium,ags_pctNRJ,sucres_libres_pctNRJ)%>%
    pivot_longer(cols=c(sodium,ags_pctNRJ,sucres_libres_pctNRJ))%>%
    left_join(reco_table%>%
                filter(NUT_inca%in%c("sucres_libres_pctNRJ","ags_pctNRJ","sodium"))
              ,by=c("name"="NUT_inca","sex_PS","tage_PS"))%>%
    mutate(MER_sub=value/LSS*100)%>%
    mutate(MER_sub=ifelse(MER_sub<100,0,MER_sub))%>%
    group_by(NOIND)%>%
    summarise(MER=mean(MER_sub),n_mer=n())
  
  res=MAR%>%left_join(MER)
  
  return(res)
}