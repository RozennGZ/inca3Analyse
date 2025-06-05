#' Fonction pour calculer le aHEI2010 suivant la publi de Chiuve et al "Alternative Dietary Indices Both Strongly Predict Risk of Chronic Disease"
#' #SANS les transfat !
#' 
#' df table qui doit contenir : NOIND, sex_PS pour le sexe (1=homme, 2=femme), et les variables suivantes
#'veg_serv : nb de portions/j de légumes, All veg (hors pdt) - ds la publi : 1 serv = 0.5 cup veg or 1 cup of green leafy veg = 236.59 g - 150 considéré pour INCA3 (GEMRCN)
#' fruit_serv : nb de portions/j de fruits (only fruits without fruit juice) ds la publi : 1 serv = one medium piece of fruit or 0.5 cup berries 236.59 g  - 100 considéré pour INCA3 (GEMRCN)
#' wgrains : g/d 0 de produits complets, normalement exprimé en grains complets bruts
#' bev_serv : nb de portions de boissons sucrées et jus de fruits par jour, 1 serv = 8 oz = 8*28.35 g
#' nut_serv : nombre de portions de nuts + légumineuses + substituts protéiques type tofu. 1 serv = 28.35 g
#' redmeat_serv : nb de portions de viande rouge et charcuterie par jour (viande rouge : boeuf, porc, agneau). 1 serv = 113.4g pour la viande et 42.525 g pour la charcuterie
#' PAS DE COMPOSANTE TRANSFAT
#' epa_dha en mg/d 0 
#' pufa_nrj : en % énergie, pufa sans epa dha : inclut ag alpha linolénique, linoléique et arachidonique
#' sodium : apport journalier en mg/j
#' alcohol_serv : nombre de portions par jour d'alcool, en considérant 1 portion = 113.4 g pour le vin, 340.2 g pour la bière et 42.525 pour les alcools forts
#' P10_sodium : 1er décile estimé par sexe d'apport journalier en sodium = considéré en seuil min par sexe
#' P90_sodium : dernier décile estimé par sexe d'apport journalier en sodium = considéré en seuil max, par sexe

ahei2010=data_deahei2010=function(df){
  df%>%mutate(
    score_veg=ifelse(veg_serv>5,10,10*veg_serv/5),
    score_fruit=ifelse(fruit_serv>4,10,10*fruit_serv/4),
    score_wgrains=ifelse(sex_PS==2,
                         ifelse(wgrains>75,10,10*wgrains/75),
                         ifelse(sex_PS==1,
                                ifelse(wgrains>90,10,10*wgrains/90),NA)),
    score_bev=ifelse(bev_serv==0,10,
                     ifelse(bev_serv>=1,0,10-10*bev_serv/1)),
    score_nuts=ifelse(nut_serv>1,10,10*nut_serv/1),
    score_redmeat=ifelse(redmeat_serv==0,10,
                         ifelse(redmeat_serv>=1.5,0,
                                10-10*redmeat_serv/1.5)),
    score_epadha=ifelse(epa_dha>250,10,10*epa_dha/250),
    score_pufa=ifelse(pufa_nrj<=2,0,
                      ifelse(pufa_nrj>=10,10,10*(pufa_nrj-2)/(10-2))),
    score_sodium=
      ifelse(sodium<=P10_sodium,10,
             ifelse(sodium>=P90_sodium,0,10-10*(sodium-P10_sodium)/(P90_sodium-P10_sodium))),
    score_alcohol=ifelse(alcohol_serv==0,2.5,
                         ifelse(sex_PS==2,
                                ifelse(alcohol_serv>=2.5,0,
                                       ifelse(alcohol_serv>=0.5&alcohol_serv<=1.5,10,
                                              ifelse(alcohol_serv<0.5,2.5+alcohol_serv/0.5*(10-2.5),
                                                     ifelse(alcohol_serv>1.5,10+(alcohol_serv-1.5)/(2.5-1.5)*(0-10),NA)))),  
                                ifelse(sex_PS==1,
                                       ifelse(alcohol_serv>=3.5,0,
                                              ifelse(alcohol_serv>=0.5&alcohol_serv<=2,10,
                                                     ifelse(alcohol_serv<0.5,2.5+alcohol_serv/0.5*(10-2.5),
                                                            ifelse(alcohol_serv>2,10+(alcohol_serv-2)/(3.5-2)*(0-10),NA))))      
                                       ,NA))),
    aHEI2010=score_veg+score_fruit+score_wgrains+
      score_bev+score_nuts+score_redmeat+score_epadha+score_pufa+score_sodium+score_alcohol
  )
  
}
