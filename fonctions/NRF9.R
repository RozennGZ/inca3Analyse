#' @author Rozenn MS-Nutrition
#' @title nrf9.3 for diet
#' @description calcul NRF.9.3 for diets, basé sur la publication https://www.mdpi.com/2072-6643/10/9/1200
#' @import dplyr
#' @param df dataframe with daily average nutrient intakes for p    
# proteine,fibre,vit_A,vit_C,vit_D,calcium,iron,potassium,magnesium,
# add_sugar,sfa,sodium,aet
#' 
#Ref https://www.frontiersin.org/journals/nutrition/articles/10.3389/fnut.2024.1438369/full
#potassium value to be validated
nrf9_3d=function(df,
    dv_prot=50,
    dv_fiber=28,
    dv_vit_A=900,
    dv_vit_C=90,
    dv_vit_D=20,
    dv_min_CA=1300,
    dv_min_FE=18,
    dv_min_K=3500,
    dv_min_MG=420,
    mrv_add_sugar=50,
    mrv_sfa=20,
    mrv_min_NA=2300){

  res_nrf=df%>%

    mutate(
      #positive nutrients
      ratio_p_prot=(proteine/aet*2000)/dv_prot,
      ratio_p_fibre=(fibre/aet*2000)/dv_fiber,
      ratio_p_vitA=(vit_A/aet*2000)/dv_vit_A,
      ratio_p_vitC=(vit_C/aet*2000)/dv_vit_C,
      ratio_p_vitD=(vit_D/aet*2000)/dv_vit_D,
      ratio_p_calcium=(calcium/aet*2000)/dv_min_CA,
      ratio_p_iron=(iron/aet*2000)/dv_min_FE,
      ratio_p_potassium=(potassium/aet*2000)/dv_min_K,
      ratio_p_magnesium=(magnesium/aet*2000)/dv_min_MG,
      
      #negative nutrients
      ratio_n_add_sugar=((add_sugar/aet*2000)/mrv_add_sugar)-1,
      ratio_n_sfa=((sfa/aet*2000)/mrv_sfa)-1,
      ratio_n_sodium=((sodium/aet*2000)/mrv_min_NA)-1)%>%
      
      #scores tronqués à 100
      mutate_at(vars(starts_with("ratio_p")),~ifelse(.>1,1,.))%>%
      #to consider excess only
      mutate_at(vars(starts_with("ratio_n")),~ifelse(.<0,0,.))%>%
    
      #subscores
    mutate(NR=ratio_p_prot+ratio_p_fibre+ratio_p_vitA+ratio_p_vitC+ratio_p_vitD+
      ratio_p_calcium+ratio_p_iron+ratio_p_potassium+ratio_p_magnesium,
    LIM=ratio_n_add_sugar+ratio_n_sfa+ratio_n_sodium,
    
    #NRF9
    NRF9=(NR-LIM)*100
    )
  
# NRF9.3 = (NR − LIM) × 100 
return(res_nrf)
  }
