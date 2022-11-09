df_list_med <- list(osm_bank_Medellin_sf, osm_bus_station_Medellin_sf, osm_casino_Medellin_sf, 
                    osm_childcare_Medellin_sf, osm_cinema_Medellin_sf,osm_clinic_Medellin_sf, 
                    osm_college_Medellin_sf, osm_community_centre_Medellin_sf, 
                    osm_conference_centre_Medellin_sf, osm_dentist_Medellin_sf, 
                    osm_doctors_Medellin_sf, osm_events_venue_Medellin_sf, osm_fast_food_Medellin_sf, 
                    osm_hospital_Medellin_sf, osm_kindergarten_Medellin_sf, osm_library_Medellin_sf,
                    osm_love_hotel_Medellin_sf, osm_marketplace_Medellin_sf, osm_monastery_Medellin_sf,
                    osm_parking_Medellin_sf, osm_police_Medellin_sf, osm_restaurant_Medellin_sf)

lista_amenities_cal_p <- list("bank", "bus_station", "casino")

df_list_med_p <- list(osm_bank_Medellin_sf, osm_bus_station_Medellin_sf, osm_casino_Medellin_sf)

columnselect<-function(df){
  df[,c("osm_id","amenity")]
}


df_list_med_p <- lapply(X=df_list_med_p,FUN=columnselect)

df_list_med <- lapply(X=df_list_med,FUN=columnselect)

for (amenity in c(lista_amenities_cal_p)){
  
  med_houses[paste0('num_', amenity)] <- NA
  med_houses[paste0('val_', amenity)] <- NA
  
}

muestra <- sample(1:nrow(med_houses), 100)
prueba <- med_houses[muestra,]

prueba_sp <- prueba%>%st_buffer(100)%>%as_Spatial()

prueba_nb <- poly2nb(pl=prueba_sp, queen= TRUE) #opcion reina

prueba$num_vecinos <- NA
prueba$val_vecinos <- NA

prueba <- prueba %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 



for (i in 1:length(prueba_nb)) {
  
  prueba$num_vecinos[i] <- size_sum(prueba_nb[[i]])
  prueba$val_vecinos[i] <- prueba_nb[[i]]==0
  
}

base_aux <- base_aux %>% mutate(res_final = ifelse(valor_res == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_res)))) 

drop <- c("num_vecinos", "val_vecinos")
prueba <- prueba[,!names(prueba)%in%drop]


for (i in 1:length(df_list_med_p)){
  
  df_list_med[i] <- bind_rows(prueba, df_list_med[i])
  
  df_list_med[i] <- prueba%>%st_buffer(20)%>%as_Spatial()
  
  df_list_med[i] <- poly2nb(pl=df_list_med[i], queen= TRUE) #opcion reina
  
  
  
  for (amenity in c(lista_amenities_cal_p)){
    
    prueba <- prueba%>%dplyr::mutate(paste0('final_', amenity) == ifelse(paste0('val_', amenity) == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", paste0('num_', amenity))))) 
    drop <- c("osm_id","amenity", paste0('num_', amenity), paste0('val_', amenity))
    base_med_aux <- base_med_aux[,!names(base_aux)%in%drop]
    
    # for (i in 1:length(base_med_aux_sp)) {
    
    #   base_med_aux[paste0('num_', amenity)][i] <- size_sum(base_med_aux_sp[[i]])
    #   base_med_aux[paste0('val_', amenity)][i] <- base_med_aux_sp[[i]]==0
    
  }
  
  base_med_aux <- base_aux%>%dplyr::mutate(paste0('final_', amenity) == ifelse(paste0('val_', amenity) == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", paste0('num_', amenity))))) 
  drop <- c("osm_id","amenity", paste0('num_', amenity), paste0('val_', amenity))
  base_med_aux <- base_med_aux[,!names(base_aux)%in%drop]
  
  
}


#base_med_aux <- bind_rows(med_houses, df_list_med)

#base_med_aux_st <- base_med_aux%>%st_buffer(50)%>%as_Spatial()

#base_med_aux_sp <- poly2nb(pl=base_med_aux_st, queen= TRUE) #opcion reina

#for (amenity in c(lista_amenities_cal)){

#base_med_aux[paste0('num_', amenity)] <- NA
#base_med_aux[paste0('val_', amenity)] <- NA

#}

for (i in 1:length(df_list_med)){
  
  base_med_aux <- bind_rows(med_houses, df_list_med[i])
  
  base_med_aux_st <- base_med_aux%>%st_buffer(20)%>%as_Spatial()
  
  base_med_aux_sp <- poly2nb(pl=base_med_aux_st, queen= TRUE) #opcion reina
  
  for (amenity in c(lista_amenities_cal)){
    
    base_med_aux[paste0('num_', amenity)] <- NA
    base_med_aux[paste0('val_', amenity)] <- NA
    
  }
  
  for (i in 1:length(base_med_aux_sp)) {
    
    base_med_aux[paste0('num_', amenity)][i] <- size_sum(base_med_aux_sp[[i]])
    base_med_aux[paste0('val_', amenity)][i] <- base_med_aux_sp[[i]]==0
    
  }
  
  base_med_aux <- base_aux%>%dplyr::mutate(paste0('final_', amenity) == ifelse(paste0('val_', amenity) == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", paste0('num_', amenity))))) 
  drop <- c("osm_id","amenity", paste0('num_', amenity), paste0('val_', amenity))
  base_med_aux <- base_med_aux[,!names(base_aux)%in%drop]
  
}  

}

base_med_aux <- base_aux%>%mutate(cafe_final = ifelse(paste0('val_', amenity) == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cafe)))) 


#base_aux <- base_aux%>%subset(is.na(amenity) == TRUE)
drop <- c("osm_id", "name","amenity","num_cafe", "valor_cafe")
base_aux <- base_aux[,!names(base_aux)%in%drop]


