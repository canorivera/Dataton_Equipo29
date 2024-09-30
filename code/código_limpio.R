# Desactivamos la notación científica
options(scipen = 999999)

# Cargamos los paquetes que requerimos
if (!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(dplyr, tidyverse, data.table, ggplot2, sf, janitor, plotly, pscl, readxl)

#abrimos datos
read_and_filter = function(filename){
  df = fread(filename) |> janitor::clean_names() |> select(nom_estab, per_ocu, entidad, cve_ent, municipio, cve_mun, localidad, manzana, tipo_vial, latitud, longitud, codigo_act, fecha_alta) |> filter(codigo_act == 464111 | codigo_act == 464112) 
}
filter_read = function(filename){
  df = fread(filename) |> janitor::clean_names() |> select(nom_estab, per_ocu, entidad, cve_ent, municipio, cve_mun, localidad, manzana, tipo_vial, latitud, longitud, codigo_act, fecha_alta) |> filter(codigo_act != 464111 & codigo_act != 464112) 
}

filenames = paste0("C:/Users/luism/Documents/dataton/datos/DENUEs/", 2015:2024, "/conjunto_de_datos/denue_inegi_46321-46531_.csv")
denues_farm = lapply(filenames, read_and_filter)
denues_nofarm = lapply(filenames, filter_read)


#filtrado

farm20y21 <-denues_farm[[6]][(denues_farm[[6]]$latitud %in% denues_farm[[7]]$latitud & denues_farm[[6]]$longitud %in% denues_farm[[7]]$longitud)]
farm20no21 <- denues_farm[[6]][!(denues_farm[[6]]$latitud %in% denues_farm[[7]]$latitud & denues_farm[[6]]$longitud %in% denues_farm[[7]]$longitud)]
farm21no20<- denues_farm[[7]][!(denues_farm[[7]]$latitud %in% denues_farm[[6]]$latitud & denues_farm[[7]]$longitud %in% denues_farm[[6]]$longitud)]
cierrecovid_farm <- denues_farm[[6]][!(denues_farm[[6]]$latitud %in% denues_farm[[7]]$latitud 
                                  & denues_farm[[6]]$longitud %in% denues_farm[[7]]$longitud) & 
                                  (denues_farm[[6]]$latitud %in% denues_farm[[5]]$latitud 
                                   & denues_farm[[6]]$longitud %in% denues_farm[[5]]$longitud &
                                     denues_farm[[6]]$latitud %in% denues_farm[[4]]$latitud 
                                   & denues_farm[[6]]$longitud %in% denues_farm[[4]]$longitud &
                                     denues_farm[[6]]$latitud %in% denues_farm[[3]]$latitud 
                                   & denues_farm[[6]]$longitud %in% denues_farm[[3]]$longitud &
                                     denues_farm[[6]]$latitud %in% denues_farm[[2]]$latitud 
                                   & denues_farm[[6]]$longitud %in% denues_farm[[2]]$longitud)]

cierrecovid_nofarm <- denues_nofarm[[6]][!(denues_nofarm[[6]]$latitud %in% denues_nofarm[[7]]$latitud 
                                           & denues_nofarm[[6]]$longitud %in% denues_nofarm[[7]]$longitud) & 
                                           (denues_nofarm[[6]]$latitud %in% denues_nofarm[[5]]$latitud 
                                            & denues_nofarm[[6]]$longitud %in% denues_nofarm[[5]]$longitud &
                                              denues_nofarm[[6]]$latitud %in% denues_nofarm[[4]]$latitud 
                                            & denues_nofarm[[6]]$longitud %in% denues_nofarm[[4]]$longitud &
                                              denues_nofarm[[6]]$latitud %in% denues_nofarm[[3]]$latitud 
                                            & denues_nofarm[[6]]$longitud %in% denues_nofarm[[3]]$longitud &
                                              denues_nofarm[[6]]$latitud %in% denues_nofarm[[2]]$latitud 
                                            & denues_nofarm[[6]]$longitud %in% denues_nofarm[[2]]$longitud)]



#agregación por municipio
aggcierrefarm <- cierrecovid_farm %>%
  group_by(cve_ent, cve_mun) %>%
  summarise(farm_cerradas=n())%>%
  ungroup()
agg_cierrenofarm <- cierrecovid_nofarm %>%
  group_by(cve_ent, cve_mun) %>%
  summarise(est_cerrados=n())%>%
  ungroup()


#unimos bases de datos
cierres <- full_join(aggcierrefarm, agg_cierrenofarm, by = c("cve_ent", "cve_mun"))
cierres[is.na(cierres)] <- 0
cierres_filtrado <- cierres  %>%
  filter(!(farm_cerradas == 0 | est_cerrados==0 ))

#plot establecimientos vs farmacias cerradas
ggplot(cierres, aes(x=est_cerrados, y=farm_cerradas)) +
  coord_cartesian(ylim = c(0, NA)) +
  geom_point()   + labs(title = "farmacias cerradas vs establecimientos (excluyendo farmacias) cerrados") +
  stat_smooth(method = "lm",
              formula = y ~ x) 
#enigh
enigh_2020 = read_excel("C:/Users/luism/Documents/dataton/datos/icmm_2020/base_datos/ENIGH_ARCHIVO_MAESTRO_2020.xlsx")
enigh_2020 = rename(enigh_2020, mkey=LLAVE)
enigh_2020$mkey = sprintf("%05d",enigh_2022$ubica_geo)
enigh_2020 = enigh_2020 %>%
  select(mkey,Ingreso)
#cercanía a centros de distribución
farm_dist <- read.csv("C:/Users/luism/Documents/dataton/datos/Farmacias_filtradas50km.csv")
agg_farm_dist <- farm_dist  %>%
  group_by(cve_ent, cve_mun) %>%
  summarise(farmacias=n())%>%
  ungroup()
#intersección de los 3 métodos
intersec <- inner_join(cierres_filtrado, agg_farm_dist, by=c("cve_ent", "cve_mun"))
intersec$mkey <- paste(sprintf("%02d", intersec$cve_ent),sprintf("%03d", intersec$cve_mun),sep="")
enigh_2020$mkey <- sprintf("%05d", as.numeric(enigh_2020$mkey))
intersec <- inner_join(intersec,enigh_2020, by="mkey")
intersec_filt <-intersec %>%
  filter(Ingreso>=25000, Ingreso<=65000, farm_cerradas<est_cerrados)
intersec_filt = rename(intersec_filt, num_farmacias = farm_cerradas)
intersec_filt = intersec_filt %>%
  select(cve_ent, cve_mun, num_farmacias, mkey)

#generamos los archivos con la lista de farmacias
farm_final <- cierrecovid[cierrecovid$mkey %in% intersec_filt$mkey]
farm_final = farm_final %>%
  select(entidad,cve_ent, municipio, cve_mun, latitud, longitud)





