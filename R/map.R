genpath<-'C:/Users/jenni/Box/NAMC WATS Department Files/GIS/Watersheds'
pred_geometry_base_path="C:/Users/jenni/Box/NAMC WATS Department Files/"
SQLite_file_path="C:/NAMC_S3/LegacyDatabases/instar.sqlite"
watershed_file_path=paste0(pred_geometry_base_path,"GIS/Watersheds/Mastersheds/mastersheds.shp")
predictor_list_path<-'C:/Users/jenni/Box/NAMC WATS Department Files/GIS/General_Layers/'
geometry_input_path="C:/Users/jenni/Box/NAMC WATS Department Files/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp"
geometry_input_name="NHD_West_str_ord"


conn <- DBI::dbConnect(RSQLite::SQLite(), SQLite_file_path)
  media = DBI::dbGetQuery(conn,sprintf("select samples.sample_id,visit_id,sample_date,
sites.site_id,samples.customer_site_code,sites.latitude,sites.longitude,sites.site_name,sites.waterbody_name,
ecosystem_name,habitat_name,
sample_method_name,
mesh,
effort_type_name,design_type_name,
area,sample_standardization_time as time, lab_split,
taxonomy.scientific_name, parent_id,level_name,life_stage_name, split_count, big_rare_count
from samples
left join sites on sites.site_id=samples.site_id
left join organisms on samples.sample_id=organisms.sample_id
left join  taxonomy on organisms.taxonomy_id=taxonomy.taxonomy_id
left join  effort_types on samples.effort_type_id=effort_types.effort_type_id
left join  design_types on samples.design_type_id=design_types.design_type_id
left join  habitats on samples.habitat_id=habitats.habitat_id
left join  ecosystems on samples.ecosystem_id=ecosystems.ecosystem_id
left join  sample_methods on samples.method_id=sample_methods.sample_method_id
left join  taxa_levels on taxonomy.level_id=taxa_levels.level_id
left join life_stages on organisms.life_stage_id=life_stages.life_stage_id
where samples.sample_id not in (select project_samples.sample_id from project_samples where project_id=354) and sites.latitude is not null"
                                       ))



#organisms
  conn <- DBI::dbConnect(RSQLite::SQLite(), SQLite_file_path)
  media = DBI::dbGetQuery(conn,sprintf("select organisms.sample_id,
taxonomy.scientific_name, parent_id,level_name,life_stage_name, split_count, big_rare_count
from organisms
left join  taxonomy on organisms.taxonomy_id=taxonomy.taxonomy_id
left join  taxa_levels on taxonomy.level_id=taxa_levels.level_id
left join life_stages on organisms.life_stage_id=life_stages.life_stage_id
where organisms.sample_id not in (select project_samples.sample_id from project_samples where project_id=354)"
  ))

  #samples
  conn <- DBI::dbConnect(RSQLite::SQLite(), SQLite_file_path)
  media = DBI::dbGetQuery(conn,sprintf("select samples.sample_id,visit_id,sample_date,
sites.site_id,samples.customer_site_code,sites.latitude,sites.longitude,sites.site_name,sites.waterbody_name,
ecosystem_name,habitat_name,
sample_method_name,
mesh,
effort_type_name,design_type_name,
area,sample_standardization_time as time, lab_split
from samples
left join sites on sites.site_id=samples.site_id
left join  effort_types on samples.effort_type_id=effort_types.effort_type_id
left join  design_types on samples.design_type_id=design_types.design_type_id
left join  habitats on samples.habitat_id=habitats.habitat_id
left join  ecosystems on samples.ecosystem_id=ecosystems.ecosystem_id
left join  sample_methods on samples.method_id=sample_methods.sample_method_id
where samples.sample_id not in (select project_samples.sample_id from project_samples where project_id=354) and sites.latitude is not null"
  ))



library(sf)
  points=sf::st_as_sf(media,coords=c('longitude','latitude'),crs=4269)
  write_sf(points,"C:/Users/jenni/Box/NAMC (Trip Armstrong)/Database/map.shp")
