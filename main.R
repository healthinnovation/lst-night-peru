rm(list = ls())
if (!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman')
pacman::p_load(
  pak,
  tidyverse,
  sf
)

# pak::pkg_install("harmonize-tools/land4health")
# pak::pkg_install('ambarja/geoidep')

library(geoidep)
library(land4health)
sf_use_s2(TRUE) 

ee_Initialize(user = 'geografo.pe@gmail.com')

distritos <- get_districts()
chunk_size <- 500
chunks <- split(1:nrow(distritos), ceiling(seq_len(nrow(distritos)) / chunk_size))

# 1. Definición de rangos temporales
periodos <- tibble::tribble(
  ~periodo_nombre, ~from,        ~to,
  "2000_2010",     "2000-01-01", "2010-12-31",
  "2011_2020",     "2011-01-01", "2020-12-31",
  "2021_2025",     "2021-01-01", "2025-12-31"
)

fun_LST <- function(indices, start_date, end_date){
  enviro <- l4h_surface_temp(
    from = start_date,
    to = end_date,
    band = 'night',
    stat = 'mean',
    region = distritos[indices,],
    level = 'moderate',
    by = 'month')
  
  enviro_df <- as_tibble(enviro)
  
  print(paste0("Carga: ", max(indices), " de ", nrow(distritos), " [", start_date, " a ", end_date, "]"))
  return(enviro_df)
}

environmental_db <- purrr::map_df(1:nrow(periodos), function(p) {
  
  p_info <- periodos[p, ]
  
  list_lst_db <- lapply(
    X = chunks,
    FUN = function(indices) fun_LST(indices, p_info$from, p_info$to)
  )
  
  df_periodo <- list_lst_db %>% 
    bind_rows() %>% 
    st_as_sf() %>% 
    st_drop_geometry() %>% 
    mutate(periodo = p_info$periodo_nombre)
  
  return(df_periodo)
})


lst_night <- environmental_db %>% 
  filter(date >= '2001-01-01') %>% 
  select(-periodo)

if(!dir.exists(paths = 'output')) dir.create(path = 'output',recursive = TRUE)

write_csv(
  x = lst_night,
  file = 'output/db_LST_night_by_districts.csv'
  )
