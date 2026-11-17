
# Randy Swaty and Copilot code to explore utility of inaturalist data 


library(sf)
library(tidyverse)
library(rinat)
library(taxize)

# --------------------------------------------------
# SETTINGS
# --------------------------------------------------

fire_year <- 2020

sf_use_s2(FALSE)

# Cameron Peak perimeter
fire <- st_read("inputs/cameron_peak_fire_2020.shp") |>
  st_make_valid() |>
  st_transform(4326) |>
  summarise(geometry = st_union(geometry))

# --------------------------------------------------
# DOWNLOAD INATURALIST PLANTS
# --------------------------------------------------

bb <- st_bbox(fire)

bounds <- c(
  bb["ymin"],
  bb["xmin"],
  bb["ymax"],
  bb["xmax"]
)

download_year <- function(yr){
  
  message("Downloading ", yr)
  
  get_inat_obs(
    taxon_name = "Plantae",
    quality = "research",
    year = yr,
    bounds = bounds,
    maxresults = 10000
  ) |>
    mutate(year = yr)
  
}

obs <- bind_rows(
  
  purrr::map_dfr(
    2016:2025,
    download_year
  )
  
)

# --------------------------------------------------
# CLIP TO FIRE PERIMETER
# --------------------------------------------------

obs_sf <- st_as_sf(
  obs,
  coords = c("longitude","latitude"),
  crs = 4326
)

inside <- lengths(
  st_intersects(obs_sf, fire)
) > 0

obs_fire <- obs_sf[inside, ]

# --------------------------------------------------
# BEFORE / AFTER
# --------------------------------------------------

obs_fire <- obs_fire |>
  mutate(
    period = ifelse(
      year <= fire_year - 1,
      "Before",
      "After"
    )
  )

# --------------------------------------------------
# RICHNESS BY YEAR
# --------------------------------------------------

richness_year <- obs_fire |>
  st_drop_geometry() |>
  group_by(year) |>
  summarise(
    richness = n_distinct(scientific_name),
    observations = n()
  )

print(richness_year)

# --------------------------------------------------
# SPECIES COMPARISON
# --------------------------------------------------

before_species <- obs_fire |>
  st_drop_geometry() |>
  filter(period == "Before") |>
  count(scientific_name)

after_species <- obs_fire |>
  st_drop_geometry() |>
  filter(period == "After") |>
  count(scientific_name)

comparison <- full_join(
  before_species,
  after_species,
  by = "scientific_name",
  suffix = c("_before","_after")
) |>
  replace_na(
    list(
      n_before = 0,
      n_after = 0
    )
  ) |>
  mutate(
    change = n_after - n_before
  ) |>
  arrange(desc(change))

# --------------------------------------------------
# COMMON NAMES
# --------------------------------------------------

common_names <- obs_fire |>
  st_drop_geometry() |>
  select(
    scientific_name,
    common_name
  ) |>
  distinct()

comparison <- comparison |>
  left_join(
    common_names,
    by = "scientific_name"
  )

# # --------------------------------------------------
# # FAMILY VIA TAXIZE
# # --------------------------------------------------
# 
# top_species <- comparison |>
#   slice_head(n = 100)
# 
# fam_list <- classification(
#   top_species$scientific_name,
#   db = "itis"
# )
# 
# family_tbl <- purrr::map_dfr(
#   seq_along(fam_list),
#   function(i){
#     
#     x <- fam_list[[i]]
#     
#     if(
#       inherits(x,"try-error") ||
#       is.null(x) ||
#       nrow(x)==0
#     ){
#       return(
#         tibble(
#           scientific_name =
#             top_species$scientific_name[i],
#           family = NA
#         )
#       )
#     }
#     
#     fam <- x |>
#       filter(rankName=="Family")
#     
#     tibble(
#       scientific_name =
#         top_species$scientific_name[i],
#       
#       family =
#         ifelse(
#           nrow(fam)==0,
#           NA,
#           fam$taxonName[1]
#         )
#     )
#     
#   }
# )
# 
# comparison <- comparison |>
#   left_join(
#     family_tbl,
#     by="scientific_name"
#   )

# --------------------------------------------------
# QUICK EXOTIC LIST
# --------------------------------------------------

exotics <- c(
  
  "Cirsium arvense",
  "Bromus tectorum",
  "Centaurea stoebe",
  "Carduus nutans",
  "Cirsium vulgare",
  "Linaria vulgaris",
  "Linaria dalmatica"
  
)

# --------------------------------------------------
# QUICK FIRE FOLLOWERS
# --------------------------------------------------

fire_followers <- c(
  
  "Chamaenerion angustifolium",
  "Phacelia sericea",
  "Populus tremuloides",
  "Ceanothus velutinus",
  "Rubus idaeus",
  "Rubus parviflorus",
  "Pulsatilla nuttalliana"
  
)

comparison <- comparison |>
  mutate(
    
    group = case_when(
      
      scientific_name %in% exotics ~
        "Exotic",
      
      scientific_name %in% fire_followers ~
        "Fire Follower",
      
      TRUE ~
        "Other"
      
    )
    
  )

# --------------------------------------------------
# SUMMARY
# --------------------------------------------------

summary_tbl <- comparison |>
  group_by(group) |>
  summarise(
    total_change = sum(change),
    species = n()
  )

print(summary_tbl)

# --------------------------------------------------
# TOP 50 INCREASERS
# --------------------------------------------------

top50 <- comparison |>
  arrange(desc(change)) |>
  slice_head(n=50)

print(top50)

write_csv(
  comparison,
  "species_comparison.csv"
)

# --------------------------------------------------
# PLOTS
# --------------------------------------------------

ggplot(
  richness_year,
  aes(year, richness)
) +
  geom_line() +
  geom_point(size=3) +
  geom_vline(
    xintercept = fire_year,
    colour="red"
  ) +
  theme_minimal()

top50 |>
  ggplot(
    aes(
      reorder(scientific_name,
              change),
      change,
      fill = group
    )
  ) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title =
      "Top species increases after Cameron Peak Fire",
    x="",
    y="Observation change"
  )

