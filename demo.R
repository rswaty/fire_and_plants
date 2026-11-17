# Plant records before and after the Cameron Peak Fire.
# Shares of records, not raw counts. Native or introduced comes from iNaturalist.
# Open the RStudio project and run this script. Delete outputs/observations.rds to download again.

library(sf)
library(tidyverse)
library(rinat)
library(httr)
library(jsonlite)
library(ggrepel)

# Settings

years <- 2016:2025
fire_start <- as.Date("2020-08-13")
fire_end <- as.Date("2020-12-02")
ring_m <- 5000

dir.create("outputs", showWarnings = FALSE)

# Fire perimeter and a ring around it

# This perimeter is invalid with spherical geometry turned on.
sf_use_s2(FALSE)

fire <- st_read("inputs/cameron_peak_fire_2020.shp", quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326) |>
  summarise(geometry = st_union(geometry))

fire_utm <- st_transform(fire, 32613)
outer <- st_buffer(fire_utm, ring_m)
ring <- st_difference(outer, fire_utm) |>
  st_transform(4326)

bb <- st_bbox(st_transform(outer, 4326))
bounds <- c(
  bb["ymin"] - 0.01, bb["xmin"] - 0.01,
  bb["ymax"] + 0.01, bb["xmax"] + 0.01
)

# Download. The file is reused on the next run.

obs_file <- "outputs/observations.rds"

download_plants <- function(year, month = NULL) {
  get_inat_obs(
    taxon_name = "Plantae",
    quality = "research",
    year = year,
    month = month,
    bounds = bounds,
    maxresults = 10000
  )
}

if (file.exists(obs_file)) {
  obs <- readRDS(obs_file)
} else {
  message("Downloading research-grade plants. The first run takes a while.")
  obs <- map(years, function(yr) {
    message(yr)
    got <- download_plants(yr)
    # iNaturalist will not return more than 10,000 records per request.
    if (nrow(got) < 10000) return(got)
    message(yr, " exceeded 10,000 records. Downloading month by month.")
    map(1:12, function(mo) download_plants(yr, mo)) |> list_rbind()
  }) |>
    list_rbind() |>
    distinct(id, .keep_all = TRUE)
  saveRDS(obs, obs_file)
}

# Drop records we cannot place on the map

obs <- obs |>
  mutate(observed_on = as.Date(observed_on)) |>
  filter(
    !is.na(latitude), !is.na(longitude), !is.na(observed_on),
    !geoprivacy %in% c("obscured", "private"),
    !taxon_geoprivacy %in% c("obscured", "private"),
    !tolower(as.character(coordinates_obscured)) %in% "true",
    is.na(positional_accuracy) | positional_accuracy <= 1000
  ) |>
  mutate(
    # Subspecies and varieties roll up to the species name.
    species = if_else(
      str_count(scientific_name, "\\S+") >= 2,
      str_c(word(scientific_name, 1), " ", word(scientific_name, 2)),
      NA_character_
    )
  ) |>
  filter(!is.na(species))

pts <- st_as_sf(obs, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
inside <- lengths(st_intersects(pts, fire)) > 0
in_ring <- lengths(st_intersects(pts, ring)) > 0

obs <- pts |>
  st_drop_geometry() |>
  mutate(
    zone = case_when(
      inside ~ "Burn",
      in_ring ~ "Ring"
    ),
    period = case_when(
      observed_on < fire_start ~ "Before",
      observed_on > fire_end ~ "After",
      TRUE ~ "During"
    ),
    year = year(observed_on)
  ) |>
  filter(!is.na(zone), year %in% years)

message(
  "Using ", nrow(obs), " records: ",
  sum(obs$zone == "Burn"), " in the burn, ",
  sum(obs$zone == "Ring"), " in the ring."
)

# Native or introduced: Colorado checklist, then the United States checklist.
checklist <- function(taxon_ids, place_id) {
  taxon_ids <- unique(taxon_ids[!is.na(taxon_ids)])
  chunks <- split(taxon_ids, ceiling(seq_along(taxon_ids) / 80))
  map(chunks, function(chunk) {
    resp <- GET(
      "https://api.inaturalist.org/v1/taxa",
      query = list(id = paste(chunk, collapse = ","), place_id = place_id, per_page = 200),
      user_agent("fire-and-plants")
    )
    stop_for_status(resp)
    Sys.sleep(0.3)
    results <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)$results
    tibble(
      taxon_id = map_dbl(results, function(t) t$id),
      status = map_chr(results, function(t) {
        means <- t$establishment_means$establishment_means
        if (is.null(means)) NA_character_ else as.character(means)
      })
    )
  }) |> list_rbind()
}

taxon_for_species <- obs |>
  count(species, taxon_id, sort = TRUE) |>
  distinct(species, .keep_all = TRUE)

common_for_species <- obs |>
  filter(!is.na(common_name), common_name != "") |>
  count(species, common_name, sort = TRUE) |>
  distinct(species, .keep_all = TRUE) |>
  select(species, common_name)

origin <- taxon_for_species |>
  left_join(checklist(taxon_for_species$taxon_id, 34), by = "taxon_id") |>
  rename(colorado = status) |>
  left_join(checklist(taxon_for_species$taxon_id, 1), by = "taxon_id") |>
  rename(united_states = status) |>
  mutate(
    origin = case_when(
      colorado %in% c("native", "endemic") ~ "Native",
      colorado == "introduced" ~ "Introduced",
      united_states %in% c("native", "endemic") ~ "Native",
      united_states == "introduced" ~ "Introduced",
      TRUE ~ "Not listed"
    )
  ) |>
  left_join(common_for_species, by = "species") |>
  select(species, common_name, origin)

# Share of records before the fire and after it

cmp <- obs |> filter(period %in% c("Before", "After"))
totals <- cmp |> count(zone, period, name = "total")

comparison <- cmp |>
  count(species, zone, period, name = "n") |>
  complete(species, zone = c("Burn", "Ring"), period = c("Before", "After"), fill = list(n = 0)) |>
  left_join(totals, by = c("zone", "period")) |>
  mutate(share = n / total) |>
  select(species, zone, period, n, share) |>
  pivot_wider(names_from = c(zone, period), values_from = c(n, share), values_fill = 0) |>
  left_join(origin, by = "species") |>
  mutate(
    common_name = replace_na(common_name, ""),
    origin = replace_na(origin, "Not listed"),
    n_burn = n_Burn_Before + n_Burn_After,
    n_ring = n_Ring_Before + n_Ring_After,
    change_burn = 100 * (share_Burn_After - share_Burn_Before),
    change_ring = 100 * (share_Ring_After - share_Ring_Before),
    excess = change_burn - change_ring,
    response = case_when(
      n_burn < 8 ~ "Too few in the burn",
      n_ring < 5 ~ "Too few in the ring",
      excess >= 0.5 & change_burn >= 0.5 ~ "Rose more in the burn",
      excess <= -0.5 & change_ring > 0 ~ "Rose more in the ring",
      TRUE ~ "Similar"
    )
  ) |>
  arrange(desc(excess))

write_csv(
  comparison |> select(
    species, common_name, origin,
    n_Burn_Before, n_Burn_After, n_Ring_Before, n_Ring_After,
    change_burn, change_ring, excess, response
  ),
  "outputs/species_comparison.csv"
)

# Plots

col_burn <- "#e07a3d"
col_ring <- "#2f5d7c"
col_paper <- "#f7f4ee"
zone_colors <- c(Burn = col_burn, Ring = col_ring)
origin_colors <- c(Native = "#1f7a4d", Introduced = "#b4233c", "Not listed" = "#b3aa9f")
zone_labels <- c(Burn = "Inside the burn", Ring = "5 km ring")
source_note <- "iNaturalist research-grade plants, 2016-2025. Cameron Peak perimeter.\nObscured or coarse coordinates are left out. A share of records is not the number of plants."

theme_fire <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = col_paper, color = NA),
      panel.background = element_rect(fill = col_paper, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e5ddd2"),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "#6d645c"),
      plot.caption = element_text(color = "#6d645c", size = 8, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      legend.position = "top", strip.text = element_text(face = "bold", hjust = 0)
    )
}

save_plot <- function(plot, filename, width, height) {
  ggsave(file.path("outputs", filename), plot, width = width, height = height, dpi = 160, bg = col_paper)
}

# One row per year and place: effort, and richness in a fixed-size sample.
set.seed(2020)
rarefy <- function(species, size) {
  if (length(species) < size) return(NA_real_)
  mean(replicate(30, length(unique(sample(species, size)))))
}

by_year <- obs |>
  group_by(year, zone) |>
  summarise(
    observations = n(), observers = n_distinct(user_login),
    recorded = n_distinct(species), spp = list(species),
    .groups = "drop"
  )
sample_n <- min(by_year$observations[by_year$observations >= 50])
by_year$rarefied <- map_dbl(by_year$spp, function(s) rarefy(s, sample_n))
by_year$spp <- NULL

effort <- by_year |>
  pivot_longer(c(observations, observers), names_to = "what", values_to = "value") |>
  mutate(what = recode(what, observations = "Observations", observers = "Observers"))

ggplot(effort, aes(year, value, fill = zone)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = col_burn) +
  facet_wrap(~what, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = zone_colors, labels = zone_labels, name = NULL) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "More plants were recorded after the fire, in both places",
       subtitle = "The dashed line is 2020. The rise on both sides is why raw counts are a poor measure.",
       x = NULL, y = NULL, caption = source_note) +
  theme_fire()
save_plot(last_plot(), "01_effort.png", 10, 7.2)

rich_long <- by_year |>
  select(year, zone, recorded, rarefied) |>
  pivot_longer(c(recorded, rarefied), names_to = "what", values_to = "species_n") |>
  mutate(what = recode(what, recorded = "Species recorded", rarefied = paste("Species in a sample of", sample_n)))

rich_floor <- tibble(year = min(years), zone = "Burn", what = paste("Species in a sample of", sample_n), species_n = 0)

ggplot(rich_long, aes(year, species_n, color = zone)) +
  geom_blank(data = rich_floor) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = col_burn) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 2.4, na.rm = TRUE) +
  facet_wrap(~what, ncol = 1, scales = "free_y") +
  scale_color_manual(values = zone_colors, labels = zone_labels, name = NULL) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "The species list got longer because the sample got bigger",
       subtitle = "Bottom: same number of records every year. A level line means the longer list is mostly more sampling.",
       x = NULL, y = "Species", caption = source_note) +
  theme_fire()
save_plot(last_plot(), "02_richness.png", 10, 7.2)

focus <- comparison |>
  filter(n_burn >= 8) |>
  mutate(label = paste(if_else(common_name == "", species, common_name), tolower(origin), sep = "   "))

focus <- bind_rows(
  focus |> slice_max(change_burn, n = 8, with_ties = FALSE),
  focus |> filter(change_burn < 0) |> slice_min(change_burn, n = 5, with_ties = FALSE)
) |>
  mutate(label = fct_reorder(label, change_burn))

with_ring <- focus |> filter(n_ring >= 5)

ggplot(focus) +
  geom_vline(xintercept = 0, color = "grey40") +
  geom_segment(data = with_ring, aes(x = change_ring, xend = change_burn, y = label, yend = label),
               color = "#d5cdc2", linewidth = 1.1) +
  geom_point(data = with_ring, aes(change_ring, label, color = "Ring"), size = 3.2) +
  geom_point(aes(change_burn, label, color = "Burn"), size = 3.4) +
  scale_color_manual(values = zone_colors, labels = zone_labels, name = NULL) +
  labs(title = "The biggest shifts in the burn, set next to the ring",
       subtitle = "Amber is the burn. Blue is the 5 km ring. Amber to the right of blue means the burn moved more.",
       x = "Change in share of records (percentage points)", y = NULL, caption = source_note) +
  theme_fire() +
  theme(panel.grid.major.y = element_blank())
save_plot(last_plot(), "03_share_shift.png", 10, 7.8)

scatter <- comparison |>
  filter(n_burn >= 8, n_ring >= 5) |>
  mutate(label = if_else(common_name == "", species, str_trunc(common_name, 28)))

labeled <- bind_rows(
  scatter |> slice_max(excess, n = 5, with_ties = FALSE),
  scatter |> slice_min(excess, n = 1, with_ties = FALSE)
)
lim <- max(abs(c(scatter$change_burn, scatter$change_ring)), 0.5) * 1.2

ggplot(scatter, aes(change_ring, change_burn)) +
  geom_hline(yintercept = 0, color = "#e5ddd2") +
  geom_vline(xintercept = 0, color = "#e5ddd2") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(color = origin, size = n_burn + n_ring), alpha = 0.9) +
  geom_text_repel(
    data = labeled, aes(label = label, color = origin),
    size = 3.2, fontface = "bold", bg.color = col_paper, bg.r = 0.12,
    show.legend = FALSE, seed = 1, max.overlaps = 20
  ) +
  annotate("text", x = -lim * 0.55, y = lim * 0.75, label = "Rose more\nin the burn",
           color = "#6d645c", fontface = "italic", size = 3.3, lineheight = 0.95) +
  annotate("text", x = lim * 0.55, y = -lim * 0.75, label = "Rose more\nin the ring",
           color = "#6d645c", fontface = "italic", size = 3.3, lineheight = 0.95) +
  scale_color_manual(values = origin_colors, name = "Checklist status") +
  scale_size_continuous(range = c(1.8, 6), name = "Records") +
  coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
  labs(title = "Did the burn change more than the land around it?",
       subtitle = "Each point is a species. Above the dashed line, its share rose more in the burn than in the ring.",
       x = "Change in the 5 km ring (percentage points)",
       y = "Change inside the burn (percentage points)", caption = source_note) +
  theme_fire() +
  theme(panel.grid = element_blank())
save_plot(last_plot(), "04_burn_vs_ring.png", 9.2, 8.4)

mix <- cmp |>
  left_join(origin |> select(species, origin), by = "species") |>
  mutate(
    origin = replace_na(origin, "Not listed"),
    origin = factor(origin, c("Native", "Not listed", "Introduced")),
    period = factor(period, c("Before", "After")),
    zone = factor(zone, c("Burn", "Ring"))
  ) |>
  count(zone, period, origin) |>
  group_by(zone, period) |>
  mutate(share = n / sum(n)) |>
  ungroup()

intro_pct <- function(which_zone, which_period) {
  val <- mix$share[mix$zone == which_zone & mix$period == which_period & mix$origin == "Introduced"]
  if (length(val) == 0) 0 else round(100 * val, 1)
}

ggplot(mix, aes(period, share, fill = origin)) +
  geom_col(width = 0.65, color = col_paper) +
  geom_text(
    aes(label = if_else(share >= 0.06, paste0(round(100 * share, 1), "%"), "")),
    position = position_stack(vjust = 0.5), color = "white", size = 3.2, fontface = "bold"
  ) +
  facet_wrap(~zone, labeller = as_labeller(zone_labels)) +
  scale_fill_manual(values = origin_colors, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%"), expand = expansion(mult = c(0, 0.04))) +
  labs(title = "Introduced plants, as a share of the records",
       subtitle = paste0(
         "Inside the burn, introduced plants were ", intro_pct("Burn", "Before"),
         "% of records before the fire and ", intro_pct("Burn", "After"),
         "% after. In the ring, ", intro_pct("Ring", "Before"), "% and ", intro_pct("Ring", "After"), "%."
       ),
       x = NULL, y = "Share of plant records",
       caption = paste("Status is the Colorado checklist, or the US checklist if Colorado has none.", source_note, sep = "\n")) +
  theme_fire() +
  theme(panel.grid.major.x = element_blank())
save_plot(last_plot(), "05_introduced_share.png", 10, 6.4)

print(filter(comparison, response == "Rose more in the burn") |>
        select(common_name, species, origin, change_burn, change_ring), n = 8)
