# Fire and plants

A demo of using iNaturalist to ask what happened to plant records after the Cameron Peak Fire (2020, northern Colorado). This is one fire. The same question, for fires across the country, needs a different data path.

Raw observation counts are a poor answer. iNaturalist grew quickly over these years, so almost every species looks like it increased. `demo.R` compares each species' **share of records** inside the burn with the same share in a **5 km ring** of surrounding land.

## Run

Open `fire_and_plants.Rproj` and run `demo.R` from the project folder.

Packages: `sf`, `tidyverse`, `rinat`, `httr`, `jsonlite`, `ggrepel`.

The first run downloads research-grade Plantae observations for 2016–2025 and can take a while. It saves them as `outputs/observations.rds` and reuses that file. Delete it to download again.

Settings at the top of `demo.R`:

- Years: 2016–2025
- Fire window: 13 August 2020 through 2 December 2020. Those dates are left out of the before/after comparison. Earlier 2020 dates count as before.
- Ring: 5 km, measured in UTM zone 13N

Records are dropped if the coordinates are obscured or coarser than 1 km. Trees and shrubs are included. The download is all of Plantae.

## What to look at

Everything the script writes is in `outputs/`.

1. `01_effort.png` shows records and observers by year, in the burn and in the ring. Both climb after 2020.
2. `02_richness.png` shows species recorded per year, and the same count drawn from a fixed sample size. The top panel follows effort. The bottom panel holds the sample size still.
3. `03_share_shift.png` shows the largest share changes inside the burn. Amber is the burn, blue is the ring. Amber to the right of blue means that species took a larger share of records in the burn than it did nearby.
4. `04_burn_vs_ring.png` shows every species with enough records in both places. Above the dashed line, its share rose more in the burn than in the ring.
5. `05_introduced_share.png` shows native, introduced, and unlisted plants as a percent of records, before and after.

`species_comparison.csv` has one row per species: counts, share change in percentage points, checklist status, and a short response label.

A species **rose more in the burn** only when its share gained at least 0.5 percentage points inside the perimeter, and that gain was at least 0.5 points larger than the change in the ring. There is no list of fire followers.

## Where native and introduced come from

For each species, the script asks iNaturalist for establishment means:

1. The Colorado checklist (place 34).
2. If Colorado has no listing, the United States checklist (place 1).

Endemic is grouped with native. Species on neither checklist are "Not listed." Colorado wins when both exist, so a plant introduced in the state is not relabeled native just because it is native somewhere in the country.

## Data

- Observations: [iNaturalist](https://www.inaturalist.org) research-grade Plantae, through the `rinat` package.
- Perimeter: `inputs/cameron_peak_fire_2020.shp`, the NIFC Cameron Peak perimeter shipped with this project (about 208,912 acres).

## Read this with these limits

- A share of records is not the number of plants. People photograph some species more than others, and a fire can change what they go looking for.
- The ring is nearby land, not a matched control. Other disturbances can sit inside it.
- A blank checklist listing means "not listed," not that the species is native or introduced.
- Obscured coordinates are removed, so some rare plants never enter the comparison.

## Other sources that can add to iNaturalist

These are not used in the demo. Each one can add plant records, or a label on those records, that iNaturalist itself is thin on. Drop records that are already in iNaturalist before combining them, or the same photo gets counted twice.

- **EDDMapS** — https://www.eddmaps.org/ — Reports of invasive plants from people and agencies, with coordinates. Use it to fill weeds that photographers skip, such as cheatgrass and thistles. It will not fill in the native flora.
- **iMapInvasives** — https://www.imapinvasives.org/ — State invasive-species records, including some that never reach iNaturalist. Use it for the same weed gap as EDDMapS. Access and bulk download depend on the state.
- **Pl@ntNet** — https://plantnet.org/ — The other large photo-identification project. Use it for extra plant photos with coordinates. Coverage in the Colorado mountains is thin compared with iNaturalist.
- **SEINet** — https://swbiodiversity.org/seinet/ — Herbarium specimens from the southern Rockies and the Southwest. Use it for species recorded in the decades before iNaturalist, with a place and a date on a specimen rather than a photo.
- **iDigBio** — https://www.idigbio.org/portal — Digitized specimens from collections across the country. Use it the same way as SEINet when the question moves beyond the Southwest. Specimen coordinates are often the place name, not a GPS point.
- **GBIF** — https://www.gbif.org/ — One search across iNaturalist, Pl@ntNet, and herbaria. Use it to pull the non-iNaturalist records in one download. Filter out the iNaturalist publisher or the same observations are in both files.
- **Nature's Notebook** — https://www.usanpn.org/natures_notebook — Repeat observations of marked plants, mostly phenology. Use it where a species was visited on many dates, which iNaturalist rarely does. It is a short list of species, not a flora.
- **USDA PLANTS** — https://plants.usda.gov/ — State-level native and introduced status, not more sightings. Use it to label iNaturalist names when the iNaturalist checklist has no entry for that state.
