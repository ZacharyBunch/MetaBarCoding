####Libraries ####

library(readxl)
library(ggplot2)
library(maps)


#### File Read ####

Sample_data <- read.csv("samples.csv", skip = 1)

BK_Con <- read.csv("2025_BK_Con.csv")

BK_Con_first4 <- substr(colnames(BK_Con)[-1], 1, 4)

#### Filtering ####


Filtered_samples <- Sample_data[Sample_data$Shortid %in% BK_Con_first4, ]
Filtered_samples <- Filtered_samples[
  !is.na(Filtered_samples$Collectionsite.Lat) & 
    !is.na(Filtered_samples$Collectionsite.Long), 
]


#### Fix incorrect / updated coordinates ####

# BD25 – Pittsburgh, PA (correct longitude sign)
Filtered_samples$Collectionsite.Lat[Filtered_samples$Shortid == "BD25"]  <- 39.43587
Filtered_samples$Collectionsite.Long[Filtered_samples$Shortid == "BD25"] <- -104.53398

# BE02 – Haiti (correct longitude sign)
Filtered_samples$Collectionsite.Lat[Filtered_samples$Shortid == "BE02"]  <- 19.6616
Filtered_samples$Collectionsite.Long[Filtered_samples$Shortid == "BE02"] <- -71.8364

# BE85 – Rockingham, VA (corrected coordinates)
Filtered_samples$Collectionsite.Lat[Filtered_samples$Shortid == "BE85"]  <- 38.519349
Filtered_samples$Collectionsite.Long[Filtered_samples$Shortid == "BE85"] <- -78.8130454

Filtered_samples$Collectionsite.Lat[Filtered_samples$Shortid == "BE25"]  <- 40.86225028545012
Filtered_samples$Collectionsite.Long[Filtered_samples$Shortid == "BE25"] <- -72.51255178549893


write.csv(Filtered_samples, "Filtered_samples.csv")


### Mapping ####

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# World boundaries as sf
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert your sample data to sf
samples_sf <- st_as_sf(
  Filtered_samples,
  coords = c("Collectionsite.Long", "Collectionsite.Lat"),
  crs = 4326
)

ggplot() +
  geom_sf(data = world,
          fill = NA,              # <-- outline only
          color = "black",
          linewidth = 0.3) +
  geom_sf(data = samples_sf,
          color = "black",
          size = 2) +
  coord_sf(xlim = c(-130, -60),
           ylim = c(10, 55)) +    # includes Haiti
  labs(
    title = "Sample Locations",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
