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


ggplot(Filtered_samples, aes(x = Collectionsite.Long, y = Collectionsite.Lat)) +
  borders("state") +
  geom_point(alpha = 0.7) +
  coord_quickmap() +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", title = "Sample Locations")



us_map <- map_data("state")

ggplot() +
  geom_polygon(data = us_map,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  geom_point(data = Filtered_samples,
             aes(x = Collectionsite.Long, y = Collectionsite.Lat),
             color = "red", size = 2, alpha = 0.7) +
  coord_quickmap(xlim = c(-125, -66), ylim = c(24, 50)) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude",
       title = "2025: Beekeeper Metabarcoding Sample Locations Across the United States")
