####Libraries ####

library(readxl)
library(ggplot2)
library(maps)
library(dplyr)
library(writexl)


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

Filtered_samples_safe <- Filtered_samples %>%
  select(Collectionsite.Lat, Collectionsite.Long)

Filtered_samples_safe <- Filtered_samples %>%
  select(-Collector.Firstname,
         -Collector.Lastname,
         -Collector.Email,
         -Collector.Phone)

#Remove outside US

Filtered_samples_safe <- Filtered_samples_safe %>%
  filter(
    Collectionsite.Lat >= 24,
    Collectionsite.Lat <= 50,
    Collectionsite.Long >= -125,
    Collectionsite.Long <= -66
  )

#Remove FL island site - BeeSpatial doesn't recognize it

Filtered_samples_safe <- Filtered_samples_safe %>%
  filter(!(Collectionsite.Lat == 30.21349 & Collectionsite.Long == -87.24103))

write.csv(Filtered_samples_safe, "Filtered_samples_safe.csv", row.names = FALSE)





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

### Cover Data ####
# ============================================================
# Bee Foraging Analysis Pipeline
# Response: Pollen diversity (richness + Shannon) per sample
# Predictors: Land cover (3km), elevation, latitude, date
# ============================================================

library(tidyverse)
library(vegan)       # Shannon diversity
library(elevatr)     # Pull elevation from coordinates
library(sf)          # Spatial operations
library(lubridate)   # Date handling

# ============================================================
# 1. LOAD DATA
# ============================================================

samples <- read.csv("Filtered_samples_safe.csv")
pollen_raw <- read.csv("2025_BK_Con.csv", check.names = FALSE)
cdl <- read.csv("cdlsummary.csv")

# ============================================================
# 2. RESHAPE POLLEN MATRIX
# ============================================================

# Rows = taxa, cols = samples — transpose so rows = samples
# First column is taxa names
taxa_names <- pollen_raw[[1]]
taxa_names <- make.unique(taxa_names)  # handle duplicate taxa names
pollen_mat <- pollen_raw[, -1]
rownames(pollen_mat) <- taxa_names

# Transpose: rows = samples, cols = taxa
pollen_t <- as.data.frame(t(pollen_mat))

# Extract Shortid from column names (e.g. "BC12_S136_L001" -> "BC12")
pollen_t$Shortid <- sub("_S[0-9]+_L[0-9]+$", "", rownames(pollen_t))

# ============================================================
# 3. CALCULATE DIVERSITY METRICS PER SAMPLE
# ============================================================

# Pollen counts only (exclude Shortid column)
count_cols <- pollen_t %>% select(-Shortid)

# Normalize to relative abundance (proportions) per sample
count_cols_norm <- count_cols / rowSums(count_cols)

diversity_df <- pollen_t %>%
  select(Shortid) %>%
  mutate(
    richness = specnumber(count_cols),                        # # of taxa with count > 0
    shannon  = diversity(count_cols_norm, index = "shannon"), # on normalized data
    evenness = shannon / log(richness)                        # Pielou's J (0-1)
  )

# ============================================================
# 4. JOIN WITH SAMPLE METADATA
# ============================================================

samples_clean <- samples %>%
  select(Shortid, Collectionsite.Lat, Collectionsite.Long,
         Collectionstartdate, Collectionsite.State) %>%
  rename(Lat = Collectionsite.Lat, Long = Collectionsite.Long) %>%
  mutate(
    date = as.Date(Collectionstartdate),
    doy  = yday(date),
    year = year(date)
  )

# Pull land cover from CDL file (already at 3km radius)
cdl_clean <- cdl %>%
  select(Shortid, Developed, Forest, Corn, Soybeans,
         PastureHay, Grain, Orchard, VegetablesSmallFruit,
         WaterWetlands, Barren, FallowIdleCropland) %>%
  mutate(
    pct_developed = Developed * 100,
    pct_forest    = Forest * 100,
    pct_ag        = (Corn + Soybeans + PastureHay + Grain +
                       Orchard + VegetablesSmallFruit +
                       FallowIdleCropland) * 100,
    pct_natural   = (WaterWetlands + Barren) * 100
  ) %>%
  select(Shortid, pct_developed, pct_forest, pct_ag, pct_natural)

analysis_df <- diversity_df %>%
  left_join(samples_clean, by = "Shortid") %>%
  left_join(cdl_clean, by = "Shortid") %>%
  filter(!is.na(Lat), !is.na(Long))

# ============================================================
# 5. PULL ELEVATION
# ============================================================

# Convert to sf object
coords_sf <- analysis_df %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

# Pull elevation (meters) using Open Topo Data via elevatr
elev_data <- get_elev_point(coords_sf, src = "epqs")  # USGS endpoint
analysis_df$elevation_m <- elev_data$elevation

# ============================================================
# 6. SAVE MERGED DATASET
# ============================================================
message("Merged dataset saved: ", nrow(analysis_df), " samples")

# ============================================================
# 8. MODELS
# ============================================================

# --- 8a. Species Richness ---
model_richness <- lm(
  richness ~ pct_developed + pct_ag + pct_forest + pct_natural +
    doy + elevation_m + Lat,
  data = analysis_df
)

summary(model_richness)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)    9.176177  13.878330   0.661   0.5100  
# pct_developed  0.105377   0.101234   1.041   0.3004  
# pct_ag         0.047107   0.092150   0.511   0.6103  
# pct_forest     0.081939   0.094171   0.870   0.3863  
# pct_natural   -0.022698   0.119519  -0.190   0.8498  
# doy           -0.004222   0.013941  -0.303   0.7626  
# elevation_m   -0.005082   0.002534  -2.006   0.0475 *
#   Lat           -0.092575   0.292727  -0.316   0.7525  
# ---


# --- 8b. Shannon Diversity ---
model_shannon <- lm(
  shannon ~ pct_developed + pct_ag + pct_forest + pct_natural +
    doy + elevation_m + Lat,
  data = analysis_df
)

summary(model_shannon)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)    1.2530335  1.0698210   1.171   0.2443  
# pct_developed -0.0059318  0.0077989  -0.761   0.4487  
# pct_ag        -0.0015836  0.0070983  -0.223   0.8239  
# pct_forest    -0.0027172  0.0072553  -0.375   0.7088  
# pct_natural   -0.0142494  0.0092069  -1.548   0.1248  
# doy           -0.0022587  0.0010738  -2.103   0.0379 *
#   elevation_m    0.0001305  0.0001955   0.667   0.5061  
# Lat            0.0319309  0.0225759   1.414   0.1603  
# ---

# --- 8c. Evenness ---
model_evenness <- lm(
  evenness ~ pct_developed + pct_ag + pct_forest + pct_natural +
    doy + elevation_m + Lat,
  data = analysis_df
)

summary(model_evenness)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)    7.951e-01  3.622e-01   2.195   0.0305 *
#   pct_developed -4.301e-03  2.638e-03  -1.631   0.1061  
# pct_ag        -1.365e-03  2.398e-03  -0.569   0.5705  
# pct_forest    -3.010e-03  2.452e-03  -1.228   0.2224  
# pct_natural   -4.654e-03  3.213e-03  -1.448   0.1507  
# doy           -6.948e-04  3.630e-04  -1.914   0.0585 .
# elevation_m    1.675e-04  6.604e-05   2.537   0.0127 *
#   Lat            9.582e-03  7.639e-03   1.254   0.2126  
# ---

# ============================================================
# 9. PLOTS
# ============================================================

# Correlation matrix of predictors
predictors <- analysis_df %>%
  select(pct_developed, pct_ag, pct_forest, pct_natural,
         doy, elevation_m, Lat)

pairs(predictors, main = "Predictor correlations")

# Richness vs each predictor
analysis_df %>%
  pivot_longer(cols = c(pct_developed, pct_ag, pct_forest, pct_natural,
                        doy, elevation_m, Lat),
               names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, y = richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title = "Pollen Richness vs Predictors",
       y = "Pollen Taxa Richness", x = "") +
  theme_bw()
ggsave("richness_predictors.png", width = 10, height = 7)

# Richness plots with and without outliers (richness > 60)
richness_long <- analysis_df %>%
  pivot_longer(cols = c(pct_developed, pct_ag, pct_forest, pct_natural,
                        doy, elevation_m, Lat),
               names_to = "predictor", values_to = "value")


p2 <- richness_long %>%
  filter(richness <= 60) %>%
  ggplot(aes(x = value, y = richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title = "Pollen Richness vs Predictors (richness ≤ 60)",
       y = "Pollen Taxa Richness", x = "") +
  theme_bw()

p2

ggsave("richness_no_outliers.png", plot = p2, width = 10, height = 7)


# Shannon vs each predictor
analysis_df %>%
  pivot_longer(cols = c(pct_developed, pct_ag, pct_forest, pct_natural,
                        doy, elevation_m, Lat),
               names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, y = shannon)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title = "Pollen Shannon Diversity vs Predictors",
       y = "Shannon Diversity", x = "") +
  theme_bw()

ggsave("shannon_predictors.png", width = 10, height = 7)

# Evenness vs each predictor
analysis_df %>%
  pivot_longer(cols = c(pct_developed, pct_ag, pct_forest, pct_natural,
                        doy, elevation_m, Lat),
               names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, y = evenness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "forestgreen") +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title = "Pollen Evenness vs Predictors",
       y = "Pielou's Evenness (J)", x = "") +
  theme_bw()

ggsave("evenness_predictors.png", width = 10, height = 7)

message("Analysis complete!")
