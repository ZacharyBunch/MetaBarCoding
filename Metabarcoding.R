library(readxl)

Sample_data <- read.csv("samples.csv", skip = 1)

BK_Con <- read.csv("2025_BK_Con.csv")

BK_Con_first4 <- substr(colnames(BK_Con)[-1], 1, 4)




Filtered_samples <- Sample_data[Sample_data$Shortid %in% BK_Con_first4, ]
Filtered_samples <- Filtered_samples[
  !is.na(Filtered_samples$Collectionsite.Lat) & 
    !is.na(Filtered_samples$Collectionsite.Long), 
]


write.csv(Filtered_samples, "Filtered_samples.csv")
