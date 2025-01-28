# Set working directory
setwd("ca/mi/nho")

# Filter criteria
year <- 0000
motivos <- c("texto1", "texto2")

# Read the SAP file
sap_data <- read.csv2("arquivo1.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
head(sap_data)

# Filter SAP data
motivos_incluso <- subset(sap_data, Motivo %in% motivos)
motivos_excluso <- subset(sap_data, !Motivo %in% motivos)

# Read the view file
vw_data <- read.csv2("aqruivo2.csv", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
head(vw_data)

# Convert date column in view data
vw_data$V8 <- as.Date(vw_data$V8, format = "%Y-%m-%d")

# Filter view data
second_view <- subset(vw_data, format(V8, "%Y") == year & V6 %in% motivos)  # view com motivos
third_view <- subset(vw_data, format(V8, "%Y") == year & !V6 %in% motivos) # view sem motivos

create_key <- function(df, columns = colnames(df)) {
  apply(df[,columns, drop = FALSE], 1, function(row) paste(row, collapse = "|"))
}

# Create unique aggregated key for sap data using the row
motivos_incluso$key <- create_key(motivos_incluso)
motivos_excluso$key <- create_key(motivos_excluso)

# Create unique aggregated key for views using specified columns
second_view$key <- create_key(second_view, columns = c("coluna","coluna"))
third_view$key <- create_key(third_view, columns = c("coluna","coluna"))

# Compare SAP com motivos vs VW com motivos
missing_data_incluso <- setdiff(motivos_incluso$V1, second_view$V1)
if (length(missing_data_incluso) > 0) {
  write.csv(data.frame(Missing_Data = missing_data_incluso), "missing_data_motivos_incluso.csv", row.names = FALSE)
  print("Missing data found in SAP motivos inclusos compared to second View. Saved in 'missing_data_motivos_incluso.csv'.")
} else {
  print("No missing data in SAP motivos inclusos compared to second View. All data is present.")
}

# Compare SAP sem motivos vs VW sem motivos
missing_data_excluso <- setdiff(motivos_excluso$V1, third_view$V1)
if (length(missing_data_excluso) > 0) {
  write.csv(data.frame(Missing_Data = missing_data_excluso), "missing_data_motivos_excluso.csv", row.names = FALSE)
  print("Missing data found in SAP motivos exclusos compared to third view. Saved in 'missing_data_motivos_excluso.csv'.")
} else {
  print("No missing data in SAP motivos exclusos compared to third view. All data is present.")
}

