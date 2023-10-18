# kurtis bertauche
# script to make flourish friendly data
# 17 oct 2023

MFCFileNames <- list.files(path = "./data/fleetReports/", pattern = "MFC")
ACFileNames <- list.files(path = "./data/fleetReports/", pattern = "AC")
filepath <- "../data/fleetReports/"

mfc <- data.frame(MFCFileNames)
mfc <- mfc[order(as.Date(substring(mfc$MFCFileNames, 1, 11), format = "%b-%d-%Y")),]
mfc <- data.frame(mfc)

ac <- data.frame(ACFileNames)
ac <- ac[order(as.Date(substring(ac$ACFileNames, 1, 11), format = "%b-%d-%Y")),]
ac <- data.frame(ac)

ac_fleet <- data.frame(Airline = character())
for(df in ac$ac)
{
  new_df <- read.csv(file = paste(filepath, df, sep = ""))
  new_df <- new_df[1:2]
  date_string <- substring(df, 1, 11)
  pretty <- format(as.Date(date_string, "%b-%d-%Y"), "%B %d, %Y")
  print(pretty)
  colnames(new_df) <- c("Airline", pretty)
  ac_fleet <- merge(ac_fleet, new_df, by = "Airline", all = TRUE)
}

mfc_fleet <- data.frame(Airline = character())
for(df in mfc$mfc)
{
  new_df <- read.csv(file = paste(filepath, df, sep = ""))
  new_df <- new_df[1:2]
  date_string <- substring(df, 1, 11)
  pretty <- format(as.Date(date_string, "%b-%d-%Y"), "%B %d, %Y")
  print(pretty)
  colnames(new_df) <- c("Airline", pretty)
  mfc_fleet <- merge(mfc_fleet, new_df, by = "Airline", all = TRUE)
}

ac_fleet[is.na(ac_fleet)] <- 0
mfc_fleet[is.na(mfc_fleet)] <- 0

write.table(ac_fleet,
            file = "./data/flourish/AC_Largest_Fleets_Flourish_Data.tsv",
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)

write.table(mfc_fleet,
            file = "./data/flourish/MFC_Largest_Fleets_Flourish_Data.tsv",
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)

################################## NOW WE DO THE PLANE TYPES ##################

ac_planes = data.frame(Model = character())
mfc_planes = data.frame(Model = character())

for(df in ac$ac)
{
  new_df <- read.csv(file = paste(filepath, df, sep = ""))
  new_df <- new_df[-(1:2)]
  cols <- colnames(new_df)
  cols <- gsub("\\.", " ", cols)
  this_df <- data.frame(Model = cols)
  this_df <- cbind(this_df, colSums(new_df))
  rownames(this_df) <- NULL
  date_string <- substring(df, 1, 11)
  pretty <- format(as.Date(date_string, "%b-%d-%Y"), "%B %d, %Y")
  colnames(this_df) <- c("Model", pretty)
  ac_planes <- merge(ac_planes, this_df, on = "Model", all = TRUE)
}

for(df in mfc$mfc)
{
  new_df <- read.csv(file = paste(filepath, df, sep = ""))
  new_df <- new_df[-(1:2)]
  cols <- colnames(new_df)
  cols <- gsub("\\.", " ", cols)
  this_df <- data.frame(Model = cols)
  this_df <- cbind(this_df, colSums(new_df))
  rownames(this_df) <- NULL
  date_string <- substring(df, 1, 11)
  pretty <- format(as.Date(date_string, "%b-%d-%Y"), "%B %d, %Y")
  print(pretty)
  colnames(this_df) <- c("Model", pretty)
  mfc_planes <- merge(mfc_planes, this_df, on = "Model", all = TRUE)
}

ac_planes[is.na(ac_planes)] <- 0
mfc_planes[is.na(mfc_planes)] <- 0

write.table(ac_planes,
            file = "./data/flourish/AC_Planes_Flourish_Data.tsv",
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)

write.table(mfc_planes,
            file = "./data/flourish/MFC_Planes_Flourish_Data.tsv",
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)
