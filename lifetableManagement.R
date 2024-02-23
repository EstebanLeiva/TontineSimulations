lifeTable <- read.table("data/LifeTables.txt", header = TRUE)
lifeTable$Age <- as.numeric(lifeTable$Age)
lifeTable$Age <- replace(lifeTable$Age, is.na(lifeTable$Age), 110)

years <- unique(lifeTable$Year)
df_list <- list()
for (year in years) {
  
  year_df <- lifeTable[lifeTable$Year==year, ]
  year_df <- year_df[year_df$Age > 65, ]
  year_df$px <- 1 - year_df$qx
  year_df$Tpx <- cumprod(year_df$px)
  
  write.csv(year_df, paste("data/LifeTablesbyYear/",as.character(year),".csv"), row.names = FALSE)
}
