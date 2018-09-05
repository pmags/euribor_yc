library(Quandl)

# Runs API and downloads last information
bundesbank_codes <- c("Euribor 1M"="BUNDESBANK/BBK01_ST0310",
                      "Euribor 3M"="BUNDESBANK/BBK01_ST0316",
                      "Euribor 6M"="BUNDESBANK/BBK01_ST0325",
                      "Euribor 9M"="BUNDESBANK/BBK01_ST0334",
                      "Euribor 12M"="BUNDESBANK/BBK01_ST0343")

euribor_api <-  Quandl (bundesbank_codes,
                        api_key="uEV_9ozUK1EEBXnDaXYy",
                        type = "xts",
                        start_date= "2000-01-01")

saveRDS(euribor_api,"temp/euribordb.rds")

euribor_db <- readRDS("temp/euribordb.rds")