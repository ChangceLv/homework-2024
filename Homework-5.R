# Load necessary libraries
library(reticulate)
library(rdataretriever)

# Set up connection to PostgreSQL or SQLite
# If using PostgreSQL
# db <- dbConnect(RPostgres::Postgres(), dbname = "your_database_name", user = "your_username", password = "your_password", host = "localhost")

# If using SQLite
# db <- dbConnect(RSQLite::SQLite(), "your_database.db")

# Specify the schema name
schema_name <- "your_schema_name"

# Load the data from the ade4 package
data("Doubs", package = "ade4")

# Convert Doubs data to a data frame
doubs_df <- as.data.frame(Doubs)

# Upload the data to the specified schema in PostgreSQL or SQLite
# Use dbWriteTable() function to write the data to the database
# For PostgreSQL
# dbWriteTable(db, paste0(schema_name, ".Doubs"), doubs_df, row.names = FALSE)

# For SQLite
# dbWriteTable(db, paste0(schema_name, ".Doubs"), doubs_df, row.names = FALSE, append = TRUE)

# Close the database connection
# dbDisconnect(db)
