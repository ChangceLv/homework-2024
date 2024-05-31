# Load required libraries
library(ade4)
library(reticulate)  # For working with Python packages
library(DBI)         # For working with databases

# Load the Doubs dataset
data("doubs")

# Convert Doubs dataset to a pandas DataFrame (Python)
py <- import("rpy2.robjects")
pd <- import("pandas")
Doubs_df <- py$convert_to(doubs, pd$dataframe)

# Connect to PostgreSQL or SQLite database
# Replace the placeholders with your actual database connection details
# For PostgreSQL
#con <- dbConnect(drv = RPostgres::Postgres(),
#                  dbname = "lcc_database_name",
#                  host = "lcc_host",
#                  port = lcc_port,
#                  user = "lcc",
#                  password = "123456")


# For SQLite
con <- dbConnect(RSQLite::SQLite(), "lcc_database.sqlite")

# Write the DataFrame to the database
dbWriteTable(con, "Doubs", fish_df, overwrite = TRUE)

# Close the database connection
dbDisconnect(con)
