library(DBI)
library(RPostgres)

# Use environment variables we set earlier
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("PGRHOST"),
  port = as.integer(Sys.getenv("PGRPORT")),
  user = Sys.getenv("PGRUSER"),
  password = Sys.getenv("PGRPASSWORD"),
  dbname = Sys.getenv("PGRDATABASE")
)

