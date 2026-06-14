
# R Shiny: Daten lesen
library(DBI)
library(RPostgres)
library(dotenv)

tmp <- tempfile(".env")
load_dot_env(tmp)


con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "Finanzen",
  host = Sys.getenv("SUPABASE_HOST"),
  port = 5432,
  user = "postgres",
  password = Sys.getenv("SUPABASE_DBPW")
)

df <- dbGetQuery(con, "SELECT * FROM sales")


host:db.zhsvfvdiplceuotinvqn.supabase.co
port:5432
database:postgres
user:postgres