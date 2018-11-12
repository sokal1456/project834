library(DBI)
# Connect to my-db as defined in ~/.my.cnf
con <- dbConnect(RMySQL::MySQL(), user = "justin", password = "justin", host= "35.229.127.110", db="db_p834")


dbListTables(con)
dbWriteTable(con, "TestTable", census_mls_data)
dbListTables(con)

dbListFields(con, "TestData")
dbReadTable(con, "TestData")

res <- dbSendQuery(con, "Drop ")

# You can fetch all results:
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)
dbClearResult(res)

# Or a chunk at a time
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
while(!dbHasCompleted(res)){
  chunk <- dbFetch(res, n = 5)
  print(nrow(chunk))
}
# Clear the result
dbClearResult(res)

# Disconnect from the database
dbDisconnect(con)