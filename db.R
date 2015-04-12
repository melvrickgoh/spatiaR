if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)

library(RPostgreSQL)
library(RJDBC)

getConn <- function() {

	driver <- JDBC("org.postgresql.Driver", "postgresql-9.4-1201.jdbc4.jar", "`")
  connection = dbConnect(driver, "jdbc:postgresql://ec2-50-17-181-147.compute-1.amazonaws.com:5432/dfu4geofvgc388?ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory", password="lw9C-ir1b-oTtSIvmq8XdKGzoT", user="loqnyipuctdbyo", dbname="dfu4geofvgc388")

  #queryString = p("SELECT * FROM rprocess WHERE userid = '","107010541319520129244","' AND uuid = '","hello","';")
  #print(queryString)

  #queryResults = dbGetQuery(testdb, queryString)
  #print(queryResults)
  #print(typeof(queryResults))
	return(list("driver" = driver, "connection" = connection))
}

endConn <- function(drv,conn) {
	## Closes the connection
	dbDisconnect(conn)
	## Frees all the resources on the driver
	dbUnloadDriver(drv)
}

dbKDEGroup <- function(userid,uuid) {
  db <- getConn()

  queryString = p("SELECT * FROM rprocess WHERE userid = '",userid,"' AND uuid = '",uuid,"';")
  
  ## Submit and execute the query
	dfPayload <- dbGetQuery(db$connection, queryString)
	print(typeof(dfPayload))

	endConn(db$driver,db$connection)

	return(dfPayload)
}