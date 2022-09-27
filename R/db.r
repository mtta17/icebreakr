# db.r
# mtta17@gmail.com
#
##### db_connect() #####
#
#' Wrappers to easily facilitate connecting to and querying from databases.
#'
#' Database connections are saved as 'con' objects to your environment with db_connect() (the argument is a pre-determined short-hand so that you do not need to pass entire server strings). When querying, all you need to do is reference the 'con', and pass the SQL syntax! Make sure to assign your query output to a variable so that you can continue to manipulate it in-session.
#'
#' Currently, this function skeleton is catered to accommodate IDOH servers. The supported short-hand database names are as follows:
#'
#' | short_hand | server_string |
#' | ---------- | ------------- |
#' | a          | x             |
#' | b          | y             |
#' | c          | z             |
#' | d          | w             |
#' | e          | v             |
#'
#' For servers requiring authentication (e.g. Oracle), you will need to set up `keyring` tokens prior to using these functions. Simply enter `keyring::key_set(service, username)` where `service` is the short-hand name of the desired database. You will then be prompted to enter your password. See `?keyring::key_set` for more information.
#'
#' @param connection String of the short-hand name of the desired database to which to connect
#' @return Connection object
#' @examples db_connect("my_fav_database")
#' @export
db_connect <- function(connection){
    # dictionary of database names and their connection details
    db_lookup <- dplyr::tibble(
        short_name = c("a",
                       "b",
                       "c",
                       "d",
                       "e"),
        connection_string = c("x",
                              "y",
                              "z",
                              "w",
                              "v"),
        db_name = c("db",
                    "db",
                    NA_character_,
                    NA_character_,
                    "db")
    )
    # check to make sure that the connection passed to the function is a db in the db_lookup table
    if(connection %in% db_lookup$short_name){
        # connections are set up depending on the DBMS and the authentication
        if(!is.na(db_lookup %>% dplyr::filter(short_name == connection) %>% dplyr::pull(db_name))){
            # SQL Server with Windows authenticaton
            tryCatch({
                con <- DBI::dbConnect(odbc::odbc(),
                                      Driver = "SQL Server",
                                      Server = db_lookup %>% dplyr::filter(short_name == connection) %>% dplyr::pull(connection_string),
                                      Database = db_lookup %>% dplyr::filter(short_name == connection) %>% dplyr::pull(db_name),
                                      Port = 1433)
            },
            error = function(c){
                message(paste0(c, "\n\n", "Ensure that you have access to the db. "))
            }
            ) # end tryCatch
        } # end if
        else{
            # Oracle
            tryCatch({
                con <- DBI::dbConnect(odbc::odbc(),
                                      Driver = "Oracle in OraClient12Home1",
                                      DBQ    = db_lookup %>% dplyr::filter(short_name == connection) %>% dplyr::pull(connection_string),
                                      UID    = keyring::key_list(connection)[1, 2],
                                      PWD    = keyring::key_get(connection, keyring::key_list(connection)[1, 2]))
            },
            error = function(c){
                message(paste0(c, "\n\n",
                               "Ensure that you have access to the db and that your keyring is set up correctly e.g.\n",
                               "\tkeyring::key_set(\"db_prod\", \"jsmith\")"))
            }
            ) # end tryCatch
        } # end else
        return(con)
    } # end if
    else{
        message(paste0("Error: connection is not a valid IDOH database.\nconnection must be one of the following:\n",
                       paste("\t", sort(db_lookup$short_name), collapse = "\n")))
    } # end else
} # end db_connect()

##### query() #####
#
#' Convenient function to query a database connection.
#'
#' Use an in-session database connection, such as from `icebreakr::db_connect` or another DBI/odbc method, to pull query results directly into your R session straight from the database.
#'
#' @param con R connection object created using `icebreakr::db_connect()` or DBI/odbc
#' @param sql_query String of your SQL query
#' @param df_type String of the desired df data type output. Defaults to a tibble. "dt" converts to a data.table
#' @return Tibble of the query results from the database
#' @examples x <- query(con, "select top 1000 * from my_fav_table")
#' @export
query <- function(con, sql_query, df_type = "tibble"){
    if(df_type == "dt"){
        data.table::setDT(DBI::dbGetQuery(con, sql_query))
    }
    else{
        dplyr::as_tibble(DBI::dbGetQuery(con, sql_query))
    }
} # end query()
