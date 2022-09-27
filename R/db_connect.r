# db_connect.r
#
# **Description ---------------------------------------------------------------
# Functions to connect to and query IDOH databases.
# author - mtta17@gmail.com
#
# ***********************************************
# **************** thoughts *********************
# ***********************************************
#
# currently supports SQL server and oracle
# would like to see about integrating other DBMSs
#
# ***********************************************
#
# db_connect()
# query()
#
# db_connect() ################################################################
# **Arguments -----------------------------------------------------------------
# connection		string of the short-hand name of the desired database to which to connect
# **Output --------------------------------------------------------------------
# Returns a connection object
# **Dependencies --------------------------------------------------------------
# DBI, odbc, dplyr, keyring
# **Note
# Keep in mind that the appropriate drivers must be installed on the user's machine in order to interface with each of the DBMSs with R. For example, my SQL Server driver came pre-installed, but I needed to ask IOT to install the Oracle "Oracle in OraClient12Home1" driver for me.
#
# query() #####################################################################
# **Arguments -----------------------------------------------------------------
# connection		string of the short-hand name of the desired database to which to connect
# sql_query			string of the desired SQL query
# df_type			string of desired df output. defaults to a tibble. "dt" converts to a data.table
# **Output --------------------------------------------------------------------
# Returns the query results from the database.
# **Dependencies --------------------------------------------------------------
# DBI, dplyr

###############################################################################
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

###############################################################################
query <- function(connection, sql_query, df_type = "tibble"){
    if(df_type == "dt"){
        data.table::setDT(DBI::dbGetQuery(connection, sql_query))
    }
    else{
        dplyr::as_tibble(DBI::dbGetQuery(connection, sql_query))
    }
} # end query()
