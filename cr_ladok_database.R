library(tidyverse)
library(readxl)

con <- dbConnect(RMySQL::MySQL(),
                 host = "localhost",
                 user = "root",
                 dbname = "test",
                 password = "pwdpwd"
                 # password = rstudioapi::askForPassword("Database password")
)

# dbSendQuery(con, "drop database ladok;")
dbSendQuery(con, "create database ladok;")

con <- dbConnect(RMySQL::MySQL(),
                 host = "localhost",
                 user = "root",
                 dbname = "ladok",
                 password = "pwdpwd"
                 # password = rstudioapi::askForPassword("Database password")
)

copy_to(con, 
        read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/kurs_alla.xlsx"), 
        "kurs",
        temporary = FALSE,
        overwrite = TRUE
)

copy_to(con, 
        read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/prov_alla.xlsx") ,
        "prov",
        temporary = FALSE,
        overwrite = TRUE
)

copy_to(con, 
        read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/ffgkurs_alla.xlsx") ,
        "ffgkurs",
        temporary = FALSE,
        overwrite = TRUE
)

copy_to(con, 
        read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/godkprov_alla.xlsx") ,
        "godkprov",
        temporary = FALSE,
        overwrite = TRUE
)

copy_to(con, 
        read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/godkkurs_alla.xlsx") ,
        "godkkurs",
        temporary = FALSE,
        overwrite = TRUE
)

dbListTables(con)
dbDisconnect(con) 
