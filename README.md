# ladok

Analysis of student grades

- cr_ladok_database.R creates a MySQL database ladok and inserts contents from tables extracted from the central database.

- cr_ladok.R extracts data tables from the local MySQL database. In principle it should work with the central database.

- Ladok.Rmd is a first analysis of Ladok data.

# Installation

1. Install mysql and change password to root
1. Install MySQL Admin and create test database: `CREATE database test;`
1. Install RMySQL and dbplyr packages
