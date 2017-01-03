#!/bin/bash

###############################################
#
# Creates the mysql user, database, table and loads data
# for the Sqoop+mysql technique.
#
###############################################


# Add -x to the above to echo each line

bin=`readlink -f $0`
bin=`dirname ${bin}`
home=${bin}/../

user=root
date=`date +"%Y%m%d-%k%M%S"`
ddl_out=$home/ddl-$date.out
load_out=$home/load-$date.out

echo -n "Enter the password for the mysql root user: "
read -s pwd

echo

echo "Creating user hip_sqoop_user"

mysql --user=$user --password=$pwd &> $ddl_out << EOF
DROP USER 'hip_sqoop_user'@'localhost';
EOF

mysql --user=$user --password=$pwd &> $ddl_out << EOF
CREATE USER 'hip_sqoop_user'@'localhost' IDENTIFIED BY 'password';
GRANT ALL PRIVILEGES ON *.* TO 'hip_sqoop_user'@'localhost' WITH GRANT OPTION;
GRANT FILE ON *.* TO 'hip_sqoop_user'@'localhost' WITH GRANT OPTION;
FLUSH PRIVILEGES;
EOF

exitCode=$?

if [ "$exitCode" != "0" ]; then
  cat $ddl_out
  echo "User creation failed, please look at $ddl_out for details."
  exit 1
fi

echo "Creating database 'sqoop_test' and tables 'stocks', 'stocks_export', 'stocks_staging'"

mysql --user=$user --password=$pwd &> $ddl_out << EOF
DROP DATABASE IF EXISTS sqoop_test;
CREATE DATABASE sqoop_test;

USE sqoop_test;

CREATE TABLE stocks (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  symbol VARCHAR(100),
  quote_date VARCHAR(100),
  open_price DOUBLE PRECISION,
  high_price DOUBLE PRECISION,
  low_price DOUBLE PRECISION,
  close_price DOUBLE PRECISION,
  volume INTEGER,
  adj_close_price DOUBLE PRECISION
);

CREATE TABLE stocks_export (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  symbol VARCHAR(100),
  quote_date VARCHAR(100),
  open_price DOUBLE PRECISION,
  high_price DOUBLE PRECISION,
  low_price DOUBLE PRECISION,
  close_price DOUBLE PRECISION,
  volume INTEGER,
  adj_close_price DOUBLE PRECISION
);

CREATE TABLE stocks_staging (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  symbol VARCHAR(100),
  quote_date VARCHAR(100),
  open_price DOUBLE PRECISION,
  high_price DOUBLE PRECISION,
  low_price DOUBLE PRECISION,
  close_price DOUBLE PRECISION,
  volume INTEGER,
  adj_close_price DOUBLE PRECISION
);
EOF

exitCode=$?

if [ "$exitCode" != "0" ]; then
  cat $ddl_out
  echo "DDL failed, please look at $ddl_out for details."
  exit 1
fi

echo "Loading data into table stocks"

mysql --user=$user --password=$pwd &> $load_out << EOF
USE sqoop_test;
LOAD DATA LOCAL INFILE '$home/test-data/stocks.txt'
INTO TABLE stocks
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
(symbol, @quote_date, open_price, high_price, low_price, close_price, volume, adj_close_price)
SET quote_date = DATE_FORMAT(@quote_date, '%Y-%m-%d');
EOF

exitCode=$?

if [ "$exitCode" != "0" ]; then
  cat $load_out
  echo "LOAD DATA failed, please look at $load_out for details."
  exit 1
fi

echo "Success!"