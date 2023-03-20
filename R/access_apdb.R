
# open shell connection from gear wheel in git pane, run the following (ssh key in .ssh folder)
#
# $ eval $(ssh-agent -s)
# $	 ssh-add <PATH to SSH key>
# $ ssh -f marcus@104.196.116.178 -L 3306:localhost:3306 -N

library(RMySQL)

pword <- Sys.getenv('apdbpword')

con <- DBI::dbConnect(
  MySQL(),
  dbname = 'tbepapdbuser', 
  host = 'localhost', 
  user = 'tbepapdbuser', 
  password = pword
)

header <- DBI::dbReadTable(con, 'header')
