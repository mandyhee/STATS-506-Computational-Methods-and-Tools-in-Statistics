library(tidyverse)
library(dbplyr)
library(Lahman)
lahman = lahman_sqlite()
batting = lahman %>% tbl("BATTING")
master = lahman %>% tbl("MASTER")


q1 = lahman %>% tbl(sql('
                    SELECT FirstName, LastName, Debut, `Country of Birth`, sum(H) as Hits
                    FROM Batting as b
                    LEFT JOIN 
                      (SELECT playerID, nameFirst as FirstName, nameLast as LastName, debut as Debut, birthCountry as `Country of Birth`
                        FROM master) as m
                    ON m.playerID = b.playerID
                    GROUP BY b.playerID
                    HAVING Hits >= 200
                    ORDER BY -Hits
                    ')) %>% collect()

