#installed.packages("rvest")
#installed.packages("XML")
library(rvest)
library(xml2)
library(tidyverse)

# For KL Rahul

url_kl="https://stats.espncricinfo.com/ci/engine/player/422108.html?class=11;template=results;type=batting;view=innings"
page <- read_html(url_kl)
col_table<-page %>%  html_nodes("table.engineTable") %>%  html_table() %>% .[[4]]
View(col_table)
write.csv(col_table,"kl_rahul.csv")


# For Shikhar Dhawan

 url_sd<- "https://stats.espncricinfo.com/ci/engine/player/28235.html?class=11;template=results;type=batting;view=innings"
 
 page_sd<- read_html(url_sd)
sd_table<- page_sd %>%  html_nodes("table.engineTable") %>%  html_table() %>% .[[4]]
View(sd_table)
write.csv(sd_table, "sd_data.csv")

# For rohit sharma

url_rs<- "https://stats.espncricinfo.com/ci/engine/player/34102.html?class=11;template=results;type=batting;view=innings"
page_rs<- read_html(url_rs)
rs_table<- page_rs %>%  html_nodes("table.engineTable") %>%  html_table() %>% .[[4]]
View(rs_table)
write.csv(rs_table, "rs_data.csv")

# For Jason roy
url_jr<-"https://stats.espncricinfo.com/ci/engine/player/298438.html?class=11;template=results;type=batting;view=innings"
page_jr<- read_html(url_jr)
jr_table<- page_jr %>%  html_nodes("table.engineTable") %>%  html_table() %>% .[[4]]
View(jr_table)
write.csv(jr_table,"jr_data.csv")


# For David Warner
url_dw<- "https://stats.espncricinfo.com/ci/engine/player/219889.html?class=11;template=results;type=batting;view=innings"
page_dw<- read_html(url_dw)
dw_table<- page_dw %>% html_nodes("table.engineTable") %>%  html_table() %>% .[[4]]
View(dw_table)
write.csv(dw_table,"sw_data.csv")

# For Kane Williamson
url_kw<- "https://stats.espncricinfo.com/ci/engine/player/277906.html?class=11;template=results;type=batting;view=innings"

page_kw<- read_html(url_kw)
kw_table<- page_kw %>% html_nodes("table.engineTable") %>%  html_table() %>% .[[4]]
View(kw_table)
write.csv(kw_table,"kw_data.csv")

# For Marnus Labusen
url_ml<- "https://stats.espncricinfo.com/ci/engine/player/787987.html?class=11;template=results;type=batting;view=innings"
page_ml<- read_html(url_ml)
ml_table<- page_ml %>% html_nodes("table.engineTable") %>%  html_table() %>% .[[4]]
View(ml_table)
write.csv(ml_table,"ml_data.csv")

