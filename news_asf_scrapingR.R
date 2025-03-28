
#1 open file CSV
library(readr)
asf <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/indonesia/asf_news_id-raw.csv")


View(asf)     #see table of content
names(asf)    #see name of coloumn 
dim(asf)
typeof(asf$date_access)


berita_asf <- table(asf$source_web) #melihat berbagai sumber berita
berita_asf
prop.table(berita_asf) #proporsi media berita

#2.1 install packages that used
install.packages("dplyr")
install.packages("polite")
install.packages("rvest")
install.packages("magrittr")
install.packages("stringr")
install.packages("RSelenium")


#2.2 activate packages that used
library(dplyr)
library(polite)
library(rvest)
library(magrittr)
library(stringr)
library(RSelenium)
library(lubridate)


#3.1 make new coloum for product of scraping
asf$news_publish <- NA        #for scraping results of date
asf$title <- "Defaul Ttle"  #for scraping results of title news
asf$news_content <- NA             #for scraping results of content

unique(asf$source_web)

#3.2 scraping based website_source  from asf
######################################################day001####################################################

#groupantara
for (i in 1:nrow(asf)) {
  #for scrape web source from antara
  if(asf$source_web[i] == "antara") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".font-weight-normal") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep1.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_ntt
  if(asf$source_web[i] == "antara_ntt") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep2.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_bengkulu
  if(asf$source_web[i] == "antara_bengkulu") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep3.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_bali
  if(asf$source_web[i] == "antara_bali") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep4.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_gorontalo
  if(asf$source_web[i] == "antara_gorontalo") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep5.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_jambi
  if(asf$source_web[i] == "antara_jambi") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep6.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_jateng
  if(asf$source_web[i] == "antara_jateng") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep7.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_jatim
  if(asf$source_web[i] == "antara_jatim") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep8.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_kaltara
  if(asf$source_web[i] == "antara_kaltara") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep9.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_kalteng
  if(asf$source_web[i] == "antara_kalteng") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep10.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_lampung
  if(asf$source_web[i] == "antara_lampung") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep11.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_papua
  if(asf$source_web[i] == "antara_papua") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep12.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_papuabarat
  if(asf$source_web[i] == "antara_papuabarat") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep13.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_sulsel
  if(asf$source_web[i] == "antara_sulsel") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep14.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_sulteng
  if(asf$source_web[i] == "antara_sulteng") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep15.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_sumbar
  if(asf$source_web[i] == "antara_sumbar") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep16.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from antara_sumsel
  if(asf$source_web[i] == "antara_sumsel") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep17.csv")

#next news group tribun

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun
  if(asf$source_web[i] == "tribun") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep18.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_bali
  if(asf$source_web[i] == "tribun_bali") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep19.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_aceh
  if(asf$source_web[i] == "tribun_aceh") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep20.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_babel
  if(asf$source_web[i] == "tribun_babel") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep21.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_bangka
  if(asf$source_web[i] == "tribun_bangka") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep22.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_banyumas
  if(asf$source_web[i] == "tribun_banyumas") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep23.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_flores
  if(asf$source_web[i] == "tribun_flores") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep24.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_gowa
  if(asf$source_web[i] == "tribun_gowa") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep25.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_jabar
  if(asf$source_web[i] == "tribun_jabar") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep26.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_jateng
  if(asf$source_web[i] == "tribun_jateng") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep27.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_kaltara
  if(asf$source_web[i] == "tribun_kaltara") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep28.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_kupang
  if(asf$source_web[i] == "tribun_kupang") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep29.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_makassar
  if(asf$source_web[i] == "tribun_makassar") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep30.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_manado
  if(asf$source_web[i] == "tribun_manado") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep31.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_medan
  if(asf$source_web[i] == "tribun_medan") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep32.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_palembang
  if(asf$source_web[i] == "tribun_palembang") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep33.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_palu
  if(asf$source_web[i] == "tribun_palu") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep34.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_papua
  if(asf$source_web[i] == "tribun_papua") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep35.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_pontianak
  if(asf$source_web[i] == "tribun_pontianak") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep36.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_sulbar
  if(asf$source_web[i] == "tribun_sulbar") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep37.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_sumsel
  if(asf$source_web[i] == "tribun_sumsel") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep38.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_toraja
  if(asf$source_web[i] == "tribun_toraja") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep39.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_wartakota
  if(asf$source_web[i] == "tribun_wartakota") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep40.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tribun_wow
  if(asf$source_web[i] == "tribun_wow") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep41.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from cnbc_indonesia
  if(asf$source_web[i] == "cnbc_indonesia") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".text-gray") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".font-extrabold") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detail-text") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep42.csv")

######################################################day002####################################################

asfv <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep42.csv")
View(asfv)

for (i in 1:nrow(asfv)) {
  #for scrape web source from detik
  if(asfv$source_web[i] == "detik") {
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detail__date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detail__title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep43.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from detik_bali
  if(asf$source_web[i] == "detik_bali") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detail__date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detail__title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep44.csv")

for (i in 1:nrow(asf)) {
  if(asf$source_web[i] == "kompas") {
    #extract article publication
    asf[i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("time") %>%                  #read the codes, take all nodes
      rvest::html_attr("datetime") %>%              # choose atribut 'datetime'
      stringr::str_trim()                             #remove extra spaces

    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".ksm-15b") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes(".ksm-2BC") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep45.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from kompas_regional
  if(asfv$source_web[i] == "kompas_regional") {
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".read__time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".read__title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep46.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from kompas_theme
  if(asfv$source_web[i] == "kompas_theme") {
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".read__time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".read__title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep47.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from kompas_tv
  if(asfv$source_web[i] == "kompas_tv") {
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".time-news") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".title-news-big-detail") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep47.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from fajar_papua
  if(asfv$source_web[i] == "fajar_papua") {
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".published") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("strong") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep48.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from koran_papua
  if(asfv$source_web[i] == "koran_papua") {
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes(".jeg_meta_date a") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces

    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep49.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from republik
  if(asf$source_web[i] == "republik") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".date-item__headline") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep50.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from salam_papua
  if(asfv$source_web[i] == "salam_papua") {

    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("ul.post-info-dark li") %>% .[2] %>%                  #read the codes, take all nodes anf choose 2nd
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h2.title-semibold-dark.size-c30") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep51.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from sindo_news
  if(asfv$source_web[i] == "sindo_news") {
    
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detail-date-artikel") %>%                  #read the codes, take all nodes anf choose 2nd
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("#detail-desc") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep52.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from tirto
  if(asfv$source_web[i] == "tirto") {
    
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("div.byline div:nth-child(2)") %>%                  #read the codes, take all nodes anf choose 2nd
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".article-title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep53.csv")

for (i in 1:nrow(asfv)) {
  #for scrape web source from kumparan
  if(asfv$source_web[i] == "kumparan") {
    
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("span[data-qa-id='publish-date']") %>%                  #read the codes, take all nodes anf choose 2nd
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".txdisplay-semilarge") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes(".txextra-large") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asfv)
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep54.csv")

######################################################day003####################################################

for (i in 1:nrow(asf)) {
  #for scrape web source from analisadaily
  if(asf$source_web[i] == "analisadaily") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("ul.post-meta li:first-child") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces

    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h2") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    

    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep55.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from bali post
  if(asf$source_web[i] == "bali_post") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".td-module-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".entry-title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("h2,p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep55.csv")

######################################################day004####################################################

for (i in 1:nrow(asf)) {
  #for scrape web source from antar_papua
  if(asf$source_web[i] == "antar_papua") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".published") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1.entry-title strong") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep62.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from ayojakarta
  if(asf$source_web[i] == "ayojakarta") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".read__info__date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".read__title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep63.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from bali_bisnis
  if(asf$source_web[i] == "bali_bisnis") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detailsAttributeDates") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detailsTitleCaption") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep64.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from bali_express
  if(asf$source_web[i] == "bali_express") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".read__info__date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".read__title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep65.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from balifactual
  if(asf$source_web[i] == "balifactual") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".published") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                            #remove extra spaces
     
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep66.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from bangkapos
  if(asf$source_web[i] == "bangkapos") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                            #remove extra spaces
    
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep67.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from bbc_news
  if(asf$source_web[i] == "bbc_news") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                            #remove extra spaces
    
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep68.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from bbc_news
  if(asf$source_web[i] == "bbc_news") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                            #remove extra spaces
    
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep68.csv")

######################################################day005-new script writing###################################################

for (i in 1:nrow(asf)) {
  #for scrape web source from betahita
  if(asf$source_web[i] == "betahita") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                            #remove extra spaces
    
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep69.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from borneonews (belum berhasil)
  if(asf$source_web[i] == "borneonews") {
    
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   # Extract article publication
    date_node <- page %>% rvest::html_node(".post-on")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".post-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("#content-berita")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep69.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from bukamata
  if(asf$source_web[i] == "bukamata") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".date_article_single")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes(".content_single")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep69.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from bukamata
  if(asf$source_web[i] == "bukamata") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".date_article_single")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes(".content_single")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep69.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from ceposonline
  if(asf$source_web[i] == "ceposonline") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep70.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from denpost
  if(asf$source_web[i] == "denpost") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep71.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from equator_online
  if(asf$source_web[i] == "equator_online") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".jeg_meta_date a")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".jeg_post_title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep72.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from garton
  if(asf$source_web[i] == "garton") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep73.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from haluan_padang
  if(asf$source_web[i] == "haluan_padang") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep74.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from harian_berkat
  if(asf$source_web[i] == "harian_berkat") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".posted-on .entry-date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep75.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from headline_kaltim
  if(asf$source_web[i] == "headline_kaltim") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".td-module-date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".tdb-title-text")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep76.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from headline_kaltim
  if(asf$source_web[i] == "headline_kaltim") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".td-module-date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".tdb-title-text")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep77.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from hitsidn
  if(asf$source_web[i] == "hitsidn") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep78.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from independen_media
  if(asf$source_web[i] == "independen_media") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep79.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from inews_sumsel
  if(asf$source_web[i] == "inews_sumsel") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".timeAndShare .createdAt")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep80.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from inikita
  if(asf$source_web[i] == "inikita") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".published")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep81.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from kalteng_pos
  if(asf$source_web[i] == "kalteng_pos") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".td-module-date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".tdb-title-text")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep82.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from kilasbali
  if(asf$source_web[i] == "kilasbali") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".date.meta-item.tie-icon")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep83.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from kobaran
  if(asf$source_web[i] == "kobaran") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep84.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from koran_kaltara
  if(asf$source_web[i] == "koran_kaltara") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".time-base")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".heading-1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes(".news-content")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep85.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from koranseruya
  if(asf$source_web[i] == "koranseruya") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".td-module-date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep86.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from kupastuntas
  if(asf$source_web[i] == "kupastuntas") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep87.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from lentera_esai
  if(asf$source_web[i] == "lentera_esai") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".published")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep88.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from makassar_terkini
  if(asf$source_web[i] == "makassar_terkini") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".td-article-date")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".td-article-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep89.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from medan_bisnis
  if(asf$source_web[i] == "medan_bisnis") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".tanggal")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".judul")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep90.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from medcom
  if(asf$source_web[i] == "medcom") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".info_ct")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("#h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("#transarticle")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep91.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from media_indonesia
  if(asf$source_web[i] == "media_indonesia") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".datetime")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep92.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from mercusuar
  if(asf$source_web[i] == "mercusuar") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".published")        # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep93.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from metro_Terkini
  if(asf$source_web[i] == "metro_Terkini") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_nodes("p") %>% 
      .[1] # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".single_post_title_main")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p") %>% .[2]   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep94.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from nabire
  if(asf$source_web[i] == "nabire") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".meta_date") # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("h4")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep95.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from nusabali
  if(asf$source_web[i] == "nusabali") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("i.fa.fa-clock-o + span") # Extract date
    if (!is.null(date_node)) {
      asf[i,"news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i,"news_publish"] <- NA  # Atau nilai default lainnya
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep96.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from okezone
  if(asf$source_web[i] == "okezone") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_nodes(".journalist span") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep97.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from opini
  if(asf$source_web[i] == "opini") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_nodes(".published") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep98.csv")


######################################################day006###################################################


for (i in 1:nrow(asf)) {
  #for scrape web source from pikiranrakyat_depok
  if(asf$source_web[i] == "pikiranrakyat_depok") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".date_detail") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep99.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from pojok_papua
  if(asf$source_web[i] == "pojok_papua") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".updated") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep100.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from pos_kupang
  if(asf$source_web[i] == "pos_kupang") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("div.grey.bdr3.pb10.pt10 time span") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep101.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from posflores
  if(asf$source_web[i] == "posflores") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep102.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from prokal
  if(asf$source_web[i] == "prokal") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".content-artikel-tanggal") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".content-artikel-judul")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep103.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from radarbali
  if(asf$source_web[i] == "radarbali") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep104.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from radio_republik_indonesia
  if(asf$source_web[i] == "radio_republik_indonesia") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("div.caption small:nth-child(3)") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("div.single-header-text h1#news-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep105.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from regional_kontan
  if(asf$source_web[i] == "regional_kontan") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".mar-t-10") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".jdl_dtl")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep105.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from riausky
  if(asf$source_web[i] == "riausky") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("div.author-description p") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".single_post_title_main")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("div.isiku")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep106.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from rri
  if(asf$source_web[i] == "rri") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("div.caption small:nth-child(3)") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("div.single-header-text h1#news-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep107.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from seputar_papua
  if(asf$source_web[i] == "seputar_papua") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".time-publikasi") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep108.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from sinarharapan
  if(asf$source_web[i] == "sinarharapan") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".read__info__date") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".read__title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep109.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from solopos
  if(asf$source_web[i] == "solopos") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".d-inline-flex") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep110.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from sripoku
  if(asf$source_web[i] == "sripoku") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("time") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep111.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from suara 
  if(asf$source_web[i] == "suara") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("div.date-article span") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep112.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from suara_kalbar 
  if(asf$source_web[i] == "suara_kalbar") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("div > span[itemprop='datePublished']") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep113.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from suara_papua 
  if(asf$source_web[i] == "suara_papua") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".td-module-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".tdb-title-text") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep114.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from suara_sumbar
  if(asf$source_web[i] == "suara_sumbar") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("div.date-article span") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep115.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from sulawesi_bisnis
  if(asf$source_web[i] == "sulawesi_bisnis") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detailsAttributeDates") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detailsTitleCaption") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep116.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from sumatra_bisnis
  if(asf$source_web[i] == "sumatra_bisnis") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detailsAttributeDates") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".detailsTitleCaption") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep117.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from sumbar_kita
  if(asf$source_web[i] == "sumbar_kita") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".jeg_meta_date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".jeg_post_title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep117.csv")

######################################################day007###################################################

for (i in 1:nrow(asf)) {
  #for scrape web source from tagar
  if(asf$source_web[i] == "tagar") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".post_date") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".new-title-tagar-01")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep118.csv")

for (i in 1:nrow(asf)) {
  #for scrape web source from tempo
  if(asf$source_web[i] == "tempo") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("p.text-neutral-900.text-sm") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1.font-bold.text-neutral-1200")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep119.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from timexkupang
  if(asf$source_web[i] == "timexkupang") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".published") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1.entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep120.csv")


for (i in 1:nrow(asf)) {
  # For scrape web source from trenasia FAILED
  if(asf$source_web[i] == "trenasia") {
    
    # Extract article publication
    page <- polite::bow(asf[i, "url"]) %>% polite::scrape()  # Ensure polite is used correctly
    
    # Extract date
    date_node <- page %>% rvest::html_node("time.text-[10px].text-[#999]") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("head > title")  # Check if this selector is correct
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")  # Extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
    
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep121.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from tvonenews
  if(asf$source_web[i] == "tvonenews") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".detail-date") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".detail-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep122.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from voa_indonesia
  if(asf$source_web[i] == "voa_indonesia") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("time") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".pg-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep123.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source from voi
  if(asf$source_web[i] == "voi") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("div.col span") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep124.csv")


for (i in 1:nrow(asf)) {
  #for scrape web source warta_lutim
  if(asf$source_web[i] == "warta_lutim") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".published") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node(".entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}
View(asf)
write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep125.csv")

#####periode2####
library(xml2)
library(httr)

for (i in 1:nrow(asf)) {
  # Filter hanya untuk "detik_bali" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "detik_bali" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    asf[i, "news_publish"] <- parsed_html %>% 
      html_node(".detail__date") %>%  
      html_text(trim = TRUE)
    
    # Extract article title
    asf[i, "news_title"] <- parsed_html %>% 
      html_node(".detail__title") %>%  
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "")
    
    # Extract article content
    asf[i, "news_content"] <- parsed_html %>% 
      html_nodes("p") %>%  # Ambil semua paragraf
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "") %>%  
      paste(collapse = " ")  # Gabungkan semua paragraf dalam satu string
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}

for (i in 1:nrow(asf)) {
  # Filter hanya untuk "digtara" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "digtara" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    asf[i, "news_publish"] <- parsed_html %>% 
      html_node(".nama-reporter") %>%  
      html_text(trim = TRUE)
    
    # Extract article title
    asf[i, "news_title"] <- parsed_html %>% 
      html_node("h1") %>%  
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "")
    
    # Extract article content
    asf[i, "news_content"] <- parsed_html %>% 
      html_nodes("p") %>%  # Ambil semua paragraf
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "") %>%  
      paste(collapse = " ")  # Gabungkan semua paragraf dalam satu string
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "fajar_papua" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "fajar_papua" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    asf[i, "news_publish"] <- parsed_html %>% 
      html_node(".published") %>%  
      html_text(trim = TRUE)
    
    # Extract article title
    asf[i, "news_title"] <- parsed_html %>% 
      html_node("strong") %>%  
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "")
    
    # Extract article content
    asf[i, "news_content"] <- parsed_html %>% 
      html_nodes("p") %>%  # Ambil semua paragraf
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "") %>%  
      paste(collapse = " ")  # Gabungkan semua paragraf dalam satu string
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "floresa" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "floresa" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    asf[i, "news_publish"] <- parsed_html %>% 
      html_node(".td-module-date") %>%  
      html_text(trim = TRUE)
    
    # Extract article title
    asf[i, "news_title"] <- parsed_html %>% 
      html_node(".tdb-title-text") %>%  
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "")
    
    # Extract article content
    asf[i, "news_content"] <- parsed_html %>% 
      html_nodes("p") %>%  # Ambil semua paragraf
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "") %>%  
      paste(collapse = " ")  # Gabungkan semua paragraf dalam satu string
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "jubi" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "jubi" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    asf[i, "news_publish"] <- parsed_html %>% 
      html_node(".updated-date") %>%  
      html_text(trim = TRUE)
    
    # Extract article title
    asf[i, "news_title"] <- parsed_html %>% 
      html_node(".fw-headline") %>%  
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "")
    
    # Extract article content
    asf[i, "news_content"] <- parsed_html %>% 
      html_nodes("p") %>%  # Ambil semua paragraf
      html_text(trim = TRUE) %>%  
      str_replace_all("[\r\n\t\"]", "") %>%  
      paste(collapse = " ")  # Gabungkan semua paragraf dalam satu string
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "kompas" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "kompas" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node(".read__time")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }  # Jika elemen tidak ditemukan, beri NA
    
    # Extract article title
    node_title <- parsed_html %>% html_node(".read__title")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "koran_papua" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "koran_papua" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node(".jeg_meta_date a")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }  # Jika elemen tidak ditemukan, beri NA
    
    # Extract article title
    node_title <- parsed_html %>% html_node("h1")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "kupang_news" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "kupang_news" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node(".read__info__date")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }  # Jika elemen tidak ditemukan, beri NA
    
    # Extract article title
    node_title <- parsed_html %>% html_node(".read__title")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "liputan6" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "liputan6" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan encoding UTF-8
    page <- GET(asf[i, "url"])
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node(".updated")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }  # Jika elemen tidak ditemukan, beri NA
    
    # Extract article title
    node_title <- parsed_html %>% html_node(".entry-title")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "radio_republik_indonesia" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "radio_republik_indonesia" & asf$date_access[i] == "03/02/2025") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node("div.caption small:nth-child(3)") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("div.single-header-text h1#news-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "suara_sikka" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "suara_sikka" & asf$date_access[i] == "03/02/2025") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".tanggal-single") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "timex_kupang" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "timex_kupang" & asf$date_access[i] == "03/02/2025") {
    # Extract article publication
    page <- polite::bow(asf[i,"url"]) %>% polite::scrape(.)   
    date_node <- page %>% rvest::html_node(".published") 
    if (length(date_node) > 0) {
      asf[i, "news_publish"] <- date_node %>% rvest::html_text() %>% stringr::str_trim()
    } else {
      asf[i, "news_publish"] <- NA
      print(paste("Tanggal tidak ditemukan untuk URL:", asf[i, "url"]))
    }
    
    # Extract article title
    title_node <- page %>% rvest::html_node("h1.entry-title")  # Pastikan ini selector yang benar
    if (!is.null(title_node)) {
      asf[i,"news_title"] <- title_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim()
    } else {
      asf[i,"news_title"] <- NA
      print(paste("Judul tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Extract article content
    content_node <- page %>% rvest::html_nodes("p")   #extract content
    if (length(content_node) > 0) {
      asf[i,"news_content"] <- content_node %>% rvest::html_text() %>% 
        stringr::str_replace_all(., "[\r\n\t\"]", "") %>% 
        stringr::str_trim() %>% 
        paste(collapse = "")
    } else {
      asf[i,"news_content"] <- NA
      print(paste("Konten tidak ditemukan untuk URL:", asf[i,"url"]))
    }
    
    # Add sleep time (10 seconds)
    Sys.sleep(10)
  } else {
    next
  }
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "tribun_bali" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "tribun_bali" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan User-Agent agar tidak terdeteksi sebagai bot
    page <- GET(asf[i, "url"], 
                user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
    
    # Jika halaman tidak bisa diakses, skip iterasi
    if (status_code(page) == 403) {
      warning(paste("403 Forbidden:", asf[i, "url"]))
      next
    }
    
    # Parse halaman HTML
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node("time")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }
    
    # Extract article title
    node_title <- parsed_html %>% html_node("h1")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "tribun_ende" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "tribun_ende" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan User-Agent agar tidak terdeteksi sebagai bot
    page <- GET(asf[i, "url"], 
                user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
    
    # Jika halaman tidak bisa diakses, skip iterasi
    if (status_code(page) == 403) {
      warning(paste("403 Forbidden:", asf[i, "url"]))
      next
    }
    
    # Parse halaman HTML
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node("time")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }
    
    # Extract article title
    node_title <- parsed_html %>% html_node("h1")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "tribun_flores" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "tribun_flores"& asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan User-Agent agar tidak terdeteksi sebagai bot
    page <- GET(asf[i, "url"], 
                user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
    
    # Jika halaman tidak bisa diakses, skip iterasi
    if (status_code(page) == 403) {
      warning(paste("403 Forbidden:", asf[i, "url"]))
      next
    }
    
    # Parse halaman HTML
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node("time")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }
    
    # Extract article title
    node_title <- parsed_html %>% html_node("h1")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "tribun_kupang" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "tribun_kupang" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan User-Agent agar tidak terdeteksi sebagai bot
    page <- GET(asf[i, "url"], 
                user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
    
    # Jika halaman tidak bisa diakses, skip iterasi
    if (status_code(page) == 403) {
      warning(paste("403 Forbidden:", asf[i, "url"]))
      next
    }
    
    # Parse halaman HTML
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node("time")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }
    
    # Extract article title
    node_title <- parsed_html %>% html_node("h1")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "tribun_toraja" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "tribun_toraja" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan User-Agent agar tidak terdeteksi sebagai bot
    page <- GET(asf[i, "url"], 
                user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
    
    # Jika halaman tidak bisa diakses, skip iterasi
    if (status_code(page) == 403) {
      warning(paste("403 Forbidden:", asf[i, "url"]))
      next
    }
    
    # Parse halaman HTML
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node("time")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }
    
    # Extract article title
    node_title <- parsed_html %>% html_node("h1")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "victory_news" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "victory_news" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan User-Agent agar tidak terdeteksi sebagai bot
    page <- GET(asf[i, "url"], 
                user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
    
    # Jika halaman tidak bisa diakses, skip iterasi
    if (status_code(page) == 403) {
      warning(paste("403 Forbidden:", asf[i, "url"]))
      next
    }
    
    # Parse halaman HTML
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node(".read__info__date")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }
    
    # Extract article title
    node_title <- parsed_html %>% html_node(".read__title")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "voi" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "voi" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan User-Agent agar tidak terdeteksi sebagai bot
    page <- GET(asf[i, "url"], 
                user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
    
    # Jika halaman tidak bisa diakses, skip iterasi
    if (status_code(page) == 403) {
      warning(paste("403 Forbidden:", asf[i, "url"]))
      next
    }
    
    # Parse halaman HTML
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node("div.col span")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }
    
    # Extract article title
    node_title <- parsed_html %>% html_node("title")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes("p")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "antara_ntt" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "antara_ntt" & asf$date_access[i] == "03/02/2025") {
    
    # Ambil halaman dengan User-Agent agar tidak terdeteksi sebagai bot
    page <- GET(asf[i, "url"], 
                user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
    
    # Jika halaman tidak bisa diakses, skip iterasi
    if (status_code(page) == 403) {
      warning(paste("403 Forbidden:", asf[i, "url"]))
      next
    }
    
    # Parse halaman HTML
    html <- content(page, as = "text", encoding = "UTF-8")
    parsed_html <- read_html(html)
    
    # Extract article publication date
    node_publish <- parsed_html %>% html_node("time")
    asf[i, "news_publish"] <- if (!is.null(node_publish)) {
      node_publish %>% html_text(trim = TRUE) %>% str_trim()
    } else { NA }
    
    # Extract article title
    node_title <- parsed_html %>% html_node("h1")
    asf[i, "news_title"] <- if (!is.null(node_title)) {
      node_title %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "")
    } else { NA }
    
    # Extract article content
    nodes_content <- parsed_html %>% html_nodes(".post-content")
    asf[i, "news_content"] <- if (length(nodes_content) > 0) {
      nodes_content %>% html_text(trim = TRUE) %>% str_replace_all("[\r\n\t\"]", "") %>% paste(collapse = " ")
    } else { NA }
    
    # Tambahkan jeda untuk menghindari rate limit
    Sys.sleep(10)
  } 
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "bali_post" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "bali_post" & asf$date_access[i] == "03/02/2025") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".td-module-date") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".entry-title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("h2,p") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences    
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "antara" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "antara" & asf$date_access[i] == "03/02/2025") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".font-weight-normal") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}


for (i in 1:nrow(asf)) {
  # Filter hanya untuk "kabar_ntt" dengan tanggal akses "03/02/2025"
  if (asf$source_web[i] == "kabar_ntt" & asf$date_access[i] == "03/02/2025") {
    #extract article publication
    asf [i,"news_publish"] <- 
      polite::bow(asf[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".font-weight-normal") %>%                  #read the codes, take all nodes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_trim()                             #remove extra spaces
    
    #extract  article title
    asf[i,"news_title"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h1") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asf[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".post-content") %>%                   #read the content codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(., "[\r\n\t\"]","") %>% #remomve line breaks and all symbols
      stringr::str_trim() %>%                         #remove extra spaces
      paste(collapse="")                              #combine all sentences
    
    #add sleep time (10 seconds)
    Sys.sleep(10)
  } else(next)
}

write.csv2(asf,"D:/010_african_swine_fever/003_media-monitoring_asf/data/indonesia/asf_news_id-raw_upd01.csv")




View(asf)
