
#1 open file CSV
library(readr)
asf <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/002_asf_source_web-rf-1.csv")


View(asf)     #see table of content
names(asf)    #see name of coloumn 
dim(asf)

berita_asf <- table(asf$source_web) #melihat berbagai sumber berita
berita_asf
prop.table(berita_asf) #proporsi media berita

#2.1 install packages that used
install.packages("dplyr")
install.packages("polite")
install.packages("rvest")
install.packages("magrittr")
install.packages("stringr")

#2.2 activate packages that used
library(dplyr)
library(polite)
library(rvest)
library(magrittr)
library(stringr)

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

for (i in 1:nrow(asfv)) {
  #for scrape web source from detik_bali
  if(asfv$source_web[i] == "detik_bali") {
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
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep44.csv")

for (i in 1:nrow(asfv)) {
  if(asfv$source_web[i] == "kompas_id") {
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_nodes("time") %>%                  #read the codes, take all nodes
      rvest::html_attr("datetime") %>%              # choose atribut 'datetime'
      stringr::str_trim()                             #remove extra spaces

    #extract  article title
    asfv[i,"news_title"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".ksm-15b") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asfv[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
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

for (i in 1:nrow(asfv)) {
  #for scrape web source from republik
  if(asfv$source_web[i] == "republik") {
    #extract article publication
    asfv [i,"news_publish"] <- 
      polite::bow(asfv[i,"url"]) %>%                   #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".date-item__headline") %>%                  #read the codes, take all nodes
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

#1 open file CSV
library(readr)
asf <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep54.csv")


View(asf)     #see table of content
names(asf)    #see name of coloumn 
dim(asf)

berita_asf <- table(asf$source_web) #melihat berbagai sumber berita
berita_asf
prop.table(berita_asf) #proporsi media berita

#2.1 install packages that used
install.packages("dplyr")
install.packages("polite")
install.packages("rvest")
install.packages("magrittr")
install.packages("stringr")

#2.2 activate packages that used
library(dplyr)
library(polite)
library(rvest)
library(magrittr)
library(stringr)    #span[data-qa-id='publish-date']   

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
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node("h2") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
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
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep55.csv")

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
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
      polite::scrape(.) %>%                           #scrape the content of authorized page
      rvest::html_node(".entry-title") %>%                      #read the title codes
      rvest::html_text() %>%                          #change in to text format
      stringr::str_replace_all(.,"[\r\n\t\"]", "") %>% #remomve line breaks and all symbols
      stringr::str_trim()                             #remove extra spaces
    
    #extract article content
    asf[i,"news_content"] <- 
      polite::bow(asfv[i,"url"]) %>%            #introduce yourself to the host
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
write.csv2(asfv,"D:/010_african_swine_fever/003_media-monitoring_asf/data/asf_newsstep55.csv")


