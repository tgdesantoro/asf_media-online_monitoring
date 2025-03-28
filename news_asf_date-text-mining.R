
#1 open file CSV
library(readr)
asf <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/001-asf_news_id-raw.csv")

print(asf)
View(asf)
names(asf)
dim(asf)

media_berita <- table(asf$source_web) #melihat berbagai sumber berita
media_berita
prop.table(media_berita) #proporsi media berita
asf$news_published


#01 aktivasi package untuk text mining
install.packages("dplyr")
install.packages("stringr")
install.packages("tidytext")
install.packages("magrittr")
install.packages("lubridate")

library(dplyr)       #manipulasi data
library(stringr)     #manipulasi teks
library(stringi)
library(tidytext)    #olahdata
library(magrittr)    #aktivasi pipe (fungsi dalam fungsi) 
library(lubridate)   #kelola data menjadi format tanggal

#02review data
typeof(asf$news_publish)
publication_news <- asf$news_publish
publication_news

# Hitung jumlah NA atau blank
qty_na_blank <- sum(is.na(publication_news) | publication_news == "")

# Cetak hasil
cat("Jumlah total NA atau blank pada kolom news_publish:", qty_na_blank, "\n")

#hapus nilai NA dan blank
publication_clean <- asf %>% 
  filter(!is.na(publication_news) & publication_news != "")

View(publication_clean)
publication_news <- publication_clean$news_publish
publication_news

#cleaning date process
cleaning_datepublication01 <- tolower(publication_news)

# Pastikan teks dalam kolom publication_news memiliki encoding UTF-8
publication_news <- stri_enc_toutf8(publication_news, validate = TRUE)

# Cari baris yang memiliki encoding tidak valid
invalid_rows <- which(!stri_enc_isutf8(publication_news))

# Lihat contoh baris yang bermasalah
print(publication_news[invalid_rows])

# Hapus karakter non-printable dari publication_news
publication_news <- stri_replace_all_regex(publication_news, "[^\\p{Print}]", "")

# Konversi ke UTF-8 setelah pembersihan
publication_news <- stri_enc_toutf8(publication_news, validate = TRUE)

print(publication_news)

cleaning_datepublication01 <- tolower(publication_news) %>% 
  gsub("\\bdi\\s|\\btayang\\b", "", .) %>%                                 # menghilangkan kata "tayang" dan "di" 
  gsub("\\bkompas\\b", "", .) %>%                                          # menghilangkan kata "kompas"
  gsub("\\bdiperbarui\\b", "", .) %>%                                      # menghilangkan kata "diperbarui"
  gsub("\\bdiposting\\b", "", .) %>%                                       # menghilangkan kata "diposting"
  gsub("\\bterbit\\b", "", .) %>%                                          # menghilangkan kata "terbit"
  gsub("\\bcom\\b", "", .) %>%                                             # menghilangkan kata "com"
  gsub("\\btv\\b", "", .) %>%                                              # menghilangkan kata "tv"
  gsub("\\banimalium\\b", "", .) %>%                                       # menghilangkan kata "animalium"
  gsub("\\bredaksi\\b", "", .) %>%                                         # menghilangkan kata "redaksi"
  gsub("\\b(senin|selasa|rabu|kamis|jumat|sabtu|minggu)\\b", "", .) %>%    # menghilangkan "nama-nama hari"
  gsub("\\s\\d{2}:\\d{2}\\s*[a-zA-Z]*", "", .) %>%                         # hilangkan jam dengan pola dua digit
  gsub("\\s\\d{1,2}:\\d{2}\\s*[a-zA-Z]*", "", .) %>%                       # hilangkan jam dengan pola satu digit
  trimws(.)                                                                # hapus spasi berlebih

print(cleaning_datepublication01)

cleaning_datepublication01 <- cleaning_datepublication01 %>% 
  gsub("[|,]", "", .)  # Hapus karakter "|" dan ","  

cleaning_datepublication01 <- cleaning_datepublication01 %>% 
  str_replace_all("(\\d{2})-(\\w{3})-(\\d{2})", function(x) {  
    format(as.Date(x, format = "%d-%b-%y"), "%d/%m/%Y")  # Ubah ke format "DD/MM/YYYY"
  })

cleaning_datepublication01 <- cleaning_datepublication01 %>% 
  str_replace_all("(\\d{2})/(\\d{2})/(\\d{4})", function(x) {  # Ubah format '12/06/2022' → '12 Juni 2022'
    as.Date(x, format = "%d/%m/%Y") %>%
      format("%d %B %Y")
  }) %>% 
  str_replace_all("(\\d{2})/(\\d{2})/(\\d{2})", function(x) {  # Ubah format '12/06/22' → '12 Juni 2022'
    as.Date(x, format = "%d/%m/%Y") %>%
      format("%d %B %Y")
  })

cleaning_datepublication01 <- cleaning_datepublication01 %>% 
  gsub("\\s\\d{2}:\\d{2}\\s*[a-zA-Z]*", "", .) %>%                         # hilangkan jam dengan pola dua digit
  gsub("\\s\\d{1,2}:\\d{2}\\s*[a-zA-Z]*", "", .) %>%                       # hilangkan jam dengan pola satu digit
  gsub("[[:punct:]]", "", .) %>%                                           # hilangkan tanda baca
  trimws(.)                                                            # hapus spasi berlebih

print(cleaning_datepublication01)

#menyamakan nama bulan dan format tanggal dua angka
cleaning_datepublication02 <-
  gsub("(\\d{2})([a-zA-Z]{3})(\\d{2})", "\\1 \\2 \\3", cleaning_datepublication01) %>%  # Memisahkan format tanggal yang menyatu, seperti "30sep23" menjadi "30 sep 23"
  gsub("\\b(jan|january|januari)\\b", "january", .) %>% 
  gsub("\\b(feb|february|februari)\\b", "february", .) %>%
  gsub("\\b(mar|maret|march)\\b", "march", .) %>%
  gsub("\\b(apr)\\b", "april", .) %>%
  gsub("\\b(mei|may)\\b", "may", .) %>%
  gsub("\\b(jun|juni|june)\\b", "june", .) %>% 
  gsub("\\b(juli|july)\\b", "july", .) %>%
  gsub("\\b(aug|agustus|august)\\b", "august", .) %>% 
  gsub("\\b(sept|sep|spetember|september)\\b", "september", .) %>% 
  gsub("\\b(oct|okt|october|oktober)\\b", "october", .) %>%
  gsub("\\b(nov)\\b", "november", .) %>%
  gsub("\\b(december|desember)\\b", "december", .) %>% 
  gsub("\\b(\\d)\\s", "0\\1 ", .) %>%  # Menyamakan format angka tanggal menjadi dua digit, seperti 1 august 2023 menjadi "01 august 2023"
  gsub("(\\d{2}\\s\\w+\\s)(\\d{2})$", "\\120\\2", .) %>%  # Menyamakan format tahun, seperti "30 september 23" menjadi "30 september 2023"
  tolower() %>%  # Mengubah semua huruf menjadi kecil
  gsub("\\s+", " ", .) %>%  # Menghilangkan spasi berlebih
  trimws(.) %>%  # Menghapus spasi di awal dan akhir
  gsub("(\\d{2} \\w+ \\d{4})\\s+\\d+$", "\\1", .)  # Menghapus angka tambahan setelah tanggal yang valid
                      
cleaning_datepublication02 <- cleaning_datepublication02 %>% 
str_replace_all("(\\w+) (\\d{1,2}) (\\d{4})",                            # ubah format "month day year"
                function(x) format(as.Date(x, format = "%B %d %Y"), "%d %B %Y"))

cleaning_datepublication02


cleaning_datepublication03 <- cleaning_datepublication02 %>% 
  str_extract(., "\\b\\d{1,2} \\w+ \\d{4}\\b")                      # mencari teks berformat dd mm yy   

cleaning_datepublication03

#mengubah kolom tanggal dari "character" ke "date" 
publication_clean$clean_date <- as.Date(cleaning_datepublication03, format = "%d %B %Y") #kolom tanggal akan tetap tersimpan sebagai tipe data character

write.csv2(publication_clean,"D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/001-asf_news_id-raw-date.csv")

asf_date <- read.csv2("D:/015_africanswinefever_rf/003_sm_asf/data/asf_cleandate01.csv")
View(asf_date)
str(asf_date$clean_date01)

asf_date <- asf_date %>% filter(!is.na(clean_date01)) #menghapus kolom dengan nilai NA
str(asf_date)
asf_date$clean_date01 <- as.Date(asf_date$clean_date01, format = "%Y-%m-%d") #kolom diubah menjadi data "date"
str(asf$clean_date01)
jumlah_berita <- 
  asf_date %>%
  group_by(tahun = format(clean_date01,"%Y")) %>%
  summarise(jumlah = n())
print(jumlah_berita)

library(ggplot2)
ggplot(jumlah_berita, aes(x = tahun, y = jumlah)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Jumlah Berita Tiap Tahun",
       x = "Tahun",
       y = "Jumlah Berita") +
  theme_minimal()

#################################################################################################################################

