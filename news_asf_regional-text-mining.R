library(readr)
asf <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/001-asf_news_id-raw-date-ft.csv")
 

View(asf)
Encoding(asf$news_content)
news_content <- asf$news_content

View(lexicon_regind)

install.packages("tidyr")
library(dplyr)       #manipulasi data
library(stringr)     #manipulasi teks
library(tidytext)    #olahdata
library(magrittr)    #aktivasi pipe (fungsi dalam fungsi)
library(stringi)     #manipulasi dan pengolahan string
library(purrr)      #deteksi kesalahan atau nilai NA
library(tidyr)      #megubah format data



#standardization teks pada kolom
asf$content01 <- news_content %>%
  iconv(., from = "latin1", to = "UTF-8") %>%              # converted encoding from Latin-1 to UTF-8
  iconv(., from = "WINDOWS-1252", to = "UTF-8") %>%        # converted encoding from WINDOWS-1252 to UTF-8
  stri_trans_general(., "latin-ascii") %>%                 # waste character problematic encoding
  str_to_lower() %>%                                       # change text to lowercase
  str_replace_all("[[:punct:]]", "") %>%                   # remove punctuation
  stri_replace_all_regex("\\d+", "") %>%                   # remove number with stringi
  stri_replace_all_regex(., "[^\\p{L}\\p{N}\\s]", "") %>%  # waste non-ASCII character
  stri_replace_all_regex("[^[:print:]]", "") %>%           # waste character non-printable with stringi
  stri_encode(., from = "", to = "UTF-8") %>%              # converted text to true UTF-8
  str_trim()                                               # reduce spaces over

View(asf)

# Hapus baris yang memiliki NA atau hanya berisi string kosong pada kolom 'news_content'
asf <- asf %>% filter(!is.na(news_content) & news_content != "")

View(asf)

#1st step tokenize unigram
content_unigram <- asf %>%
  mutate(line_id = row_number()) %>%                                      #Buat ID baris agar urutan tetap terjaga
  unnest_tokens(word, content01, token = "words", to_lower = TRUE) %>%    #Tanpa pembersihan stop_words !!! token = "words"/1 words| token = "ngrams", n = 2,3, ., etc
  group_by(doc_id) %>%
  mutate(word_position = row_number()) %>%                                #Menyimpan posisi kata dalam dokumen
  ungroup()

#membersihkan kata yang tidak berarti
stopwords_id <- readLines("D:/010_african_swine_fever/003_media-monitoring_asf/data/lexicon/ID-Stopwords-master/id.stopwords.02.01.2016.txt") 

# Menghapus stopwords dari barisan kata
content_unigram_ft <- filter(content_unigram,!(word %in% stopwords_id))

#analisis tf-idf "melihat kepentingan kata"
tfidf_first_content <- content_unigram_ft %>%
  count(doc_id, word, sort = TRUE) %>%  # Menghitung TF (frekuensi kata per dokumen) no itu nomor artikel
  bind_tf_idf(word, doc_id, n)          # Menghitung TF-IDF

View(tfidf_first_content)

# Hapus unigram berupa huruf terdiri dari satu hingga tiga, seperti 'a', 'aa', 'aaa'
content_unigram01 <- content_unigram_ft %>%
  filter(!str_detect(word, "^([a-zA-Z])\\1{0,2}$")) %>%
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) %>%
  mutate(word = trimws(word))

View(content_unigram01)

# Menghapus awalan 'aaaoe' dari tiap unigram
content_unigram01 <- content_unigram01 %>%
  mutate(word = str_replace(word, "^aaaoe|^aaa", "")) %>%
  mutate(word = na_if(word, "")) %>%  # Ubah string kosong menjadi NA
  mutate(word = trimws(word))         # Hilangkan spasi ekstra

View(content_unigram01)

# Menghapus 'aaaoe' di mana saja dalam kata
content_unigram01 <- content_unigram01 %>%
  mutate(word = str_replace_all(word, "aaaoe", " "))

# Menghapus 'aa' diakhir kata
content_unigram01 <- content_unigram01 %>%
  mutate(word = str_replace_all(word, "aa\\b", ""))

#mengganti kata
content_unigram02 <- content_unigram01 %>%
  mutate(word = trimws(word)) %>%  # Hapus spasi ekstra
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) %>%  # Hapus tanda baca
  mutate(word = str_replace(word, "^(desadesa|desanya)$", "desa")) %>% 
  mutate(word = str_replace(word, "^(kecamatankecamatan|bugbugkecamatan|pidpidkecamatan|
                            kecamatanini|kecamatana|kecamatannya)$", "kecamatan")) %>% 
  filter(str_squish(word) != "")

View(content_unigram02)

# **Mengembalikan ke Teks Asli Sesuai Urutan Awal**
content_restored01 <- content_unigram02 %>%
  arrange(doc_id, word_position) %>%  # Mengurutkan kembali berdasarkan posisi kata
  group_by(doc_id) %>%
  summarise(content_rs = str_c(word, collapse = " ")) %>%
  ungroup()

# Gabungkan kembali hasil tokenisasi ke database awal berdasarkan doc_id
asf_clean01 <- asf %>%
  left_join(content_restored01, by = "doc_id")

View(asf_clean01)

write.csv2(asf_clean01, "D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/001-asf_news_id-raw-content.csv")




# Buat kolom baru untuk menyimpan hasil pencarian kabupaten
asf$kabupaten01 <- NA

# Loop untuk setiap baris di content_standard
for (i in seq_along(content_standard)) {
  # Lewati jika content_standard[i] adalah NA
  if (is.na(content_standard[i])) next
  
  # Cari kabupaten yang cocok di content_standard[i]
  matches <- kabupaten_borneo02[!is.na(kabupaten_borneo02) & str_detect(content_standard[i], kabupaten_borneo02)]
  
  # Jika ditemukan kecocokan, simpan hasil di kabupaten01
  if (length(matches) > 0) {
    asf$kabupaten01[i] <- paste(matches, collapse = ", ")  # Gabungkan jika ada beberapa kecocokan
  }
}

# Memisahkan kabupaten01 menjadi baris baru jika ada lebih dari satu kabupaten
asf_expanded <- asf %>%
  separate_rows(kabupaten01, sep = ",\\s*") %>% # Pisahkan kabupaten01 menjadi baris baru
  mutate(kabupaten01 = str_trim(kabupaten01))  # Trim spasi tambahan di setiap baris

# Hasil akhir
asf_final <- asf_expanded %>%
  select(date_publish, province, source_web, url, key_word, date_access, species, kabupaten01, quantity, news_publish, news_title, news_content, clean_date01)

# Cetak hasil
print(asf_final)
View(asf_final)


write.csv2(asf_final,"D:/015_africanswinefever_rf/003_sm_asf/data/asf_kabupaten01.csv")
asf_kabupaten01 <- read.csv2("D:/015_africanswinefever_rf/003_sm_asf/data/asf_kabupaten01.csv")

View(asf_kabupaten01)

# Count frequency news of asf outbreak by kabupaten

kabupaten_freq <- asf_kabupaten01 %>%
  count(kabupaten01) %>%
  mutate(persen = n / sum(n) * 100) %>%
  arrange(desc(n))  # Mengurutkan berdasarkan frekuensi (n) secara menurun

kabupaten_freq

##############analisa frequency asf outbreak ###################################


# Menghitung frekuensi 'domestic' dan 'wild' untuk setiap kabupaten
species_comparison <- asf_kabupaten01 %>%
  group_by(kabupaten01, species) %>%         # Mengelompokkan berdasarkan kabupaten dan species
  count() %>%                               # Menghitung jumlah setiap kombinasi kabupaten dan species
  spread(species, n, fill = 0) %>%           # Memisahkan kolom 'domestic' dan 'wild' untuk frekuensi
  mutate(total = domestic + wild,            # Menambahkan kolom total frekuensi
         domestic_pct = domestic / total * 100,  # Menghitung persentase 'domestic'
         wild_pct = wild / total * 100)      # Menghitung persentase 'wild'

# Menampilkan hasil
print(species_comparison)

# Memuat paket yang diperlukan
library(dplyr)
library(lubridate)

# Menghitung frekuensi 'domestic' dan 'wild' berdasarkan tahun pada kolom clean_date01
species_comparison_yearly <- asf_kabupaten01 %>%
  mutate(year = year(clean_date01)) %>%           # Ekstrak tahun dari clean_date01
  group_by(year, species) %>%                     # Mengelompokkan berdasarkan tahun dan species
  count() %>%                                     # Menghitung jumlah setiap kombinasi tahun dan species
  spread(species, n, fill = 0) %>%                # Memisahkan kolom 'domestic' dan 'wild' untuk frekuensi
  mutate(total = domestic + wild,                 # Menambahkan kolom total frekuensi
         domestic_pct = domestic / total * 100,   # Menghitung persentase 'domestic'
         wild_pct = wild / total * 100)           # Menghitung persentase 'wild

# Memuat paket yang diperlukan
library(dplyr)
library(lubridate)
library(ggplot2)

# Menghitung frekuensi 'domestic' dan 'wild' berdasarkan tahun
species_comparison_yearly <- asf_kabupaten01 %>%
  mutate(year = year(clean_date01)) %>%           # Ekstrak tahun dari clean_date01
  group_by(year, species) %>%                     # Mengelompokkan berdasarkan tahun dan species
  count() %>%                                     # Menghitung jumlah setiap kombinasi tahun dan species
  spread(species, n, fill = 0) %>%                 # Memisahkan kolom 'domestic' dan 'wild' untuk frekuensi
  mutate(total = domestic + wild)                 # Menambahkan kolom total frekuensi

species_comparison_yearly

# Membuat grafik perbandingan jumlah frekuensi 'domestic' dan 'wild' berdasarkan tahun
ggplot(species_comparison_yearly, aes(x = as.factor(year))) +
  geom_bar(aes(y = domestic, fill = "Domestic"), stat = "identity", position = position_dodge(width = 0.9)) +  # Grafik batang untuk 'domestic'
  geom_bar(aes(y = wild, fill = "Wild"), stat = "identity", position = position_dodge(width = 0.9)) +  # Grafik batang untuk 'wild'
  geom_text(aes(y = domestic, label = domestic, group = "Domestic"), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Menambahkan label untuk 'domestic'
  geom_text(aes(y = wild, label = wild, group = "Wild"), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Menambahkan label untuk 'wild'
  labs(x = "Year", y = "Frequency", fill = "Species") +  # Label sumbu dan legenda
  scale_x_discrete(name = "Year") +  # Menambahkan nama sumbu x sebagai kategori
  theme_minimal() +  # Tema minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Memiringkan teks pada sumbu x agar mudah dibaca





