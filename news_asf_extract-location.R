

library(readr)
asf <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/001-asf_news_id-raw-content-ft.csv")
lexicon_regind <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/lexicon/lexcion_regind.csv")

library(dplyr)       #manipulasi data
library(stringr)     #manipulasi teks
library(tidytext)    #olahdata
library(magrittr)    #aktivasi pipe (fungsi dalam fungsi)
library(stringi)     #manipulasi dan pengolahan string
library(purrr)      #deteksi kesalahan atau nilai NA
library(tidyr)      #megubah format data

asf <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/002-asf-regency.csv")

colnames(asf)
colnames(lexicon_regind)

# Buat kolom baru untuk menyimpan hasil pencarian kabupaten
asf$regency <- NA

daftar_kabupaten <- unique(na.omit(trimws(lexicon_regind$regency)))
daftar_kabupaten <- daftar_kabupaten[daftar_kabupaten != ""]
  
  
# Cari kabupaten (regency) yang muncul dalam teks
extract_kabupaten <- function(text, daftar_kabupaten) {
  if (is.na(text) || trimws(text) == "") return(NA)
  
  ditemukan <- daftar_kabupaten[str_detect(
    tolower(text), 
    fixed(tolower(daftar_kabupaten))
  )]
  
  if (length(ditemukan) == 0) {
    return(NA)
  } else {
    return(paste(unique(ditemukan), collapse = ", "))
  }
}




asf <- asf %>%
  mutate(regency = sapply(content_rs, extract_kabupaten, daftar_kabupaten = daftar_kabupaten))

asf <- asf %>%
  mutate(regency = tolower(regency))

# Memisahkan regency menjadi baris baru jika ada lebih dari satu regency
asf_expanded <- asf %>%
  separate_rows(regency, sep = ",\\s*") %>% # Pisahkan kabupaten01 menjadi baris baru
  mutate(regency = str_trim(regency))  # Trim spasi tambahan di setiap baris

write.csv2(asf, "D:/010_african_swine_fever/003_media-monitoring_asf/data/asf-regency.csv")

##validasi regency-proses##
library(dplyr)
library(stringr)

# Pastikan nama kabupaten dan provinsi di regional tidak kosong
regional_clean <- lexicon_regind %>%
  filter(!is.na(province), !is.na(regency)) %>%
  mutate(
    province = tolower(trimws(province)),
    regency = tolower(trimws(regency)),
    valid_pair = paste(province, regency, sep = "_")
  )

# Siapkan asf_expanded untuk verifikasi
asf_expanded <- asf_expanded %>%
  mutate(
    province_clean = tolower(trimws(province)),
    regency_clean = tolower(trimws(regency)),
    pair_check = paste(province_clean, regency_clean, sep = "_"),
    validasi_regency = ifelse(pair_check %in% regional_clean$valid_pair, "benar", "salah")
  )
View(asf_expanded)


write.csv2(asf_expanded, "D:/010_african_swine_fever/003_media-monitoring_asf/data/asf-regency-valid.csv")

# Hapus baris yang tidak valid
asf_regency_valid <- asf_expanded %>%
  filter(validasi_regency == "benar")


View(asf_regency_valid)
colnames(asf)
colnames(lexicon_regind)

## Buat kolom baru untuk menyimpan hasil pencarian kecamatan (sub_district)
# Buat kolom baru untuk menyimpan hasil pencarian sub_district
asf_regency_valid$sub_district <- NA

# Ambil daftar nama subdistrict (kecamatan) dari lexicon
daftar_subdistrict <- unique(na.omit(trimws(lexicon_regind$subdistrict)))
daftar_subdistrict <- daftar_subdistrict[daftar_subdistrict != ""]
daftar_subdistrict

# Fungsi ekstrak subdistrict
extract_subdistrict <- function(text, daftar_subdistrict) {
  if (is.na(text) || trimws(text) == "") return(NA)
  
  ditemukan <- daftar_subdistrict[str_detect(
    tolower(text), 
    fixed(tolower(daftar_subdistrict))
  )]
  
  if (length(ditemukan) == 0) {
    return(NA)
  } else {
    return(paste(unique(ditemukan), collapse = ", "))
  }
}

# Terapkan ke kolom content_rs
asf_subdistrict <- asf_regency_valid %>%
  mutate(sub_district = sapply(content_rs, extract_subdistrict, daftar_subdistrict = daftar_subdistrict)) %>%
  mutate(sub_district = tolower(sub_district))  # Ubah jadi huruf kecil

View(asf_subdistrict)

# Memisahkan subdistrict menjadi baris baru jika ada lebih dari satu subdistrict
asf_subdistrict_expanded <- asf_subdistrict %>%
  separate_rows(sub_district, sep = ",\\s*") %>% # Pisahkan kabupaten01 menjadi baris baru
  mutate(sub_district = str_trim(sub_district))  # Trim spasi tambahan di setiap baris

View(asf_subdistrict_expanded)

#validasi subdistrict
# 1. Buat pasangan valid kecamatan-kabupaten-provinsi dari lexicon
valid_kecamatan_combo <- lexicon_regind %>%
  filter(!is.na(subdistrict), !is.na(regency), !is.na(province)) %>%
  mutate(
    subdistrict = tolower(trimws(subdistrict)),
    regency = tolower(trimws(regency)),
    province = tolower(trimws(province)),
    subdistrict_regency_province = paste(subdistrict, regency, province, sep = "_")
  )

# 2. Buat kombinasi sama di asf_expanded
asf_subdistrict_expanded <- asf_subdistrict_expanded %>%
  mutate(
    subdistrict_clean = tolower(trimws(sub_district)),
    pair_check_subdistrict = paste(subdistrict_clean, regency_clean, province_clean, sep = "_"),
    
    validasi_subdistrict = ifelse(
      pair_check_subdistrict %in% valid_kecamatan_combo$subdistrict_regency_province,
      "benar", "salah"
    )
  )

View(asf_subdistrict_expanded)

write.csv2(asf_subdistrict_expanded, "D:/010_african_swine_fever/003_media-monitoring_asf/data/002-asf-subdistrict-valid.csv")

library(dplyr)

asf_filtered01 <- asf_subdistrict_expanded %>%
  group_by(doc_id, province, regency) %>%
  mutate(
    keep_flag = case_when(
      validasi_regency == "benar" & validasi_subdistrict == "benar" ~ TRUE,                       # Biarkan
      validasi_regency == "benar" & validasi_subdistrict == "salah" ~ row_number() == 1,          # Simpan hanya satu
      TRUE ~ FALSE                                                                         # Hapus sisanya
    )
  ) %>%
  ungroup() %>%
  filter(keep_flag) %>%
  select(-keep_flag)  # Hapus kolom bantu

View(asf_filtered01)

write.csv2(asf_filtered01, "D:/010_african_swine_fever/003_media-monitoring_asf/data/002-asf-subdistrict-valid-ft.csv")

## Buat kolom baru untuk menyimpan hasil pencarian desa (village)
# Buat kolom baru untuk menyimpan hasil pencarian village
asf_filtered01$village <- NA


# Ambil daftar nama subdistrict (kecamatan) dari lexicon
daftar_village <- unique(na.omit(trimws(lexicon_regind$village)))
daftar_village <- daftar_village[daftar_village != ""]
daftar_village

# Fungsi ekstrak subdistrict
extract_village <- function(text, daftar_village) {
  if (is.na(text) || trimws(text) == "") return(NA)
  
  ditemukan <- daftar_village[str_detect(
    tolower(text), 
    fixed(tolower(daftar_village))
  )]
  
  if (length(ditemukan) == 0) {
    return(NA)
  } else {
    return(paste(unique(ditemukan), collapse = ", "))
  }
}

# Terapkan ke kolom content_rs
asf_village <- asf_filtered01 %>%
  mutate(village = sapply(content_rs, extract_village, daftar_village = daftar_village)) %>%
  mutate(village = tolower(village))  # Ubah jadi huruf kecil

View(asf_village)

# Memisahkan village menjadi baris baru jika ada lebih dari satu subdistrict
asf_village_expanded <- asf_village %>%
  separate_rows(village, sep = ",\\s*") %>% # Pisahkan kabupaten01 menjadi baris baru
  mutate(village = str_trim(village))  # Trim spasi tambahan di setiap baris

View(asf_village_expanded)

#validasi village
# 1. Buat pasangan valid village-subdistrict-kabupaten-provinsi dari lexicon
valid_village_combo <- lexicon_regind %>%
  filter(!is.na(village),!is.na(subdistrict), !is.na(regency), !is.na(province)) %>%
  mutate(
    village = tolower(trimws(village)),
    subdistrict = tolower(trimws(subdistrict)),
    regency = tolower(trimws(regency)),
    province = tolower(trimws(province)),
    village_subdistrict_regency_province = paste(village, subdistrict, regency, province, sep = "_")
  )

# 2. Buat kombinasi sama di asf_expanded
asf_village_expanded <- asf_village_expanded %>%
  mutate(
    village_clean = tolower(trimws(village)),
    pair_check_village = paste(village_clean, subdistrict_clean, regency_clean, province_clean, sep = "_"),
    
    validasi_village = ifelse(
      pair_check_village %in% valid_village_combo$village_subdistrict_regency_province,
      "benar", "salah"
    )
  )

View(asf_village_expanded)

write.csv2(asf_village_expanded, "D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/002-asf-village-valid.csv")

library(dplyr)

asf_filtered02 <- asf_village_expanded %>%
  group_by(doc_id, province, regency) %>%
  mutate(
    keep_village = case_when(
      # Kondisi 1
      validasi_regency == "benar" & validasi_subdistrict == "salah" & validasi_village == "salah" ~ row_number() == 1,
      
      # Kondisi 2
      validasi_regency == "benar" & validasi_subdistrict == "benar" & validasi_village == "salah" ~ row_number() == 1,
      
      # Kondisi 3
      validasi_regency == "benar" & validasi_subdistrict == "benar" & validasi_village == "benar" ~ TRUE,
      
      # Sisanya tidak disimpan
      TRUE ~ FALSE
    )
  ) %>%
  ungroup() %>%
  filter(keep_village) %>%
  select(-keep_village)  # Hapus kolom bantu

View(asf_filtered02)

write.csv2(asf_filtered02, "D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/002-asf-village-valid-ft.csv")


##selective data##
library(readr)
asf_region <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/002-asf-village-valid-ft.csv")
View(asf_region)

library(magrittr)
library(dplyr)
library(stringr)
library(stringi)

asf_region01 <- asf_region %>%
  mutate(
    village = ifelse(validasi_village == "salah", "undetected", village)
  )
View(asf_region01)


asf_region02 <- asf_region01 %>%
  mutate(
    sub_district = ifelse(validasi_subdistrict == "salah", "undetected", sub_district)
  )
View(asf_region02)

library(dplyr)

# Seleksi baris berdasarkan prioritas village, lalu sub_district
library(dplyr)

# Step 1: Tandai baris yang 'undetected' untuk sub_district dan village
asf_tagged <- asf_region02 %>%
  mutate(
    is_undetected_sub_village = sub_district == "undetected" & village == "undetected",
    is_detected_subdistrict = sub_district != "undetected"
  )

# Step 2: Seleksi logika berdasarkan doc_id + clean_date
asf_selected01 <- asf_tagged %>%
  group_by(doc_id, clean_date, regency) %>%
  # Prioritaskan baris yang punya sub_district terdeteksi
  arrange(desc(is_detected_subdistrict)) %>%
  # Ambil hanya satu baris per group
  slice(1) %>%
  ungroup()

View(asf_selected01)

asf_selected01 <- asf_selected01 %>%
  select(doc_id, species, clean_date, province, regency, sub_district, village, validasi_regency, validasi_subdistrict, validasi_village, source_web, url, key_word, date_access, news_publish, news_title, news_content, content_rs, pair_check, pair_check_subdistrict, pair_check_village)

View(asf_selected01)

write.csv2(asf_selected01, "D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/003-asf-selected-location.csv")

##selective part2##
library(dplyr)

asf_selected02 <- asf_selected01 %>%
  group_by(clean_date, regency, sub_district) %>%
  mutate(row_count = n()) %>%
  ungroup() %>%
  group_by(clean_date, regency) %>%
  # Untuk setiap kombinasi clean_date + regency
  mutate(
    duplicated_subdistrict = duplicated(sub_district) | duplicated(sub_district, fromLast = TRUE)
  ) %>%
  ungroup() %>%
  group_by(clean_date, regency, sub_district) %>%
  mutate(
    row_rank = row_number()
  ) %>%
  ungroup() %>%
  # Seleksi logika:
  filter(
    # Biarkan semua jika sub_district berbeda
    !duplicated_subdistrict |
      # Jika sub_district sama, ambil hanya satu
      (duplicated_subdistrict & row_rank == 1)
  ) %>%
  select(-row_count, -duplicated_subdistrict, -row_rank)

View(asf_selected02)

write.csv2(asf_selected02, "D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/003-asf-selected-same-date.csv")


library(dplyr)
library(lubridate)
library(purrr)

# Pastikan kolom tanggal dalam format Date
asf_selected03 <- asf_selected02 %>%
  mutate(clean_date = as.Date(clean_date, format = "%d/%m/%Y"))

# Urutkan data terlebih dahulu
asf_ordered <- asf_selected03 %>%
  arrange(clean_date)

# Lakukan seleksi berdasarkan sliding window 5 hari
final_selected <- map_dfr(1:nrow(asf_ordered), function(i) {
  current_row <- asf_ordered[i, ]
  current_date <- current_row$clean_date
  
  # Buat window ±2 hari
  window_df <- asf_ordered %>%
    filter(clean_date >= current_date - 2,
           clean_date <= current_date + 2)
  
  regency_counts <- window_df %>%
    count(regency) %>%
    filter(n > 1) %>%
    pull(regency)
  
  if (length(regency_counts) == 0) {
    # Tidak ada regency yang sama -> pertahankan semua
    return(current_row)
  } else {
    if (current_row$regency %in% regency_counts) {
      same_regency_rows <- window_df %>%
        filter(regency == current_row$regency)
      
      # Jika ada sub_district yang bukan undetected, pilih salah satunya
      if (any(same_regency_rows$sub_district != "undetected")) {
        # Pertahankan baris dengan sub_district bukan undetected pertama
        first_detected <- same_regency_rows %>%
          filter(sub_district != "undetected") %>%
          slice(1)
        if (identical(current_row, first_detected)) {
          return(current_row)
        }
      } else {
        # Semua undetected, pertahankan satu saja
        first_one <- same_regency_rows %>% slice(1)
        if (identical(current_row, first_one)) {
          return(current_row)
        }
      }
    } else {
      # regency-nya unik di window ini → pertahankan
      return(current_row)
    }
  }
  return(NULL)  # sisanya dibuang
})

View(final_selected)

write.csv2(final_selected, "D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/003-asf-selected-range5days.csv")





















