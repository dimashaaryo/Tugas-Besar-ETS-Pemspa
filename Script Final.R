# ===================== PROJECT ETS KELOMPOK 17 =====================
# Pemrograman Spasial A
# Analisis Indeks Spektral & Klasifikasi Tutupan Lahan - Kab. Banyumas

# 0. ===== PERBAIKI KONFLIK PROJ (POSTGRESQL) =====
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("PROJ_DATA")

# 1. ===== LOAD PACKAGE =====
library(terra)
library(sf)
library(dplyr)
library(knitr)

# 2. ===== ATUR PATH & CEK NAMA FILE =====
safe    <- "C:/Users/faird/OneDrive/Documents/ETS_Project_Pemspa/Data/Sentinel/S2A_MSIL2A_20261014T025151_N0513_R132_T49MCN_20261014T073215.SAFE"
dir_shp <- "C:/Users/faird/OneDrive/Documents/ETS_Project_Pemspa/Data/SHP"

semua_dir <- list.dirs(safe, recursive = TRUE, full.names = TRUE)
dir10 <- semua_dir[grepl("IMG_DATA/R10m$", semua_dir)]
dir20 <- semua_dir[grepl("IMG_DATA/R20m$", semua_dir)]

# Folder penyimpanan hasil
folder_out <- "data/Hasil"

# Buat folder jika belum tersedia
if (!dir.exists(folder_out)) {
  dir.create(folder_out, recursive = TRUE)
}

# Cek lokasi output
print(folder_out)
dir.exists(folder_out)

# 3. ===== BACA BAND SENTINEL-2 =====
# Band: B02 Blue, B03 Green, B04 Red, B08 NIR, B11 SWIR1
ambil_band <- function(kode, resolusi = "10m") {
  file_band <- list.files(
    safe_dir,
    pattern = paste0(kode, "_", resolusi, "\\.jp2$"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(file_band) == 0) {
    stop(paste("Band", kode, "resolusi", resolusi, "tidak ditemukan"))
  }
  
  rast(file_band[1])
}

b02 <- ambil_band("B02", "10m")
b03 <- ambil_band("B03", "10m")
b04 <- ambil_band("B04", "10m")
b08 <- ambil_band("B08", "10m")
b11 <- ambil_band("B11", "20m")

print(b02)
print(b03)
print(b04)
print(b08)
print(b11)

# B11 resolusinya 20 m, samakan ke 10 m
b11 <- resample(b11, b02, method = "bilinear")

# Gabungkan jadi satu raster multi-band
s2 <- c(b02, b03, b04, b08, b11)
names(s2) <- c("B02", "B03", "B04", "B08", "B11")

# Pasang CRS manual (JP2 tidak membawa CRS): UTM zona 49S
crs(s2) <- "EPSG:32749"

# 4. ===== BACA SHAPEFILE =====
kab <- st_read(file.path(dir_shp, "Kabupaten.shp"))
kec <- st_read(file.path(dir_shp, "Kecamatan.shp"))

# 5. ===== SAMAKAN CRS & FILTER KECAMATAN PEKALONGAN =====
# Samakan CRS shapefile dengan citra (UTM 49S)
kab <- st_transform(kab, crs(s2))
kec <- st_transform(kec, crs(s2))

# Ambil hanya kecamatan di Kabupaten Pekalongan (bukan Kota Pekalongan)
kec <- kec[kec$WADMKK == "Pekalongan", ]

# Ubah ke format terra
kab_v <- vect(kab)
kec_v <- vect(kec)

# Kolom WADMKC ada yang kosong, pakai NAMOBJ sebagai nama kecamatan
kec$Kecamatan <- kec$NAMOBJ
kec_v <- vect(kec)

# 6. ===== CROPPING CITRA DENGAN BATAS KABUPATEN =====
s2_crop <- crop(s2, kab_v)
s2_crop <- mask(s2_crop, kab_v)

# 7. ===== KONVERSI KE REFLEKTANSI =====
# Processing baseline >= 04.00: kurangi offset 1000, bagi 10000
s2_ref <- (s2_crop - 1000) / 10000
s2_ref <- clamp(s2_ref, 0, 1)   # nilai di luar 0-1 dipotong

# 8. ===== HITUNG INDEKS SPEKTRAL =====
B02 <- s2_ref$B02
B03 <- s2_ref$B03
B04 <- s2_ref$B04
B08 <- s2_ref$B08
B11 <- s2_ref$B11

# NDVI = (NIR - Red) / (NIR + Red)        -> vegetasi
ndvi <- (B08 - B04) / (B08 + B04)
writeRaster(ndvi, file.path(folder_out, "ndvi.tif"), overwrite = TRUE)

# NDBI = (SWIR - NIR) / (SWIR + NIR)      -> area terbangun
ndbi <- (B11 - B08) / (B11 + B08)
writeRaster(ndbi, file.path(folder_out, "ndbi.tif"), overwrite = TRUE)

# NDWI = (Green - NIR) / (Green + NIR)    -> perairan
ndwi <- (B03 - B08) / (B03 + B08)
writeRaster(ndwi, file.path(folder_out, "ndwi.tif"), overwrite = TRUE)

# NDCI = (Blue - SWIR) / (Blue + SWIR)    -> awan (asumsi, konfirmasi rumus ke dosen)
ndci <- (B02 - B11) / (B02 + B11)
writeRaster(ndci, file.path(folder_out, "ndci.tif"), overwrite = TRUE)

# Gabungkan jadi satu raster 4 layer
indeks <- c(ndvi, ndbi, ndwi, ndci)
names(indeks) <- c("NDVI", "NDBI", "NDWI", "NDCI")

# 9. ===== BAND SCL SEBAGAI PEMBANDING =====
scl <- rast(list.files(dir20, pattern = "SCL_20m.jp2$", full.names = TRUE))
crs(scl) <- "EPSG:32749"

# Potong dan samakan ke resolusi 10 m (nearest, karena data kategori)
scl_crop <- crop(scl, kab_v)
scl10 <- resample(scl_crop, indeks, method = "near",
                  filename = file.path(folder_out, "scl10.tif"), overwrite = TRUE)
scl10 <- mask(scl10, kab_v)
names(scl10) <- "SCL"

# Gabungan indeks + reflektansi biru untuk dibandingkan
bandingan <- c(indeks, B02)
names(bandingan) <- c("NDVI", "NDBI", "NDWI", "NDCI", "B02")


# 10. ===== CEK NILAI INDEKS BERDASARKAN SCL =====

# Gabungkan indeks, B02, dan SCL
data_cek <- c(indeks, B02, scl10)
names(data_cek) <- c("NDVI", "NDBI", "NDWI", "NDCI", "B02", "SCL")

# Ambil sampel piksel agar tidak terlalu berat
set.seed(17)
sampel <- spatSample(
  data_cek,
  size = 100000,
  method = "random",
  na.rm = TRUE,
  as.df = TRUE
)

# Ringkasan nilai indeks per kelas SCL
ringkasan_scl <- sampel %>%
  group_by(SCL) %>%
  summarise(
    jumlah = n(),
    NDVI_mean = mean(NDVI, na.rm = TRUE),
    NDBI_mean = mean(NDBI, na.rm = TRUE),
    NDWI_mean = mean(NDWI, na.rm = TRUE),
    NDCI_mean = mean(NDCI, na.rm = TRUE),
    B02_mean = mean(B02, na.rm = TRUE),
    .groups = "drop"
  )

print(ringkasan_scl)

write.csv(
  ringkasan_scl,
  file.path(folder_out, "ringkasan_scl.csv"),
  row.names = FALSE
)


# 11. ===== THRESHOLD AWAL KLASIFIKASI =====

# Threshold awal, dapat direvisi setelah evaluasi SCL
th_ndvi <- 0.30
th_ndbi <- 0.00
th_ndwi <- 0.00
th_ndci <- 0.10

# Lihat distribusi indeks
summary(sampel[, c("NDVI", "NDBI", "NDWI", "NDCI")])

# Hitung kuantil per indeks
kuantil <- sampel %>%
  group_by(SCL) %>%
  summarise(
    NDVI_P50 = median(NDVI, na.rm = TRUE),
    NDBI_P50 = median(NDBI, na.rm = TRUE),
    NDWI_P50 = median(NDWI, na.rm = TRUE),
    NDCI_P50 = median(NDCI, na.rm = TRUE),
    .groups = "drop"
  )

print(kuantil)

# 12. ===== KLASIFIKASI TUTUPAN LAHAN =====

# Kelas awal = NA
kelas <- ifel(!is.na(ndvi), 0, NA)

# Kelas 1: Vegetasi
kelas <- ifel(
  ndvi > th_ndvi,
  1,
  kelas
)

# Kelas 2: Bangunan
kelas <- ifel(
  ndbi > th_ndbi & ndvi <= th_ndvi,
  2,
  kelas
)

# Kelas 3: Perairan
kelas <- ifel(
  ndwi > th_ndwi,
  3,
  kelas
)

# Kelas 4: Awan berdasarkan SCL sementara
kelas <- ifel(
  scl10 %in% c(8, 9, 10),
  4,
  kelas
)

# Piksel belum terklasifikasi tetap NA
kelas <- ifel(kelas == 0, NA, kelas)

names(kelas) <- "Tutupan_Lahan"

writeRaster(
  kelas,
  file.path(folder_out, "klasifikasi_tutupan_lahan.tif"),
  overwrite = TRUE
)

# Tampilkan peta sederhana
plot(
  kelas,
  col = c("forestgreen", "red", "dodgerblue", "white"),
  main = "Klasifikasi Tutupan Lahan"
)

# 13. ===== HITUNG LUAS TUTUPAN LAHAN =====

# Hitung luas setiap piksel dalam hektare
luas_piksel <- cellSize(kelas, unit = "ha")

# Hitung total luas per kelas
luas_kelas <- zonal(
  luas_piksel,
  kelas,
  fun = "sum",
  na.rm = TRUE
)

names(luas_kelas) <- c("Kode", "Luas_Ha")

luas_kelas$Kelas <- c(
  "Vegetasi", "Bangunan", "Perairan", "Awan"
)[match(luas_kelas$Kode, 1:4)]

# Persentase dari total luas terklasifikasi
luas_kelas$Persen <- round(
  100 * luas_kelas$Luas_Ha /
    sum(luas_kelas$Luas_Ha),
  2
)

print(luas_kelas)

write.csv(
  luas_kelas,
  file.path(folder_out, "luas_tutupan_lahan.csv"),
  row.names = FALSE
)

# 14. ===== ZONAL STATISTICS PER KECAMATAN =====

# Pastikan setiap kecamatan punya ID unik
kec$ID_Kec <- seq_len(nrow(kec))

# Ubah ke terra
kec_v <- vect(kec)

# Fungsi statistik untuk setiap kecamatan
hitung_zonal <- function(raster_indeks, nama_indeks) {
  
  nilai <- extract(
    raster_indeks,
    kec_v,
    fun = function(x, ...) {
      x <- x[!is.na(x)]
      
      if (length(x) == 0) {
        return(rep(NA_real_, 5))
      }
      
      c(
        mean(x),
        min(x),
        max(x),
        median(x),
        if (length(x) > 1) sd(x) else NA_real_
      )
    }
  )
  
  # Cara yang lebih stabil: hitung statistik terpisah
  hasil <- data.frame(
    ID_Kec = kec$ID_Kec,
    Kecamatan = kec$Kecamatan
  )
  
  fungsi <- list(
    Mean = mean,
    Minimum = min,
    Maximum = max,
    Median = median,
    SD = sd
  )
  
  for (nm in names(fungsi)) {
    v <- extract(
      raster_indeks,
      kec_v,
      fun = fungsi[[nm]],
      na.rm = TRUE
    )
    
    hasil[[paste0(nama_indeks, "_", nm)]] <- v[[2]]
  }
  
  hasil
}

# Hitung untuk keempat indeks
z_ndvi <- hitung_zonal(ndvi, "NDVI")
z_ndbi <- hitung_zonal(ndbi, "NDBI")
z_ndwi <- hitung_zonal(ndwi, "NDWI")
z_ndci <- hitung_zonal(ndci, "NDCI")

# Gabungkan menjadi satu tabel
zonal_final <- z_ndvi %>%
  left_join(
    select(z_ndbi, -Kecamatan),
    by = "ID_Kec"
  ) %>%
  left_join(
    select(z_ndwi, -Kecamatan),
    by = "ID_Kec"
  ) %>%
  left_join(
    select(z_ndci, -Kecamatan),
    by = "ID_Kec"
  )

# Pembulatan angka
zonal_final <- zonal_final %>%
  mutate(across(where(is.numeric) & !all_of("ID_Kec"),
                ~ round(.x, 4)))

print(zonal_final)

write.csv(
  zonal_final,
  file.path(folder_out, "zonal_statistics_kecamatan.csv"),
  row.names = FALSE
)

# Tampilkan tabel
knitr::kable(zonal_final)

# Install hanya jika belum tersedia
paket <- c("ggplot2", "tidyterra", "ggspatial")

belum_ada <- paket[
  !sapply(paket, requireNamespace, quietly = TRUE)
]

if (length(belum_ada) > 0) {
  install.packages(belum_ada)
}

library(ggplot2)
library(tidyterra)
library(ggspatial)


# =====================================================
# 15. ===== PETA KLASIFIKASI (REVISI FINAL) =====
# =====================================================

library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(ggspatial)

# 1. Ubah raster klasifikasi menjadi kategorikal
kelas_f <- as.factor(kelas)

levels(kelas_f) <- data.frame(
  value = 1:4,
  label = c(
    "Vegetasi",
    "Bangunan",
    "Perairan",
    "Awan"
  )
)

names(kelas_f) <- "Tutupan_Lahan"

# 2. Tentukan warna setiap kelas
warna <- c(
  "Vegetasi" = "#228B22",
  "Bangunan" = "#E63946",
  "Perairan" = "#2171B5",
  "Awan" = "#D9D9D9"
)

# 3. Samakan sistem koordinat ke WGS84
kab_peta <- st_transform(kab, 4326)
kec_peta <- st_transform(kec, 4326)

kelas_peta <- project(
  kelas_f,
  "EPSG:4326",
  method = "near"
)

# 4. Tentukan batas peta
batas <- st_bbox(kab_peta)

# 5. Buat peta klasifikasi
peta_final <- ggplot() +
  
  # Raster tutupan lahan
  geom_spatraster(
    data = kelas_peta,
    maxcell = 1500000
  ) +
  
  scale_fill_manual(
    values = warna,
    name = "Tutupan Lahan",
    na.translate = FALSE,
    drop = FALSE
  ) +
  
  # Batas kecamatan
  geom_sf(
    data = kec_peta,
    fill = NA,
    color = "grey50",
    linewidth = 0.15
  ) +
  
  # Batas kabupaten
  geom_sf(
    data = kab_peta,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  
  # Atur area peta
  
  # Tampilkan longitude dan latitude
  coord_sf(
    xlim = c(batas["xmin"] - 0.025,
             batas["xmax"] + 0.025),
    ylim = c(batas["ymin"] - 0.025,
             batas["ymax"] + 0.025),
    expand = FALSE,
    datum = sf::st_crs(4326)
  ) +
  
  # Atur jarak koordinat agar tidak bertumpuk
  scale_x_continuous(
    breaks = seq(109.4, 110.0, by = 0.1),
    labels = function(x) paste0(
      sprintf("%.1f", x), "°E"
    )
  ) +
  
  scale_y_continuous(
    breaks = seq(-7.4, -6.7, by = 0.1),
    labels = function(y) paste0(
      sprintf("%.1f", abs(y)), "°S"
    )
  ) +

  
  # Skala peta
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm")
  ) +
  
  # Panah utara
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # Judul dan sumber
  labs(
    title = "PETA KLASIFIKASI TUTUPAN LAHAN",
    subtitle = "Kabupaten Banyumas, Jawa Tengah | 2026",
    caption = paste0(
      "Sumber: Sentinel-2 Level-2A, ",
      "Copernicus (2026)\n",
      "Batas Administrasi: Ina-Geoportal"
    ),
    x = NULL,
    y = NULL
  ) +
  
  # Tema peta
  theme_minimal(base_size = 11) +
  
  theme(
    plot.title = element_text(
      size = 17,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    
    plot.caption = element_text(
      size = 9,
      hjust = 0,
      margin = margin(t = 15)
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    legend.key.height = unit(0.7, "cm"),
    legend.key.width = unit(0.7, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    
    # Pengaturan koordinat
    axis.text.x = element_text(
      size = 9,
      color = "black",
      margin = margin(t = 8)
    ),
    
    axis.text.y = element_text(
      size = 9,
      color = "black",
      margin = margin(r = 8)
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.3
    ),
    
    axis.title = element_blank(),
    
    plot.margin = margin(
      t = 20,
      r = 25,
      b = 20,
      l = 25
    )
  )

# 6. Tampilkan peta
print(peta_final)

# 7. Simpan peta resolusi tinggi
ggsave(
  filename = file.path(
    folder_out,
    "Peta_Klasifikasi_Banyumas_FINAL.png"
  ),
  plot = peta_final,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

# 8. Simpan juga sebagai PDF
ggsave(
  filename = file.path(
    folder_out,
    "Peta_Klasifikasi_Banyumas_FINAL.pdf"
  ),
  plot = peta_final,
  width = 12,
  height = 10,
  units = "in",
  bg = "white"
)

cat("Peta berhasil disimpan!\n")


# =====================================================
# 16. ===== CEK SELURUH HASIL PROJECT ETS =====
# =====================================================

cat("\n===== HASIL PROJECT ETS KELOMPOK 17 =====\n")

# 1. Cek folder hasil
cat("\nLokasi folder hasil:\n")
print(normalizePath(folder_out, mustWork = FALSE))

# 2. Tampilkan seluruh file hasil
cat("\nDaftar file hasil:\n")
print(list.files(folder_out, full.names = TRUE))

# 3. Cek jumlah kecamatan
cat("\nJumlah kecamatan:\n")
print(nrow(kec))

# 4. Cek jumlah kolom zonal statistics
cat("\nJumlah kolom zonal statistics:\n")
print(ncol(zonal_final))

# 5. Cek frekuensi kelas tutupan lahan
cat("\nFrekuensi kelas tutupan lahan:\n")
frekuensi_kelas <- as.data.frame(freq(kelas))

print(frekuensi_kelas)

# 6. Cek file peta final
file_peta <- file.path(
  folder_out,
  "Peta_Klasifikasi_Banyumas_FINAL.png"
)

cat("\nApakah peta final tersedia?\n")
print(file.exists(file_peta))

cat("\n===== STEP 16 SELESAI =====\n")

# =====================================================
# 17. ===== EVALUASI KLASIFIKASI DENGAN SCL =====
# =====================================================

library(terra)
library(dplyr)
library(knitr)

# 1. Gabungkan klasifikasi dan SCL
evaluasi <- c(kelas, scl10)

names(evaluasi) <- c("Klasifikasi", "SCL")

# 2. Ambil sampel piksel
set.seed(17)

sampel_eval <- spatSample(
  evaluasi,
  size = 100000,
  method = "random",
  na.rm = TRUE,
  as.df = TRUE
)

# 3. Tentukan kelas referensi SCL
sampel_eval$Referensi <- case_when(
  sampel_eval$SCL == 4 ~ "Vegetasi",
  sampel_eval$SCL == 6 ~ "Perairan",
  sampel_eval$SCL %in% c(8, 9, 10) ~ "Awan",
  TRUE ~ NA_character_
)

# SCL kelas 5 tidak otomatis berarti bangunan

# 4. Tentukan nama kelas hasil klasifikasi
sampel_eval$Prediksi <- case_when(
  sampel_eval$Klasifikasi == 1 ~ "Vegetasi",
  sampel_eval$Klasifikasi == 2 ~ "Bangunan",
  sampel_eval$Klasifikasi == 3 ~ "Perairan",
  sampel_eval$Klasifikasi == 4 ~ "Awan",
  TRUE ~ NA_character_
)

# 5. Filter data yang memiliki referensi
data_validasi <- sampel_eval %>%
  filter(
    !is.na(Referensi),
    !is.na(Prediksi)
  )

# 6. Buat tabel perbandingan
tabel_validasi <- table(
  Referensi = data_validasi$Referensi,
  Prediksi = data_validasi$Prediksi
)

cat("\n===== TABEL PERBANDINGAN SCL =====\n")
print(tabel_validasi)

# 7. Hitung persentase kesesuaian
akurasi_awal <- if (nrow(data_validasi) > 0) {
  mean(
    data_validasi$Referensi ==
      data_validasi$Prediksi
  ) * 100
} else {
  NA_real_
}

cat(
  "\nPersentase kesesuaian dengan SCL:",
  round(akurasi_awal, 2),
  "%\n"
)

# 8. Simpan hasil evaluasi
write.csv(
  as.data.frame(tabel_validasi),
  file.path(
    folder_out,
    "evaluasi_klasifikasi_SCL.csv"
  ),
  row.names = FALSE
)

cat("\n===== STEP 17 SELESAI =====\n")

# =====================================================
# 18. ===== CEK AREA BELUM TERKLASIFIKASI =====
# =====================================================

library(terra)

# 1. Tentukan area yang memiliki data indeks
area_valid <- !is.na(ndvi)

# 2. Identifikasi piksel belum terklasifikasi
area_na <- ifel(
  area_valid & is.na(kelas),
  1,
  NA
)

names(area_na) <- "Belum_Terklasifikasi"

# 3. Hitung luas piksel dalam hektare
luas_piksel <- cellSize(
  ndvi,
  unit = "ha"
)

# 4. Hitung luas area belum terklasifikasi
luas_na <- global(
  ifel(
    area_na == 1,
    luas_piksel,
    NA
  ),
  fun = "sum",
  na.rm = TRUE
)[1, 1]

# 5. Hitung luas area valid
luas_valid <- global(
  ifel(
    area_valid,
    luas_piksel,
    NA
  ),
  fun = "sum",
  na.rm = TRUE
)[1, 1]

# 6. Hitung persentase area belum terklasifikasi
persen_na <- if (luas_valid > 0) {
  100 * luas_na / luas_valid
} else {
  NA_real_
}

# 7. Tampilkan hasil
cat("\n===== HASIL ANALISIS AREA NA =====\n")

cat(
  "Luas area valid:",
  round(luas_valid, 2),
  "Ha\n"
)

cat(
  "Luas belum terklasifikasi:",
  round(luas_na, 2),
  "Ha\n"
)

cat(
  "Persentase belum terklasifikasi:",
  round(persen_na, 2),
  "%\n"
)

# 8. Simpan raster area NA
writeRaster(
  area_na,
  file.path(
    folder_out,
    "area_belum_terklasifikasi.tif"
  ),
  overwrite = TRUE
)

# 9. Tampilkan peta area NA
plot(
  area_na,
  col = "orange",
  main = "Area Belum Terklasifikasi - Banyumas",
  axes = TRUE
)

# 10. Simpan hasil ringkasan ke CSV
ringkasan_na <- data.frame(
  Wilayah = "Kabupaten Banyumas",
  Luas_Valid_Ha = luas_valid,
  Luas_NA_Ha = luas_na,
  Persentase_NA = persen_na
)

write.csv(
  ringkasan_na,
  file.path(
    folder_out,
    "ringkasan_area_NA.csv"
  ),
  row.names = FALSE
)

cat("\n===== STEP 18 SELESAI =====\n")

# =====================================================
# 19. ===== CEK FINAL ZONAL STATISTICS =====
# =====================================================

library(dplyr)
library(knitr)

# 1. Cek jumlah kecamatan
cat("\n===== JUMLAH KECAMATAN =====\n")
print(nrow(zonal_final))

# 2. Cek jumlah kolom
cat("\n===== JUMLAH KOLOM =====\n")
print(ncol(zonal_final))

# 3. Cek nama kecamatan
cat("\n===== DAFTAR KECAMATAN =====\n")
print(zonal_final$Kecamatan)

# 4. Cek kelengkapan statistik
indeks_wajib <- c(
  "NDVI", "NDBI", "NDWI", "NDCI"
)

stat_wajib <- c(
  "Mean", "Minimum", "Maximum",
  "Median", "SD"
)

kolom_wajib <- as.vector(
  outer(
    indeks_wajib,
    stat_wajib,
    paste,
    sep = "_"
  )
)

kolom_kurang <- setdiff(
  kolom_wajib,
  names(zonal_final)
)

if (length(kolom_kurang) == 0) {
  cat("\nSemua 20 statistik tersedia!\n")
} else {
  cat("\nKolom statistik yang kurang:\n")
  print(kolom_kurang)
}

# 5. Cek nilai kosong
cat("\n===== CEK NILAI NA =====\n")
print(colSums(is.na(zonal_final)))

# 6. Cek duplikasi ID kecamatan
cat("\nJumlah ID kecamatan duplikat:\n")
print(sum(duplicated(zonal_final$ID_Kec)))

# 7. Urutkan berdasarkan nama kecamatan
zonal_final <- zonal_final %>%
  arrange(Kecamatan)

# 8. Simpan hasil final
write.csv(
  zonal_final,
  file.path(
    folder_out,
    "Zonal_Statistics_Banyumas_FINAL.csv"
  ),
  row.names = FALSE
)

# 9. Tampilkan tabel
knitr::kable(
  zonal_final,
  digits = 4,
  caption = paste(
    "Zonal Statistics Indeks Spektral",
    "Kabupaten Banyumas"
  )
)

cat("\n===== STEP 19 SELESAI =====\n")


# =====================================================
# 20. ===== FINALISASI NAMA WILAYAH PEKALONGAN =====
# =====================================================

# 1. Perbaiki judul dan subtitle peta
peta_final <- peta_final +
  labs(
    title = "PETA KLASIFIKASI TUTUPAN LAHAN",
    subtitle = "Kabupaten Pekalongan, Jawa Tengah | 2026"
  )

# 2. Tampilkan peta
print(peta_final)

# 3. Simpan peta dengan nama yang benar
ggsave(
  filename = file.path(
    folder_out,
    "Peta_Klasifikasi_Pekalongan_FINAL.png"
  ),
  plot = peta_final,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

# 4. Simpan tabel zonal statistics
write.csv(
  zonal_final,
  file.path(
    folder_out,
    "Zonal_Statistics_Pekalongan_FINAL.csv"
  ),
  row.names = FALSE
)

# 5. Perbaiki nama wilayah pada ringkasan NA
ringkasan_na$Wilayah <- "Kabupaten Pekalongan"

write.csv(
  ringkasan_na,
  file.path(
    folder_out,
    "Ringkasan_Area_NA_Pekalongan.csv"
  ),
  row.names = FALSE
)

cat("\n===== FINALISASI PEKALONGAN SELESAI =====\n")

# =====================================================
# 21. ===== CEK KESESUAIAN WILAYAH PEKALONGAN =====
# =====================================================

library(terra)
library(sf)

# 1. Cek sistem koordinat
cat("\n===== CRS DATA =====\n")

cat("\nCRS Raster:\n")
print(crs(ndvi))

cat("\nCRS Kabupaten:\n")
print(st_crs(kab)$epsg)

cat("\nCRS Kecamatan:\n")
print(st_crs(kec)$epsg)

# 2. Cek jumlah kecamatan
cat("\nJumlah kecamatan:\n")
print(nrow(kec))

# 3. Cek cakupan wilayah
cat("\n===== CAKUPAN WILAYAH =====\n")

print(ext(ndvi))
print(ext(vect(kab)))
print(ext(vect(kec)))

# 4. Visualisasi kesesuaian wilayah
plot(
  ndvi,
  main = "Pemeriksaan Wilayah Kabupaten Pekalongan"
)

plot(
  vect(kab),
  add = TRUE,
  border = "red",
  lwd = 2
)

plot(
  vect(kec),
  add = TRUE,
  border = "black",
  lwd = 0.5
)

cat("\n===== STEP 21 SELESAI =====\n")

# =====================================================
# 22. ===== LUAS TUTUPAN LAHAN PEKALONGAN =====
# =====================================================

library(terra)
library(dplyr)

# 1. Hitung luas setiap piksel (hektare)
luas_pixel <- cellSize(
  kelas,
  unit = "ha"
)

# 2. Hitung luas berdasarkan kelas
luas_tutupan <- zonal(
  luas_pixel,
  kelas,
  fun = "sum",
  na.rm = TRUE
)

names(luas_tutupan) <- c(
  "Kode",
  "Luas_Ha"
)

# 3. Tambahkan nama kelas
nama_kelas <- c(
  "Vegetasi",
  "Bangunan",
  "Perairan",
  "Awan"
)

luas_tutupan$Kelas <- nama_kelas[
  match(luas_tutupan$Kode, 1:4)
]

# 4. Hitung persentase
total_luas <- sum(
  luas_tutupan$Luas_Ha,
  na.rm = TRUE
)

luas_tutupan$Persentase <- round(
  luas_tutupan$Luas_Ha /
    total_luas * 100,
  2
)

# 5. Urutkan kelas
luas_tutupan <- luas_tutupan %>%
  arrange(Kode)

# 6. Tampilkan hasil
cat("\n===== LUAS TUTUPAN LAHAN =====\n")

print(luas_tutupan)

# 7. Simpan hasil
write.csv(
  luas_tutupan,
  file.path(
    folder_out,
    "Luas_Tutupan_Lahan_Pekalongan_FINAL.csv"
  ),
  row.names = FALSE
)

cat("\n===== STEP 22 SELESAI =====\n")

# =====================================================
# 23. ===== GRAFIK LUAS TUTUPAN LAHAN =====
# =====================================================

library(ggplot2)

# 1. Warna kelas
warna_kelas <- c(
  "Vegetasi" = "#228B22",
  "Bangunan" = "#E63946",
  "Perairan" = "#2171B5",
  "Awan" = "#BDBDBD"
)

# 2. Grafik batang
grafik_luas <- ggplot(
  luas_tutupan,
  aes(
    x = reorder(Kelas, Luas_Ha),
    y = Luas_Ha,
    fill = Kelas
  )
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        round(Persentase, 2),
        "%"
      )
    ),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = warna_kelas
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    title = "Luas Tutupan Lahan Kabupaten Pekalongan",
    subtitle = "Hasil Klasifikasi Citra Sentinel-2 Tahun 2026",
    x = NULL,
    y = "Luas (Hektare)",
    caption = "Sumber: Pengolahan Citra Sentinel-2 Level-2A"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

# 3. Tampilkan grafik
print(grafik_luas)

# 4. Simpan grafik
ggsave(
  filename = file.path(
    folder_out,
    "Grafik_Luas_Tutupan_Lahan_Pekalongan.png"
  ),
  plot = grafik_luas,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("\n===== STEP 23 SELESAI =====\n")

# =====================================================
# 24. ===== REKAPITULASI TUTUPAN LAHAN =====
# =====================================================

library(dplyr)
library(knitr)

# 1. Susun tabel rekapitulasi
rekap_tutupan <- luas_tutupan %>%
  arrange(Kode) %>%
  mutate(
    Luas_Ha = round(Luas_Ha, 2),
    Persentase = round(Persentase, 2)
  ) %>%
  select(
    Kode,
    Kelas,
    Luas_Ha,
    Persentase
  )

# 2. Tambahkan total
total_rekap <- data.frame(
  Kode = NA_integer_,
  Kelas = "TOTAL",
  Luas_Ha = sum(rekap_tutupan$Luas_Ha),
  Persentase = sum(rekap_tutupan$Persentase)
)

rekap_tutupan <- bind_rows(
  rekap_tutupan,
  total_rekap
)

# 3. Tampilkan tabel
knitr::kable(
  rekap_tutupan,
  digits = 2,
  col.names = c(
    "Kode",
    "Kelas Tutupan Lahan",
    "Luas (Ha)",
    "Persentase (%)"
  ),
  caption = paste(
    "Rekapitulasi Tutupan Lahan",
    "Kabupaten Pekalongan Tahun 2026"
  )
)

# 4. Simpan CSV
write.csv(
  rekap_tutupan,
  file.path(
    folder_out,
    "Rekap_Tutupan_Lahan_Pekalongan.csv"
  ),
  row.names = FALSE
)

cat("\n===== STEP 24 SELESAI =====\n")


# =====================================================
# 25. ===== GRAFIK INDEKS SPEKTRAL =====
# KABUPATEN PEKALONGAN
# =====================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Siapkan data rata-rata indeks
data_mean <- zonal_final %>%
  select(
    Kecamatan,
    NDVI_Mean,
    NDBI_Mean,
    NDWI_Mean,
    NDCI_Mean
  )

# 2. Ubah data menjadi format panjang
data_panjang <- data_mean %>%
  pivot_longer(
    cols = -Kecamatan,
    names_to = "Indeks",
    values_to = "Nilai"
  )

# 3. Ubah nama indeks
data_panjang$Indeks <- factor(
  data_panjang$Indeks,
  levels = c(
    "NDVI_Mean",
    "NDBI_Mean",
    "NDWI_Mean",
    "NDCI_Mean"
  ),
  labels = c(
    "NDVI - Vegetasi",
    "NDBI - Area Terbangun",
    "NDWI - Perairan",
    "NDCI - Awan"
  )
)

# 4. Atur urutan nama kecamatan
data_panjang$Kecamatan <- factor(
  data_panjang$Kecamatan,
  levels = sort(unique(data_panjang$Kecamatan))
)

# 5. Tentukan warna setiap indeks
warna_indeks <- c(
  "NDVI - Vegetasi" = "#2E8B57",
  "NDBI - Area Terbangun" = "#E76F51",
  "NDWI - Perairan" = "#3182BD",
  "NDCI - Awan" = "#9467BD"
)

# 6. Buat grafik
grafik_indeks_final <- ggplot(
  data_panjang,
  aes(
    x = Nilai,
    y = Kecamatan,
    fill = Indeks
  )
) +
  
  geom_col(
    width = 0.65
  ) +
  
  facet_wrap(
    ~Indeks,
    ncol = 2,
    scales = "free_x"
  ) +
  
  scale_fill_manual(
    values = warna_indeks
  ) +
  
  # Garis referensi nilai nol
  geom_vline(
    xintercept = 0,
    color = "grey35",
    linewidth = 0.4
  ) +
  
  labs(
    title = "RATA-RATA INDEKS SPEKTRAL PER KECAMATAN",
    subtitle = "Kabupaten Pekalongan, Jawa Tengah | 2026",
    x = "Nilai Rata-Rata Indeks",
    y = NULL,
    caption = paste(
      "Sumber: Pengolahan Citra Sentinel-2 Level-2A",
      "| Kelompok 17"
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20)
    ),
    
    strip.text = element_text(
      size = 12,
      face = "bold",
      margin = margin(b = 10)
    ),
    
    axis.text.y = element_text(
      size = 9,
      color = "black"
    ),
    
    axis.text.x = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 15)
    ),
    
    panel.spacing.x = unit(2, "cm"),
    panel.spacing.y = unit(2, "cm"),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "none",
    
    plot.caption = element_text(
      size = 9,
      hjust = 1,
      margin = margin(t = 20)
    ),
    
    plot.margin = margin(
      t = 25,
      r = 30,
      b = 25,
      l = 25
    )
  )

# 7. Tampilkan grafik
print(grafik_indeks_final)

# 8. Simpan PNG ukuran besar
ggsave(
  filename = file.path(
    folder_out,
    "Grafik_Indeks_Pekalongan_FINAL.png"
  ),
  plot = grafik_indeks_final,
  width = 18,
  height = 15,
  units = "in",
  dpi = 300,
  bg = "white"
)

# 9. Simpan PDF
ggsave(
  filename = file.path(
    folder_out,
    "Grafik_Indeks_Pekalongan_FINAL.pdf"
  ),
  plot = grafik_indeks_final,
  width = 18,
  height = 15,
  units = "in",
  bg = "white"
)

cat("\n===== STEP 25 REVISI SELESAI =====\n")

# =====================================================
# 26. ===== PERSIAPAN DATA WEBSITE SHINY =====
# =====================================================

library(terra)
library(sf)

# 1. Buat folder aplikasi
folder_web <- file.path(getwd(), "Web_Pekalongan")
folder_data <- file.path(folder_web, "data")

dir.create(folder_data, recursive = TRUE,
           showWarnings = FALSE)

# 2. Simpan raster klasifikasi
writeRaster(
  kelas,
  file.path(folder_data, "klasifikasi.tif"),
  overwrite = TRUE
)

# 3. Simpan indeks spektral
writeRaster(
  indeks,
  file.path(folder_data, "indeks.tif"),
  overwrite = TRUE
)

# 4. Simpan batas kabupaten
st_write(
  kab,
  file.path(folder_data, "kabupaten.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# 5. Simpan batas kecamatan
st_write(
  kec,
  file.path(folder_data, "kecamatan.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# 6. Simpan zonal statistics
write.csv(
  zonal_final,
  file.path(folder_data, "zonal.csv"),
  row.names = FALSE
)

# 7. Cek hasil
print(list.files(folder_data))

cat("\n===== STEP 26 SELESAI =====\n")

# =====================================================
# 27. ===== INSTALL PACKAGE WEBSITE =====
# =====================================================

paket <- c(
  "shiny",
  "shinydashboard",
  "leaflet",
  "terra",
  "sf",
  "ggplot2",
  "DT",
  "dplyr",
  "htmltools"
)

belum_ada <- paket[
  !paket %in% rownames(installed.packages())
]

if (length(belum_ada) > 0) {
  install.packages(belum_ada)
}

cat("\n===== STEP 27 SELESAI =====\n")
