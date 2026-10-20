# 🌍 GeoPekalongan
### Analisis Indeks Spektral dan Klasifikasi Tutupan Lahan Kabupaten Pekalongan

**ETS Pemrograman Spasial A | Kelompok 17 | Tahun 2026**

GeoPekalongan merupakan proyek pemrograman spasial berbasis bahasa R yang bertujuan untuk menganalisis tutupan lahan dan indeks spektral di Kabupaten Pekalongan, Jawa Tengah, menggunakan citra Sentinel-2 Level-2A. Hasil analisis disajikan melalui aplikasi WebGIS interaktif berbasis R Shiny dan Leaflet.

## 🎯 Tujuan Proyek

1. Mengolah citra Sentinel-2 Level-2A untuk wilayah Kabupaten Pekalongan.
2. Menghitung indeks spektral NDVI, NDBI, NDWI, dan NDCI.
3. Mengklasifikasikan tutupan lahan menjadi vegetasi, bangunan, perairan, dan awan.
4. Menganalisis statistik zonal pada 19 kecamatan.
5. Mengembangkan dashboard WebGIS untuk visualisasi dan eksplorasi data spasial.

## 🛰️ Data dan Teknologi

**Data yang digunakan:**
- Citra Sentinel-2 Level-2A tahun 2026.
- Batas administrasi Kabupaten Pekalongan dan 19 kecamatan.
- Raster hasil klasifikasi tutupan lahan dan indeks spektral.
- Tabel statistik zonal per kecamatan.

**Bahasa pemrograman:** R

**Perangkat lunak:** RStudio

**Library utama:** `terra`, `sf`, `dplyr`, `shiny`, `shinydashboard`, `leaflet`, `ggplot2`, dan `DT`.

## ⚙️ Metodologi

Pengolahan dilakukan melalui tahapan berikut:

1. **Prapengolahan citra:** pembacaan band Sentinel-2, penyesuaian resolusi spasial, dan pemotongan citra sesuai wilayah kajian.
2. **Perhitungan indeks spektral:** NDVI, NDBI, NDWI, dan indeks Blue–SWIR yang diberi nama NDCI dalam skrip proyek.
3. **Klasifikasi tutupan lahan:** menggunakan pendekatan *rule-based classification* dan Scene Classification Layer (SCL).
4. **Analisis spasial:** perhitungan luas tutupan lahan serta statistik zonal pada 19 kecamatan.
5. **Visualisasi:** penyajian peta, grafik, tabel, dan hasil analisis melalui WebGIS GeoPekalongan.

## 📊 Hasil Klasifikasi Tutupan Lahan

| Kelas Tutupan Lahan | Luas (ha) | Persentase |
|---|---:|---:|
| Vegetasi | 68.281,68 | 77,43% |
| Bangunan | 14.029,44 | 15,91% |
| Perairan | 1.537,40 | 1,74% |
| Awan | 4.336,31 | 4,92% |
| **Total** | **88.184,84** | **100,00%** |

Hasil klasifikasi menunjukkan bahwa vegetasi merupakan kelas tutupan lahan paling dominan di Kabupaten Pekalongan, diikuti bangunan, awan, dan perairan.

## 🗺️ Fitur WebGIS GeoPekalongan

Aplikasi GeoPekalongan dikembangkan menggunakan R Shiny dan Leaflet dengan fitur:

- Peta klasifikasi tutupan lahan interaktif.
- Visualisasi indeks spektral NDVI, NDBI, NDWI, dan NDCI.
- Grafik dan tabel statistik zonal pada 19 kecamatan.
- Informasi batas administrasi wilayah.
- Fitur pengunduhan data hasil analisis.

## 📂 Struktur Repository

```text
Web_Pekalongan/
├── app.R
├── .gitignore
└── data/
    ├── kabupaten.gpkg
    ├── kabupaten_web.gpkg
    ├── kecamatan.gpkg
    └── zonal.csv
```

**Catatan:** File raster berukuran besar (`.tif`) tidak disertakan dalam repository GitHub. Untuk menjalankan aplikasi secara lokal, pengguna perlu menyediakan file raster yang digunakan oleh `app.R` di dalam folder `data/`.

## 🚀 Menjalankan Aplikasi

1. Unduh atau *clone* repository ini.
2. Buka proyek menggunakan RStudio.
3. Instal paket R yang diperlukan.
4. Pastikan seluruh data raster yang dibutuhkan tersedia pada folder `data/`.
5. Jalankan file `app.R` menggunakan tombol **Run App** di RStudio.

Aplikasi akan terbuka melalui browser atau jendela Viewer RStudio.

## 👥 Anggota Kelompok 17

| Nama | NRP |
|---|---|
| Lalu Gde Rindang Kusuma Ardha Wiguna | 5016241095 |
| Dimas Haryo Harmadji Putra | 5016241102 |

**Mata Kuliah:** Pemrograman Spasial A  
**Dosen Pengampu:** Mohammad Rohmaneo Darminto, S.T., M.Sc.  
**Program Studi:** Teknik Geomatika  
**Institusi:** Institut Teknologi Sepuluh Nopember (ITS)  
**Tahun:** 2026

---

*Proyek ini dikembangkan untuk memenuhi tugas Evaluasi Tengah Semester (ETS) mata kuliah Pemrograman Spasial A.*
