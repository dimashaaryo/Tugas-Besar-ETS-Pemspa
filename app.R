
# =====================================================
# 28. ===== WEBSITE SHINY LEAFLET PEKALONGAN =====
# PROJECT ETS KELOMPOK 17
# PEMROGRAMAN SPASIAL A - 2026
# =====================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(terra)
library(sf)
library(ggplot2)
library(DT)
library(dplyr)
library(htmltools)

# =====================================================
# A. BACA DATA WEBSITE PEKALONGAN - FINAL
# =====================================================

library(terra)
library(sf)


# Folder data mengikuti direktori aplikasi Shiny
folder_data <- "data"

cat("Direktori kerja aplikasi:", getwd(), "\n")
cat("Lokasi folder data:", normalizePath(folder_data, mustWork = FALSE), "\n")

# Pastikan folder data ditemukan
if (!dir.exists(folder_data)) {
  stop(
    "Folder data tidak ditemukan. Direktori kerja: ",
    getwd()
  )
}


# Periksa kelengkapan file
file_wajib <- c(
  "klasifikasi.tif",
  "indeks.tif",
  "kabupaten_web.gpkg",
  "kecamatan.gpkg",
  "zonal.csv"
)

cek_file <- data.frame(
  File = file_wajib,
  Tersedia = file.exists(
    file.path(folder_data, file_wajib)
  )
)

print(cek_file)

if (!all(cek_file$Tersedia)) {
  stop("Ada file data website yang belum tersedia.")
}

# =====================================================
# 1. BACA RASTER KLASIFIKASI
# =====================================================

kelas <- terra::rast(
  file.path(folder_data, "klasifikasi.tif")
)

# =====================================================
# 2. BACA RASTER INDEKS SPEKTRAL
# =====================================================

indeks <- terra::rast(
  file.path(folder_data, "indeks.tif")
)

names(indeks) <- c(
  "NDVI", "NDBI", "NDWI", "NDCI"
)

# =====================================================
# 3. BACA BATAS KABUPATEN YANG SUDAH DIPERBAIKI
# =====================================================

kab <- sf::st_read(
  file.path(folder_data, "kabupaten_web.gpkg"),
  quiet = TRUE
)

# =====================================================
# 4. BACA BATAS KECAMATAN
# =====================================================

kec <- sf::st_read(
  file.path(folder_data, "kecamatan.gpkg"),
  quiet = TRUE
)

# =====================================================
# 5. BACA STATISTIK ZONAL
# =====================================================

zonal <- read.csv(
  file.path(folder_data, "zonal.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Pertahankan nama objek yang digunakan sebelumnya
zonal_final <- zonal

# =====================================================
# 6. SISTEM KOORDINAT UNTUK LEAFLET
# =====================================================

kab <- sf::st_transform(kab, 4326)
kec <- sf::st_transform(kec, 4326)

# Batas wilayah untuk zoom peta
bbox <- sf::st_bbox(kab)

# =====================================================
# 7. CEK HASIL
# =====================================================

cat("\n====================================\n")
cat("DATA WEBSITE BERHASIL DIBACA!\n")
cat("====================================\n")

cat("Jumlah kabupaten:", nrow(kab), "\n")
cat("Jumlah kecamatan:", nrow(kec), "\n")
cat("Indeks:", paste(names(indeks), collapse = ", "), "\n")

print(bbox)

# =====================================================
# B. WARNA DAN INFORMASI
# =====================================================

warna_kelas <- c(
  "#188A27",
  "#E93648",
  "#2479BD",
  "#D9D9D9"
)

nama_kelas <- c(
  "Vegetasi",
  "Bangunan",
  "Perairan",
  "Awan"
)

# Palet warna klasifikasi tutupan lahan
pal_kelas <- leaflet::colorBin(
  palette = warna_kelas,
  domain = c(0.5, 4.5),
  bins = seq(0.5, 4.5, by = 1),
  na.color = "transparent",
  right = FALSE
)

# Batas peta
bbox <- st_bbox(kab)

# Luas per kelas (hektare)
luas_raster <- cellSize(kelas, unit = "ha")

luas_kelas <- sapply(1:4, function(i) {
  nilai <- global(
    ifel(kelas == i, luas_raster, NA),
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  if (is.na(nilai)) 0 else nilai
})

stat_kelas <- data.frame(
  Kelas = nama_kelas,
  Luas_Ha = as.numeric(luas_kelas)
)

stat_kelas$Persentase <- 100 *
  stat_kelas$Luas_Ha /
  sum(stat_kelas$Luas_Ha)

# =====================================================
# C. USER INTERFACE
# =====================================================

ui <- dashboardPage(
  
  skin = "green",
  
  dashboardHeader(
    title = "GeoPekalongan"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      id = "menu",
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("home")
      ),
      
      menuItem(
        "Peta Tutupan Lahan",
        tabName = "peta",
        icon = icon("globe")
      ),
      
      menuItem(
        "Indeks Spektral",
        tabName = "indeks",
        icon = icon("map")
      ),
      
      menuItem(
        "Statistik Kecamatan",
        tabName = "statistik",
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Unduh Data",
        tabName = "unduh",
        icon = icon("download")
      )
    ),
    
    br(),
    
    tags$div(
      style = paste(
        "padding:15px;",
        "color:#dddddd;",
        "font-size:12px;"
      ),
      "ETS Pemrograman Spasial A",
      tags$br(),
      "Kelompok 17 | 2026"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
    /* Tampilan utama */
    .content-wrapper, .right-side {
      background-color: #F2F6FA;
    }

    /* Sidebar */
    .skin-blue .main-sidebar {
      background-color: #143C56;
    }

    .skin-blue .sidebar-menu > li > a {
      color: #E5EEF5;
      padding: 14px 15px;
    }

    .skin-blue .sidebar-menu > li.active > a,
    .skin-blue .sidebar-menu > li:hover > a {
      background-color: #205A77;
      border-left-color: #38BDF8;
    }

    /* Header */
    .skin-blue .main-header .logo,
    .skin-blue .main-header .navbar {
      background-color: #143C56;
    }

    /* Kotak konten */
    .box {
      border-radius: 12px;
      border-top: 3px solid #2387AD;
      box-shadow: 0 4px 16px rgba(0,0,0,0.06);
    }

    /* Judul */
    .content-header h1 {
      font-weight: 700;
      color: #143C56;
    }

    /* Kartu informasi */
    .small-box {
      border-radius: 12px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.08);
    }

    /* Peta */
    .leaflet-container {
      border-radius: 10px;
    }

    /* Footer */
    .main-footer {
      color: #607D8B;
      font-size: 12px;
    }
  "))
    ),
    
    
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          background-color: #f4f7f6;
        }

        .box {
          border-radius: 8px;
        }

        .small-box {
          border-radius: 8px;
        }

        .main-header .logo {
          font-weight: bold;
        }
      "))
    ),
    
    div(
      style = paste(
        "background: linear-gradient(110deg,",
        "#143C56, #2387AD);",
        "color: white;",
        "padding: 20px 25px;",
        "border-radius: 12px;",
        "margin-bottom: 20px;"
      ),
      
      h3(
        "GeoPekalongan",
        style = "margin-top: 0; font-weight: 700;"
      ),
      
      p(
        "Dashboard Analisis Tutupan Lahan dan Indeks Spektral",
        style = "font-size: 15px;"
      ),
      
      tags$hr(
        style = "border-color: rgba(255,255,255,0.3);"
      ),
      
      p(
        tags$b("Kelompok 17 | Pemrograman Spasial A")
      ),
      
      p(
        "Lalu Gde Rindang Kusuma Ardha Wiguna",
        tags$br(),
        "NRP: 5016241095"
      ),
      
      p(
        "Dimas Haryo Harmdji Putra",
        tags$br(),
        "NRP: 5016241102"
      )
    ),
    
    
    tabItems(
      
      # ==========================================
      # MENU 1 - DASHBOARD
      # ==========================================
      
      tabItem(
        tabName = "dashboard",
        
        h2("Dashboard Analisis Tutupan Lahan"),
        
        h4(
          "Kabupaten Pekalongan, Jawa Tengah | 2026"
        ),
        
        fluidRow(
          
          valueBox(
            value = nrow(kec),
            subtitle = "Kecamatan",
            icon = icon("map-marker"),
            color = "green"
          ),
          
          valueBox(
            value = "4 Indeks",
            subtitle = "NDVI, NDBI, NDWI, NDCI",
            icon = icon("chart-line"),
            color = "aqua"
          ),
          
          valueBox(
            value = "Sentinel-2",
            subtitle = "Citra Level-2A",
            icon = icon("globe"),
            color = "yellow"
          )
        ),
        
        fluidRow(
          
          box(
            title = "Komposisi Tutupan Lahan",
            width = 8,
            status = "success",
            solidHeader = TRUE,
            
            plotOutput(
              "grafik_luas",
              height = "400px"
            )
          ),
          
          box(
            title = "Informasi Penelitian",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            
            h4("PROJECT ETS KELOMPOK 17"),
            
            p("Mata Kuliah: Pemrograman Spasial A"),
            
            p("Wilayah: Kabupaten Pekalongan"),
            
            p("Tahun Analisis: 2026"),
            
            p("Data: Sentinel-2 Level-2A"),
            
            p(
              "Metode: Indeks Spektral dan",
              "Klasifikasi Tutupan Lahan"
            ),
            
            hr(),
            
            p(
              "Dashboard ini menyajikan",
              "hasil pengolahan citra satelit",
              "dan analisis spasial."
            )
          )
        )
      ),
      
      # ==========================================
      # MENU 2 - PETA TUTUPAN LAHAN
      # ==========================================
      
      tabItem(
        tabName = "peta",
        
        h2("Peta Klasifikasi Tutupan Lahan"),
        
        fluidRow(
          
          box(
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            checkboxInput(
              "batas_kec",
              "Tampilkan Batas Kecamatan",
              value = TRUE
            ),
            
            leafletOutput(
              "peta_klasifikasi",
              height = "650px"
            )
          )
        )
      ),
      
      # ==========================================
      # MENU 3 - INDEKS SPEKTRAL
      # ==========================================
      
      tabItem(
        tabName = "indeks",
        
        h2("Peta Indeks Spektral"),
        
        fluidRow(
          
          box(
            title = "Pengaturan Indeks",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            selectInput(
              "pilih_indeks",
              "Pilih Indeks Spektral:",
              choices = c(
                "NDVI",
                "NDBI",
                "NDWI",
                "NDCI"
              ),
              selected = "NDVI"
            ),
            
            leafletOutput(
              "peta_indeks",
              height = "600px"
            )
          )
        )
      ),
      
      # ==========================================
      # MENU 4 - STATISTIK
      # ==========================================
      
      tabItem(
        tabName = "statistik",
        
        h2("Zonal Statistics per Kecamatan"),
        
        fluidRow(
          
          box(
            title = "Grafik Rata-Rata Indeks",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            selectInput(
              "indeks_grafik",
              "Pilih Indeks:",
              choices = c(
                "NDVI",
                "NDBI",
                "NDWI",
                "NDCI"
              )
            ),
            
            plotOutput(
              "grafik_zonal",
              height = "550px"
            )
          )
        ),
        
        fluidRow(
          
          box(
            title = "Tabel Zonal Statistics",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            DTOutput("tabel_zonal")
          )
        )
      ),
      
      # ==========================================
      # MENU 5 - DOWNLOAD
      # ==========================================
      
      tabItem(
        tabName = "unduh",
        
        h2("Unduh Hasil Analisis"),
        
        box(
          title = "Data Zonal Statistics",
          width = 6,
          status = "success",
          
          p(
            "Unduh hasil statistik",
            "indeks spektral per kecamatan."
          ),
          
          downloadButton(
            "download_zonal",
            "Download Zonal Statistics CSV"
          )
        ),
        
        box(
          title = "Data Luas Tutupan Lahan",
          width = 6,
          status = "primary",
          
          p(
            "Unduh luas dan persentase",
            "masing-masing kelas."
          ),
          
          downloadButton(
            "download_luas",
            "Download Luas Tutupan CSV"
          )
        )
      )
    )
  )
)

# =====================================================
# D. SERVER
# =====================================================

server <- function(input, output, session) {
  
  # ==========================================
  # 1. GRAFIK LUAS TUTUPAN LAHAN
  # ==========================================
  
  output$grafik_luas <- renderPlot({
    
    ggplot(
      stat_kelas,
      aes(
        x = reorder(Kelas, Luas_Ha),
        y = Luas_Ha,
        fill = Kelas
      )
    ) +
      
      geom_col(width = 0.65) +
      
      geom_text(
        aes(
          label = paste0(
            round(Persentase, 2), "%"
          )
        ),
        hjust = -0.1,
        size = 4
      ) +
      
      coord_flip() +
      
      scale_fill_manual(
        values = setNames(
          warna_kelas,
          nama_kelas
        )
      ) +
      
      scale_y_continuous(
        expand = expansion(
          mult = c(0, 0.18)
        )
      ) +
      
      labs(
        x = NULL,
        y = "Luas (Hektare)"
      ) +
      
      theme_minimal(base_size = 12) +
      
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank()
      )
  })
  
  
  
  # =====================================================
  # PETA KLASIFIKASI TUTUPAN LAHAN - FINAL
  # =====================================================
  
  output$peta_klasifikasi <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE
      )
    ) %>%
      
      # Basemap OpenStreetMap
      addProviderTiles(
        providers$OpenStreetMap,
        group = "OpenStreetMap"
      ) %>%
      
      # Basemap citra satelit
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Citra Satelit"
      ) %>%
      
      # Raster klasifikasi
      addRasterImage(
        kelas,
        colors = pal_kelas,
        opacity = 0.8,
        maxBytes = 20 * 1024 * 1024,
        project = TRUE,
        group = "Klasifikasi"
      ) %>%
      
      # Batas kabupaten
      addPolygons(
        data = kab,
        fill = FALSE,
        color = "black",
        weight = 2,
        group = "Batas Kabupaten"
      ) %>%
      
      # Legenda tutupan lahan
      addLegend(
        position = "bottomright",
        colors = warna_kelas,
        labels = nama_kelas,
        title = "Tutupan Lahan"
      ) %>%
      
      # Kontrol layer
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "Citra Satelit"
        ),
        overlayGroups = c(
          "Klasifikasi",
          "Batas Kabupaten",
          "Batas Kecamatan"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      
      # Tampilkan OpenStreetMap sebagai basemap awal
      hideGroup("Citra Satelit") %>%
      
      # Zoom ke Kabupaten Pekalongan
      fitBounds(
        lng1 = unname(bbox["xmin"]),
        lat1 = unname(bbox["ymin"]),
        lng2 = unname(bbox["xmax"]),
        lat2 = unname(bbox["ymax"])
      )
    
  })
  
  # Tampilkan / sembunyikan kecamatan
  observeEvent(input$batas_kec, {
    
    proxy <- leafletProxy("peta_klasifikasi")
    
    proxy %>% clearGroup("Batas Kecamatan")
    
    if (isTRUE(input$batas_kec)) {
      
      proxy %>%
        addPolygons(
          data = kec,
          fill = FALSE,
          color = "white",
          weight = 1,
          opacity = 0.8,
          group = "Batas Kecamatan",
          label = ~as.character(Kecamatan)
        )
    }
  })
  
  
  
  
  # ==========================================
  # 3. PETA INDEKS SPEKTRAL + RASTER
  # ==========================================
  
  output$peta_indeks <- renderLeaflet({
    
    req(input$pilih_indeks)
    
    r <- indeks[[input$pilih_indeks]]
    
    # Ambil nilai minimum dan maksimum
    batas <- as.numeric(terra::minmax(r))
    
    validate(
      need(
        length(batas) == 2 &&
          all(is.finite(batas)) &&
          batas[1] < batas[2],
        "Rentang nilai indeks tidak valid."
      )
    )
    
    # Palet warna
    pal <- leaflet::colorNumeric(
      palette = "RdYlGn",
      domain = batas,
      na.color = "transparent"
    )
    
    leaflet() %>%
      addProviderTiles(
        providers$OpenStreetMap
      ) %>%
      addRasterImage(
        r,
        colors = pal,
        opacity = 0.85,
        maxBytes = 20 * 1024 * 1024,
        project = TRUE
      ) %>%
      
      addPolygons(
        data = kab,
        fill = FALSE,
        color = "black",
        weight = 2
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = batas,
        title = input$pilih_indeks
      ) %>%
      
      fitBounds(
        lng1 = unname(bbox["xmin"]),
        lat1 = unname(bbox["ymin"]),
        lng2 = unname(bbox["xmax"]),
        lat2 = unname(bbox["ymax"])
      )
    
  })
  
  # ==========================================
  # 4. GRAFIK ZONAL STATIS
  # ==========================================
  
  output$grafik_zonal <- renderPlot({
    
    kolom <- paste0(
      input$indeks_grafik,
      "_Mean"
    )
    
    validate(
      need(
        kolom %in% names(zonal),
        "Kolom indeks tidak ditemukan"
      )
    )
    
    data_grafik <- zonal %>%
      mutate(
        Nilai = .data[[kolom]]
      )
    
    ggplot(
      data_grafik,
      aes(
        x = reorder(Kecamatan, Nilai),
        y = Nilai
      )
    ) +
      
      geom_col(
        fill = "#218C74",
        width = 0.7
      ) +
      
      coord_flip() +
      
      labs(
        title = paste(
          "Rata-Rata",
          input$indeks_grafik,
          "per Kecamatan"
        ),
        x = NULL,
        y = "Nilai Rata-Rata"
      ) +
      
      theme_minimal(base_size = 12) +
      
      theme(
        plot.title = element_text(
          face = "bold"
        ),
        panel.grid.major.y = element_blank()
      )
  })
  
  # ==========================================
  # 5. TABEL ZONAL STATISTICS
  # ==========================================
  
  output$tabel_zonal <- renderDT({
    
    datatable(
      zonal,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = "Bfrtip",
        buttons = c(
          "copy", "csv", "excel"
        )
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  # ==========================================
  # 6. DOWNLOAD DATA
  # ==========================================
  
  output$download_zonal <- downloadHandler(
    
    filename = function() {
      "Zonal_Statistics_Pekalongan.csv"
    },
    
    content = function(file) {
      write.csv(
        zonal,
        file,
        row.names = FALSE
      )
    }
  )
  
  output$download_luas <- downloadHandler(
    
    filename = function() {
      "Luas_Tutupan_Lahan_Pekalongan.csv"
    },
    
    content = function(file) {
      write.csv(
        stat_kelas,
        file,
        row.names = FALSE
      )
    }
  )
}


# =====================================================
# E. ===== JALANKAN APLIKASI SHINY =====
# =====================================================

# Pastikan UI dan server sudah dibuat
stopifnot(exists("ui"))
stopifnot(exists("server"))

# Buat objek aplikasi Shiny
app <- shiny::shinyApp(
  ui = ui,
  server = server
)

# Kembalikan objek aplikasi sebagai hasil akhir
app
