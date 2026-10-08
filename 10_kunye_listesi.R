# =============================================================================
#  10 — KÜNYE LİSTESİ  (Hakem 2: atıf ve kaynakça denetimi)
#
#  Amaç: Kaynakçada olması gereken çalışmaların tam künyesini KENDİ verinden
#        çekmek (yazar, yıl, dergi, cilt, sayfa, DOI). Hiçbir bilgi tahmin
#        edilmez; veride bulunamayan çalışma "BULUNAMADI" diye yazılır.
#  GİRDİ : M_etiketli.RData, outputs/en_cok_atif_alanlar.csv (05'ten)
#  ÇIKTI : outputs/kunye_listesi.txt   <- bu dosyayı bana at
#  ÇALIŞTIRMA: source("10_kunye_listesi.R")
# =============================================================================
load("M_etiketli.RData"); M <- M_etiketli
cat("Korpus:", nrow(M), "kayıt\n")
al <- function(x) if (x %in% names(M)) as.character(M[[x]]) else rep(NA_character_, nrow(M))
AU <- toupper(al("AU")); AF <- al("AF"); TI <- al("TI"); SO <- al("SO"); PY <- suppressWarnings(as.integer(al("PY")))
VL <- al("VL"); IS <- al("IS"); BP <- al("BP"); EP <- al("EP"); AR <- al("AR"); DI <- al("DI")
TC <- suppressWarnings(as.integer(al("TC")))

# (A) Korpus içindeki çalışmalar: yazar deseni + yıl (+ başlıkta aranacak sözcük)
HEDEF <- list(
  list("Wang 2024",        "^WANG",        2024, ""),
  list("Mienye & Sun 2022","MIENYE",       2022, ""),
  list("Mansour 2022",     "MANSOUR",      2022, "bibliometric|fraud"),
  list("Alarfaj 2022",     "ALARFAJ",      2022, ""),
  list("Taha & Malebary",  "TAHA",         2020, ""),
  list("Btoush 2023",      "BTOUSH",       2023, ""),
  list("Muria 2023",       "MURIA",        2023, ""),
  list("Mahya",            "MAHYA",        NA,   ""),
  list("Hove",             "HOVE",         NA,   ""),
  list("Tiwari",           "TIWARI",       NA,   "bibliometric|review|survey"),
  list("Gouda",            "GOUDA",        NA,   ""),
  list("Jurgovsky 2018",   "JURGOVSKY",    2018, ""),
  list("Randhawa 2018",    "RANDHAWA",     2018, ""),
  list("Dornadula 2019",   "DORNADULA",    2019, ""),
  list("Awoyemi 2017",     "AWOYEMI",      2017, ""),
  list("Dal Pozzolo",      "DAL POZZOLO",  NA,   "")
)
sink("outputs/kunye_listesi.txt", split = TRUE)
cat("==== A. KORPUS KAYITLARI ====\n")
for (h in HEDEF) {
  ok <- grepl(h[[2]], AU)
  if (!is.na(h[[3]])) ok <- ok & !is.na(PY) & PY == h[[3]]
  if (nzchar(h[[4]])) ok <- ok & grepl(h[[4]], TI, ignore.case = TRUE)
  ix <- which(ok)
  cat("\n--- ", h[[1]], " (", length(ix), " kayıt) ---\n", sep = "")
  if (!length(ix)) { cat("BULUNAMADI\n"); next }
  ix <- ix[order(-ifelse(is.na(TC[ix]), 0, TC[ix]))]
  for (i in head(ix, 6))
    cat(sprintf("AU: %s | PY: %s | TC: %s\n  TI: %s\n  SO: %s | VL: %s | IS: %s | BP: %s | EP: %s | AR: %s\n  DI: %s\n",
                substr(AU[i], 1, 120), PY[i], TC[i], TI[i], SO[i], VL[i], IS[i], BP[i], EP[i], AR[i], DI[i]))
}

# (B) Korpusta olmayan klasik kaynaklar: atıf yapılan işler tablosundan
cat("\n\n==== B. EN ÇOK ATIF ALAN İŞLER (05) ====\n")
f <- "outputs/en_cok_atif_alanlar.csv"
if (file.exists(f)) {
  E <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  txt <- toupper(apply(E, 1, paste, collapse = " | "))
  PAT <- c("KNORR","CHAWLA","GHOSH","MAES","TANG","BHATTACHARYYA","QUAH","SRIVASTAVA",
           "AKBANI","DAL POZZOLO","JURGOVSKY","WANG","MIENYE","BREIMAN","LECUN","HOCHREITER")
  for (p in PAT) {
    ix <- which(grepl(p, txt))
    cat("\n--- ", p, " (", length(ix), " satır) ---\n", sep = "")
    for (i in head(ix, 8)) cat(substr(paste(E[i, ], collapse = " | "), 1, 300), "\n")
  }
} else cat("outputs/en_cok_atif_alanlar.csv yok — önce 05'i çalıştır.\n")

# (C) Yazılım ve sürümler (Yöntem bölümü için)
cat("\n\n==== C. YAZILIM SÜRÜMLERİ ====\n")
cat("R:", R.version.string, "\n")
for (p in c("bibliometrix","igraph","dplyr","readr","ragg"))
  cat(p, ":", tryCatch(as.character(packageVersion(p)), error = function(e) "kurulu değil"), "\n")
sink()
cat("\nTamam. outputs/kunye_listesi.txt dosyasını bana at.\n")
