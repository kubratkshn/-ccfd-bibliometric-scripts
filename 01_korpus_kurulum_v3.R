# =============================================================================
#  01 — KORPUS KURULUMU  (v3)
#
#  v2'den farkı: K3 — VERİTABANI İÇİ KOPYA TEMİZLİĞİ eklendi.
#  Kural: aynı DOI = kopya. DOI'si olmayanlarda aynı başlık + aynı yıl = kopya.
#  Aynı başlık fakat FARKLI DOI olanlar KORUNUR — konferans literatüründe
#  benzer başlıklı ayrı bildiriler yaygındır (Scopus'ta 45 böyle kayıt var).
#
#  ÇALIŞTIRMA: source("01_korpus_kurulum_v3.R")   <- satır satır değil!
#  SONRA     : iki TARA dosyasını doldur -> source("02_manuel_tarama_uygula.R")
# =============================================================================

pkgs <- c("bibliometrix", "dplyr", "readr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))
dir.create("outputs", showWarnings = FALSE)

# ---- DOSYA YOLLARI: yalnızca burayı düzenle --------------------------------
DIR        <- "Downloads"
SCOPUS_CSV <- file.path(DIR, "scopus2807.csv")
WOS_TXT    <- file.path(DIR, c("wos1384a.txt", "wos1384b.txt", "wos1384c.txt"))
ORNEK_N    <- 100
# ----------------------------------------------------------------------------
stopifnot(file.exists(SCOPUS_CSV), all(file.exists(WOS_TXT)))

norm_doi <- function(x) {
  s <- tolower(trimws(ifelse(is.na(x), "", as.character(x))))
  s <- sub("^https?://(dx\\.)?doi\\.org/", "", s)
  ifelse(startsWith(s, "10."), s, NA_character_)
}
norm_ti <- function(x) {
  s <- gsub("[^a-z0-9]", "", tolower(ifelse(is.na(x), "", as.character(x))))
  ifelse(nchar(s) > 15, s, NA_character_)
}
blob <- function(d) {
  f <- function(x) ifelse(is.na(x), "", as.character(x))
  tolower(paste(f(d$TI), f(d$AB)))
}
kisalt <- function(x, n = 600) substr(gsub("\\s+", " ", ifelse(is.na(x), "", x)), 1, n)

## 1. Dönüştürme -------------------------------------------------------------
cat("\n--- 1. DÖNÜŞTÜRME ---\n")
S <- convert2df(SCOPUS_CSV, dbsource = "scopus", format = "csv")
W_parts <- lapply(WOS_TXT, function(f) convert2df(f, dbsource = "wos", format = "plaintext"))
W <- Reduce(function(a, b) mergeDbSources(a, b, remove.duplicated = TRUE), W_parts)
nS_raw <- nrow(S); nW_raw <- nrow(W)
S$DB_SRC <- "Scopus"; W$DB_SRC <- "WoS"
cat("Scopus:", nS_raw, "| WoS:", nW_raw, "| Ham toplam:", nS_raw + nW_raw, "\n")

## 2. Yıl filtresi -----------------------------------------------------------
cat("\n--- 2. YIL FİLTRESİ (1993-2025) ---\n")
S$PY <- as.numeric(S$PY); W$PY <- as.numeric(W$PY)
n_yil <- sum(S$PY > 2025, na.rm = TRUE) + sum(W$PY > 2025, na.rm = TRUE)
S <- S[!is.na(S$PY) & S$PY >= 1993 & S$PY <= 2025, ]
W <- W[!is.na(W$PY) & W$PY >= 1993 & W$PY <= 2025, ]
cat("Çıkarılan >2025:", n_yil, "| Kalan:", nrow(S) + nrow(W), "\n")

## 3. K3a — VERİTABANI İÇİ KOPYA ---------------------------------------------
cat("\n--- 3. K3a: VERİTABANI İÇİ KOPYA ---\n")
ic_kopya_temizle <- function(d, etiket) {
  d$.doi <- norm_doi(d$DI); d$.ti <- norm_ti(d$TI)
  # (a) aynı DOI
  dup_a <- !is.na(d$.doi) & duplicated(d$.doi)
  # (b) DOI'siz + aynı başlık&yıl
  key   <- ifelse(is.na(d$.doi) & !is.na(d$.ti), paste(d$.ti, d$PY), NA_character_)
  dup_b <- !is.na(key) & duplicated(key)
  dup   <- dup_a | dup_b
  cat(sprintf("  %s — aynı DOI: %d | DOI'siz aynı başlık+yıl: %d | silinen: %d\n",
              etiket, sum(dup_a), sum(dup_b), sum(dup)))
  list(kalan = d[!dup, ], silinen = d[dup, ])
}
rS <- ic_kopya_temizle(S, "Scopus"); rW <- ic_kopya_temizle(W, "WoS")
n_ic <- nrow(rS$silinen) + nrow(rW$silinen)
readr::write_csv(
  data.frame(kaynak = c(rep("Scopus", nrow(rS$silinen)), rep("WoS", nrow(rW$silinen))),
             baslik = c(rS$silinen$TI, rW$silinen$TI),
             yil    = c(rS$silinen$PY, rW$silinen$PY),
             doi    = c(rS$silinen$DI, rW$silinen$DI)),
  "outputs/K3a_ic_kopyalar.csv")
S <- rS$kalan; W <- rW$kalan
cat("  TOPLAM iç kopya:", n_ic, "| Kalan:", nrow(S) + nrow(W), "\n")
cat("  Not: aynı başlık fakat farklı DOI olan kayıtlar KORUNDU.\n")

## 4. K3b — VERİTABANLARI ARASI KOPYA ----------------------------------------
cat("\n--- 4. K3b: VERİTABANLARI ARASI KOPYA ---\n")
dup_doi <- intersect(na.omit(S$.doi), na.omit(W$.doi))
dup_ti  <- intersect(na.omit(S[!(S$.doi %in% dup_doi), ]$.ti),
                     na.omit(W[!(W$.doi %in% dup_doi), ]$.ti))
cat("  DOI eşleşmesi:", length(dup_doi), "| Başlık eşleşmesi:", length(dup_ti), "\n")
W_uniq <- W[!(W$.doi %in% dup_doi) & !(W$.ti %in% dup_ti), ]
M <- dplyr::bind_rows(S, W_uniq)
U <- nrow(M)
cat("  BENZERSİZ KAYIT:", U, "\n")

## 5. Örtüşme testleri -------------------------------------------------------
cat("\n--- 5. ÖRTÜŞME TESTLERİ ---\n")
nS <- nrow(S); nW <- nrow(W); D <- length(dup_doi) + length(dup_ti)
ov <- data.frame(
  metrik = c("Scopus'a özgü", "WoS'a özgü", "Meyer Index (Scopus)", "Meyer Index (WoS)",
             "Traditional Overlap (%)", "Overlap Scopus içinde (%)", "Overlap WoS içinde (%)"),
  deger  = c(nS - D, nW - D, round((nS - D + 0.5*D)/U, 3), round((nW - D + 0.5*D)/U, 3),
             round(100*D/U, 2), round(100*D/nS, 2), round(100*D/nW, 2)))
print(ov, row.names = FALSE)
readr::write_csv(ov, "outputs/ortusme_testleri.csv")

## 6. K1 — geri çekilmiş -----------------------------------------------------
cat("\n--- 6. K1: GERİ ÇEKİLMİŞ ---\n")
retr <- grepl("retract", tolower(ifelse(is.na(M$DT), "", M$DT)))
cat("  Elenen:", sum(retr), "\n")
readr::write_csv(data.frame(baslik = M$TI[retr], yil = M$PY[retr]),
                 "outputs/K1_geri_cekilmis.csv")
M <- M[!retr, ]

## 7. K4 — tespit boyutu (sözlüksel aşama) -----------------------------------
#  Dayanak: anti-fraud literatürü prevention / detection / containment olarak
#  ayrılır (Hove et al., 2024). Bu çalışma DETECTION koluna odaklanır.
cat("\n--- 7. K4: SÖZLÜKSEL AŞAMA ---\n")
DET_TERMS <- c("detect", "identif", "classif", "predict", "recogni",
               "screening", "early warning", "anomal", "discover", "monitor")
DET <- paste(DET_TERMS, collapse = "|")

M_pre <- M
save(M_pre, file = "M_pre.RData")

has_det <- grepl(DET, blob(M), perl = TRUE)
n_k4 <- sum(!has_det)
cat("  Sözlüksel kuralla elenen:", n_k4, "| Kalan:", sum(has_det), "\n")
M <- M[has_det, ]

## 8. Manuel tarama dosyaları (İKİ YÖNLÜ, konu gruplu) -----------------------
cat("\n--- 8. MANUEL TARAMA DOSYALARI ---\n")
# Okuma hızını artırmak için kaba konu grubu — KARAR DEĞİL, yalnızca sıralama
grupla <- function(d) {
  b <- blob(d)
  dplyr::case_when(
    grepl("law|legal|criminal|court|regulat|polic(y|ies)|legislat", b) ~ "1-hukuk/politika",
    grepl("consumer|behavio|attitude|intention|survey|student|perception", b) ~ "2-tuketici/davranis",
    grepl("prevent|authenticat|biometric|encrypt|blockchain|secur", b) ~ "3-onleme/guvenlik",
    grepl("review|survey of|bibliometric|overview|taxonomy", b) ~ "4-derleme",
    TRUE ~ "5-teknik/diger")
}
elenen <- M_pre[!has_det, ]
elenen$.grup <- grupla(elenen)
elenen <- elenen[order(elenen$.grup), ]
readr::write_csv(
  data.frame(id = seq_len(nrow(elenen)), grup = elenen$.grup, baslik = elenen$TI,
             yil = elenen$PY, tur = elenen$DT, ozet = kisalt(elenen$AB), karar = ""),
  "outputs/K4_elenen_TARA.csv")
cat("  YÖN 1 ->", nrow(elenen), "kayıt (konu gruplu): outputs/K4_elenen_TARA.csv\n")
print(table(elenen$.grup))

set.seed(2026)
orn <- M[sample(nrow(M), min(ORNEK_N, nrow(M))), ]
readr::write_csv(
  data.frame(id = seq_len(nrow(orn)), baslik = orn$TI, yil = orn$PY,
             tur = orn$DT, ozet = kisalt(orn$AB), karar = ""),
  "outputs/korpus_ornek_TARA.csv")
cat("  YÖN 2 ->", nrow(orn), "kayıt: outputs/korpus_ornek_TARA.csv\n")
cat("\n  karar sütunu: IN = tespit yöntemi/modeli/sistemi sunuyor\n",
    "                OUT = yalnızca önleme, hukuk, politika, tüketici davranışı\n",
    "                Sınır vaka (hem tespit hem başka kol) -> IN\n")

## 9. Profil -----------------------------------------------------------------
cat("\n--- 9. PROFİL (sözlüksel aşama sonrası) ---\n")
dt <- tolower(ifelse(is.na(M$DT), "", M$DT))
cat("  Makale:", sum(grepl("article", dt)),
    "| Bildiri:", sum(grepl("conference|proceeding", dt)), "\n")
print(table(M$DB_SRC))
p1 <- sum(M$PY <= 2015); p2 <- sum(M$PY >= 2016 & M$PY <= 2020); p3 <- sum(M$PY >= 2021)
cat(sprintf("  1993-2015: %d (%.1f%%) | 2016-2020: %d (%.1f%%) | 2021-2025: %d (%.1f%%)\n",
            p1, 100*p1/nrow(M), p2, 100*p2/nrow(M), p3, 100*p3/nrow(M)))
n0 <- sum(M$PY == 1993); n1 <- sum(M$PY == 2025)
if (n0 > 0) cat(sprintf("  CAGR 1993-2025: %.1f%% (%d -> %d)\n", ((n1/n0)^(1/32)-1)*100, n0, n1))
cr_ok <- !is.na(M$CR) & nchar(as.character(M$CR)) > 10
cat(sprintf("  CR doluluk: %.1f%%\n", 100*mean(cr_ok)))
# iç tutarlılık: artık yinelenen başlık kalmamalı (farklı DOI'liler hariç)
cat("  Benzersiz başlık:", length(unique(norm_ti(M$TI))), "/", nrow(M), "\n")

## 10. PRISMA ----------------------------------------------------------------
prisma <- data.frame(
  asama = c("Identification — Scopus", "Identification — WoS", "Ham toplam",
            "Yıl filtresi (>2025)", "K3a veritabanı içi kopya",
            "K3b veritabanları arası — DOI", "K3b veritabanları arası — başlık",
            "Benzersiz kayıt", "K1 geri çekilmiş", "K4 sözlüksel eleme",
            "Sözlüksel aşama sonrası"),
  n = c(nS_raw, nW_raw, nS_raw + nW_raw, n_yil, n_ic,
        length(dup_doi), length(dup_ti), U, sum(retr), n_k4, nrow(M)))
cat("\n--- 10. PRISMA ---\n"); print(prisma, row.names = FALSE)
readr::write_csv(prisma, "outputs/prisma_sayilari.csv")

## 11. Kaydet ----------------------------------------------------------------
M$.doi <- NULL; M$.ti <- NULL
save(M, file = "M_corpus.RData")
cat("\n>> M_pre.RData   (", nrow(M_pre), ") — K4 öncesi\n")
cat(">> M_corpus.RData(", nrow(M), ") — sözlüksel aşama sonrası\n")
cat("\nSIRADAKİ: iki TARA dosyasını doldur, sonra source(\"02_manuel_tarama_uygula.R\")\n")
