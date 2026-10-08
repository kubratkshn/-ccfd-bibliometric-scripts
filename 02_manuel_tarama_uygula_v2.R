# =============================================================================
#  02 — MANUEL TARAMA KARARLARINI UYGULA  (v2)
#
#  v1'deki HATA DÜZELTİLDİ: son adımdaki !duplicated(M_final$TI) satırı,
#  01'de bilerek korunan "aynı başlık / farklı DOI" kayıtlarını siliyordu
#  (45 geçerli konferans bildirisi). Bu satır kaldırıldı; geri alınan kayıtlar
#  artık doğrudan K4'ün elediği kümeden çekiliyor, dolayısıyla M ile çakışma
#  teorik olarak imkânsız ve dedup'a gerek yok.
#
#  ÇALIŞTIRMA: source("02_manuel_tarama_uygula_v2.R")
# =============================================================================

pkgs <- c("dplyr", "readr"); invisible(lapply(pkgs, library, character.only = TRUE))

load("M_pre.RData")        # K4 öncesi (3001)
load("M_corpus.RData")     # sözlüksel aşama sonrası (2725) -> M
cat("Başlangıç — M_pre:", nrow(M_pre), "| M_corpus:", nrow(M), "\n")

# K4'ün elediği kümeyi yeniden üret (01'deki ile birebir aynı kural)
DET_TERMS <- c("detect", "identif", "classif", "predict", "recogni",
               "screening", "early warning", "anomal", "discover", "monitor")
DET <- paste(DET_TERMS, collapse = "|")
f <- function(x) ifelse(is.na(x), "", as.character(x))
blob_pre <- tolower(paste(f(M_pre$TI), f(M_pre$AB)))
ELENEN <- M_pre[!grepl(DET, blob_pre, perl = TRUE), ]
stopifnot(nrow(ELENEN) + nrow(M) == nrow(M_pre))   # tutarlılık kontrolü
cat("Elenen küme yeniden üretildi:", nrow(ELENEN), "\n")

## 1. YÖN 1 — yanlış elenenleri geri al -------------------------------------
e <- readr::read_csv("outputs/K4_elenen_TARA.csv", show_col_types = FALSE)
e$karar <- toupper(trimws(e$karar))
bos <- is.na(e$karar) | !(e$karar %in% c("IN", "OUT"))
if (any(bos)) stop("K4_elenen_TARA.csv: ", sum(bos), " satırda karar eksik/hatalı.")

geri <- ELENEN[ELENEN$TI %in% e$baslik[e$karar == "IN"], ]
cat("\nYÖN 1 — taranan:", nrow(e), "| geri alınan:", nrow(geri), "\n")
fn_oran <- 100 * nrow(geri) / nrow(e)
cat(sprintf("  Yanlış eleme oranı (false negative): %.1f%%\n", fn_oran))

## 2. YÖN 2 — yanlış kalanları çıkar ----------------------------------------
#  Bu dosya henüz doldurulmadıysa adım atlanır ve precision raporlanmaz.
yon2_var <- FALSE; cikar_baslik <- character(0); fp_oran <- NA_real_; n_orn <- 0
if (file.exists("outputs/korpus_ornek_TARA.csv")) {
  o <- readr::read_csv("outputs/korpus_ornek_TARA.csv", show_col_types = FALSE)
  o$karar <- toupper(trimws(o$karar))
  bos2 <- is.na(o$karar) | !(o$karar %in% c("IN", "OUT"))
  if (all(bos2)) {
    cat("\nYÖN 2 — dosya BOŞ, bu adım atlandı.\n",
        "  Metodolojide precision raporlamak için bu dosya doldurulmalı.\n")
  } else if (any(bos2)) {
    stop("korpus_ornek_TARA.csv: ", sum(bos2), " satırda karar eksik/hatalı.")
  } else {
    yon2_var <- TRUE; n_orn <- nrow(o)
    cikar_baslik <- o$baslik[o$karar == "OUT"]
    fp_oran <- 100 * length(cikar_baslik) / n_orn
    cat("\nYÖN 2 — taranan:", n_orn, "| kapsam dışı:", length(cikar_baslik), "\n")
    cat(sprintf("  PRECISION: %.1f%%\n", 100 - fp_oran))
  }
}

## 3. Nihai korpus -----------------------------------------------------------
#  DİKKAT: burada !duplicated(TI) YOK. M ile geri ayrık kümeler; ayrıca
#  "aynı başlık / farklı DOI" kayıtları 01'de bilerek korundu.
M_final <- dplyr::bind_rows(M, geri)
if (yon2_var && length(cikar_baslik) > 0)
  M_final <- M_final[!(M_final$TI %in% cikar_baslik), ]
cat("\n--- NİHAİ KORPUS:", nrow(M_final), "---\n")
cat("  (", nrow(M), "+", nrow(geri), "-", length(cikar_baslik), ")\n")

## 4. Profil -----------------------------------------------------------------
dt <- tolower(f(M_final$DT))
cat("\nMakale:", sum(grepl("article", dt)),
    "| Bildiri:", sum(grepl("conference|proceeding", dt)), "\n")
print(table(M_final$DB_SRC))
n <- nrow(M_final)
p1 <- sum(M_final$PY <= 2015); p2 <- sum(M_final$PY >= 2016 & M_final$PY <= 2020)
p3 <- sum(M_final$PY >= 2021)
cat(sprintf("1993-2015: %d (%.1f%%) | 2016-2020: %d (%.1f%%) | 2021-2025: %d (%.1f%%)\n",
            p1, 100*p1/n, p2, 100*p2/n, p3, 100*p3/n))
n0 <- sum(M_final$PY == 1993); n1 <- sum(M_final$PY == 2025)
if (n0 > 0) cat(sprintf("CAGR 1993-2025: %.1f%% (%d -> %d)\n", ((n1/n0)^(1/32)-1)*100, n0, n1))
cr_ok <- !is.na(M_final$CR) & nchar(as.character(M_final$CR)) > 10
cat(sprintf("CR doluluk: %.1f%%\n", 100*mean(cr_ok)))

## 5. Rapor ------------------------------------------------------------------
rap <- data.frame(
  olcum = c("Sözlüksel kuralla elenen", "Manuel tarama sonrası geri alınan",
            "Yanlış eleme oranı (%)", "Taranan örneklem (YÖN 2)",
            "Örneklemde kapsam dışı", "Tahmini precision (%)", "NİHAİ KORPUS"),
  deger = c(nrow(e), nrow(geri), round(fn_oran, 1),
            n_orn, length(cikar_baslik),
            ifelse(yon2_var, round(100 - fp_oran, 1), NA), nrow(M_final)))
cat("\n"); print(rap, row.names = FALSE)
readr::write_csv(rap, "outputs/manuel_tarama_raporu.csv")
readr::write_csv(e[, c("baslik", "yil", "karar")], "outputs/EK_K4_kararlari.csv")

save(M_final, file = "M_final.RData")
cat("\n>> M_final.RData kaydedildi (", nrow(M_final), "kayıt )\n")

## 6. Metodoloji cümlesi -----------------------------------------------------
cat("\n--- METODOLOJİ CÜMLESİ ---\n")
if (yon2_var) {
  cat(sprintf(
"Scope screening (K4) was conducted in two stages. First, a lexical rule was applied
to title and abstract fields, retaining records containing at least one detection-related
term; this excluded %d records. Second, all %d excluded records were manually reviewed,
and %d (%.1f%%) were found to present a detection method despite lacking the lexical
markers and were reinstated. To assess the rule in the opposite direction, a random
sample of %d retained records was reviewed; %d were judged out of scope, implying an
estimated precision of %.1f%%. The final corpus comprises %d records.\n",
nrow(e), nrow(e), nrow(geri), fn_oran, n_orn, length(cikar_baslik),
100 - fp_oran, nrow(M_final)))
} else {
  cat(sprintf(
"Scope screening (K4) was conducted in two stages. First, a lexical rule was applied
to title and abstract fields, retaining records containing at least one detection-related
term; this excluded %d records. Second, all %d excluded records were manually reviewed,
and %d (%.1f%%) were found to present a detection method despite lacking the lexical
markers and were reinstated. The final corpus comprises %d records.\n",
nrow(e), nrow(e), nrow(geri), fn_oran, nrow(M_final)))
  cat("\n[YÖN 2 yapılmadı — precision cümlesi eklenmedi.]\n")
}
