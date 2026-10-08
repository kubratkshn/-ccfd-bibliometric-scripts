# =============================================================================
#  09 — DÖNEM SINIRI DUYARLILIK KONTROLÜ  (Hakem 2: "period sensitivity analysis")
#
#  Soru: ML payı ve ensemble / derin öğrenme artışı, dönem sınırlarını
#        değiştirince de aynı mı çıkıyor?
#  Yöntem: (1) dört farklı sınır kuralı, (2) sınırsız kontrol: yıl başına lojistik
#          eğim (odds oranı / yıl). Dönemler kavramsaldır; bu betik onların
#          sonuçları belirlemediğini gösterir.
#  GİRDİ : M_etiketli.RData     ÇIKTI: outputs/duyarlilik_*.csv
#  ÇALIŞTIRMA: source("09_donem_duyarlilik.R")
# =============================================================================
pkgs <- c("dplyr", "readr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
dir.create("outputs", showWarnings = FALSE)
load("M_etiketli.RData"); M <- M_etiketli
M <- M[M$PY >= 1993 & M$PY <= 2025, ]
cat("Korpus:", nrow(M), "kayıt\n")

IZLE <- c("ML", "Ensemble", "Deep learning")
stopifnot(all(IZLE %in% names(M)))

KURAL <- list(
  "A. Makale (2015 / 2020)"        = c(1992, 2015, 2020, 2025),
  "B. Kaydırılmış (2016 / 2021)"   = c(1992, 2016, 2021, 2025),
  "C. 5 yıllık bloklar"            = c(1992, 2005, 2010, 2015, 2020, 2025),
  "D. Eşit büyüklükte üç grup"     = unique(c(1992, quantile(M$PY, c(1/3, 2/3), type = 1), 2025))
)

sonuc <- list()
for (ad in names(KURAL)) {
  br <- KURAL[[ad]]
  M$G <- cut(M$PY, breaks = br)
  lab <- levels(M$G)
  tab <- lapply(lab, function(l) {
    d <- M[M$G == l, ]
    data.frame(kural = ad, grup = l, n = nrow(d),
               ML = round(100 * mean(d$ML), 1),
               Ensemble = round(100 * mean(d$Ensemble), 1),
               DeepLearning = round(100 * mean(d[["Deep learning"]]), 1))
  })
  sonuc[[ad]] <- do.call(rbind, tab)
}
sonuc <- do.call(rbind, sonuc); rownames(sonuc) <- NULL
cat("\n--- PAYLAR (%): dört farklı sınır kuralı ---\n"); print(sonuc, row.names = FALSE)
readr::write_csv(sonuc, "outputs/duyarlilik_donemler.csv")

# Her kuralda artış tek yönlü mü?
cat("\n--- Her kuralda ilk grup -> son grup değişimi ve sıra tek yönlü mü ---\n")
yon <- do.call(rbind, lapply(split(sonuc, sonuc$kural), function(d) {
  data.frame(kural = d$kural[1],
             ML_artis = d$ML[nrow(d)] - d$ML[1], ML_tek_yonlu = all(diff(d$ML) >= 0),
             Ens_artis = d$Ensemble[nrow(d)] - d$Ensemble[1], Ens_tek_yonlu = all(diff(d$Ensemble) >= 0),
             DL_artis = d$DeepLearning[nrow(d)] - d$DeepLearning[1], DL_tek_yonlu = all(diff(d$DeepLearning) >= 0))
}))
print(yon, row.names = FALSE)
readr::write_csv(yon, "outputs/duyarlilik_yon.csv")

# Sınırsız kontrol: yıl başına odds oranı
cat("\n--- SINIRSIZ KONTROL: yıl başına lojistik eğim (dönem yok) ---\n")
egim <- do.call(rbind, lapply(c("ML", "Ensemble", "Deep learning"), function(v) {
  y <- as.integer(M[[v]]); x <- M$PY - 2000
  m <- glm(y ~ x, family = binomial())
  b <- coef(summary(m))["x", ]
  data.frame(degisken = v, OR_yil = round(exp(b[1]), 3),
             alt = round(exp(b[1] - 1.96 * b[2]), 3), ust = round(exp(b[1] + 1.96 * b[2]), 3),
             p = signif(b[4], 3))
}))
print(egim, row.names = FALSE)
readr::write_csv(egim, "outputs/duyarlilik_egim.csv")
cat("\nOKUMA: OR_yil > 1 ve güven aralığı 1'i içermiyorsa pay yıllara göre artıyor;\n",
    "dönem sınırı seçiminden bağımsızdır. Bu çıktıyı (konsol) bana at.\n")
