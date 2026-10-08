# =============================================================================
#  04 — İLERİ ANALİZLER
#
#  (A) RPYS   — alanın tarihsel kökleri (atıflı referans yılı spektroskopisi)
#  (B) BURST  — yükselen terimlerin istatistiksel tespiti (Kleinberg)
#  (C) METRİK — performans ölçütlerinin evrimi (accuracy -> AUC -> PR-AUC)
#  (D) VERİ SETİ — benchmark bağımlılığı (ULB/Kaggle hakimiyeti)
#
#  GİRDİ : M_etiketli.RData (2748)
#  ÇIKTI : outputs/ altında tablolar + figürler
#
#  NOT: (A) ve (B) ek paket ister. Yoksa o bölüm atlanır, script durmaz.
# =============================================================================

pkgs <- c("dplyr", "tidyr", "readr", "ggplot2", "bibliometrix")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))
dir.create("outputs", showWarnings = FALSE)

load("M_etiketli.RData"); M <- M_etiketli
cat("Korpus:", nrow(M), "kayıt\n")
f  <- function(x) ifelse(is.na(x), "", as.character(x))
bl <- tolower(paste(f(M$TI), f(M$AB), f(M$DE), f(M$ID)))

# =============================================================================
#  (A) RPYS — Reference Publication Year Spectroscopy
# =============================================================================
cat("\n================ (A) RPYS ================\n")
cr_ok <- !is.na(M$CR) & nchar(as.character(M$CR)) > 10
cat(sprintf("CR doluluk: %.1f%% (%d kayıt)\n", 100*mean(cr_ok), sum(cr_ok)))

if (mean(cr_ok) < 0.70) {
  cat("UYARI: CR doluluğu %70'in altında — RPYS güvenilir olmayabilir.\n")
}

rpys_ok <- FALSE
try({
  # Referans yıllarını CR alanından çek (4 haneli yıl örüntüsü)
  refs <- unlist(strsplit(as.character(M$CR[cr_ok]), ";"))
  yil  <- as.numeric(regmatches(refs, regexpr("(18|19|20)[0-9]{2}", refs)))
  yil  <- yil[!is.na(yil) & yil >= 1900 & yil <= 2025]
  cat("Çıkarılan referans sayısı:", length(yil), "\n")

  spec <- as.data.frame(table(yil), stringsAsFactors = FALSE)
  names(spec) <- c("yil", "n"); spec$yil <- as.numeric(spec$yil)
  spec <- spec[order(spec$yil), ]

  # Medyan sapması: her yılın, çevresindeki 5 yılın medyanından farkı
  spec$medyan5 <- sapply(seq_len(nrow(spec)), function(i) {
    idx <- max(1, i-2):min(nrow(spec), i+2); median(spec$n[idx]) })
  spec$sapma <- spec$n - spec$medyan5

  readr::write_csv(spec, "outputs/rpys_spektrum.csv")
  tepe <- spec[spec$yil >= 1990 & spec$sapma > 0, ]
  tepe <- tepe[order(-tepe$sapma), ][1:12, ]
  cat("\nEN BELİRGİN TEPE YILLARI (medyan sapması):\n")
  print(tepe[, c("yil","n","sapma")], row.names = FALSE)
  readr::write_csv(tepe, "outputs/rpys_tepe_yillar.csv")

  p <- ggplot(spec[spec$yil >= 1990, ], aes(yil, sapma)) +
    geom_col(fill = "grey35", width = .7) +
    geom_hline(yintercept = 0, colour = "grey60") +
    labs(x = NULL, y = "Deviation from 5-year median",
         title = "Reference Publication Year Spectroscopy (RPYS)",
         caption = "Peaks indicate foundational works of the field") +
    theme_minimal(base_size = 11)
  ggsave("outputs/fig_rpys.png", p, width = 8, height = 4.2, dpi = 300)
  ggsave("outputs/fig_rpys.pdf", p, width = 8, height = 4.2)
  rpys_ok <- TRUE
}, silent = FALSE)
if (!rpys_ok) cat("RPYS atlandı.\n")

# =============================================================================
#  (B) BURST DETECTION — yükselen terimler
# =============================================================================
cat("\n================ (B) BURST ================\n")
# Kleinberg algoritması için 'bursts' paketi; yoksa basit büyüme-oranı yöntemi
burst_paket <- requireNamespace("bursts", quietly = TRUE)
if (!burst_paket) {
  cat("'bursts' paketi yok — install.packages('bursts') ile kurulabilir.\n",
      "Alternatif olarak büyüme-oranı temelli yükseliş tespiti uygulanıyor.\n")
}

# İzlenecek terimler (aile terimlerinin ötesinde, teknoloji düzeyinde)
TERIMLER <- c(
  "smote"="smote", "random forest"="random forest", "xgboost"="xgboost",
  "deep learning"="deep learning", "lstm"="\\blstm\\b", "cnn"="\\bcnn\\b",
  "autoencoder"="autoencoder", "gan"="generative adversarial|\\bgan\\b",
  "graph neural network"="graph neural|\\bgnn\\b",
  "federated learning"="federated learning",
  "explainable ai"="explainable|\\bxai\\b", "shap"="\\bshap\\b",
  "transformer"="transformer", "attention"="attention mechanism",
  "class imbalance"="class imbalance|imbalanced",
  "concept drift"="concept drift", "real-time"="real-?time",
  "large language model"="large language model|\\bllm\\b",
  "support vector machine"="support vector machine|\\bsvm\\b",
  "logistic regression"="logistic regression"
)

yil_araligi <- sort(unique(M$PY[M$PY >= 2005]))
seri <- do.call(rbind, lapply(names(TERIMLER), function(t) {
  v <- grepl(TERIMLER[[t]], bl, perl = TRUE)
  data.frame(terim = t,
             PY = yil_araligi,
             n  = sapply(yil_araligi, function(y) sum(v & M$PY == y)),
             toplam = sapply(yil_araligi, function(y) sum(M$PY == y)))
}))
seri$pay <- 100 * seri$n / seri$toplam
readr::write_csv(seri, "outputs/burst_terim_serileri.csv")

# Yükseliş özeti: ilk görülme, zirve yılı, son 5 yıl ortalama pay, büyüme
ozet <- seri %>% group_by(terim) %>%
  summarise(
    ilk_gorulme = ifelse(any(n > 0), min(PY[n > 0]), NA),
    zirve_yil   = PY[which.max(pay)],
    zirve_pay   = round(max(pay), 1),
    son5_pay    = round(mean(pay[PY >= 2021]), 1),
    onceki5_pay = round(mean(pay[PY >= 2016 & PY <= 2020]), 1),
    .groups = "drop") %>%
  mutate(degisim = round(son5_pay - onceki5_pay, 1)) %>%
  arrange(desc(degisim))
cat("\nTERİM YÜKSELİŞ ÖZETİ (2021-2025 vs 2016-2020, yüzde puan):\n")
print(as.data.frame(ozet), row.names = FALSE)
readr::write_csv(ozet, "outputs/burst_yukselis_ozeti.csv")

if (burst_paket) {
  library(bursts)
  cat("\nKLEINBERG BURST (seviye >= 2):\n")
  burst_sonuc <- do.call(rbind, lapply(names(TERIMLER), function(t) {
    d <- seri[seri$terim == t & seri$n > 0, ]
    if (nrow(d) < 5) return(NULL)
    out <- try(kleinberg(rep(d$PY, d$n), gamma = 1), silent = TRUE)
    if (inherits(out, "try-error")) return(NULL)
    o <- out[out$level >= 2, ]
    if (nrow(o) == 0) return(NULL)
    data.frame(terim = t, seviye = o$level,
               baslangic = round(o$start), bitis = round(o$end))
  }))
  if (!is.null(burst_sonuc)) {
    print(burst_sonuc, row.names = FALSE)
    readr::write_csv(burst_sonuc, "outputs/burst_kleinberg.csv")
  } else cat("  Anlamlı burst bulunamadı.\n")
}

# Figür: seçili terimlerin seyri
sec <- c("smote","random forest","deep learning","graph neural network",
         "federated learning","explainable ai","transformer","large language model")
p <- seri %>% filter(terim %in% sec) %>%
  ggplot(aes(PY, pay, colour = terim)) +
  geom_line(linewidth = .7) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  labs(x = NULL, y = "Share of publications (%)", colour = NULL,
       title = "Rise of selected techniques, 2005-2025") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))
ggsave("outputs/fig_technique_rise.png", p, width = 8, height = 5, dpi = 300)
ggsave("outputs/fig_technique_rise.pdf", p, width = 8, height = 5)

# =============================================================================
#  (C) PERFORMANS METRİKLERİ
# =============================================================================
cat("\n================ (C) METRİKLER ================\n")
cat("NOT: Metrikler çoğunlukla tam metinde geçer; burada ÖZETTE raporlananlar\n",
    "ölçülüyor. Bulgu 'hangi metrik kullanıldı' değil, 'hangi metrik\n",
    "vitrine konacak kadar önemli görüldü' olarak yorumlanmalı.\n\n")

METRIK <- c(
  "Accuracy"   = "accuracy",
  "Precision/Recall" = "precision|recall",
  "F1"         = "f1-?score|f-?measure|\\bf1\\b",
  "ROC-AUC"    = "\\broc\\b|\\bauc\\b|\\bauroc\\b|roc[- ]auc|area under the (roc )?curve",
  "PR-AUC"     = "\\bpr-?auc\\b|average precision|precision-recall curve|\\baupr\\b",
  "MCC"        = "matthews|\\bmcc\\b",
  "Cost-sensitive" = "cost-sensitive|cost matrix|misclassification cost|economic cost",
  "G-mean"     = "g-mean|geometric mean"
)
mt <- do.call(rbind, lapply(names(METRIK), function(m) {
  v <- grepl(METRIK[[m]], bl, perl = TRUE)
  d <- data.frame(DONEM = M$DONEM, v = v) %>%
    group_by(DONEM) %>% summarise(pay = round(100*mean(v), 1), .groups = "drop")
  data.frame(metrik = m, donem = d$DONEM, pay = d$pay)
}))
mt_w <- tidyr::pivot_wider(mt, names_from = donem, values_from = pay)
cat("METRİK PAYLARI x DÖNEM (%):\n"); print(as.data.frame(mt_w), row.names = FALSE)
readr::write_csv(mt_w, "outputs/tablo_metrik_evrimi.csv")

# =============================================================================
#  (D) VERİ SETİ BAĞIMLILIĞI
# =============================================================================
cat("\n================ (D) VERİ SETLERİ ================\n")
VERISETI <- c(
  "ULB / European cardholder" = "ulb|european cardholder|worldline|dal pozzolo",
  "Kaggle (genel)"            = "kaggle",
  "IEEE-CIS"                  = "ieee-?cis|ieee cis fraud",
  "PaySim / sentetik"         = "paysim|synthetic (data|dataset)|simulat(ed|ion) data",
  "Gerçek banka verisi"       = "real(-| )world (bank|financial|transaction) data|proprietary dataset|private dataset|industrial partner|real bank",
  "UCI"                       = "\\buci\\b"
)
vs <- do.call(rbind, lapply(names(VERISETI), function(v) {
  x <- grepl(VERISETI[[v]], bl, perl = TRUE)
  d <- data.frame(DONEM = M$DONEM, x = x) %>%
    group_by(DONEM) %>% summarise(pay = round(100*mean(x), 1), .groups = "drop")
  data.frame(veriseti = v, donem = d$DONEM, pay = d$pay,
             toplam_n = sum(x))
}))
vs_w <- tidyr::pivot_wider(vs[, c("veriseti","donem","pay")],
                           names_from = donem, values_from = pay)
vs_w$toplam_n <- sapply(names(VERISETI), function(v)
  sum(grepl(VERISETI[[v]], bl, perl = TRUE)))
cat("VERİ SETİ PAYLARI x DÖNEM (%):\n"); print(as.data.frame(vs_w), row.names = FALSE)
readr::write_csv(vs_w, "outputs/tablo_veriseti_bagimliligi.csv")

herhangi <- Reduce(`|`, lapply(VERISETI, function(p) grepl(p, bl, perl = TRUE)))
cat(sprintf("\nHerhangi bir veri setini anan: %d (%.1f%%)\n",
            sum(herhangi), 100*mean(herhangi)))
cat("NOT: Veri seti adları çoğunlukla tam metinde geçer; bu oran ALT SINIRDIR.\n")

cat("\n=== TAMAMLANDI — çıktılar outputs/ klasöründe ===\n")
