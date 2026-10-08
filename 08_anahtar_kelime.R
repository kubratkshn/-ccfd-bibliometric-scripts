# =============================================================================
#  08 — ANAHTAR KELİME KATMANI  (makalenin bibliyometrik çekirdeği)
#
#  Hakem 2  : "anahtar kelime frekansına dayalı normalize yükselen-konu analizi"
#  Hakem 3  : "thesaurus ile anahtar kelime birleştirme; ağ ayarlarını raporla;
#              Biblioshiny tematik harita / tematik evrim ayarlarını raporla;
#              'yükselen' için açık ölçüt (büyüme oranı vb.) ver"
#
#  (A) THESAURUS  : normalizasyon kuralları + elle düzenlenebilir sözlük
#  (B) FREKANS    : en sık anahtar kelimeler (ham ve birleştirilmiş, karşılaştırma)
#  (C) AĞ         : anahtar kelime ortak-geçiş ağı (association strength, Louvain)
#  (D) TEMATİK HARİTA : Callon merkezilik-yoğunluk (tüm dönem + 3 dönem)
#  (E) TEMATİK EVRİM  : ardışık dönemler arası küme akışı (kapsama / Jaccard)
#  (F) YÜKSELEN KONULAR: dönemler arası pay, değişim oranı, Fisher + BH testi
#
#  YÖNTEM KARARLARI (Methods'a bire bir yazılacak)
#   - Birim        : kayıt başına AUTHOR keywords (DE). Keywords Plus kullanılmaz.
#   - Sayım        : full counting (bir kayıtta bir kelime en fazla 1 kez).
#   - Thesaurus    : (i) küçük harf, tire/noktalama sadeleştirme, tekil-çoğul,
#                    (ii) kısaltma ve eş anlam sözlüğü (outputs/thesaurus_kelime.csv).
#   - Arama terimleri (credit card fraud, fraud detection, fraud, credit card)
#                    frekans tablosunda GÖSTERİLİR; ağ ve haritadan ÇIKARILIR,
#                    çünkü her şeye bağlanıp yapıyı örtüyorlar (EXCL_TANIM).
#   - Ağ           : min. oluşum MIN_OCC, en çok N_AG düğüm, ağırlık =
#                    association strength (e_ij = c_ij / (s_i * s_j)), Louvain,
#                    set.seed(2026), resolution = 1. Kenar eşiği: c_ij >= 2.
#   - Tematik harita: Callon (1991) / Cobo vd. (2011) mantığı.
#                    Merkezilik = 10 x (kümenin dışarıya olan bağ ağırlıkları
#                    toplamı), yoğunluk = 100 x (iç bağ ağırlıkları toplamı /
#                    düğüm sayısı); bağ ağırlığı = eşdeğerlik indeksi
#                    c_ij^2 / (s_i * s_j). Küme adı = kümenin en sık kelimesi.
#   - Dönemler     : 1993-2015, 2016-2020, 2021-2025 (kavramsal; istatistiksel
#                    kırılma iddiası YOK). 2026 korpusta yok.
#
#  GİRDİ : M_etiketli.RData
#  ÇIKTI : outputs/kw_*.csv, thesaurus_kelime.csv, kw_network.net,
#          fig_kw_network / fig_thematic_map / fig_thematic_periods (PNG/PDF/EPS/TIFF)
#  ÇALIŞTIRMA: setwd(<proje klasörü>); source("08_anahtar_kelime.R")
# =============================================================================

pkgs <- c("dplyr", "tidyr", "readr", "ggplot2", "igraph", "ggrepel")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
dir.create("outputs", showWarnings = FALSE)

## --- AYARLAR (makalede raporlanacak) -----------------------------------------
MIN_OCC    <- 10     # ağ için min. kelime oluşumu (tüm dönem)
N_AG       <- 60     # ağdaki en çok düğüm
MIN_KENAR  <- 2      # ağda min. ortak geçiş sayısı
N_HARITA   <- 100    # tüm dönem tematik haritası için en çok kelime
MIN_F_HAR  <- 5      # tematik harita min. frekans (tüm dönem)
N_DONEM    <- 40     # dönem haritaları için en çok kelime
MIN_F_DON  <- 3      # dönem haritaları min. frekans
YUK_MIN_N  <- 10     # "yükselen" adayı: 2021-25'te en az bu kadar kayıt
YUK_ORAN   <- 2      # ... ve 2021-25 payı >= bu kat x 2016-20 payı
SEED       <- 2026
EXCL_TANIM <- c("credit card fraud", "fraud detection", "fraud", "credit card")

load("M_etiketli.RData"); M <- M_etiketli
cat("Korpus:", nrow(M), "kayıt\n")
M$DONEM <- cut(M$PY, breaks = c(1992, 2015, 2020, 2025),
               labels = c("1993-2015", "2016-2020", "2021-2025"))
stopifnot(!any(is.na(M$DONEM)))

kaydet_sekil <- function(p, ad, w, h) {
  ggsave(sprintf("outputs/%s.png",  ad), p, width = w, height = h, dpi = 300)
  ggsave(sprintf("outputs/%s.pdf",  ad), p, width = w, height = h, device = cairo_pdf)
  try(ggsave(sprintf("outputs/%s.eps",  ad), p, width = w, height = h, device = cairo_ps), silent = TRUE)
  try(ggsave(sprintf("outputs/%s.tiff", ad), p, width = w, height = h, dpi = 300,
         compression = "lzw"), silent = TRUE)
}

# =============================================================================
#  (A) THESAURUS
# =============================================================================
cat("\n================ (A) THESAURUS ================\n")
de_ham <- as.character(M$DE)
de_ham[is.na(de_ham)] <- ""
cat(sprintf("Yazar anahtar kelimesi olan kayıt: %d / %d (%.1f%%)\n",
            sum(nzchar(de_ham)), nrow(M), 100 * mean(nzchar(de_ham))))

# 1) Kural tabanlı normalizasyon
norm_kelime <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[‐‑–—-]", " ", x)            # tire türleri -> boşluk
  x <- gsub("[\"'’`.,:/\\\\]", "", x)
  x <- gsub("\\s*\\([^)]*\\)", "", x)      # parantez içi (kısaltma) at
  x <- gsub("\\s+", " ", trimws(x))
  # tekil-çoğul: son kelimeyi sadeleştir (yalnızca güvenli kalıplar)
  tekil <- function(s) {
    w <- strsplit(s, " ")[[1]]; if (!length(w)) return(s)
    son <- w[length(w)]
    if (nchar(son) > 4 && grepl("ies$", son)) son <- sub("ies$", "y", son)
    else if (nchar(son) > 4 && grepl("(ches|shes|sses|xes)$", son)) son <- sub("es$", "", son)
    else if (nchar(son) > 3 && grepl("[^su]s$", son) &&
             !grepl("(analysis|bias|loss|class|process|business|ss)$", son))
      son <- sub("s$", "", son)
    w[length(w)] <- son; paste(w, collapse = " ")
  }
  vapply(x, tekil, character(1), USE.NAMES = FALSE)
}

# 2) Elle düzenlenebilir sözlük (ilk çalıştırmada yazılır; sonra OKUNUR)
TH_DOSYA <- "outputs/thesaurus_kelime.csv"
th_varsayilan <- tibble::tribble(
  ~from, ~to,
  "credit card fraud detection", "credit card fraud",
  "credit card frauds", "credit card fraud",
  "credit card fraud prevention", "credit card fraud",
  "creditcard fraud", "credit card fraud",
  "card fraud", "credit card fraud",
  "payment card fraud", "credit card fraud",
  "bank card fraud", "credit card fraud",
  "fraud detection system", "fraud detection",
  "fraud detection model", "fraud detection",
  "fraudulent transaction", "fraud detection",
  "fraud identification", "fraud detection",
  "fraud prevention", "fraud detection",
  "financial fraud detection", "financial fraud",
  "online fraud", "online payment fraud",
  "e commerce fraud", "online payment fraud",
  "ml", "machine learning",
  "machine learning algorithm", "machine learning",
  "machine learning technique", "machine learning",
  "machine learning model", "machine learning",
  "supervised machine learning", "supervised learning",
  "unsupervised machine learning", "unsupervised learning",
  "dl", "deep learning",
  "deep neural network", "deep learning",
  "deep neural", "deep learning",
  "ann", "neural network",
  "artificial neural network", "neural network",
  "nn", "neural network",
  "cnn", "convolutional neural network",
  "rnn", "recurrent neural network",
  "lstm", "long short term memory",
  "gan", "generative adversarial network",
  "gnn", "graph neural network",
  "gcn", "graph neural network",
  "graph convolutional network", "graph neural network",
  "xai", "explainable ai",
  "explainable artificial intelligence", "explainable ai",
  "explainability", "explainable ai",
  "interpretable machine learning", "explainable ai",
  "interpretability", "explainable ai",
  "ai", "artificial intelligence",
  "svm", "support vector machine",
  "rf", "random forest",
  "dt", "decision tree",
  "knn", "k nearest neighbor",
  "k nn", "k nearest neighbor",
  "k nearest neighbour", "k nearest neighbor",
  "xgb", "xgboost",
  "extreme gradient boosting", "xgboost",
  "gradient boosting", "boosting",
  "ensemble learning", "ensemble",
  "ensemble method", "ensemble",
  "ensemble model", "ensemble",
  "ensemble classifier", "ensemble",
  "smote", "oversampling",
  "synthetic minority oversampling technique", "oversampling",
  "synthetic minority over sampling technique", "oversampling",
  "over sampling", "oversampling",
  "under sampling", "undersampling",
  "imbalanced data", "class imbalance",
  "imbalanced dataset", "class imbalance",
  "imbalanced classification", "class imbalance",
  "unbalanced data", "class imbalance",
  "unbalanced dataset", "class imbalance",
  "data imbalance", "class imbalance",
  "skewed data", "class imbalance",
  "anomaly", "anomaly detection",
  "outlier detection", "anomaly detection",
  "novelty detection", "anomaly detection",
  "federated", "federated learning",
  "privacy preserving", "privacy",
  "privacy preservation", "privacy",
  "data privacy", "privacy",
  "block chain", "blockchain",
  "big data analytic", "big data",
  "large language model", "large language model",
  "llm", "large language model",
  "chatgpt", "large language model",
  "gpt", "large language model",
  "generative ai", "generative artificial intelligence",
  "aml", "anti money laundering",
  "money laundering", "anti money laundering",
  "concept drift", "concept drift",
  "feature selection", "feature selection",
  "feature engineering", "feature engineering",
  "data mining technique", "data mining",
  "cybersecurity", "cyber security",
  "cyber crime", "cybercrime",
  "e commerce", "e commerce",
  "ecommerce", "e commerce",
  "electronic commerce", "e commerce",
  "online transaction", "online payment fraud",
  "psd2", "regulation",
  "gdpr", "regulation"
)
th_varsayilan$from <- norm_kelime(th_varsayilan$from)
th_varsayilan$to   <- norm_kelime(th_varsayilan$to)
th_varsayilan <- th_varsayilan[th_varsayilan$from != th_varsayilan$to, ]
th_varsayilan <- th_varsayilan[!duplicated(th_varsayilan$from), ]

if (!file.exists(TH_DOSYA)) {
  readr::write_csv(th_varsayilan, TH_DOSYA)
  cat("Thesaurus ilk kez yazıldı:", TH_DOSYA, "(", nrow(th_varsayilan), "satır )\n")
  cat("Eksik eş anlamlıları 'kw_adaylar_150.csv'ye bakıp bu dosyaya ekleyebilirsin.\n")
} else cat("Mevcut thesaurus okunuyor:", TH_DOSYA, "\n")
TH <- readr::read_csv(TH_DOSYA, show_col_types = FALSE)
TH$from <- norm_kelime(TH$from); TH$to <- norm_kelime(TH$to)
TH_MAP <- setNames(TH$to, TH$from)

kelime_listesi <- function(s, thes = TRUE) {
  k <- trimws(strsplit(s, ";")[[1]]); k <- k[nzchar(k)]
  if (!length(k)) return(character(0))
  k <- norm_kelime(k)
  if (thes) { h <- TH_MAP[k]; k[!is.na(h)] <- h[!is.na(h)] }
  k <- k[nzchar(k)]
  unique(k)
}
KW_HAM <- lapply(de_ham, function(s) { k <- trimws(strsplit(s, ";")[[1]]); unique(tolower(k[nzchar(k)])) })
KW     <- lapply(de_ham, kelime_listesi, thes = TRUE)
KW_N   <- lapply(de_ham, kelime_listesi, thes = FALSE)   # yalnızca kural normalizasyonu

n_tekil <- function(L) length(unique(unlist(L)))
cat(sprintf("Farklı kelime  — ham: %d | kural-normalize: %d | thesaurus sonrası: %d\n",
            n_tekil(KW_HAM), n_tekil(KW_N), n_tekil(KW)))
cat(sprintf("Azalma: %.1f%% (ham -> thesaurus)\n",
            100 * (1 - n_tekil(KW) / n_tekil(KW_HAM))))

# =============================================================================
#  (B) FREKANS TABLOSU
# =============================================================================
cat("\n================ (B) FREKANS ================\n")
frek <- function(L) { t <- sort(table(unlist(L)), decreasing = TRUE)
  data.frame(kelime = names(t), n = as.integer(t), stringsAsFactors = FALSE) }
f_ham <- frek(KW_HAM); f_th <- frek(KW)
f_th$pay_kayit <- round(100 * f_th$n / sum(lengths(KW) > 0), 2)

# aday listesi: elle gözden geçirmek için (ham, thesaurus uygulanmamış kural-normal)
aday <- frek(KW_N); aday <- aday[seq_len(min(150, nrow(aday))), ]
aday$thesaurusta_mi <- aday$kelime %in% TH$from
readr::write_csv(aday, "outputs/kw_adaylar_150.csv")
readr::write_csv(f_th[seq_len(min(100, nrow(f_th))), ], "outputs/kw_frekans_top100.csv")
cat("EN SIK 20 KELİME (thesaurus sonrası):\n")
print(head(f_th, 20), row.names = FALSE)

# karşılaştırma: thesaurus etkisi (Hakem 3'e kanıt)
kars <- merge(head(f_ham, 15), f_th[, c("kelime", "n")], by = "kelime",
              all.x = TRUE, suffixes = c("_ham", "_thesaurus"))
kars <- kars[order(-kars$n_ham), ]
readr::write_csv(kars, "outputs/kw_thesaurus_etkisi.csv")
cat("\nTHESAURUS ETKİSİ (ham en sık 15 kelime):\n"); print(kars, row.names = FALSE)

# =============================================================================
#  ORTAK GEÇİŞ YARDIMCILARI
# =============================================================================
ortak_gecis <- function(L, kelimeler) {
  L2 <- lapply(L, function(k) intersect(k, kelimeler))
  L2 <- L2[lengths(L2) > 0]
  mat <- matrix(0L, length(kelimeler), length(kelimeler),
                dimnames = list(kelimeler, kelimeler))
  for (k in L2) if (length(k) > 1) {
    pr <- t(combn(k, 2))
    mat[cbind(pr[, 1], pr[, 2])] <- mat[cbind(pr[, 1], pr[, 2])] + 1L
    mat[cbind(pr[, 2], pr[, 1])] <- mat[cbind(pr[, 2], pr[, 1])] + 1L
  }
  mat
}
secim <- function(L, n_max, min_f, haric = EXCL_TANIM) {
  t <- sort(table(unlist(L)), decreasing = TRUE)
  t <- t[!(names(t) %in% haric) & t >= min_f]
  names(t)[seq_len(min(n_max, length(t)))]
}

# =============================================================================
#  (C) ANAHTAR KELİME AĞI
# =============================================================================
cat("\n================ (C) AĞ ================\n")
nodes <- secim(KW, N_AG, MIN_OCC)
cat(sprintf("Ağ düğümü: %d (min. oluşum %d, en çok %d)\n", length(nodes), MIN_OCC, N_AG))
C <- ortak_gecis(KW, nodes)
s <- table(unlist(lapply(KW, function(k) intersect(k, nodes))))[nodes]
s <- as.numeric(s)
A <- C / outer(s, s)                         # association strength
A[C < MIN_KENAR] <- 0
diag(A) <- 0
g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE, diag = FALSE)
V(g)$freq <- s
g <- delete_vertices(g, which(degree(g) == 0))
set.seed(SEED)
kum <- cluster_louvain(g, weights = E(g)$weight, resolution = 1)
V(g)$kume <- membership(kum)
cat(sprintf("Düğüm: %d | Kenar: %d | Küme: %d | Modülerlik: %.3f\n",
            vcount(g), ecount(g), length(unique(V(g)$kume)), modularity(kum)))

nd <- data.frame(kelime = V(g)$name, frekans = V(g)$freq, kume = V(g)$kume,
                 derece = degree(g), stringsAsFactors = FALSE)
nd <- nd[order(nd$kume, -nd$frekans), ]
readr::write_csv(nd, "outputs/kw_ag_dugumler.csv")
ed <- as_data_frame(g, what = "edges")
names(ed) <- c("kaynak", "hedef", "agirlik")
ed$ortak_gecis <- mapply(function(a, b) C[a, b], ed$kaynak, ed$hedef)
readr::write_csv(ed[order(-ed$ortak_gecis), ], "outputs/kw_ag_kenarlar.csv")

ozet <- do.call(rbind, lapply(split(nd, nd$kume), function(d) {
  data.frame(kume = d$kume[1], n_kelime = nrow(d), toplam_frekans = sum(d$frekans),
             ilk_kelimeler = paste(head(d$kelime, 8), collapse = "; "))
}))
readr::write_csv(ozet, "outputs/kw_ag_kume_ozeti.csv")
cat("\nKÜME ÖZETİ:\n"); print(ozet, row.names = FALSE)

# Pajek (.net): VOSviewer ile aynı ağı açmak için
wr <- function(g, dosya) {
  ids <- seq_len(vcount(g))
  satir <- c(sprintf("*Vertices %d", vcount(g)),
             sprintf('%d "%s"', ids, V(g)$name), "*Edges",
             sprintf("%d %d %.6f", match(as_data_frame(g)$from, V(g)$name),
                     match(as_data_frame(g)$to, V(g)$name), E(g)$weight))
  writeLines(satir, dosya)
}
wr(g, "outputs/kw_network.net")

# Şekil
set.seed(SEED)
xy <- layout_with_fr(g, weights = E(g)$weight * 10)
nd_p <- data.frame(x = xy[, 1], y = xy[, 2], kelime = V(g)$name,
                   frekans = V(g)$freq, kume = factor(V(g)$kume))
eg <- as_data_frame(g, what = "edges")
eg$x <- nd_p$x[match(eg$from, nd_p$kelime)]; eg$y <- nd_p$y[match(eg$from, nd_p$kelime)]
eg$xend <- nd_p$x[match(eg$to, nd_p$kelime)]; eg$yend <- nd_p$y[match(eg$to, nd_p$kelime)]
pal <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#999999", "#332288")
p_ag <- ggplot() +
  geom_segment(data = eg, aes(x = x, y = y, xend = xend, yend = yend, alpha = weight),
               colour = "grey55", linewidth = 0.3, show.legend = FALSE) +
  geom_point(data = nd_p, aes(x, y, size = frekans, colour = kume), alpha = 0.9) +
  geom_text_repel(data = nd_p, aes(x, y, label = kelime), size = 3, max.overlaps = Inf,
                  box.padding = 0.3, segment.size = 0.2, min.segment.length = 0.1) +
  scale_size_continuous(range = c(2, 11), name = "Records") +
  scale_colour_manual(values = pal, name = "Cluster") +
  scale_alpha_continuous(range = c(0.15, 0.8)) +
  theme_void(base_size = 11) + theme(legend.position = "bottom")
kaydet_sekil(p_ag, "fig_kw_network", 9, 7.5)

# =============================================================================
#  (D) TEMATİK HARİTA
# =============================================================================
cat("\n================ (D) TEMATİK HARİTA ================\n")
tema_harita <- function(L, n_max, min_f, etiket) {
  nodes <- secim(L, n_max, min_f)
  if (length(nodes) < 8) { cat("  ", etiket, ": yetersiz kelime (", length(nodes), ")\n"); return(NULL) }
  C <- ortak_gecis(L, nodes)
  s <- as.numeric(table(unlist(lapply(L, function(k) intersect(k, nodes))))[nodes])
  E <- C^2 / outer(s, s)                    # eşdeğerlik indeksi (Callon)
  diag(E) <- 0
  g <- graph_from_adjacency_matrix(E, mode = "undirected", weighted = TRUE, diag = FALSE)
  A2 <- C / outer(s, s); A2[C < 1] <- 0; diag(A2) <- 0
  gl <- graph_from_adjacency_matrix(A2, mode = "undirected", weighted = TRUE, diag = FALSE)
  set.seed(SEED)
  mem <- membership(cluster_louvain(gl, weights = E(gl)$weight))
  nodes_df <- data.frame(kelime = nodes, frekans = s, kume = as.integer(mem[nodes]))
  out <- do.call(rbind, lapply(sort(unique(nodes_df$kume)), function(k) {
    ic  <- nodes_df$kelime[nodes_df$kume == k]
    dis <- setdiff(nodes, ic)
    ic_w  <- sum(E[ic, ic][upper.tri(E[ic, ic])])
    dis_w <- sum(E[ic, dis, drop = FALSE])
    d <- nodes_df[nodes_df$kume == k, ]
    data.frame(donem = etiket, kume = k, ad = d$kelime[which.max(d$frekans)],
               n_kelime = nrow(d), frekans = sum(d$frekans),
               merkezilik = 10 * dis_w, yogunluk = 100 * ic_w / nrow(d),
               kelimeler = paste(head(d$kelime[order(-d$frekans)], 8), collapse = "; "),
               stringsAsFactors = FALSE)
  }))
  list(kume = out, dugum = transform(nodes_df, donem = etiket))
}

h_tum <- tema_harita(KW, N_HARITA, MIN_F_HAR, "1993-2025")
readr::write_csv(h_tum$kume[order(-h_tum$kume$frekans), ], "outputs/kw_tematik_harita_tum.csv")
readr::write_csv(h_tum$dugum, "outputs/kw_tematik_harita_tum_dugumler.csv")
cat("TÜM DÖNEM — kümeler:\n"); print(h_tum$kume[, c("kume","ad","n_kelime","frekans","merkezilik","yogunluk")], row.names = FALSE)

h_don <- lapply(levels(M$DONEM), function(d) {
  idx <- which(M$DONEM == d)
  tema_harita(KW[idx], N_DONEM, MIN_F_DON, d)
})
names(h_don) <- levels(M$DONEM)
don_k <- do.call(rbind, lapply(h_don, function(h) if (!is.null(h)) h$kume))
don_d <- do.call(rbind, lapply(h_don, function(h) if (!is.null(h)) h$dugum))
readr::write_csv(don_k, "outputs/kw_tematik_harita_donemler.csv")
readr::write_csv(don_d, "outputs/kw_tematik_harita_donemler_dugumler.csv")

kadran <- function(df) {
  mc <- median(df$merkezilik); my <- median(df$yogunluk)
  df$kadran <- ifelse(df$merkezilik >= mc & df$yogunluk >= my, "Motor themes",
               ifelse(df$merkezilik <  mc & df$yogunluk >= my, "Niche themes",
               ifelse(df$merkezilik <  mc & df$yogunluk <  my, "Emerging or declining themes",
                      "Basic themes")))
  df }
h_tum$kume <- kadran(h_tum$kume)
don_k <- do.call(rbind, lapply(split(don_k, don_k$donem), kadran))
readr::write_csv(h_tum$kume, "outputs/kw_tematik_harita_tum.csv")
readr::write_csv(don_k, "outputs/kw_tematik_harita_donemler.csv")

harita_cizim <- function(df, baslik = NULL, facet = FALSE) {
  mc <- ave(df$merkezilik, df$donem, FUN = median)
  my <- ave(df$yogunluk,  df$donem, FUN = median)
  p <- ggplot(df, aes(merkezilik, yogunluk)) +
    geom_vline(data = unique(data.frame(donem = df$donem, v = mc)), aes(xintercept = v),
               colour = "grey60", linetype = "dashed") +
    geom_hline(data = unique(data.frame(donem = df$donem, h = my)), aes(yintercept = h),
               colour = "grey60", linetype = "dashed") +
    geom_point(aes(size = frekans), colour = "#0072B2", alpha = 0.55) +
    geom_text_repel(aes(label = ad), size = 3, max.overlaps = Inf) +
    scale_size_continuous(range = c(3, 14), name = "Keyword\noccurrences") +
    labs(x = "Relevance degree (Callon centrality)", y = "Development degree (Callon density)",
         title = baslik) +
    theme_bw(base_size = 11)
  if (facet) p <- p + facet_wrap(~donem, scales = "free")
  p }
kaydet_sekil(harita_cizim(h_tum$kume), "fig_thematic_map", 7.5, 6)
kaydet_sekil(harita_cizim(don_k, facet = TRUE), "fig_thematic_periods", 13, 4.8)

# =============================================================================
#  (E) TEMATİK EVRİM  (ardışık dönemler arası)
# =============================================================================
cat("\n================ (E) TEMATİK EVRİM ================\n")
evrim <- list()
dn <- levels(M$DONEM)
for (i in seq_len(length(dn) - 1)) {
  a <- h_don[[dn[i]]]; b <- h_don[[dn[i + 1]]]
  if (is.null(a) || is.null(b)) next
  for (ka in unique(a$dugum$kume)) for (kb in unique(b$dugum$kume)) {
    A <- a$dugum$kelime[a$dugum$kume == ka]; B <- b$dugum$kelime[b$dugum$kume == kb]
    ort <- intersect(A, B)
    if (!length(ort)) next
    evrim[[length(evrim) + 1]] <- data.frame(
      kaynak_donem = dn[i], kaynak_kume = a$kume$ad[a$kume$kume == ka],
      hedef_donem = dn[i + 1], hedef_kume = b$kume$ad[b$kume$kume == kb],
      ortak_kelime = length(ort),
      kapsama = round(length(ort) / min(length(A), length(B)), 3),
      jaccard = round(length(ort) / length(union(A, B)), 3),
      ortak = paste(head(ort, 6), collapse = "; "), stringsAsFactors = FALSE)
  }
}
evrim <- do.call(rbind, evrim)
if (!is.null(evrim)) {
  evrim <- evrim[order(evrim$kaynak_donem, -evrim$kapsama), ]
  readr::write_csv(evrim, "outputs/kw_tematik_evrim.csv")
  cat("En güçlü geçişler (kapsama >= 0.30):\n")
  print(evrim[evrim$kapsama >= 0.30, c("kaynak_donem","kaynak_kume","hedef_donem","hedef_kume","ortak_kelime","kapsama","jaccard")],
        row.names = FALSE)
}

# =============================================================================
#  (F) YÜKSELEN KONULAR — normalize ve test edilmiş
# =============================================================================
cat("\n================ (F) YÜKSELEN KONULAR ================\n")
d1 <- which(M$DONEM == "2016-2020"); d2 <- which(M$DONEM == "2021-2025")
d0 <- which(M$DONEM == "1993-2015")
bos <- lengths(KW) == 0
N1 <- sum(!bos[d1]); N2 <- sum(!bos[d2]); N0 <- sum(!bos[d0])
cat(sprintf("Anahtar kelimesi olan kayıt: %d / %d / %d (1993-2015 / 2016-2020 / 2021-2025)\n", N0, N1, N2))

sayac <- function(idx) { t <- table(unlist(KW[idx])); setNames(as.integer(t), names(t)) }
c0 <- sayac(d0); c1 <- sayac(d1); c2 <- sayac(d2)
tum <- union(union(names(c0), names(c1)), names(c2))
g0 <- function(v, k) ifelse(is.na(v[k]), 0L, v[k])
Y <- data.frame(kelime = tum, n_1993_2015 = g0(c0, tum), n_2016_2020 = g0(c1, tum),
                n_2021_2025 = g0(c2, tum), stringsAsFactors = FALSE)
Y$pay_93_15 <- 100 * Y$n_1993_2015 / N0
Y$pay_16_20 <- 100 * Y$n_2016_2020 / N1
Y$pay_21_25 <- 100 * Y$n_2021_2025 / N2
Y$oran_21_25_vs_16_20 <- ifelse(Y$pay_16_20 > 0, Y$pay_21_25 / Y$pay_16_20, NA)
Y$fark_pp <- Y$pay_21_25 - Y$pay_16_20
# kelime başına ortalama yayın yılı (VOSviewer 'avg. pub. year' ile aynı mantık)
ort_yil <- tapply(rep(M$PY, lengths(KW)), unlist(KW), mean)
Y$ort_yil <- round(as.numeric(ort_yil[Y$kelime]), 2)
Y$toplam <- Y$n_1993_2015 + Y$n_2016_2020 + Y$n_2021_2025
# Test yalnızca toplam >= 5 kayıtlı kelimelere uygulanır (BH düzeltmesi anlamlı kalsın)
Y <- Y[Y$toplam >= 5, ]
# Fisher: kelime 2021-25'te mi, 2016-20'de mi daha yaygın? (iki taraflı)
Y$p_fisher <- mapply(function(a, b) fisher.test(matrix(c(a, N2 - a, b, N1 - b), 2))$p.value,
                     Y$n_2021_2025, Y$n_2016_2020)
Y$p_bh <- p.adjust(Y$p_fisher, method = "BH")
Y <- Y[Y$kelime %in% secim(KW, 9999, 1) , ]            # arama terimleri hariç
Y$yukselen <- with(Y, n_2021_2025 >= YUK_MIN_N &
                      (is.na(oran_21_25_vs_16_20) | oran_21_25_vs_16_20 >= YUK_ORAN) &
                      p_bh < 0.05)
Y <- Y[order(-Y$n_2021_2025), ]
Yr <- Y; num <- sapply(Yr, is.numeric); Yr[num] <- lapply(Yr[num], round, 3)
readr::write_csv(Yr, "outputs/kw_yukselen_tum.csv")
sec <- Yr[Yr$yukselen, ]; sec <- sec[order(-sec$n_2021_2025), ]
readr::write_csv(sec, "outputs/kw_yukselen_adaylar.csv")
cat(sprintf("Ölçüt: 2021-25'te >= %d kayıt VE pay >= %.1f x (2016-20) VE Fisher BH p < 0.05\n",
            YUK_MIN_N, YUK_ORAN))
cat("YÜKSELEN KONULAR (ölçütü sağlayan):\n")
print(sec[, c("kelime","n_2016_2020","n_2021_2025","pay_16_20","pay_21_25",
              "oran_21_25_vs_16_20","ort_yil","p_bh")], row.names = FALSE)

# Belirli teknolojiler (makalede adı geçenler) — ölçütü sağlamasa da raporla
izle <- c("graph neural network", "federated learning", "explainable ai",
          "large language model", "generative artificial intelligence", "blockchain",
          "transformer", "autoencoder", "deep learning", "class imbalance",
          "oversampling", "concept drift")
izle_t <- Yr[Yr$kelime %in% izle, c("kelime","n_1993_2015","n_2016_2020","n_2021_2025",
                                    "pay_16_20","pay_21_25","oran_21_25_vs_16_20",
                                    "ort_yil","p_bh","yukselen")]
readr::write_csv(izle_t, "outputs/kw_izlenen_teknolojiler.csv")
cat("\nİZLENEN TEKNOLOJİLER:\n"); print(izle_t, row.names = FALSE)

# =============================================================================
#  ÖZET (makaleye geçecek sayılar)
# =============================================================================
cat("\n================ ÖZET ================\n")
cat(sprintf("Yazar anahtar kelimesi olan kayıt : %d / %d (%.1f%%)\n",
            sum(!bos), nrow(M), 100 * mean(!bos)))
cat(sprintf("Farklı kelime (ham -> thesaurus)  : %d -> %d\n", n_tekil(KW_HAM), n_tekil(KW)))
cat(sprintf("Ağ: %d düğüm, %d kenar, %d küme, modülerlik %.3f\n",
            vcount(g), ecount(g), length(unique(V(g)$kume)), modularity(kum)))
cat(sprintf("Tematik harita (tüm dönem): %d küme\n", nrow(h_tum$kume)))
cat(sprintf("Yükselen ölçütünü sağlayan kelime : %d\n", nrow(sec)))
cat("\nÇIKTI: outputs/ altında kw_*.csv, thesaurus_kelime.csv, kw_network.net, fig_*\n")
cat("Bu çıktıyı (konsol) bana at; makale 3.5/4.3 ve cevap maddelerini ona göre yazarım.\n")
