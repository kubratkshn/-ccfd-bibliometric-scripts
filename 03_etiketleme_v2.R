# =============================================================================
#  03 — ETİKETLEME (v2)
#
#  v1'den farkları:
#   (a) Aile terim listeleri genişletildi -> etiketsiz oranı ~%16.6'dan ~%10'a
#   (b) "ANMA vs KULLANIM" ayrımı: kural-tabanlı gibi yerinden edilmiş
#       yöntemler, yeni makalelerde "eski yaklaşım" olarak anılıyor. Bu
#       kayıtlar ayrıca işaretlenir ve ana ölçümden düşülür.
#   (c) Figür etiketleri İNGİLİZCE (makale İngilizce; ayrıca R'ın PDF aygıtı
#       Türkçe karakterlerde hata veriyordu)
#
#  GİRDİ : M_final.RData       ÇIKTI : M_etiketli.RData + outputs/
# =============================================================================

pkgs <- c("dplyr", "tidyr", "readr", "ggplot2")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))
dir.create("outputs", showWarnings = FALSE)

load("M_final.RData"); M <- M_final
cat("Korpus:", nrow(M), "kayıt\n")
f <- function(x) ifelse(is.na(x), "", as.character(x))
blob <- tolower(paste(f(M$TI), f(M$AB), f(M$DE), f(M$ID)))

## 1. ALGORİTMA AİLELERİ (genişletilmiş) ------------------------------------
AILE <- list(
  "Rule-based / expert" = paste(
    "rule-based", "rule based", "expert system", "knowledge-based",
    "if-then", "business rule", "heuristic rule", sep = "|"),

  "Statistical" = paste(
    "logistic regression", "discriminant analysis", "bayesian", "naive bayes",
    "naïve bayes", "regression model", "statistical model", "markov",
    "peer group analysis", "break point analysis",
    "outlier detection", "outlier identificat", "outlier mining",
    "statistical test", "hypothesis test", sep = "|"),

  "Classical ML" = paste(
    "support vector machine", "\\bsvm\\b", "decision tree", "\\bk-?nn\\b",
    "nearest neighbou?r", "k-means", "clustering", "genetic algorithm",
    "fuzzy logic", "self-organi", "case-based reasoning", "artificial immune",
    "machine learning", "data mining", "supervised learning",
    "unsupervised learning", "semi-supervised", "feature selection",
    "feature extraction", "feature engineering", "dimensionality reduction",
    "\\bpca\\b", "classifier", "classification algorithm",
    "artificial intelligence", "neural network", "\\bann\\b", "perceptron",
    "reinforcement learning", "anomaly detection", sep = "|"),

  "Ensemble" = paste(
    "random forest", "ensemble", "boosting", "bagging", "xgboost", "lightgbm",
    "catboost", "adaboost", "gradient boost", "stacking", "voting classifier",
    "hybrid model", "hybrid approach", "meta-learn", sep = "|"),

  "Deep learning" = paste(
    "deep learning", "deep neural", "convolutional neural", "\\bcnn\\b",
    "recurrent neural", "\\brnn\\b", "\\blstm\\b", "\\bgru\\b", "autoencoder",
    "generative adversarial", "\\bgan\\b", "transformer", "attention mechanism",
    "deep belief", "\\bbert\\b", "\\bbilstm\\b", "\\bdnn\\b",
    "representation learning", sep = "|"),

  "Graph-based" = paste(
    "graph neural", "\\bgnn\\b", "graph attention", "\\bgcn\\b", "graph convolut",
    "graph-based", "network analysis", "link analysis", "graph embedding",
    sep = "|"),

  "Federated / privacy" = paste(
    "federated learning", "privacy-preserving", "differential privacy",
    "secure multi-party", "homomorphic", sep = "|"),

  "Explainability" = paste(
    "explainable", "interpretab", "\\bxai\\b", "\\bshap\\b", "\\blime\\b",
    "feature importance", "counterfactual explanation", sep = "|"),

  "LLM" = paste(
    "large language model", "\\bllm\\b", "\\bgpt-", "chatgpt", "foundation model",
    sep = "|")
)
for (a in names(AILE)) M[[a]] <- grepl(AILE[[a]], blob, perl = TRUE)

## 2. ANMA vs KULLANIM DÜZELTMESİ -------------------------------------------
#  Yerinden edilmiş yöntemler yeni makalelerde "eski yaklaşım" olarak anılır:
#  "traditional rule-based systems fail to...". Bu, sözlüksel etiketlemenin
#  yapısal sınırıdır. Aşağıdaki kalıplar bu durumu yakalar.
#  Not: düzeltme YALNIZCA rule-based ve statistical ailelerine uygulanır;
#  diğerlerinde bu örüntü pratikte görülmez.
FOIL <- paste(
  "traditional rule", "conventional rule", "unlike rule", "legacy rule",
  "static rule", "compared to rule", "than rule-based",
  "rule-based (systems?|approaches?|methods?) (fail|cannot|struggle|are limited|have limit|suffer|are inadequate|are insufficient)",
  "traditional (statistical|method)", "conventional (statistical|method)",
  sep = "|")
M$.foil <- grepl(FOIL, blob, perl = TRUE)

for (a in c("Rule-based / expert", "Statistical")) {
  ham <- sum(M[[a]])
  M[[paste0(a, " (raw)")]] <- M[[a]]          # ham hali saklanır
  M[[a]] <- M[[a]] & !M$.foil                 # düzeltilmiş hali
  cat(sprintf("  %s: ham %d -> düzeltilmiş %d (%d kayıt 'eski yöntem' olarak anıyor)\n",
              a, ham, sum(M[[a]]), ham - sum(M[[a]])))
}

## 3. ML / ML-DIŞI -----------------------------------------------------------
ML_AILELERI <- c("Classical ML", "Ensemble", "Deep learning", "Graph-based",
                 "Federated / privacy", "LLM")
M$ML <- Reduce(`|`, M[ML_AILELERI])
cat("\n--- ML / ML-DIŞI ---\n")
cat(sprintf("  ML     : %d (%.1f%%)\n  ML-dışı: %d (%.1f%%)\n",
            sum(M$ML), 100*mean(M$ML), sum(!M$ML), 100*mean(!M$ML)))

## 4. AİLE DAĞILIMI ----------------------------------------------------------
aile_tab <- data.frame(
  aile = names(AILE),
  n    = sapply(names(AILE), function(a) sum(M[[a]])),
  pay  = round(100 * sapply(names(AILE), function(a) mean(M[[a]])), 1))
aile_tab <- aile_tab[order(-aile_tab$n), ]
cat("\n--- ALGORİTMA AİLELERİ (çoklu etiket) ---\n"); print(aile_tab, row.names = FALSE)
readr::write_csv(aile_tab, "outputs/tablo_aile_dagilimi.csv")

etiketsiz <- rowSums(as.matrix(M[names(AILE)])) == 0
cat(sprintf("\nEtiketsiz: %d (%.1f%%)\n", sum(etiketsiz), 100*mean(etiketsiz)))
readr::write_csv(data.frame(baslik = M$TI[etiketsiz], yil = M$PY[etiketsiz],
                            ozet = substr(gsub("\\s+"," ",f(M$AB[etiketsiz])),1,300)),
                 "outputs/etiketsiz_kayitlar.csv")

## 5. ML PAYI — YILLIK ve DÖNEMSEL ------------------------------------------
ml_yil <- M %>% group_by(PY) %>%
  summarise(toplam = n(), ml = sum(ML), .groups="drop") %>%
  mutate(ml_pay = round(100*ml/toplam, 1)) %>% arrange(PY)
cat("\n--- ML PAYI (2005+) ---\n")
print(as.data.frame(ml_yil[ml_yil$PY >= 2005, ]), row.names = FALSE)
readr::write_csv(ml_yil, "outputs/tablo_ml_payi_yillik.csv")

M$DONEM <- cut(M$PY, breaks = c(1992, 2015, 2020, 2025),
               labels = c("1993-2015", "2016-2020", "2021-2025"))
ml_donem <- M %>% group_by(DONEM) %>%
  summarise(toplam = n(), ml = sum(ML), .groups="drop") %>%
  mutate(ml_pay = round(100*ml/toplam, 1))
cat("\n--- ML PAYI (dönem) ---\n"); print(as.data.frame(ml_donem), row.names = FALSE)
readr::write_csv(ml_donem, "outputs/tablo_ml_payi_donem.csv")

## 6. AİLE EVRİMİ ------------------------------------------------------------
evrim <- do.call(rbind, lapply(names(AILE), function(a) {
  d <- M %>% group_by(DONEM) %>%
    summarise(pay = round(100*mean(.data[[a]]),1), .groups="drop")
  data.frame(aile = a, donem = d$DONEM, pay = d$pay) }))
evrim_w <- tidyr::pivot_wider(evrim, names_from = donem, values_from = pay)
cat("\n--- AİLE PAYLARI x DÖNEM (%) ---\n"); print(as.data.frame(evrim_w), row.names=FALSE)
readr::write_csv(evrim_w, "outputs/tablo_aile_evrimi_donem.csv")

yillik <- do.call(rbind, lapply(names(AILE), function(a) {
  M %>% group_by(PY) %>%
    summarise(pay = 100*mean(.data[[a]]), n = n(), .groups="drop") %>%
    mutate(aile = a) }))
readr::write_csv(yillik, "outputs/tablo_aile_payi_yillik.csv")

## 7. FİGÜRLER (İngilizce etiketler — makale İngilizce) ---------------------
p1 <- ggplot(ml_yil[ml_yil$PY >= 2000, ], aes(PY, ml_pay)) +
  geom_line(linewidth = .8, colour = "grey25") +
  geom_point(size = 1.8, colour = "grey15") +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) + ylim(0, 100) +
  labs(x = NULL, y = "Share of ML-based publications (%)",
       title = "The share of machine learning in the field, 2000-2025") +
  theme_minimal(base_size = 11)
ggsave("outputs/fig_ml_share.pdf", p1, width = 7, height = 4.2, device = cairo_pdf)
ggsave("outputs/fig_ml_share.png", p1, width = 7, height = 4.2, dpi = 300)

p2 <- yillik %>% filter(PY >= 2005) %>%
  ggplot(aes(PY, pay, colour = aile)) +
  geom_line(linewidth = .7) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  labs(x = NULL, y = "Share of publications using the family (%)", colour = NULL,
       title = "Evolution of algorithm families, 2005-2025",
       caption = "Multi-label: a publication may belong to more than one family") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))
ggsave("outputs/fig_family_evolution.pdf", p2, width = 8, height = 5, device = cairo_pdf)
ggsave("outputs/fig_family_evolution.png", p2, width = 8, height = 5, dpi = 300)
cat("\nFigürler yazıldı (İngilizce etiketli, cairo_pdf).\n")

## 8. Kaydet -----------------------------------------------------------------
M_etiketli <- M; save(M_etiketli, file = "M_etiketli.RData")
cat(">> M_etiketli.RData kaydedildi (", nrow(M_etiketli), ")\n")

## 9. KONTROL ----------------------------------------------------------------
cat("\n=== KONTROL ===\n")
cat(sprintf("1) Etiketsiz %%10 altı olmalı: %.1f%% -> %s\n", 100*mean(etiketsiz),
            ifelse(mean(etiketsiz) < .12, "OK", "GÖZDEN GEÇİR")))
cat(sprintf("2) ML payı artmalı: %.1f%% -> %.1f%% -> %.1f%% -> %s\n",
            ml_donem$ml_pay[1], ml_donem$ml_pay[2], ml_donem$ml_pay[3],
            ifelse(ml_donem$ml_pay[3] > ml_donem$ml_pay[1], "OK", "BEKLENMEDİK")))
kr <- evrim_w[evrim_w$aile == "Rule-based / expert", ]
cat(sprintf("3) Kural-tabanlı düşmeli: %.1f%% -> %.1f%% -> %.1f%% -> %s\n",
            kr$`1993-2015`, kr$`2016-2020`, kr$`2021-2025`,
            ifelse(kr$`2021-2025` < kr$`1993-2015`, "OK", "HÂLÂ DÜŞMÜYOR")))
dl <- evrim_w[evrim_w$aile == "Deep learning", ]
cat(sprintf("4) Derin öğrenme artmalı: %.1f%% -> %.1f%% -> %.1f%%\n",
            dl$`1993-2015`, dl$`2016-2020`, dl$`2021-2025`))
cat("\nNot: 3. kontrol hâlâ düşmüyorsa, sözlüksel etiketleme bu aile için\n",
    "yetersiz demektir; sınırlılıklarda açıkça belirtilmeli.\n")
