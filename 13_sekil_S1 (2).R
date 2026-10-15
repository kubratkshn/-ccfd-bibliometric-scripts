# =============================================================================
#  13 — ŞEKİL S1: YILLIK YAYIN SAYISI, 1993–2025  (Hakem 2, madde 2.7)
#
#  Ne yapar: 2.748 kaydın yıllara göre sayısını çizer. Her sütun ML ve ML olmayan
#            kayıtları ayrı renkte gösterir; üç dönemin sınırı (2015|2016,
#            2020|2021) kesik çizgiyle işaretlidir. Dönemler kavramsaldır.
#  GİRDİ : M_etiketli.RData   (07, 09 ve 12 numaralı betiklerle aynı dosya)
#  ÇIKTI: outputs/FigS1_yillik_yayin.png / .pdf / .tiff  ve  FigS1_yillik_sayilar.csv
#  ÇALIŞTIRMA: source("13_sekil_S1.R")
# =============================================================================
pkgs <- c("dplyr", "readr", "ggplot2", "tidyr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
dir.create("outputs", showWarnings = FALSE)

load("M_etiketli.RData"); M <- M_etiketli
M$PY <- as.numeric(M$PY)
M <- M[M$PY >= 1993 & M$PY <= 2025, ]
stopifnot("ML" %in% names(M), !any(is.na(M$PY)))

# --- Doğrulama: makaledeki sayılarla aynı olmalı ----------------------------
DONEM <- cut(M$PY, c(1992, 2015, 2020, 2025), labels = c("1993-2015", "2016-2020", "2021-2025"))
cat("Korpus:", nrow(M), "kayıt\n"); print(table(DONEM))
stopifnot(
  "Kayıt sayısı 2.748 değil: M_etiketli.RData eski olabilir" = nrow(M) == 2748,
  "Dönem sayıları 346 / 561 / 1.841 değil"                    = identical(as.integer(table(DONEM)), c(346L, 561L, 1841L))
)

# --- Yıllık sayılar ------------------------------------------------------------
yillik <- M |>
  mutate(Grup = ifelse(as.integer(ML) == 1, "Records with an ML label", "Records without an ML label")) |>
  count(PY, Grup, name = "Kayit") |>
  complete(PY = 1993:2025, Grup, fill = list(Kayit = 0)) |>
  arrange(PY, Grup)
toplam <- yillik |> group_by(PY) |> summarise(Toplam = sum(Kayit), .groups = "drop")
write.csv(toplam, "outputs/FigS1_yillik_sayilar.csv", row.names = FALSE)
cat("1993:", toplam$Toplam[toplam$PY == 1993], "| 2018:", toplam$Toplam[toplam$PY == 2018],
    "| 2025:", toplam$Toplam[toplam$PY == 2025], "\n")   # makale: 2, 116, 587

# --- Şekil -------------------------------------------------------------------------
yillik$Grup <- factor(yillik$Grup, levels = c("Records without an ML label", "Records with an ML label"))
ymax <- max(toplam$Toplam)

# Renk paleti: "A" (yeşilimsi mavi), "B" (bordo), "C" (kiremit). İlk renk ML'li, ikinci ML'siz kayıtlar.
PAL <- "A"
PALET <- list(A = c("#0B6E6E", "#D8D2C4"), B = c("#8C2D3A", "#D9D4D0"), C = c("#C8553D", "#CFD8DC"))[[PAL]]
g <- ggplot(yillik, aes(PY, Kayit, fill = Grup)) +
  geom_col(width = 0.8) +
  geom_vline(xintercept = c(2015.5, 2020.5), linetype = "dashed", colour = "grey35", linewidth = 0.4) +
  annotate("text", x = c(2004, 2018, 2023), y = ymax * 1.06, size = 3.4, colour = "grey20",
           label = c("1993\u20132015\nn = 346", "2016\u20132020\nn = 561", "2021\u20132025\nn = 1,841")) +
  scale_fill_manual(values = c("Records without an ML label" = PALET[2], "Records with an ML label" = PALET[1]), name = NULL) +
  scale_x_continuous(breaks = seq(1993, 2025, 4), expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = "Publication year", y = "Number of records") +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", axis.line = element_line(colour = "grey30", linewidth = 0.3))

ggsave("outputs/FigS1_yillik_yayin.png",  g, width = 7.5, height = 4.2, dpi = 600, bg = "white")
ggsave("outputs/FigS1_yillik_yayin.pdf",  g, width = 7.5, height = 4.2, device = "pdf")   # cairo_pdf Mac'te XQuartz ister; "pdf" ister istemez
ggsave("outputs/FigS1_yillik_yayin.tiff", g, width = 7.5, height = 4.2, dpi = 600, bg = "white", compression = "lzw")
cat("Tamam: outputs/FigS1_yillik_yayin.[png|pdf|tiff]\n")
