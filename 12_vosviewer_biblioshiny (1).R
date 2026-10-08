# =============================================================================
#  12 — VOSviewer VE BIBLIOSHINY İÇİN HAZIR DOSYALAR
#
#  Karar: Sayılar (küme, modülerlik, yoğunluk, merkezilik) R betiklerinden gelir.
#  VOSviewer bu aynı ağları ÇİZER (aynı kümeler, aynı düğüm boyutu), Biblioshiny
#  ise özet göstergeleri ayrıca kontrol etmek içindir. Böylece iki araç iki farklı
#  sonuç üretmez ve makale metni ile şekiller tutarlı kalır.
#
#  GİRDİ : M_etiketli.RData ve outputs/ altındaki 05, 06, 08 çıktıları
#  ÇIKTI : outputs/vos_<ağ>_map.txt, outputs/vos_<ağ>_network.txt  (3 ağ için)
#          M_biblioshiny.RData  (Biblioshiny'ye yüklenecek veri)
#  ÇALIŞTIRMA: source("12_vosviewer_biblioshiny.R")
#
#  VOSviewer'da açma: Create > "Create a map based on network data" >
#     "Read data from VOSviewer files" > Map file = vos_X_map.txt,
#     Network file = vos_X_network.txt
# =============================================================================
pkgs <- c("igraph", "readr", "dplyr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
dir.create("outputs", showWarnings = FALSE)

# Pajek .net -> (etiketler, kenarlar)
oku_net <- function(dosya) {
  s <- readLines(dosya, encoding = "UTF-8", warn = FALSE)
  iv <- grep("^\\*Vertices", s); ie <- grep("^\\*Edges", s)
  v <- s[(iv + 1):(ie - 1)]
  lab <- sub('^[0-9]+ "(.*)"$', "\\1", v)
  e <- strsplit(trimws(s[(ie + 1):length(s)]), "\\s+")
  e <- e[lengths(e) >= 3]
  list(label = lab,
       edge = data.frame(a = as.integer(vapply(e, `[`, "", 1)),
                         b = as.integer(vapply(e, `[`, "", 2)),
                         w = as.numeric(vapply(e, `[`, "", 3))))
}

yaz_vos <- function(ad, lab, kume, agirlik, agirlik_ad, edge, cekim = FALSE) {
  g <- graph_from_data_frame(
    data.frame(from = edge$a, to = edge$b, weight = edge$w),
    directed = FALSE, vertices = data.frame(name = seq_along(lab)))
  set.seed(2026)
  if (!cekim) {
    xy <- layout_with_fr(g, weights = E(g)$weight)
    xy <- scale(xy)                     # VOSviewer ölçeği için
  } else {
    # Çekim düğümü: az bağlantılı düğümlerin ve ayrık kümelerin haritadan
    # dışarı savrulmasını önler (yalnızca yerleşim için; ağ değişmez).
    n <- length(lab)
    w <- log1p(E(g)$weight); w <- w / max(w)
    h <- add_vertices(g, 1)
    h <- add_edges(h, as.vector(rbind(seq_len(n), n + 1)))
    E(h)$weight <- c(w, rep(0.05, n))
    xy <- layout_with_fr(h, weights = E(h)$weight, niter = 1000)[seq_len(n), ]
    xy <- scale(xy)
    r <- sqrt(rowSums(xy^2)); k <- 2.2
    xy <- xy * ifelse(r > 0, tanh(r / k) * k / r, 1)
  }
  map <- data.frame(id = seq_along(lab), label = lab, x = round(xy[, 1], 4),
                    y = round(xy[, 2], 4), cluster = kume, w = agirlik)
  names(map)[6] <- paste0("weight<", agirlik_ad, ">")
  write.table(map, sprintf("outputs/vos_%s_map.txt", ad), sep = "\t",
              quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(edge, sprintf("outputs/vos_%s_network.txt", ad), sep = "\t",
              quote = FALSE, row.names = FALSE, col.names = FALSE)
  cat(sprintf("%-12s düğüm %d | kenar %d | küme %d\n", ad, length(lab), nrow(edge),
              length(unique(kume))))
}

# 1) Anahtar kelime ağı  (08)
n <- oku_net("outputs/kw_network.net")
nd <- read_csv("outputs/kw_ag_dugumler.csv", show_col_types = FALSE)
i <- match(n$label, nd$kelime)
yaz_vos("kelime", n$label, nd$kume[i], nd$frekans[i], "Records", n$edge)

# 2) Eş-atıf ağı  (05)
n <- oku_net("outputs/cocitation.net")
nd <- read_csv("outputs/cocitation_kumeler.csv", show_col_types = FALSE)
nd <- nd[!duplicated(nd$etiket), ]
i <- match(n$label, nd$etiket)
yaz_vos("cocitation", n$label, nd$kume[i], nd$atif[i], "Citations", n$edge)

# 3) Bibliyografik eşleşme ağı  (06)
n <- oku_net("outputs/bibcoupling.net")
nd <- read_csv("outputs/bibcoupling_makaleler.csv", show_col_types = FALSE)
nd <- nd[!duplicated(nd$etiket), ]
i <- match(n$label, nd$etiket)
yaz_vos("bibcoupling", n$label, nd$kume[i], nd$TC[i], "Citations", n$edge, cekim = TRUE)

# Biblioshiny için veri: nesne adı M olmalı
load("M_etiketli.RData"); M <- M_etiketli
save(M, file = "M_biblioshiny.RData")
cat("\nBiblioshiny: bibliometrix::biblioshiny() > Data > Import or Load >",
    "'Load bibliometrix file(s)' > M_biblioshiny.RData\n")
cat("Kontrol: üç ağın düğüm ve küme sayısı betik çıktılarıyla aynı olmalı",
    "(kelime 60 düğüm/6 küme; eş-atıf 60/4; eşleşme 86/5).\n")
cat("VOSviewer sürümünü (Help > About) bana yaz.\n")
