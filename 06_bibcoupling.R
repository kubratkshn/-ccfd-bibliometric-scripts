# =============================================================================
#  06 — BIBLIOGRAPHIC COUPLING (Hakem 2'nin istediği analiz)
#
#  MANTIK
#   Co-citation  : iki ESER birlikte atıf alıyorsa bağlıdır (geçmişe bakar).
#   Coupling     : iki MAKALE ortak referans veriyorsa bağlıdır (korpusun
#                  kendi makalelerini, yani sahanın GÜNCEL cephesini gösterir).
#   İkisi birbirini tamamlar: co-citation "entelektüel temel", coupling
#   "bugün sahada ne yapılıyor" sorusuna cevap verir.
#
#  YÖNTEM
#   - Birim: korpustaki makale. En çok atıf alan N makale (TC) seçilir;
#     en az MIN_REF referansı olanlar dahil.
#   - Referanslar, 05_cocitation.R'ın normalize ettiği çalışma anahtarlarıdır
#     (Scopus ';' sorunu çözülmüş, Scopus/WoS birleşik) -> 05 ÖNCE çalışmalı.
#   - Bağ gücü  : ortak referans sayısı; kümeleme için kosinüs (Salton)
#                 normalizasyonu  = ortak / sqrt(n_i * n_j).
#   - Gürültü   : ortak referans sayısı MIN_ORTAK'ın altındaki bağlar atılır.
#   - Kümeleme  : Louvain (set.seed(2026)); VOSviewer için Pajek (.net).
#
#  GİRDİ : M_etiketli.RData + refs_normalized.RData (05'ten)
#  ÇIKTI : outputs/bibcoupling_*.csv, outputs/bibcoupling.net
#  ÇALIŞTIRMA: source("05_cocitation.R"); source("06_bibcoupling.R")
# =============================================================================

pkgs <- c("dplyr", "readr", "igraph", "Matrix")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
dir.create("outputs", showWarnings = FALSE)

if (!file.exists("refs_normalized.RData"))
  stop("refs_normalized.RData yok. Önce source('05_cocitation.R') çalıştır.")
load("M_etiketli.RData"); M <- M_etiketli
load("refs_normalized.RData")          # uzun: doc, key, ...

TOP_N     <- 100   # ağa alınacak en çok atıflı makale sayısı
MIN_REF   <- 10    # asgari referans sayısı (az referanslı kayıt anlamlı bağ kuramaz)
MIN_ORTAK <- 3     # bağ için asgari ortak referans

## 1. Aday makaleler ----------------------------------------------------------
n_ref <- tabulate(uzun$doc, nbins = nrow(M))
tc <- suppressWarnings(as.numeric(M$TC)); tc[is.na(tc)] <- 0
aday <- which(n_ref >= MIN_REF)
cat(sprintf("Korpus: %d | >= %d referanslı: %d\n", nrow(M), MIN_REF, length(aday)))
secili <- aday[order(-tc[aday], -n_ref[aday])][seq_len(min(TOP_N, length(aday)))]
cat(sprintf("Seçilen makale: %d (TC aralığı %d – %d)\n",
            length(secili), min(tc[secili]), max(tc[secili])))

## 2. Etiketler ---------------------------------------------------------------
ilk_au <- function(x) {
  a <- trimws(sub(";.*$", "", as.character(x)))
  a[is.na(a) | a == ""] <- "N/A"; toupper(a)
}
etiket <- if ("SR" %in% names(M) && !all(is.na(M$SR[secili]))) {
  as.character(M$SR[secili])
} else paste0(ilk_au(M$AU[secili]), " (", M$PY[secili], ")")
# tekrarlayan etiketlere a/b/c ekle
sfx <- ave(seq_along(etiket), etiket, FUN = function(i)
  if (length(i) > 1) seq_along(i) else 0L)
etiket <- ifelse(sfx == 0, etiket, paste0(etiket, "-", letters[pmin(sfx, 26)]))

## 3. Referans-makale matrisi ve bağ gücü --------------------------------------
u <- uzun[uzun$doc %in% secili, c("doc", "key")]
u <- u[!duplicated(u), ]
ks <- unique(u$key)
A <- sparseMatrix(i = match(u$doc, secili), j = match(u$key, ks),
                  x = 1, dims = c(length(secili), length(ks)))
ortak <- as.matrix(tcrossprod(A))               # ortak referans sayısı
nr <- diag(ortak); diag(ortak) <- 0
cos_m <- ortak / sqrt(outer(nr, nr))            # Salton kosinüs
cos_m[is.nan(cos_m)] <- 0
cos_m[ortak < MIN_ORTAK] <- 0

ij <- which(upper.tri(cos_m) & cos_m > 0, arr.ind = TRUE)
kenar <- data.frame(from = ij[, 1], to = ij[, 2],
                    ortak = ortak[ij], cosine = cos_m[ij])
yog <- nrow(kenar) / choose(length(secili), 2)
cat(sprintf("Bağ sayısı: %d (yoğunluk %.1f%%; MIN_ORTAK = %d)\n",
            nrow(kenar), 100 * yog, MIN_ORTAK))

## 4. Ağ ve kümeleme ----------------------------------------------------------
g <- graph_from_data_frame(
  data.frame(from = etiket[kenar$from], to = etiket[kenar$to],
             ortak = kenar$ortak, weight = kenar$cosine),
  directed = FALSE,
  vertices = data.frame(name = etiket, doc = secili))
izole <- V(g)[degree(g) == 0]
if (length(izole)) g <- delete_vertices(g, izole)
cat(sprintf("Ağdaki makale: %d (izole atılan: %d)\n", vcount(g), length(izole)))

set.seed(2026)
cl <- cluster_louvain(g, weights = E(g)$weight)
V(g)$kume <- membership(cl)
cat(sprintf("Louvain küme: %d | modülerlik: %.3f\n",
            length(unique(V(g)$kume)), modularity(cl)))

## 5. Küme profili -----------------------------------------------------------
d <- data.frame(etiket = V(g)$name, doc = V(g)$doc, kume = V(g)$kume,
                PY = as.numeric(M$PY[V(g)$doc]), TC = tc[V(g)$doc],
                n_ref = n_ref[V(g)$doc], stringsAsFactors = FALSE) %>%
  arrange(kume, desc(TC))
d$baslik <- if ("TI" %in% names(M)) substr(as.character(M$TI[d$doc]), 1, 90) else NA

anahtar <- function(idx, k = 6) {
  x <- tolower(trimws(unlist(strsplit(as.character(M$DE[idx]), ";"))))
  x <- x[!is.na(x) & nchar(x) > 2 &
           !x %in% c("credit card fraud detection", "fraud detection",
                     "credit card fraud", "credit card", "machine learning")]
  paste(names(head(sort(table(x), decreasing = TRUE), k)), collapse = "; ")
}
prof <- do.call(rbind, lapply(sort(unique(d$kume)), function(k) {
  z <- d[d$kume == k, ]
  data.frame(kume = k, n_makale = nrow(z), medyan_yil = median(z$PY, na.rm = TRUE),
             ort_TC = round(mean(z$TC), 1),
             en_atifli = paste(head(z$etiket, 3), collapse = " | "),
             anahtar_kelimeler = anahtar(z$doc))
}))
cat("\n--- KÜME PROFİLİ (anahtar kelimeler: 'fraud detection' vb. genel terimler çıkarıldı) ---\n")
for (i in seq_len(nrow(prof)))
  cat(sprintf("\n Küme %d | %d makale | medyan yıl %s | ort. TC %.1f\n   En atıflı: %s\n   Kelimeler: %s\n",
              prof$kume[i], prof$n_makale[i], prof$medyan_yil[i], prof$ort_TC[i],
              prof$en_atifli[i], prof$anahtar_kelimeler[i]))

readr::write_csv(d, "outputs/bibcoupling_makaleler.csv")
readr::write_csv(prof, "outputs/bibcoupling_kumeler.csv")

## 6. VOSviewer (Pajek) --------------------------------------------------------
en <- ends(g, E(g))
idx <- matrix(match(en, V(g)$name), ncol = 2)
pj <- c(sprintf("*Vertices %d", vcount(g)),
        sprintf('%d "%s"', seq_len(vcount(g)), gsub('"', "'", V(g)$name)),
        "*Edges", sprintf("%d %d %d", idx[, 1], idx[, 2], E(g)$ortak))
writeLines(pj, "outputs/bibcoupling.net", useBytes = TRUE)
cat("\n>> outputs/bibcoupling.net (VOSviewer: Create > network data > Pajek)\n")

## 7. Metodoloji raporu --------------------------------------------------------
rap <- data.frame(
  olcum = c("Korpus", "Aday (>= MIN_REF referans)", "Ağa alınan makale (TOP_N)",
            "Bağ (>= MIN_ORTAK ortak referans)", "Ağ yoğunluğu (%)",
            "Ağdaki makale (izolesiz)", "Louvain küme", "Modülerlik"),
  deger = c(nrow(M), length(aday), length(secili), nrow(kenar),
            round(100 * yog, 1), vcount(g),
            length(unique(V(g)$kume)), round(modularity(cl), 3)))
print(rap, row.names = FALSE)
readr::write_csv(rap, "outputs/bibcoupling_raporu.csv")
cat("\n=== TAMAMLANDI ===\n")
