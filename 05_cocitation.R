# =============================================================================
#  05 — REFERANS NORMALİZASYONU ve CO-CITATION ANALİZİ
#
#  NEDEN AYRI BİR SCRIPT GEREKLİ (Biblioshiny'nin kendi co-citation'ı yerine):
#   (1) Scopus, bir referansın İÇİNDEKİ yazarları da noktalı virgülle ayırır —
#       referansları ayırdığı karakterle aynı:
#         "...(2024); Chawla N.V.; Bowyer K.W.; Hall L.O., SMOTE..., (2002);"
#       ';' ile bölmek her referansı parçalar; yazar adları sahte "referans"
#       olur. bibliometrix de ';' ile böler -> bu Scopus verisinde Biblioshiny
#       co-citation sonuçları HATALI çıkar.
#   (2) Aynı çalışma dergi kısaltmasına göre onlarca farklı dizgiyle geçer
#       (SMOTE: ~130 dizgi). Normalize edilmezse atıflar dağılır.
#   (3) Scopus ve WoS referans biçimleri farklıdır.
#
#  ÇÖZÜM:
#   (a) Scopus referansları "(YYYY);" sınırından bölünür (yazar listesi korunur)
#   (b) Format-bağımsız anahtar  YIL | CİLT | İLK SAYFA
#         Scopus: "..., 16, pp. 321-357, (2002)"         -> 2002|16|321
#         WoS   : "Chawla NV, 2002, J ARTIF..., V16, P321" -> 2002|16|321
#       Cilt yoksa (bildiri, kitap): başlık+yıl anahtarı.
#   (c) İkinci geçiş: aynı çalışmanın cilt'li ve cilt'siz yazımları,
#       "ilk yazar soyadı | yıl | başlık başı" imzasıyla birleştirilir.
#   (d) Etiket: çalışmanın tüm atıflarındaki EN SIK ilk-yazar yazımı.
#
#  GİRDİ : M_etiketli.RData        ÇIKTI : outputs/ + cocitation.net (VOSviewer)
#  ÇALIŞTIRMA: source("05_cocitation.R")
# =============================================================================

pkgs <- c("dplyr", "readr", "igraph")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
dir.create("outputs", showWarnings = FALSE)

load("M_etiketli.RData"); M <- M_etiketli
cat("Korpus:", nrow(M), "kayıt\n")
# Geçersiz karakter kodlamalarını temizle (aksi halde tolower() çöker)
M$CR <- iconv(as.character(M$CR), from = "UTF-8", to = "UTF-8", sub = "")

TOP_N   <- 60     # ağa alınacak en çok atıflı çalışma sayısı
MIN_CIT <- 10     # ağa girmek için asgari yerel atıf

## 1. Yardımcı fonksiyonlar --------------------------------------------------
key_scopus <- function(r) {
  y  <- regmatches(r, regexpr("\\((\\d{4})\\)\\s*$", r, perl = TRUE))
  y  <- gsub("\\D", "", y)
  vp <- regmatches(r, regexec(",\\s*(\\d+),\\s*(?:\\d+,\\s*)?(?i:pp)\\.\\s*(\\d+)", r, perl = TRUE))
  if (length(y) == 1 && length(vp[[1]]) == 3)
    return(paste(y, vp[[1]][2], vp[[1]][3], sep = "|"))
  ti <- regmatches(r, regexec("^[^,]+,\\s*([^,]{12,})", r, perl = TRUE))
  if (length(y) == 1 && length(ti[[1]]) == 2)
    return(paste0("T|", substr(gsub("[^a-z0-9]", "", tolower(ti[[1]][2])), 1, 60), "|", y))
  NA_character_
}
key_wos <- function(r) {
  y <- regmatches(r, regexec(",\\s*(\\d{4}),", r, perl = TRUE))[[1]]
  v <- regmatches(r, regexec(",\\s*V(\\d+)", r, perl = TRUE))[[1]]
  p <- regmatches(r, regexec(",\\s*P(\\d+)", r, perl = TRUE))[[1]]
  if (length(y) == 2 && length(v) == 2 && length(p) == 2)
    return(paste(y[2], v[2], p[2], sep = "|"))
  NA_character_
}
ilk_yazar <- function(r, s) {
  if (s == "WoS") return(toupper(trimws(sub(",.*$", "", r))))
  a <- trimws(sub("[;,].*$", "", r)); a <- gsub("\\.", "", a)
  toupper(gsub("\\s+", " ", a))
}
yil_lab <- function(r, s) {
  y <- if (s == "WoS") regmatches(r, regexpr("\\b(19|20)\\d{2}\\b", r))
       else gsub("\\D", "", regmatches(r, regexpr("\\((\\d{4})\\)\\s*$", r, perl = TRUE)))
  if (length(y)) y[1] else NA_character_
}

## 2. Referansları doğru böl ve anahtarla ------------------------------------
cat("\n--- 2. REFERANS ANAHTARLAMA ---\n")
uzun <- do.call(rbind, lapply(seq_len(nrow(M)), function(i) {
  cr <- M$CR[i]
  if (is.na(cr) || nchar(cr) < 10) return(NULL)
  bol <- if (M$DB_SRC[i] == "WoS") ";" else "(?<=\\(\\d{4}\\));\\s*"
  r <- trimws(unlist(strsplit(cr, bol, perl = TRUE))); r <- r[nchar(r) > 10]
  if (!length(r)) return(NULL)
  data.frame(doc = i, src = M$DB_SRC[i], ref = r, stringsAsFactors = FALSE)
}))
cat("Toplam referans:", nrow(uzun), "\n")

uzun$key <- mapply(function(r, s) if (s == "WoS") key_wos(r) else key_scopus(r),
                   uzun$ref, uzun$src, USE.NAMES = FALSE)
yok <- is.na(uzun$key)
uzun$key[yok] <- paste0("R|", substr(gsub("[^a-z0-9]", "", tolower(uzun$ref[yok])), 1, 80))
cat(sprintf("Anahtar türü — cilt|sayfa: %.1f%% | başlık: %.1f%% | ham: %.1f%%\n",
            100*mean(grepl("^\\d{4}\\|", uzun$key)),
            100*mean(startsWith(uzun$key, "T|")),
            100*mean(startsWith(uzun$key, "R|"))))
n_ilk <- length(unique(uzun$key))

## 3. İkinci geçiş — imza ile birleştirme ------------------------------------
sco <- uzun$src != "WoS"
soyad <- toupper(sub("[\\s.,;].*$", "", trimws(uzun$ref), perl = TRUE))
bt <- regmatches(uzun$ref, regexec("^[^,]+,\\s*([^,]{12,})", uzun$ref, perl = TRUE))
bb <- vapply(bt, function(x) if (length(x) == 2)
  substr(gsub("[^a-z0-9]", "", tolower(x[2])), 1, 30) else NA_character_, "")
yy <- mapply(yil_lab, uzun$ref, uzun$src, USE.NAMES = FALSE)
uzun$imza <- ifelse(sco & !is.na(yy) & !is.na(bb), paste(soyad, yy, bb, sep = "|"), NA)

ki <- uzun[!is.na(uzun$imza), c("key", "imza")] %>%
  count(key, imza) %>% group_by(key) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>% left_join(count(uzun, key, name = "boyut"), by = "key")
kanon <- ki %>% group_by(imza) %>% slice_max(boyut, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(imza, kanonik = key)
esle <- ki %>% left_join(kanon, by = "imza") %>% filter(key != kanonik) %>% select(key, kanonik)
if (nrow(esle)) {
  m <- match(uzun$key, esle$key)
  uzun$key[!is.na(m)] <- esle$kanonik[m[!is.na(m)]]
}
cat(sprintf("İkinci geçiş: %d anahtar birleştirildi\n", nrow(esle)))
## 3b. WoS kalıntıları — Scopus'taki aynı çalışmayla eşleştirme -------------
#  WoS'ta cilt/sayfa yoksa (yalnız DOI vb.) anahtar eşleşmez. Muhafazakâr kural:
#   (i)  yazar|yıl|ilk sayfa Scopus'ta tek çalışmaya karşılık geliyorsa birleştir;
#   (ii) değilse yazar|yıl, >= MIN_CIT atıflı TEK Scopus çalışmasına karşılık
#        geliyorsa birleştir. Belirsizse (birden çok aday) DOKUNMA.
uzun$ay <- mapply(ilk_yazar, uzun$ref, uzun$src, USE.NAMES = FALSE)
uzun$yl <- mapply(yil_lab,   uzun$ref, uzun$src, USE.NAMES = FALSE)
sayfa <- function(r, s) {
  m <- if (s == "WoS") regmatches(r, regexec(",\\s*P(\\d+)", r, perl = TRUE))[[1]]
       else regmatches(r, regexec("(?i:pp)\\.\\s*(\\d+)", r, perl = TRUE))[[1]]
  if (length(m) == 2) m[2] else NA_character_
}
uzun$pg <- mapply(sayfa, uzun$ref, uzun$src, USE.NAMES = FALSE)
sk <- unique(uzun$key[uzun$src != "WoS"])
scr <- uzun[uzun$src != "WoS" & !is.na(uzun$ay) & !is.na(uzun$yl), ]
buyuk_k <- names(which(table(uzun$key) >= MIN_CIT))
s3 <- scr[!is.na(scr$pg), ] %>% count(ay, yl, pg, key) %>% group_by(ay, yl, pg) %>%
  filter(n_distinct(key) == 1 | n == max(n)) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup()
s2 <- scr[scr$key %in% buyuk_k, ] %>% distinct(ay, yl, key) %>%
  group_by(ay, yl) %>% filter(n() == 1) %>% ungroup()
frag <- which(uzun$src == "WoS" & !(uzun$key %in% sk) & !is.na(uzun$ay) & !is.na(uzun$yl))
yeni <- uzun$key[frag]
k3 <- s3$key[match(paste(uzun$ay[frag], uzun$yl[frag], uzun$pg[frag]),
                   paste(s3$ay, s3$yl, s3$pg))]
k2 <- s2$key[match(paste(uzun$ay[frag], uzun$yl[frag]), paste(s2$ay, s2$yl))]
yeni <- ifelse(!is.na(k3), k3, ifelse(!is.na(k2), k2, yeni))
n_wos_birles <- sum(yeni != uzun$key[frag])
uzun$key[frag] <- yeni
cat(sprintf("WoS kalıntı eşleştirme: %d referans Scopus'taki çalışmaya bağlandı\n", n_wos_birles))
cat(sprintf("Konsolidasyon: %d ham dizgi -> %d (1. geçiş) -> %d çalışma\n",
            length(unique(uzun$ref)), n_ilk, length(unique(uzun$key))))
uzun <- uzun[!duplicated(uzun[, c("doc", "key")]), ]

## 4. Etiketler: en sık ilk-yazar yazımı ------------------------------------
etiket <- uzun %>% filter(!is.na(ay), nchar(ay) > 1) %>%
  count(key, ay, yl) %>% group_by(key) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(etiket = ifelse(is.na(yl), ay, paste0(ay, " (", yl, ")"))) %>%
  select(key, etiket) %>%
  left_join(uzun %>% group_by(key) %>% summarise(ornek = ref[1], .groups = "drop"), by = "key")

# Kaynak verideki bilinen bozukluklar — BELGELENMİŞ elle düzeltmeler.
# Gerekirse genişlet:  "veride görünen" = "doğru biçim"
ETIKET_DUZELT <- c(
  "RB A (2021)" = "ASHA RB (2021)"     # Scopus ismi ters ayrıştırmış
)
hit <- etiket$etiket %in% names(ETIKET_DUZELT)
etiket$etiket[hit] <- ETIKET_DUZELT[etiket$etiket[hit]]

## 5. Yerel atıf ------------------------------------------------------------
cat("\n--- 5. EN ÇOK ATIF ALAN ÇALIŞMALAR ---\n")
atif <- uzun %>% count(key, name = "yerel_atif") %>%
  left_join(etiket, by = "key") %>% arrange(desc(yerel_atif))
varyant <- uzun %>% group_by(key) %>% summarise(varyant = n_distinct(ref), .groups = "drop")
atif <- atif %>% left_join(varyant, by = "key")
# Aynı "YAZAR (YIL)" etiketi, ağa girebilecek (>= MIN_CIT atıf) BİRDEN ÇOK farklı
# çalışmaya aitse a/b/c eki. Küçük parçalar (az atıflı yazım varyantları) ek almaz.
atif <- atif %>% mutate(buyuk = yerel_atif >= MIN_CIT & !is.na(etiket)) %>%
  group_by(etiket, buyuk) %>%
  mutate(etiket = if (buyuk[1] && n() > 1)
           paste0(sub("\\)$", "", etiket), letters[rank(-yerel_atif, ties.method = "first")], ")")
         else etiket) %>% ungroup() %>% select(-buyuk) %>% arrange(desc(yerel_atif))
print(as.data.frame(head(atif[, c("etiket", "yerel_atif", "varyant")], 25)), row.names = FALSE)
readr::write_csv(atif[atif$yerel_atif >= 5, c("key","etiket","yerel_atif","varyant","ornek")],
                 "outputs/en_cok_atif_alanlar.csv")
cat("\n'varyant' = aynı çalışmanın kaç farklı dizgiyle yazıldığı.\n",
    "Etiketleri outputs/en_cok_atif_alanlar.csv'de kontrol et.\n")

## 6. Co-citation ağı -------------------------------------------------------
cat("\n--- 6. CO-CITATION AĞI ---\n")
secili <- head(atif$key[atif$yerel_atif >= MIN_CIT], TOP_N)
u <- uzun[uzun$key %in% secili, c("doc", "key")]
ciftler <- do.call(rbind, lapply(split(u$key, u$doc), function(k) {
  k <- sort(unique(k)); if (length(k) < 2) return(NULL); t(combn(k, 2)) }))
kenar <- as.data.frame(ciftler, stringsAsFactors = FALSE); names(kenar) <- c("from", "to")
kenar <- kenar %>% count(from, to, name = "weight")

g <- igraph::graph_from_data_frame(kenar, directed = FALSE,
       vertices = data.frame(name = secili,
                             label = atif$etiket[match(secili, atif$key)],
                             cit   = atif$yerel_atif[match(secili, atif$key)]))
s  <- igraph::strength(g)
en <- igraph::ends(g, igraph::E(g))
igraph::E(g)$assoc <- igraph::E(g)$weight / (s[en[, 1]] * s[en[, 2]])   # association strength

set.seed(2026)
cl <- igraph::cluster_louvain(g, weights = igraph::E(g)$assoc)
igraph::V(g)$cluster <- igraph::membership(cl)
cat(sprintf("Çalışma: %d | kenar: %d | Louvain küme: %d | modülerlik: %.3f\n",
            igraph::vcount(g), igraph::ecount(g),
            length(unique(igraph::membership(cl))), igraph::modularity(cl)))

kume <- data.frame(etiket = igraph::V(g)$label, atif = igraph::V(g)$cit,
                   kume = igraph::V(g)$cluster) %>% arrange(kume, desc(atif))
for (k in sort(unique(kume$kume))) {
  cat(sprintf("\n  Küme %d:\n", k))
  x <- head(kume[kume$kume == k, ], 6)
  for (j in seq_len(nrow(x))) cat(sprintf("    [%3d] %s\n", x$atif[j], x$etiket[j]))
}
readr::write_csv(kume, "outputs/cocitation_kumeler.csv")

## 7. VOSviewer (Pajek) -----------------------------------------------------
#  VOSviewer: Create > based on network data > Pajek network file
pj <- c(sprintf("*Vertices %d", igraph::vcount(g)),
        sprintf('%d "%s"', seq_len(igraph::vcount(g)), gsub('"', "'", igraph::V(g)$label)),
        "*Edges")
idx <- matrix(match(en, igraph::V(g)$name), ncol = 2)
pj <- c(pj, sprintf("%d %d %d", idx[, 1], idx[, 2], igraph::E(g)$weight))
writeLines(pj, "outputs/cocitation.net", useBytes = TRUE)
cat("\n>> outputs/cocitation.net (VOSviewer, Pajek formatı)\n")

## 8. Metodoloji raporu -----------------------------------------------------
rap <- data.frame(
  olcum = c("Referans (doğru bölünmüş)", "Benzersiz ham dizgi", "Normalize çalışma",
            "Cilt|sayfa anahtarı (%)", "İkinci geçişte birleşen anahtar",
            "Ağdaki çalışma", "Ağdaki kenar", "Louvain küme", "Modülerlik"),
  deger = c(nrow(uzun), length(unique(uzun$ref)), length(unique(uzun$key)),
            round(100*mean(grepl("^\\d{4}\\|", uzun$key)), 1), nrow(esle),
            igraph::vcount(g), igraph::ecount(g),
            length(unique(igraph::membership(cl))), round(igraph::modularity(cl), 3)))
print(rap, row.names = FALSE)
readr::write_csv(rap, "outputs/cocitation_raporu.csv")
cat("\n=== TAMAMLANDI ===\n")

