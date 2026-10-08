# =============================================================================
#  07 — TABLO 3 (yazarlar), TABLO 4 (dergiler), TABLO 6 (ülkeler), kurumlar
#
#  HAKEM İSTEKLERİ ve BU SCRIPT'İN CEVABI
#   Hakem: "ZHANG Y, WANG Y gibi kısaltmalar farklı kişileri birleştiriyor"
#     -> Yazarlar Scopus Author ID ile sayılır (kişiye özgü kimlik).
#   Hakem: "Tablo 3'te bazı atıflar '—', kaynak karışık"
#     -> Yazar tablosu yalnızca Scopus kayıtlarından; atıf = Scopus 'Cited by'.
#   Hakem: "Q1/Q2 kaynağı ve yılı belirtilmemiş"
#     -> SCImago (SJR) dosyasından, yıl ve kaynak tabloda yazılı.
#   Hakem: "ülke ataması ve SCP/MCP nasıl hesaplandı"
#     -> Ülke = ilk kayıtlı yazar kurumunun ülkesi; SCP/MCP tanımı aşağıda.
#   Hakem: "atıf karşılaştırmasında yayın yaşı etkisi"
#     -> Her tabloya yaşa göre normalize atıf (NCS) eklenir.
#
#  GİRDİ  : M_etiketli.RData
#  İSTEĞE BAĞLI: SCImago dosyası (dergi çeyrekliği için), bkz. SJR_DOSYA
#  ÇIKTI  : outputs/tablo3_yazarlar.csv, tablo4_dergiler.csv,
#           tablo6_ulkeler.csv, tablo_kurumlar.csv, tablo_notlari.csv
#  ÇALIŞTIRMA: source("07_tablolar.R")
# =============================================================================

pkgs <- c("dplyr", "tidyr", "readr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
dir.create("outputs", showWarnings = FALSE)

## AYARLAR -------------------------------------------------------------------
RET_YIL   <- 2026     # verinin çekildiği yıl (yaş hesabı için)
TOP_YAZAR <- 15       # Tablo 3'te gösterilecek yazar sayısı
TOP_DERGI <- 10
TOP_ULKE  <- 10
TOP_KURUM <- 20
SJR_DOSYA <- "scimagojr_2024.csv"   # https://www.scimagojr.com/journalrank.php
SJR_YIL   <- 2024                    #  -> yıl seç -> "Download data"

load("M_etiketli.RData"); M <- M_etiketli
M$doc <- seq_len(nrow(M))
M$PY  <- as.numeric(M$PY)
M$TC  <- suppressWarnings(as.numeric(M$TC)); M$TC[is.na(M$TC)] <- 0
M$YAS <- RET_YIL - M$PY + 1                      # bibliometrix'in TCperYear mantığı
ort_yil <- ave(M$TC, M$PY, FUN = mean)           # aynı yıl yayınlarının ortalaması
M$NCS <- ifelse(ort_yil > 0, M$TC / ort_yil, 0)  # 1 = o yılın ortalaması
cat("Korpus:", nrow(M), "| Scopus kaynaklı:", sum(M$DB_SRC == "Scopus"),
    "| yalnız WoS:", sum(M$DB_SRC == "WoS"), "\n")
notlar <- list()

## ============================ TABLO 3 — YAZARLAR ============================
cat("\n================ TABLO 3: YAZARLAR ================\n")
idcol <- intersect(c("AU_ID", "Author(s) ID", "Author.s..ID"), names(M))[1]
if (is.na(idcol)) stop("Scopus yazar kimliği sütunu (AU_ID) yok; M_etiketli'yi kontrol et.")
sco <- which(M$DB_SRC == "Scopus")
al <- do.call(rbind, lapply(sco, function(i) {
  a <- trimws(strsplit(as.character(M$AU[i]), ";")[[1]]); a <- a[nzchar(a)]
  d <- trimws(strsplit(as.character(M[[idcol]][i]), ";")[[1]]); d <- d[nzchar(d)]
  if (!length(a)) return(NULL)
  if (length(d) != length(a)) d <- rep(NA_character_, length(a))   # hizalanamazsa kimlik yok say
  data.frame(doc = i, au = a, id = d, stringsAsFactors = FALSE)
}))
al$id[grepl("no author id", tolower(al$id))] <- NA
al$key <- ifelse(is.na(al$id), paste0("N:", al$au), paste0("S:", al$id))
id_oran <- 100 * mean(!is.na(al$id))
cat(sprintf("Yazar kaydı: %d | Scopus kimlikli: %.1f%% | benzersiz kişi (kimlik): %d\n",
            nrow(al), id_oran, length(unique(al$key))))

tab <- al %>% distinct(doc, key, .keep_all = TRUE) %>%
  left_join(M[, c("doc", "TC", "NCS", "PY")], by = "doc") %>%
  group_by(key) %>%
  summarise(yazar = names(sort(table(au), decreasing = TRUE))[1],
            scopus_id = dplyr::first(id),
            yayin = n(), toplam_atif = sum(TC), ort_atif = round(mean(TC), 1),
            ort_NCS = round(mean(NCS), 2), ilk_yil = min(PY), son_yil = max(PY),
            yazimlar = paste(unique(au), collapse = " | "), .groups = "drop") %>%
  group_by(yazar) %>% mutate(ayni_kisaltma_kisi = n()) %>% ungroup() %>%
  arrange(desc(yayin), desc(toplam_atif))
tab$profil <- ifelse(is.na(tab$scopus_id), NA,
  paste0("https://www.scopus.com/authid/detail.uri?authorId=", tab$scopus_id))
t3 <- head(tab, TOP_YAZAR)
t3$sira <- seq_len(nrow(t3))
print(as.data.frame(t3[, c("sira", "yazar", "scopus_id", "yayin", "toplam_atif",
                           "ort_atif", "ort_NCS", "ilk_yil", "son_yil",
                           "ayni_kisaltma_kisi")]), row.names = FALSE)
readr::write_csv(t3, "outputs/tablo3_yazarlar.csv")
readr::write_csv(head(tab, 200), "outputs/yazar_adaylari_200.csv")
cat("\n'ayni_kisaltma_kisi' > 1 ise aynı kısaltma birden çok KİŞİ'ye ait (kimlikle ayrıldı).\n",
    "Tablo 3'teki her yazarın 'profil' bağlantısını aç; adı/kurumu ve gerekirse\n",
    "aynı kişinin ikinci bir Scopus profilini kontrol et (bölünmüş profil riski).\n")
notlar$yazar <- c(kayit = nrow(al), kimlikli_pct = round(id_oran, 1),
                  kisi = length(unique(al$key)))

## ============================ TABLO 4 — DERGİLER ============================
cat("\n================ TABLO 4: KAYNAKLAR ================\n")
M$SO_N <- toupper(trimws(gsub("\\s+", " ", as.character(M$SO))))
dt <- tolower(ifelse(is.na(M$DT), "", as.character(M$DT)))
M$TUR <- ifelse(grepl("conference|proceeding", dt), "Bildiri",
                ifelse(grepl("article", dt), "Makale", "Diğer"))
cat("Belge türü:", paste(names(table(M$TUR)), table(M$TUR), collapse = " | "), "\n")
issn1 <- function(x) { x <- as.character(x); x <- x[!is.na(x) & nzchar(x)]
                       if (length(x)) gsub("[^0-9Xx]", "", x[1]) else NA_character_ }
snc <- if ("SN" %in% names(M)) "SN" else NA
kaynak_tablo <- function(D) {
  D %>% group_by(SO_N) %>%
    summarise(kayit = n(), toplam_atif = sum(TC), ort_atif = round(mean(TC), 1),
              ort_NCS = round(mean(NCS), 2),
              issn = if (!is.na(snc)) issn1(.data[[snc]]) else NA_character_,
              .groups = "drop") %>%
    mutate(pay_turde = round(100 * kayit / nrow(D), 1)) %>% arrange(desc(kayit))
}
dg  <- kaynak_tablo(M[M$TUR == "Makale", ])   # dergiler (makale kayıtları)
dgb <- kaynak_tablo(M[M$TUR == "Bildiri", ])  # bildiri / kitap serileri
dg$SJR_ceyreklik <- NA_character_; dg$SJR_tur <- NA_character_
if (file.exists(SJR_DOSYA)) {
  sj <- readr::read_delim(SJR_DOSYA, delim = ";", show_col_types = FALSE,
                          locale = readr::locale(decimal_mark = ","))
  q <- intersect(c("SJR Best Quartile", "SJR.Best.Quartile"), names(sj))[1]
  sj$tit <- toupper(trimws(sj$Title))
  sj_iss <- sj %>% mutate(i = strsplit(gsub("[ -]", "", Issn), ",")) %>%
    tidyr::unnest(i) %>% select(i, Type, Q = all_of(q))
  m1 <- match(dg$issn, sj_iss$i)
  m2 <- match(dg$SO_N, sj$tit)
  dg$SJR_ceyreklik <- ifelse(!is.na(m1), sj_iss$Q[m1], sj[[q]][m2])
  dg$SJR_tur       <- ifelse(!is.na(m1), sj_iss$Type[m1], sj$Type[m2])
  cat(sprintf("SJR eşleşmesi (ilk %d dergi): %d\n", TOP_DERGI,
              sum(!is.na(head(dg$SJR_ceyreklik, TOP_DERGI)))))
} else {
  cat(sprintf("UYARI: '%s' yok -> çeyreklik boş. İndir: scimagojr.com/journalrank.php\n",
              SJR_DOSYA), "  (yıl seç, 'Download data'), dosyayı bu klasöre koy, yeniden çalıştır.\n")
}
dg$SJR_yil <- ifelse(is.na(dg$SJR_ceyreklik), NA, SJR_YIL)
t4 <- head(dg, TOP_DERGI); t4$sira <- seq_len(nrow(t4))
cat("\n--- 4a. EN ÇOK MAKALE YAYIMLAYAN DERGİLER ---\n")
print(as.data.frame(t4[, c("sira", "SO_N", "kayit", "pay_turde", "toplam_atif",
                           "ort_atif", "ort_NCS", "SJR_ceyreklik", "SJR_tur")]), row.names = FALSE)
readr::write_csv(t4, "outputs/tablo4_dergiler.csv")
readr::write_csv(head(dg, 100), "outputs/dergi_adaylari_100.csv")
t4b <- head(dgb, TOP_DERGI); t4b$sira <- seq_len(nrow(t4b))
cat("\n--- 4b. EN ÇOK BİLDİRİ YAYIMLAYAN SERİLER / ETKİNLİKLER ---\n")
print(as.data.frame(t4b[, c("sira", "SO_N", "kayit", "pay_turde", "toplam_atif",
                            "ort_atif", "ort_NCS")]), row.names = FALSE)
readr::write_csv(t4b, "outputs/tablo4b_bildiri_serileri.csv")
cat(sprintf("\nBenzersiz dergi: %d | benzersiz bildiri kaynağı: %d\n", nrow(dg), nrow(dgb)),
    sprintf("İlk %d derginin makale payı: %.1f%% | ilk %d serinin bildiri payı: %.1f%%\n",
            TOP_DERGI, sum(t4$pay_turde), TOP_DERGI, sum(t4b$pay_turde)))
cat("Not: SJR çeyrekliği 'Best Quartile' (konu kategorileri arasında en iyisi).\n")
notlar$kaynak <- c(dergi = nrow(dg), bildiri_kaynagi = nrow(dgb),
                   ilk_dergi_pay = round(sum(t4$pay_turde), 1),
                   ilk_seri_pay = round(sum(t4b$pay_turde), 1))

## ============================ TABLO 6 — ÜLKELER =============================
cat("\n================ TABLO 6: ÜLKELER ================\n")
if (!"AU_CO" %in% names(M)) {
  if (!requireNamespace("bibliometrix", quietly = TRUE)) stop("bibliometrix gerekli")
  M <- bibliometrix::metaTagExtraction(M, Field = "AU_CO", sep = ";")
}
ULKE_DUZELT <- c("UNITED STATES" = "USA", "UNITED STATES OF AMERICA" = "USA",
                 "U.S.A." = "USA", "UK" = "UNITED KINGDOM", "ENGLAND" = "UNITED KINGDOM",
                 "SCOTLAND" = "UNITED KINGDOM", "WALES" = "UNITED KINGDOM",
                 "RUSSIAN FEDERATION" = "RUSSIA", "KOREA" = "SOUTH KOREA",
                 "REPUBLIC OF KOREA" = "SOUTH KOREA", "VIET NAM" = "VIETNAM",
                 "TURKIYE" = "TURKEY", "TÜRKIYE" = "TURKEY", "HONG KONG" = "CHINA")
fixc <- function(x) ifelse(x %in% names(ULKE_DUZELT), ULKE_DUZELT[x], x)
# NEDEN C1? AU_CO'daki ülkeler ALFABETİK sıralıdır; 'ilk ülke' olarak almak
# çok ülkeli kayıtlarda yanlış ülkeyi seçer (USA'nın MCP'sini sıfıra düşürür).
# İlk yazar ülkesi, kurum adreslerinin (C1) İLK kaydından alınır.
if (!"C1" %in% names(M)) stop("C1 (kurum adresi) sütunu yok; M_etiketli'yi kontrol et.")
ULKE_DUZELT <- c(ULKE_DUZELT, "PEOPLES R CHINA" = "CHINA", "P.R. CHINA" = "CHINA",
                 "CZECH REPUBLIC" = "CZECHIA", "TÜRKIYE" = "TURKEY", "IRAN, ISLAMIC REPUBLIC OF" = "IRAN")
fixc <- function(x) ifelse(x %in% names(ULKE_DUZELT), ULKE_DUZELT[x], x)
cs <- lapply(strsplit(as.character(M$AU_CO), ";"), function(x) {
  x <- toupper(trimws(x)); x <- x[!is.na(x) & nzchar(x) & x != "NA"]; unique(fixc(x)) })
# YEDEK: AU_CO boşsa (bibliometrix bazı kayıtlarda ülke çıkaramaz) ülke kümesi,
# C1'deki TÜM adreslerin son öğesinden kurulur (aynı ULKE_DUZELT yazım düzeltmesiyle).
ulke_c1 <- function(c1) {
  c1 <- gsub("\\[[^]]*\\]", "", as.character(c1))
  seg <- trimws(strsplit(c1, ";")[[1]]); seg <- seg[nzchar(seg)]
  if (!length(seg)) return(character(0))
  x <- fixc(toupper(trimws(sub("\\.$", "", sub(".*,", "", seg)))))
  unique(x[!is.na(x) & nzchar(x) & x != "NA"])
}
bos_au_co <- lengths(cs) == 0
cat(sprintf("AU_CO'su boş kayıt: %d (Scopus %d, yalnız WoS %d)\n", sum(bos_au_co),
            sum(bos_au_co & M$DB_SRC == "Scopus"), sum(bos_au_co & M$DB_SRC == "WoS")))
cs[bos_au_co] <- lapply(M$C1[bos_au_co], ulke_c1)
cat(sprintf("C1 yedeğiyle ülkesi kurulan: %d | hâlâ ülkesiz: %d\n",
            sum(bos_au_co & lengths(cs) > 0), sum(lengths(cs) == 0)))
# Ülke KÜMESİ AU_CO'dan (tek, tutarlı kaynak); MCP = kümede birden çok ülke.
# İlk yazar ülkesi YALNIZCA çok ülkeli kayıtlarda C1'den seçilir ve kümedeki
# bir ülkeye eşleşmek zorundadır (yazım farkıyla sahte MCP üretmemek için).
ilk_ulke <- function(c1, set) {
  if (!length(set)) return(NA_character_)
  if (length(set) == 1) return(set[1])
  c1 <- gsub("\\[[^]]*\\]", "", as.character(c1))          # WoS yazar parantezleri
  seg <- trimws(strsplit(c1, ";")[[1]]); seg <- seg[nzchar(seg)]
  if (!length(seg)) return(NA_character_)
  cand <- fixc(toupper(trimws(sub("\\.$", "", sub(".*,", "", seg[1])))))
  if (cand %in% set) return(cand)
  hit <- set[vapply(set, function(k) grepl(k, toupper(seg[1]), fixed = TRUE), logical(1))]
  if (length(hit)) hit[1] else NA_character_
}
M$ilk_ulke <- mapply(ilk_ulke, M$C1, cs, USE.NAMES = FALSE)
M$coklu    <- vapply(cs, function(x) length(x) > 1, logical(1))
coklu_n <- sum(M$coklu)
belirsiz <- sum(M$coklu & is.na(M$ilk_ulke))
cat(sprintf("Çok ülkeli kayıt: %d (%.1f%%) | bunlardan ilk yazar ülkesi belirlenemeyen: %d\n",
            coklu_n, 100 * coklu_n / nrow(M), belirsiz))
# Tanı: C1'den çıkan ilk ülke adı, AU_CO kümesinde bulunmayanların en sık yazımları
tani <- mapply(function(c1, set) {
  if (length(set) < 2) return(NA_character_)
  seg <- trimws(strsplit(gsub("\\[[^]]*\\]", "", as.character(c1)), ";")[[1]])
  if (!length(seg) || !nzchar(seg[1])) return(NA_character_)
  cand <- fixc(toupper(trimws(sub("\\.$", "", sub(".*,", "", seg[1])))))
  if (cand %in% set) NA_character_ else cand }, M$C1, cs, USE.NAMES = FALSE)
if (any(!is.na(tani))) {
  cat("Eşleşmeyen ilk-ülke yazımları (en sık 8) — ULKE_DUZELT'e eklenebilir:\n")
  print(head(sort(table(tani), decreasing = TRUE), 8))
}
ok <- !is.na(M$ilk_ulke)
cat(sprintf("Ülke belirlenen kayıt: %d / %d (%.1f%%)\n", sum(ok), nrow(M), 100 * mean(ok)))
ul <- M[ok, ] %>% group_by(ulke = ilk_ulke) %>%
  summarise(kayit = n(), SCP = sum(!coklu), MCP = sum(coklu),
            toplam_atif = sum(TC), ort_atif = round(mean(TC), 1),
            ort_NCS = round(mean(NCS), 2), .groups = "drop") %>%
  mutate(pay = round(100 * kayit / sum(ok), 1),
         MCP_orani = round(100 * MCP / kayit, 1))
katilim <- table(unlist(cs))
ul$katilim_tam_sayim <- as.integer(katilim[ul$ulke])
ul <- ul %>% arrange(desc(kayit)); t6 <- head(ul, TOP_ULKE); t6$sira <- seq_len(nrow(t6))
print(as.data.frame(t6[, c("sira", "ulke", "kayit", "pay", "SCP", "MCP", "MCP_orani",
                           "katilim_tam_sayim", "toplam_atif", "ort_atif", "ort_NCS")]),
      row.names = FALSE)
readr::write_csv(t6, "outputs/tablo6_ulkeler.csv")
readr::write_csv(head(ul, 50), "outputs/ulke_adaylari_50.csv")
ulus_ort <- 100 * mean(M$coklu[lengths(cs) > 0])   # ülke kümesi olan tüm kayıtlar
cat(sprintf("\nUluslararası ortak yazarlı kayıt oranı: %.2f%%\n", ulus_ort))
cat("TANIM: ülke = ilk kayıtlı yazar kurumunun ülkesi; SCP = tüm yazar ülkeleri aynı;\n",
    "MCP = birden çok ülke; 'katilim_tam_sayim' = ülkenin yer aldığı kayıt sayısı.\n",
    "Biblioshiny'nin 'corresponding author' atamasından farklıdır; metinde belirt.\n")
notlar$ulke <- c(belirlenen = sum(ok), uluslararasi_pct = round(ulus_ort, 2))

## ============================ KURUMLAR ======================================
cat("\n================ KURUMLAR ================\n")
if (!"AU_UN" %in% names(M)) {
  M <- bibliometrix::metaTagExtraction(M, Field = "AU_UN", sep = ";")
}
norm_k <- function(x) {
  x <- toupper(x); x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\bUNIV\\b", "UNIVERSITY", x); x <- gsub("\\bINST\\b", "INSTITUTE", x)
  x <- gsub("^THE ", "", trimws(x)); gsub("\\s+", " ", trimws(x)) }
ku <- lapply(strsplit(as.character(M$AU_UN), ";"), function(x) {
  x <- norm_k(x); x <- x[nzchar(x) & x != "NA"]
  # birim adları (bölüm/fakülte) kurum sayılmaz
  unique(x[!grepl("^(DEPARTMENT|DEPT|FACULTY|DIVISION|SCHOOL OF|COLLEGE OF|LABORATORY|CENTRE|CENTER)\\b", x)]) })
# Aynı kurumun farklı yazımlarını birleştir (desen -> kanonik ad). Gerekirse genişlet.
KURUM_ESLE <- c("^TONGJI" = "TONGJI UNIVERSITY",
                "^FLORIDA ATLANTIC" = "FLORIDA ATLANTIC UNIVERSITY",
                "^SAVEETHA" = "SAVEETHA INSTITUTE OF MEDICAL AND TECHNICAL SCIENCES",
                "^GRAPHIC ERA" = "GRAPHIC ERA UNIVERSITY",
                "^SRM " = "SRM INSTITUTE OF SCIENCE AND TECHNOLOGY",
                "^SYMBIOSIS" = "SYMBIOSIS INTERNATIONAL UNIVERSITY",
                "^ZHEJIANG GONGSHANG" = "ZHEJIANG GONGSHANG UNIVERSITY",
                "^KING SAUD" = "KING SAUD UNIVERSITY",
                "^KING ABDULAZIZ" = "KING ABDULAZIZ UNIVERSITY")
# Çok kampüslü sistem adları tek kurum DEĞİLDİR; sıralamadan çıkarılır.
KURUM_DISLA <- c("NOTREPORTED", "NATIONAL INSTITUTE OF TECHNOLOGY",
                 "INDIAN INSTITUTE OF TECHNOLOGY", "INDIAN INSTITUTE OF INFORMATION TECHNOLOGY")
esle_k <- function(x) { for (p in names(KURUM_ESLE)) x[grepl(p, x)] <- KURUM_ESLE[[p]]; x }
ku <- lapply(ku, function(x) unique(esle_k(x[!x %in% KURUM_DISLA])))
kt <- sort(table(unlist(ku)), decreasing = TRUE)
kdf <- data.frame(kurum = names(kt), kayit = as.integer(kt))
print(head(kdf, TOP_KURUM), row.names = FALSE)
readr::write_csv(head(kdf, 100), "outputs/tablo_kurumlar.csv")
cat("\nUYARI: kurum adları elle gözden geçirilmeli (aynı kurumun farklı yazımları).\n",
    "outputs/tablo_kurumlar.csv içinde benzer satırları topla ve bana bildir.\n")

## ============================ NOT DOSYASI ===================================
readr::write_csv(data.frame(alan = rep(names(notlar), lengths(notlar)),
                            olcum = unlist(lapply(notlar, names)),
                            deger = unlist(notlar)), "outputs/tablo_notlari.csv")
cat("\n=== TAMAMLANDI ===\n")
