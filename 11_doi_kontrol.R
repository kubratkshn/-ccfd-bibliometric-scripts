# =============================================================================
#  11 — KAYNAKÇA DOI KONTROLÜ  (Hakem 2: atıf ve kaynakça denetimi)
#
#  Amaç: Kaynakçadaki her DOI'yi Crossref'ten sorgulayıp başlık, dergi, cilt,
#        sayfa ve yazarları kayıtla karşılaştırmak. Uyuşmazlık = elle düzelt.
#  GİRDİ : internet (Crossref, ücretsiz)       ÇIKTI: outputs/doi_kontrol.csv / .txt
#  ÇALIŞTIRMA: source("11_doi_kontrol.R")      Sonra doi_kontrol.txt'i bana at.
# =============================================================================
for (p in "jsonlite") if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
dir.create("outputs", showWarnings = FALSE)

# Kaynakçadaki girişler: "Kısa ad | yıl | DOI"
REF <- c(
  "Akbani 2004|10.1007/978-3-540-30115-8_7",
  "Alarfaj 2022|10.1109/ACCESS.2022.3166891",
  "Aria 2017|10.1016/j.joi.2017.08.007",
  "Aros 2024|10.1057/s41599-024-03606-0",
  "Awoyemi 2017|10.1109/ICCNI.2017.8123782",
  "Benjamini 1995|10.1111/j.2517-6161.1995.tb02031.x",
  "Bhattacharyya 2011|10.1016/j.dss.2010.08.008",
  "Blondel 2008|10.1088/1742-5468/2008/10/P10008",
  "Bolton 2002|10.1214/ss/1042727940",
  "Btoush 2023|10.7717/peerj-cs.1278",
  "Chaquet-Ulldemolins 2022|10.3390/app12083856",
  "Chawla 2002|10.1613/jair.953",
  "Cobo 2011|10.1016/j.joi.2010.10.002",
  "Dal Pozzolo 2018|10.1109/TNNLS.2017.2736643",
  "Dal Pozzolo 2015|10.1109/SSCI.2015.33",
  "Donthu 2021|10.1016/j.jbusres.2021.04.070",
  "Dornadula 2019|10.1016/j.procs.2020.01.057",
  "Falagas 2008|10.1096/fj.07-9492LSF",
  "Fiore 2019|10.1016/j.ins.2017.12.030",
  "Ghosh 1994|10.1109/HICSS.1994.323314",
  "Gouda 2025|10.1109/ICCA66035.2025.11430904",
  "Hove 2024|10.5530/jscires.13.1.4",
  "Jurgovsky 2018|10.1016/j.eswa.2018.01.037",
  "Knorr 2000|10.1007/s007780050006",
  "Mahya 2023|10.22219/jrak.v13i3.28487",
  "Mansour 2022|10.1108/JFC-03-2021-0052",
  "Mienye 2022|10.1109/ACCESS.2022.3207287",
  "Muria 2023|10.47353/ijema.v1i5.69",
  "Ngai 2011|10.1016/j.dss.2010.08.006",
  "Page 2021|10.1136/bmj.n71",
  "Quah 2008|10.1016/j.eswa.2007.08.093",
  "Randhawa 2018|10.1109/ACCESS.2018.2806420",
  "Srivastava 2008|10.1109/TDSC.2007.70228",
  "Taha 2020|10.1109/ACCESS.2020.2971354",
  "Thakkar 2025|10.56042/alis.v72i2.14752",
  "Van Eck 2009|10.1002/asi.21075",
  "Van Eck 2010|10.1007/s11192-009-0146-3",
  "Wang 2003|10.1145/956750.956778",
  "Wang 2024|10.1186/s40537-024-00905-w"
)

oku <- function(doi) {
  u <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE),
              "?mailto=kubra.tkshn@gmail.com")
  r <- tryCatch(jsonlite::fromJSON(u, simplifyVector = FALSE)$message, error = function(e) NULL)
  if (is.null(r)) return(data.frame(doi = doi, durum = "BULUNAMADI/HATA", stringsAsFactors = FALSE))
  yaz <- vapply(r$author %||% list(), function(a) paste0(a$family %||% "", ", ", substr(a$given %||% "", 1, 1)), "")
  yil <- (r$issued$`date-parts`[[1]])[1]
  data.frame(doi = doi, durum = "ok",
             yil = yil %||% NA,
             baslik = substr(paste(r$title, collapse = " "), 1, 110),
             dergi = substr(paste(r$`container-title`, collapse = " "), 1, 70),
             cilt = r$volume %||% "", sayi = r$issue %||% "",
             sayfa = r$page %||% r$`article-number` %||% "",
             yazarlar = paste(head(yaz, 4), collapse = "; "),
             n_yazar = length(yaz), stringsAsFactors = FALSE)
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

out <- list()
for (s in REF) {
  p <- strsplit(s, "\\|")[[1]]
  cat("Sorgulanıyor:", p[1], "\n")
  d <- oku(p[2]); d$kisa <- p[1]; out[[length(out) + 1]] <- d
  Sys.sleep(0.6)
}
cols <- c("kisa","doi","durum","yil","baslik","dergi","cilt","sayi","sayfa","yazarlar","n_yazar")
out <- do.call(rbind, lapply(out, function(d) { for (k in setdiff(cols, names(d))) d[[k]] <- NA; d[, cols] }))
write.csv(out, "outputs/doi_kontrol.csv", row.names = FALSE, fileEncoding = "UTF-8")
sink("outputs/doi_kontrol.txt", split = FALSE)
for (i in seq_len(nrow(out))) {
  o <- out[i, ]
  cat(sprintf("%s | %s | %s\n   %s\n   %s | cilt %s | sayı %s | sayfa %s\n   %s (n=%s)\n",
              o$kisa, o$doi, o$durum, o$baslik, o$dergi, o$cilt, o$sayi, o$sayfa, o$yazarlar, o$n_yazar))
}
sink()
cat("\nTamam. outputs/doi_kontrol.txt dosyasını bana at.\n")
