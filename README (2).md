# Three Decades of Machine Learning in Credit Card Fraud Detection: R scripts

R code behind the bibliometric analysis (Scopus + Web of Science, 1993–2025, English, articles and conference papers; 2,748 records after screening).

The database exports cannot be shared because of licence terms. With your own licensed account, repeat the searches (strings and dates are in the supplement) and run the scripts in the order below.

## Order of the scripts

| Step | File | Purpose |
|---|---|---|
| 01 | `01_korpus_kurulum_v3.R` | Merge Scopus and WoS exports, remove duplicates |
| 02 | `02_manuel_tarama_uygula_v2.R` | Apply the manual scope-screening decisions |
| 03 | `03_etiketleme_v2.R` | Label ML / non-ML and algorithm families |
| 04 | `04_ileri_analizler.R` | RPYS and further analyses |
| 05 | `05_cocitation.R` | Reference normalisation and co-citation network |
| 06 | `06_bibcoupling.R` | Bibliographic coupling network |
| 07 | `07_tablolar.R` | Tables: authors, journals (SJR 2024), countries, institutions |
| 08 | `08_anahtar_kelime.R` | Keyword layer: network, rising keywords, thematic map |
| 09 | `09_donem_duyarlilik.R` | Period-boundary sensitivity check |
| 10 | `10_kunye_listesi.R` | Reference details taken from the corpus |
| 11 | `11_doi_kontrol.R` | DOI check of the reference list through Crossref |
| 12 | `12_vosviewer_biblioshiny.R` | Files for VOSviewer and Biblioshiny |
| 13 | `13_sekil2_yillik.R` | Figure 2: records per year, 1993–2025 |

## Software

R 4.5.3, bibliometrix 5.2.1, igraph 2.2.2, dplyr 1.2.0, readr 2.2.0, VOSviewer 1.6.20.
Random seed for network layouts and clustering: 2026.

## Extra input

`scimagojr_2024.csv` (SCImago Journal Rank, 2024 edition) from https://www.scimagojr.com/journalrank.php is needed by step 07 for journal quartiles.

## Licence

Code: MIT.
