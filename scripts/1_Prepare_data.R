library(magrittr)

# Import raw data --------------------------------------------------------------

# Import database or text files

db_all_data <- readr::read_tsv("data/Data.tsv", guess_max = 200000)

db_all_references <- readr::read_tsv("data/References.tsv", guess_max = 2000)

nomenclator <- googlesheets4::read_sheet(
  ss = "1sYpyYLVejyACo_ktMReHoHKcXsqX5LIYU9Zc2ZOe07g",
  sheet = "Taxa"
)

# Prepare data -----------------------------------------------------------------

# First, subset needed data

agbol <- 
  db_all_data %>%
  dplyr::filter(group %in% c("agaricoid", "boletoid")) %>% 
  dplyr::left_join(db_all_references) %>% 
  dplyr::filter(!PublicationYear %in% c(2021, 2022),
                !ZoteroKey %in% c("RQUDMHC8",
                                  "Z2PZIK7S",
                                  "2QS37RRK",
                                  "5JJ3SN2X",
                                  "VKEDT2F9")
                )

aphhet_euro <-
  db_all_data %>%
  dplyr::filter(
    !group %in% c("agaricoid",
                  "boletoid",
                  "gasteroid"),
    sector == "Europe",
    is.na(higherGeography) | higherGeography != "Northern Caucasus",
    !stateProvince %in% c("Crimea",
                          "Sevastopol",
                          "Chelyabinsk Oblast",
                          "Sverdlovsk Oblast",
                          "Yamalo-Nenets Autonomous Okrug",
                          "ZZ Urals"),
    !grepl("Weinmann", bibliographicCitation),
    !grepl("Fries", bibliographicCitation)
  ) %>%
  dplyr::left_join(db_all_references)

# Second, export Zotero Scannable Citations

agbol %>% 
  dplyr::select(ZoteroScannableCite) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(ZoteroScannableCite) %>% 
  readr::write_tsv("output/References_agbol.tsv")

aphhet_euro %>% 
  dplyr::select(ZoteroScannableCite) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(ZoteroScannableCite) %>% 
  readr::write_tsv("output/References_aphhet_euro.tsv")

# Third, add all additional needed citations to the file
# (e.g., paste these citations to a document with review and/or notes),
# export to ODT file,
# pipe to Zotero ODF Scan,
# restore citations with desired style,
# add citations to initial file to data folder

# Finally, re-import this updated file

agbol_refs <- readr::read_tsv("data/References_agbol.tsv",
                              guess_max = 1000)

aphhet_euro_refs <- readr::read_tsv("data/References_aphhet_euro.tsv",
                                    guess_max = 1000)

# Join project citations to data

agbol <- 
  dplyr::left_join(agbol, agbol_refs) %>% 
  dplyr::mutate(bibliographicCitation_cl = stringr::str_remove_all(
    string = bibliographicCitation_clear,
    pattern = "[()]"
  ))

aphhet_euro <- 
  dplyr::left_join(aphhet_euro, aphhet_euro_refs) %>% 
  dplyr::mutate(bibliographicCitation_cl = stringr::str_remove_all(
    string = bibliographicCitation_cl,
    pattern = "[()]"
  ))

# Select dataframe you need

data_raw <- agbol

data_raw <- aphhet_euro
