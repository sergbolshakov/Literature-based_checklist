# Import raw data --------------------------------------------------------------

# Import database or text files

data <- readr::read_tsv("data/Data.tsv",
                        guess_max = 200000)

references <- readr::read_tsv("data/References.tsv",
                               guess_max = 2000)

nomenclator <- googlesheets4::read_sheet(
  ss = "1sYpyYLVejyACo_ktMReHoHKcXsqX5LIYU9Zc2ZOe07g",
  sheet = "Taxa"
)

# Prepare needed data -----------------------------------------------------------

# First, subset needed data

aphhet_euro <-
  data %>%
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
    !grepl("Weinmann", bibliographicCitation)
  ) %>%
  dplyr::left_join(references)

# Second, export Zotero Scannable Citations

aphhet_euro %>% 
  dplyr::select(ZoteroScannableCite) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(ZoteroScannableCite) %>% 
  readr::write_tsv("output/References_aphhet.tsv")

# Third, add all additional needed citations (e.g., used in notes) to the file,
# export to ODT,
# pipe to Zotero ODF Scan,
# restore citations with desired style,
# add citations to initial file to data folder

# Finally, re-import this updated file

aphhet_refs <- readr::read_tsv("data/References_aphhet.tsv",
                                guess_max = 1000)

# Join to data

aphhet_euro <- 
  dplyr::left_join(aphhet_euro, aphhet_refs) %>% 
  dplyr::mutate(bibliographicCitation_clear = stringr::str_remove_all(
    string = bibliographicCitation_clear,
    pattern = "[()]"
  ))
