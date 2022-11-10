library(magrittr)

# Import raw data --------------------------------------------------------------

# Import database, Excel or text files

db_all_data <- readr::read_tsv("data/Data.tsv", guess_max = 200000)
db_all_references <- readr::read_tsv("data/References.tsv", guess_max = 2000)

db_all_data <- readxl::read_xlsx("d:/GoogleDrive/FungalDB/Basidiomycetes_Russia.xlsx",
                                 sheet = "Data",
                                 guess_max = 200000) %>%  
  dplyr::select(bibliographicCitation,
                acceptedNameUsage,
                previousIdentifications,
                scientificName,
                taxonomicStatus,
                morphogroup,
                occurrenceRemarks,
                associatedTaxa,
                locality,
                stateProvince,
                higherGeography,
                sector) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(bibliographicCitation,
                 acceptedNameUsage,
                 stateProvince)

db_all_references <- readxl::read_xlsx("d:/GoogleDrive/FungalDB/Basidiomycetes_Russia.xlsx",
                                       sheet = "References",
                                       guess_max = 2000) %>%  
  dplyr::select(bibliographicCitation,
                ZoteroScannableCite,
                PublicationYear)

nomenclator <- googlesheets4::read_sheet(
  ss = "1sYpyYLVejyACo_ktMReHoHKcXsqX5LIYU9Zc2ZOe07g",
  sheet = "Taxa"
) %>% 
  dplyr::select(scientificName,
                protonymID,
                taxonRank,
                namePhrase)

# import data on province for checklist in Russian

provinces <- googlesheets4::read_sheet(
  ss = "1R8tXMAOmgFZyS1Ypsox4Kpqe9IqJjq6hk0vIJtmLR08",
  sheet = "States"
) %>% 
  dplyr::select(alt_name_ru,
                alt_name_en)

# Prepare data -----------------------------------------------------------------

# First, subset needed data

aphyllo <-
  db_all_data %>%
  dplyr::filter(
    !is.na(acceptedNameUsage),
    !morphogroup %in% c("agaricoid", "boletoid", "gasteroid"),
    sector == "Europe" | stateProvince == "Sverdlovsk Oblast",
    is.na(higherGeography) | higherGeography != "Northern Caucasus",
    !stateProvince %in% c("Yamalo-Nenets Autonomous Okrug"),
    !grepl("Weinmann", bibliographicCitation),
    !grepl("Fries", bibliographicCitation),
    !grepl("2022", bibliographicCitation)
  ) %>%
  dplyr::left_join(db_all_references)

# Second, export Zotero Scannable Citations

aphyllo %>% 
  dplyr::select(ZoteroScannableCite) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(ZoteroScannableCite) %>% 
  readr::write_tsv("output/References_aphyllo.tsv")

# Third, add all additional needed citations to the file
# (e.g., paste these citations to a document with review and/or notes),
# export to ODT file,
# pipe to Zotero ODF Scan,
# restore citations with desired style,
# add citations to initial file to data folder

# Finally, re-import this updated file

aphyllo_refs <- readxl::read_xlsx("data/References_aphyllo.xlsx",
                                  guess_max = 1100)

# Join project citations to data

data_raw <- 
  aphyllo_refs %>% 
  dplyr::left_join(aphyllo) %>% 
  dplyr::mutate(citation = stringr::str_remove_all(
    string = citation,
    pattern = "[()]"
  )) %>% 
  # join data on province if need checklist in Russian
  dplyr::left_join(provinces,
                   by = c("stateProvince" = "alt_name_en")) %>% 
  dplyr::mutate(alt_name_ru = dplyr::case_when(
    locality == "Arid territories of the South-West of Russia" ~ "ЯЯ АТ",
    locality == "Lapland" ~ "ЯЯ Лапландия",
    locality == "Urals" ~ "ЯЯ Урал",
    TRUE ~ alt_name_ru
  )) %>% 
  dplyr::select(citation,
                PublicationYear,
                -ZoteroScannableCite,
                -bibliographicCitation,
                stateProvince = alt_name_ru,
                acceptedNameUsage:associatedTaxa) %>% 
  dplyr::distinct()
