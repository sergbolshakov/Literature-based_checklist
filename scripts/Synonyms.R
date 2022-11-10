# Assign indices for synonyms --------------------------------------------------

synonyms_indexed <- 
  data %>% 
  dplyr::left_join(nomenclator,
                   by = c("scientificName" = "scientificName")) %>%
  dplyr::select(acceptedNameUsage,
                scientificName,
                taxonRank,
                namePhrase,
                taxonomicStatus,
                protonymID) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(taxonName = dplyr::case_when(
    taxonRank == "species" ~ stringr::word(scientificName, 1, 2),
    TRUE ~ stringr::word(scientificName, 1, 4)
  )) %>% 
  dplyr::mutate(genericName = stringr::word(taxonName, 1)) %>%
  dplyr::mutate(specificEpithet = stringr::word(taxonName, 2)) %>%
  dplyr::mutate(verbatimTaxonRank = dplyr::case_when(
    taxonRank != "species" ~ stringr::word(taxonName, 3),
    TRUE ~ NA_character_
  )) %>% 
  dplyr::mutate(infraspecificEpithet = dplyr::case_when(
    taxonRank != "species" ~ stringr::word(taxonName, 4),
    TRUE ~ NA_character_
  )) %>% 
  dplyr::mutate(
    scientificNameAuthorship = stringr::str_remove_all(string = scientificName,
                                                       stringr::coll(taxonName)),
    scientificNameAuthorship = stringr::str_trim(scientificNameAuthorship, side = "left")
  ) %>% 
  # dplyr::filter(!grepl("reidentification", identificationRemarks)) %>% 
  dplyr::mutate(scientificName_formatted = dplyr::case_when(
    taxonRank == "species" ~ stringr::str_c("*", taxonName, "* ",
                                            scientificNameAuthorship),
    TRUE ~ stringr::str_c("*", genericName, " ", specificEpithet, "* ",
                          verbatimTaxonRank, " *", infraspecificEpithet, "* ",
                          scientificNameAuthorship)
    )) %>% 
  dplyr::select(-(taxonName:scientificNameAuthorship)) %>% 
  dplyr::group_by(acceptedNameUsage,
                  protonymID) %>% 
  dplyr::arrange(acceptedNameUsage,
                 factor(taxonomicStatus,
                        levels = c("accepted",
                                   "homotypic synonym",
                                   "heterotypic synonym",
                                   "misapplied name")),
                 scientificName,
                 protonymID) %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(group_size = dplyr::n(),
                name_number = dplyr::case_when(
                  group_size == 1 & taxonomicStatus != "accepted" ~ dplyr::row_number(),
                  group_size > 1 ~ dplyr::row_number()
                )) %>% 
  dplyr::mutate(name_number = tidyr::replace_na(as.character(name_number), ""))

# Concatenate synonyms ---------------------------------------------------------

synonyms <-
  synonyms_indexed %>% 
  dplyr::mutate(scientificName_formatted = stringr::str_c(
    dplyr::case_when(
      taxonomicStatus == "homotypic synonym" ~ "≡ ",
      taxonomicStatus == "heterotypic synonym" | taxonomicStatus == "misapplied name" ~ "= ",
      TRUE ~ ""
    ),
    scientificName_formatted
  ),
  scientificName_formatted = dplyr::case_when(
    taxonomicStatus == "misapplied name" ~ stringr::str_c(
      scientificName_formatted, " ", namePhrase
    ),
    TRUE ~ scientificName_formatted
  )
  ) %>% 
  dplyr::select(-(namePhrase:protonymID),
                -group_size) %>% 
  dplyr::filter(acceptedNameUsage != scientificName) %>% 
  dplyr::mutate(scientificName_formatted = stringr::str_c(scientificName_formatted, 
                                                "^", name_number, "^")) %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::summarize(synonyms = stringr::str_c(scientificName_formatted, collapse = " "))
