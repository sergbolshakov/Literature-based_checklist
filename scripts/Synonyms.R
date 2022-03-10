# Assign indices for synonyms --------------------------------------------------

synonyms_indexed <- 
  data %>% 
  dplyr::left_join(nomenclator,
                   by = c("scientificName" = "scientificName"),
                   keep = TRUE) %>%
  dplyr::select(acceptedNameUsage = acceptedNameUsage.x,
                scientificName = scientificName.x,
                taxonRank,
                taxonName,
                genericName:scientificNameAuthorship,
                taxonRemarks = taxonRemarks.y,
                taxonomicStatus = taxonomicStatus.x,
                protonymID) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(!taxonomicStatus == "reidentification") %>% 
  dplyr::mutate(scientificName_formatted = dplyr::case_when(
    taxonRank == "species" ~ stringr::str_c("*", taxonName, "* ",
                                            scientificNameAuthorship),
    TRUE ~ stringr::str_c("*", genericName, " ", specificEpithet, "* ",
                          verbatimTaxonRank, " *", infraspecificEpithet, "* ",
                          scientificNameAuthorship)
    )) %>% 
  dplyr::select(-(taxonRank:scientificNameAuthorship)) %>% 
  dplyr::group_by(acceptedNameUsage,
                  protonymID) %>% 
  dplyr::arrange(acceptedNameUsage,
                 factor(taxonomicStatus,
                        levels = c("accepted",
                                   "homotypic synonym",
                                   "heterotypic synonym",
                                   "misapplied name")),
                 protonymID,
                 scientificName) %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(group_size = dplyr::n(),
                counter = dplyr::case_when(
                  group_size == 1 & taxonomicStatus != "accepted" ~ dplyr::row_number(),
                  group_size > 1 ~ dplyr::row_number()
                )) %>% 
  dplyr::mutate(counter = tidyr::replace_na(as.character(counter), ""))

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
      scientificName_formatted, " ", taxonRemarks
    ),
    TRUE ~ scientificName_formatted
  )
  ) %>% 
  dplyr::select(-(taxonRemarks:protonymID),
                -group_size) %>% 
  dplyr::filter(acceptedNameUsage != scientificName) %>% 
  dplyr::mutate(scientificName_formatted = stringr::str_c(scientificName_formatted, 
                                                "^", counter, "^")) %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::summarize(synonyms = stringr::str_c(scientificName_formatted, collapse = " "))
