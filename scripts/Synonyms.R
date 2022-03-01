# Assign indices for synonyms ---------------------------------------------------------

synonyms_indexed <- 
  data %>% 
  dplyr::left_join(nomenclator,
                   by = c("scientificName" = "scientificName"),
                   keep = TRUE) %>%
  dplyr::select(acceptedNameUsage = acceptedNameUsage.x,
                scientificName = scientificName.x,
                taxonRemarks = taxonRemarks.y,
                taxonomicStatus = taxonomicStatus.x,
                protonymID) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(!taxonomicStatus %in% c("doubtful",
                                        "reidentification",
                                        "absent name",
                                        "ambiguous name"),
                acceptedNameUsage != "#N/A",
                taxonomicStatus != "#N/A") %>% 
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
  dplyr::mutate(scientificName = stringr::str_c(
    dplyr::case_when(
      taxonomicStatus == "homotypic synonym" ~ "≡ ",
      taxonomicStatus == "heterotypic synonym" | taxonomicStatus == "misapplied name" ~ "= ",
      TRUE ~ ""
    ),
    scientificName
  ),
  scientificName = dplyr::case_when(
    taxonomicStatus == "misapplied name" ~ stringr::str_c(
      scientificName, " ", taxonRemarks
    ),
    TRUE ~ scientificName
  )
  ) %>% 
  dplyr::select(-(taxonRemarks:group_size)) %>% 
  dplyr::filter(acceptedNameUsage != scientificName) %>% 
  dplyr::mutate(scientificName = stringr::str_c(scientificName, 
                                                "^", counter, "^")) %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::summarize(synonyms = stringr::str_c(scientificName, collapse = " "))
