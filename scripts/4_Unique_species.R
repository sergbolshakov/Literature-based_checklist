library(magrittr)

# Get list of species reported only from an one region -------------------------

unique_species <- 
  data_raw %>% 
  dplyr::filter(!taxonomicStatus %in% c("absent name",
                                        "ambiguous name",
                                        "ambiguous name authorship"),
                !occurrenceRemarks %in% c("doubtful occurrence")) %>% 
  dplyr::arrange(acceptedNameUsage, stateProvince, PublicationYear) %>%
  dplyr::group_by(acceptedNameUsage) %>%
  dplyr::mutate(count_regions = dplyr::n_distinct(stateProvince)) %>%
  dplyr::filter(count_regions == 1) %>%
  dplyr::select(-count_regions) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(nomenclator,
                   by = c("scientificName" = "scientificName")) %>%
  dplyr::select(acceptedNameUsage,
                stateProvince,
                citation,
                scientificName,
                taxonRank) %>% 
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
    scientificNameAuthorship = stringr::str_remove_all(
      string = scientificName,
      stringr::coll(taxonName)
    ),
    scientificNameAuthorship = stringr::str_trim(
      scientificNameAuthorship, side = "left"
    )
  ) %>% 
  dplyr::mutate(scientificName_formatted = dplyr::case_when(
    taxonRank == "species" ~ stringr::str_c("*", taxonName, "* ",
                                            scientificNameAuthorship),
    TRUE ~ stringr::str_c("*", genericName, " ", specificEpithet, "* ",
                          verbatimTaxonRank, " *", infraspecificEpithet, "* ",
                          scientificNameAuthorship)
  )) %>% 
  dplyr::select(-(taxonRank:scientificNameAuthorship)) %>% 
  dplyr::group_by(stateProvince, acceptedNameUsage) %>% 
  dplyr::mutate(citation = dplyr::case_when(
    acceptedNameUsage != scientificName ~ stringr::str_c(
      citation, ", as ", scientificName_formatted
    ),
    TRUE ~ citation
  )) %>% 
  dplyr::summarize(citations = stringr::str_c(citation, collapse = "; ")) %>% 
  dplyr::group_by(stateProvince) %>% 
  dplyr::mutate(species = dplyr::n_distinct(acceptedNameUsage),
                stateProvince = stringr::str_c("**", stateProvince, "**",
                                               " — ", species,
                                               #" species:"
                                               " видов:"
                                               )
                ) %>% 
  dplyr::mutate(taxonName = stringr::word(acceptedNameUsage, 1, 2),
                acceptedNameUsage = stringr::str_replace_all(
                  string = acceptedNameUsage,
                  pattern = stringr::coll(taxonName),
                  replacement = stringr::str_c("*", taxonName, "*")
                )) %>% 
  dplyr::summarize(species_citations = stringr::str_c(acceptedNameUsage,
                                                      " (", citations, ")."))

# Output list of unique species to docx file -----------------------------------
  
save(unique_species, file = "output/Unique_species.Rda")

rmarkdown::render(input = "scripts/Unique_species.Rmd",
                  output_file = "../output/Unique_species.docx")
