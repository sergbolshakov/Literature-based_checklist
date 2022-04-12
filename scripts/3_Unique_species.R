library(magrittr)

# Unique species

data_raw %>% 
  dplyr::filter(!taxonomicStatus %in% c("doubtful",
                                        "ambiguous name",
                                        "absent name",
                                        "#N/A")) %>% 
  dplyr::arrange(acceptedNameUsage, stateProvince, PublicationYear) %>%
  dplyr::group_by(acceptedNameUsage) %>%
  dplyr::mutate(count_regions = dplyr::n_distinct(stateProvince)) %>%
  dplyr::filter(count_regions == 1) %>%
  dplyr::select(-count_regions) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(nomenclator,
                   by = c("scientificName" = "scientificName"),
                   keep = TRUE) %>%
  dplyr::select(bibliographicCitation_cl,
                #PublicationYear,
                stateProvince,
                acceptedNameUsage = acceptedNameUsage.x,
                scientificName = scientificName.x,
                taxonRank,
                taxonName,
                genericName:scientificNameAuthorship) %>% 
  dplyr::distinct() %>% 
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
    acceptedNameUsage != scientificName ~ stringr::str_c(bibliographicCitation_cl, 
                                                         ", as ", scientificName_formatted),
    TRUE ~ bibliographicCitation_cl
  )) %>% 
  dplyr::summarize(citations = stringr::str_c(citation, collapse = "; ")) %>% 
  dplyr::group_by(stateProvince) %>% 
  dplyr::mutate(species = dplyr::n_distinct(acceptedNameUsage),
                stateProvince = stringr::str_c("**", stateProvince, "**",
                                               " — ", species, " species:")) %>% 
  dplyr::summarize(species_citations = stringr::str_c(acceptedNameUsage,
                                                      " (", citations, ").")) -> 
  unique_species
  
save(unique_species, file = "output/Unique_species.Rda")

rmarkdown::render(input = "scripts/Unique_species.Rmd",
                  output_file = "../output/Unique_species.docx")
