# Checklist preparation

citations <- 
  aphhet_euro %>% 
  dplyr::left_join(synonyms_indexed,
                   by = c("acceptedNameUsage", "scientificName")) %>% 
  dplyr::select(bibliographicCitation_clear,
                PublicationYear,
                acceptedNameUsage,
                scientificName,
                taxonomicStatus = taxonomicStatus.x,
                stateProvince,
                counter) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(counter = as.numeric(counter)) %>% 
  dplyr::arrange(acceptedNameUsage,
                 stateProvince,
                 PublicationYear,
                 counter) %>% 
  dplyr::group_by(acceptedNameUsage,
                  bibliographicCitation_clear,
                  stateProvince) %>% 
  dplyr::mutate(
    counter = stringr::str_c(counter,
                             collapse = ","),
    bibliographicCitation_clear = dplyr::case_when(
      counter != "" ~ stringr::str_c("^",
                                     counter,
                                     "^",
                                     bibliographicCitation_clear),
      TRUE ~ bibliographicCitation_clear
      ),
    bibliographicCitation_clear = dplyr::case_when(
      taxonomicStatus == "reidentification" ~ stringr::str_c(
        bibliographicCitation_clear,
        ", as ",
        scientificName
        ),
      TRUE ~ bibliographicCitation_clear
      )
    ) %>% 
  dplyr::filter(!taxonomicStatus %in% c("#N/A",
                                        "doubtful",
                                        "absent name",
                                        "ambiguous name")
                ) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                bibliographicCitation_clear) %>% 
  dplyr::group_by(acceptedNameUsage,
                  stateProvince) %>% 
  dplyr::distinct() %>% 
  dplyr::summarize(citations = stringr::str_c(bibliographicCitation_clear,
                                              collapse = "; ")
                   ) %>% 
  dplyr::ungroup()
  
distributions <- 
  citations %>% 
  dplyr::mutate(region_citations = stringr::str_c("**",
                                                  stateProvince,
                                                  "** (",
                                                  citations,
                                                  ")")
                ) %>% 
  dplyr::select(acceptedNameUsage,
                region_citations) %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::summarize(region_citations = stringr::str_c(region_citations,
                                                     collapse = "; ")
                   )

checklist <- 
  distributions %>% 
  dplyr::left_join(synonyms) %>%
  dplyr::left_join(substrates) %>%
  dplyr::left_join(synonyms_indexed %>% 
                     dplyr::filter(taxonomicStatus == "accepted"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")
                   ) %>% 
  dplyr::mutate(acceptedNameUsage = dplyr::case_when(
    counter != "" ~ stringr::str_c(acceptedNameUsage,
                                   "!",
                                   counter),
    TRUE ~ acceptedNameUsage
    )) %>% 
  dplyr::select(acceptedNameUsage,
                synonyms,
                substrates,
                region_citations) %>% 
  dplyr::filter(acceptedNameUsage != "#N/A",
                !grepl("Ω", acceptedNameUsage)) %>% 
  dplyr::mutate(substrates = tidyr::replace_na(substrates, "No data about substrate."))
    

readr::write_tsv(checklist,
                 file = "output/Checklist_aphhet_euro.tsv")

save(checklist,
     file = "output/Checklist_aphhet_euro.rda")
