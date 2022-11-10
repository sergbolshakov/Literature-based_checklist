# Merge citations for each region ----------------------------------------------

citations <- 
  data %>% 
  dplyr::left_join(synonyms_indexed,
                   by = c("acceptedNameUsage", "scientificName")) %>% 
  dplyr::select(citation,
                PublicationYear,
                acceptedNameUsage,
                previousIdentifications,
                scientificName,
                taxonomicStatus = taxonomicStatus.x,
                stateProvince,
                name_number) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(name_number = dplyr::case_when(
    !is.na(previousIdentifications) ~ "",
    TRUE ~ name_number
  )) %>% 
  dplyr::arrange(acceptedNameUsage,
                 stateProvince,
                 PublicationYear,
                 citation,
                 desc(is.na(previousIdentifications)),
                 as.numeric(name_number)) %>% 
  dplyr::group_by(acceptedNameUsage,
                  stateProvince,
                  citation) %>% 
  dplyr::mutate(
    # group name numbers for a one reference
    name_number = dplyr::case_when(
      is.na(previousIdentifications) ~ stringr::str_c(as.character(name_number),
                                                      collapse = ","),
      TRUE ~ as.character(name_number)),
    name_number = stringr::str_remove_all(string = name_number,
                                      pattern = ",$"),
    # superscript name numbers
    citation = dplyr::case_when(
      name_number != "" & is.na(previousIdentifications) ~ stringr::str_c("^", name_number, "^",
                                     citation),
      TRUE ~ citation
    ),
    # add previous identifications
    # its should be in italics
    citation = dplyr::case_when(
      !is.na(previousIdentifications) ~ stringr::str_c(
        citation,
        #", as ",
        ", как ",
        previousIdentifications
      ),
      TRUE ~ citation
    )
  ) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                citation) %>% 
  dplyr::group_by(acceptedNameUsage,
                  stateProvince) %>% 
  dplyr::distinct() %>% 
  dplyr::summarize(citations = stringr::str_c(citation,
                                              collapse = "; ")
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(stateProvince = stringr::str_replace_all(
    string = stateProvince,
    #pattern = "ZZ ",
    pattern = "ЯЯ ",
    replacement = "")
  ) %>% 
  dplyr::filter(!is.na(stateProvince))

# Merge regions with citations for each accepted species -----------------------

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

# Combine synonyms, data on substrates and distribution per regions ------------

checklist <- 
  distributions %>% 
  dplyr::left_join(synonyms) %>%
  dplyr::left_join(substrates) %>%
  dplyr::left_join(synonyms_indexed %>% 
                     dplyr::filter(taxonomicStatus == "accepted"),
                   by = c("acceptedNameUsage" = "acceptedNameUsage")
  ) %>% 
  dplyr::mutate(acceptedNameUsage = dplyr::case_when(
    name_number != "" ~ stringr::str_c(acceptedNameUsage,
                                   "^", name_number, "^"),
    TRUE ~ acceptedNameUsage
  )) %>% 
  dplyr::select(acceptedNameUsage,
                synonyms,
                substrates,
                region_citations) %>% 
  dplyr::mutate(taxonName = stringr::word(acceptedNameUsage, 1, 2),
                acceptedNameUsage = stringr::str_replace_all(
                  string = acceptedNameUsage,
                  pattern = stringr::coll(taxonName),
                  replacement = stringr::str_c("*", taxonName, "*")
                  ),
                acceptedNameUsage = stringr::str_c("**",
                                                   acceptedNameUsage,
                                                   "**"),
                synonyms = tidyr::replace_na(synonyms, ""),
                #substrates = tidyr::replace_na(substrates, "No data about substrate.")
                substrates = tidyr::replace_na(substrates, "Нет данных о субстрате.")
  ) %>% 
  dplyr::select(-taxonName)
