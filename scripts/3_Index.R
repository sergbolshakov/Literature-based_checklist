# Get all names for index ------------------------------------------------------

checklist_index <- 
  list(dplyr::select(data_raw, acceptedNameUsage),
       dplyr::select(data_raw, previousIdentifications),
       dplyr::select(data_raw, scientificName)) %>% 
  purrr::map(~ purrr::set_names(.x, nm = c("scientificName"))) %>%
  dplyr::bind_rows() %>% 
  dplyr::distinct() %>% 
  dplyr::filter(!is.na(scientificName),
                !grepl("Ω", scientificName),
                scientificName != "Athelia Pers.",
                scientificName != "Dichostereum Pilát") %>% 
  dplyr::left_join(nomenclator,
                   by = c("scientificName" = "scientificName")) %>%
  dplyr::select(scientificName,
                taxonRank) %>% 
  dplyr::mutate(taxonRank = tidyr::replace_na(taxonRank, "species")) %>% 
  dplyr::mutate(taxonName = dplyr::case_when(
    taxonRank == "species" ~ stringr::word(scientificName, 1, 2),
    TRUE ~ stringr::word(scientificName, 1, 4)
  )) %>% 
  dplyr::mutate(lastPart = stringr::word(taxonName, -1)) %>% 
  dplyr::mutate(firstPart = dplyr::case_when(
    taxonRank == "species" ~ stringr::word(taxonName, 1),
    TRUE ~ stringr::word(taxonName, 1, 2)
  )) %>% 
  dplyr::mutate(middlePart = dplyr::case_when(
    taxonRank == "species" ~ "",
    TRUE ~ stringr::word(taxonName, -2)
  )) %>% 
  dplyr::left_join(data_raw,
                   by = c("scientificName" = "acceptedNameUsage"),
                   keep = TRUE) %>% 
  dplyr::select(taxonName,
                lastPart,
                firstPart,
                middlePart,
                acceptedNameUsage) %>% 
  dplyr::distinct() %>%
  dplyr::mutate(lastPart = dplyr::case_when(
    !is.na(acceptedNameUsage) ~ stringr::str_c("***", lastPart, "***"),
    TRUE ~ stringr::str_c("*", lastPart, "*")
  )) %>% 
  dplyr::mutate(firstPart = dplyr::case_when(
    !is.na(acceptedNameUsage) ~ stringr::str_c("***", firstPart, "***"),
    TRUE ~ stringr::str_c("*", firstPart, "*")
  )) %>% 
  dplyr::mutate(taxonName2 = stringr::str_c(lastPart, firstPart, sep = ", "),
                taxonName2 = dplyr::case_when(
                  !is.na(acceptedNameUsage) ~ taxonName2,
                  TRUE ~ stringr::str_c(taxonName2, middlePart, sep = " ")
                )) %>% 
  dplyr::select(taxonName, taxonName2) %>% 
  dplyr::arrange(taxonName)

# Save index concordance file --------------------------------------------------

save(checklist_index, file = "output/Checklist_index.Rda")

rmarkdown::render(input = "scripts/Checklist_index.Rmd",
                  output_file = "../output/Checklist_index.docx")
