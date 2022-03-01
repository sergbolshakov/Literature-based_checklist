library(magrittr)

# Select dataframe you need ----------------------------------------------------

data <- aphhet_euro

# Get synonyms and substrates if you need --------------------------------------

source("scripts/Synonyms.R")

source("scripts/Substrates.R")

# Merge citations for each region ----------------------------------------------

citations <- 
  data %>% 
  dplyr::left_join(synonyms_indexed,
                   by = c("acceptedNameUsage", "scientificName")) %>% 
  dplyr::select(bibliographicCitation_cl,
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
                  bibliographicCitation_cl,
                  stateProvince) %>% 
  dplyr::mutate(
    counter = stringr::str_c(counter,
                             collapse = ","),
    bibliographicCitation_cl = dplyr::case_when(
      counter != "" ~ stringr::str_c("^", counter, "^",
                                     bibliographicCitation_cl),
      TRUE ~ bibliographicCitation_cl
      ),
    bibliographicCitation_cl = dplyr::case_when(
      taxonomicStatus == "reidentification" ~ stringr::str_c(
        bibliographicCitation_cl,
        ", as ",
        scientificName
        ),
      TRUE ~ bibliographicCitation_cl
      )
    ) %>% 
  dplyr::filter(!taxonomicStatus %in% c("#N/A",
                                        "doubtful",
                                        "absent name",
                                        "ambiguous name")
                ) %>% 
  dplyr::select(acceptedNameUsage,
                stateProvince,
                bibliographicCitation_cl) %>% 
  dplyr::group_by(acceptedNameUsage,
                  stateProvince) %>% 
  dplyr::distinct() %>% 
  dplyr::summarize(citations = stringr::str_c(bibliographicCitation_cl,
                                              collapse = "; ")
                   ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(stateProvince = stringr::str_replace_all(
    string = stateProvince,
    pattern = "ZZ ",
    replacement = "")
  )

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
    counter != "" ~ stringr::str_c(acceptedNameUsage,
                                   "^", counter, "^"),
    TRUE ~ acceptedNameUsage
    )) %>% 
  dplyr::select(acceptedNameUsage,
                synonyms,
                substrates,
                region_citations) %>% 
  dplyr::filter(acceptedNameUsage != "#N/A",
                !grepl("Ω", acceptedNameUsage)) %>% 
  dplyr::mutate(acceptedNameUsage = stringr::str_c("**",
                                                   acceptedNameUsage,
                                                   "**"),
                synonyms = tidyr::replace_na(synonyms, ""),
                substrates = tidyr::replace_na(substrates, "No data about substrate.")
                )

# Save to the file for further formatting of the checklist in Word -------------
    
save(checklist, file = "output/Checklist_aphhet_euro.Rda")

# Output the checklist as docx document ----------------------------------------

rmarkdown::render(input = "scripts/Checklist.Rmd",
                  output_file = "../output/Checklist_aphhet_euro.docx")
