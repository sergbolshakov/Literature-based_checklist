library(magrittr)

# Checklist of names can be ascribed unambiguously to accepted current names

data_raw %>% 
  dplyr::filter(!taxonomicStatus %in% c("doubtful",
                                        "ambiguous name",
                                        "absent name",
                                        "#N/A")) ->
  data

source("scripts/Synonyms.R")

source("scripts/Substrates.R")

source("scripts/Checklist.R")

save(checklist, file = "output/Checklist.Rda")

rmarkdown::render(input = "scripts/Checklist.Rmd",
                  output_file = "../output/Checklist_clean.docx")

# Checklist of names can't be ascribed to accepted current names:
# - doubtful records
# - ambiguous names having no accepted current names
# - names with existing binominals but with wrong authors’ citation
# - names absent from Index Fungorum

data_raw %>% 
  dplyr::filter(taxonomicStatus %in% c("doubtful",
                                       "ambiguous name",
                                       "absent name",
                                       "#N/A")) %>% 
  dplyr::mutate(acceptedNameUsage = dplyr::case_when(
    acceptedNameUsage == "#N/A" ~ scientificName,
    grepl("Ω", acceptedNameUsage) ~ scientificName,
    TRUE ~ acceptedNameUsage
  )) %>% 
  dplyr::mutate(taxonomicStatus = dplyr::case_when(
    acceptedNameUsage == scientificName ~ "accepted",
    TRUE ~ "homotypic synonym"
  )) ->
  data

source("scripts/Synonyms.R")

source("scripts/Substrates.R")

source("scripts/Checklist.R")

save(checklist, file = "output/Checklist.Rda")

rmarkdown::render(input = "scripts/Checklist.Rmd",
                  output_file = "../output/Checklist_doubt.docx")
