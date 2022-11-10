library(magrittr)

# Checklist of names can be ascribed unambiguously to accepted current names

data <- 
  data_raw %>% 
  dplyr::filter(!taxonomicStatus %in% c("absent name",
                                        "ambiguous name",
                                        "ambiguous name authorship"),
                !occurrenceRemarks %in% c("doubtful occurrence"))

source("scripts/Synonyms.R")

#source("scripts/Substrates.R")
source("scripts/Substrates_ru.R")

source("scripts/Checklist.R")

save(checklist, file = "output/Checklist.Rda")

rmarkdown::render(input = "scripts/Checklist.Rmd",
                  output_file = "../output/Checklist_clean.docx")

# Checklist of names can't be ascribed to accepted current names:
# - doubtful occurence
# - ambiguous names having no accepted current names
# - names with existing binominals but with wrong authors’ citation
# - names absent from Index Fungorum

data <- 
  data_raw %>% 
  dplyr::filter(taxonomicStatus %in% c("absent name",
                                       "ambiguous name",
                                       "ambiguous name authorship") |
                  occurrenceRemarks == "doubtful occurrence") %>% 
  dplyr::mutate(acceptedNameUsage = dplyr::case_when(
    grepl("Ω ", acceptedNameUsage) ~ scientificName,
    TRUE ~ acceptedNameUsage
  )) %>% 
  dplyr::mutate(taxonomicStatus = dplyr::case_when(
    acceptedNameUsage == scientificName ~ "accepted",
    TRUE ~ "homotypic synonym"
  ))

source("scripts/Synonyms.R")

#source("scripts/Substrates.R")
source("scripts/Substrates_ru.R")

source("scripts/Checklist.R")

save(checklist, file = "output/Checklist.Rda")

rmarkdown::render(input = "scripts/Checklist.Rmd",
                  output_file = "../output/Checklist_doubt.docx")
