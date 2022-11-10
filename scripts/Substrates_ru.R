# Subset substrates ------------------------------------------------------------

substrates_ungrouped <- 
  data %>% 
  dplyr::select(acceptedNameUsage, associatedTaxa) %>% 
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(!is.na(associatedTaxa)) %>% 
  dplyr::summarize(substrates = paste(associatedTaxa, collapse = ", ")) %>% 
  dplyr::mutate(substrate = stringr::str_split(
    string = substrates,
    pattern = ", "
  )) %>% 
  dplyr::select(-substrates) %>%
  tidyr::unnest(substrate) %>% 
  dplyr::arrange(acceptedNameUsage, substrate) %>% 
  dplyr::mutate(substrate = stringr::str_split_fixed(
    string = substrate,
    pattern = " ",
    n = 2)[,1]
  ) %>% 
  dplyr::distinct() %>%
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::mutate(group_size = dplyr::n(),
                substrate = dplyr::case_when(
                  group_size == 1 & substrate == "NO" ~ "Нет данных о субстрате",
                  TRUE ~ substrate
                )
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-group_size)

# Add auxiliary data -----------------------------------------------------------

substrate_groups <- readr::read_tsv("data/Substrate_groups.tsv")

coniferous <- c("Abies",
                "Cedrus",
                "Cephalotaxus",
                "Cupressus",
                "Ephedra",
                "Juniperus",
                "Larix",
                "Metasequoia",
                "Picea",
                "Pinus",
                "Platycladus",
                "Pseudotsuga",
                "Sequoia",
                "Sequoiadendron",
                "Taxodium",
                "Taxus",
                "Thuja")

deciduous <- 
  substrate_groups %>% 
  dplyr::filter(type_en == "wood",
                !(substrate %in% coniferous),
                !(substrate %in% c("wood", "coniferous", "deciduous", "timber"))
  ) %>% 
  dplyr::pull(substrate)

# Group substrates -------------------------------------------------------------

substrates_grouped <- 
  substrates_ungrouped %>% 
  dplyr::left_join(substrate_groups,
                   .keep = TRUE) %>% 
  dplyr::arrange(acceptedNameUsage,
                 factor(substrate,
                        levels = substrate_groups$substrate)
  ) %>% 
  dplyr::group_by(acceptedNameUsage, type_ru) %>% 
  dplyr::mutate(group_size = dplyr::n()) %>%
  dplyr::filter(!(group_size > 1 & substrate == "NO"),
                dplyr::case_when(
                  any(type_en == "wood" & group_size > 1 & substrate  == "wood") ~ 
                    substrate != "wood",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type_en == "wood" & group_size > 1 & substrate %in% coniferous) ~ 
                    substrate != "coniferous",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type_en == "wood" & group_size > 1 & substrate %in% deciduous) ~ 
                    substrate != "deciduous",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type_en == "ferns" & group_size > 1 & substrate  == "ferns") ~ 
                    substrate != "ferns",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type_en == "herbs" & group_size > 1 & substrate  == "herbs") ~ 
                    substrate != "herbs",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type_en == "basidiomata" & group_size > 1 & substrate  == "basidiomata") ~ 
                    substrate != "basidiomata",
                  TRUE ~ all()
                )
  ) %>% 
  dplyr::summarize(substrate = stringr::str_c("*", substrate, "*"),
                   substrate_groups = stringr::str_c(substrate,
                                                     collapse = ", ")
  ) %>% 
  dplyr::arrange(acceptedNameUsage,
                 factor(type_ru,
                        levels = c("древесине",
                                   "подстилке",
                                   "травах",
                                   "папоротниках",
                                   "мхах",
                                   "базидиомах",
                                   "аскомах",
                                   "лишайниках",
                                   "водорослях",
                                   "почве",
                                   "камнях")
                        )
                 ) %>% 
  dplyr::mutate(substrates = stringr::str_c(type_ru,
                                            " (",
                                            substrate_groups,
                                            ")")
                ) %>% 
  dplyr::ungroup() %>%
  dplyr::select(-type_ru, -substrate, -substrate_groups) %>% 
  dplyr::filter(!is.na(substrates)) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(substrates = stringr::str_replace_all(
    string = substrates,
    stringr::coll(
      c("древесине (*wood*)" = "древесине",
        "*coniferous*" = "хвойных",
        "*deciduous*" = "лиственных",
        "*timber*" = "обработанная древесина",
        "подстилке (*litter*)" = "подстилке",
        "подстилке (*debris*)" = "подстилке",
        "подстилке (*debris*, *litter*)" = "подстилке",
        "подстилке (*cones*)" = "шишках",
        "подстилке (*cones*, *litter*)" = "подстилке, шишках",
        "подстилке (*cones*, *debris*, *fruits*, *litter*)" = "подстилке, шишках",
        "подстилке (*leaves*)" = "отмерших листьях",
        "подстилке (*leaves*, *litter*)" = "подстилке, отмерших листьях",
        "травах (*herbs*)" = "травах",
        "папоротниках (*ferns*)" = "папоротниках",
        "мхах (*mosses*)" = "мхах",
        ", *mosses*" = "",
        "*mosses*, " = "",
        "базидиомах (*basidiomata*)" = "базидиомах",
        "водорослях (*algae*)" = "водорослях",
        "почве (*soil*)" = "почве",
        "камнях (*stones*)" = "камнях"
    ))
  ))

# Assembly substrates ----------------------------------------------------------

substrates <- 
  substrates_grouped %>%
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::summarise(substrates = stringr::str_c(substrates,
                                               collapse = ", "),
                   substrates = dplyr::case_when(
                     substrates == "Нет данных о субстрате" ~ stringr::str_c(substrates, "."),
                     TRUE ~ stringr::str_c("На ", substrates, ".")
                   )
  )
