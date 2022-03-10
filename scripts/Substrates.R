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
                  group_size == 1 & substrate == "NO" ~ "No data about substrate",
                  TRUE ~ substrate
                )
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-group_size)

# Add auxiliary data -----------------------------------------------------------

substrate_groups <- readr::read_tsv("data/Substrate_groups.tsv")

coniferous <- c("Abies",
                "Juniperus",
                "Larix",
                "Picea",
                "Pinus",
                "Pseudotsuga",
                "Thuja")

deciduous <- 
  substrate_groups %>% 
  dplyr::filter(type == "wood",
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
  dplyr::group_by(acceptedNameUsage, type) %>% 
  dplyr::mutate(group_size = dplyr::n()) %>%
  dplyr::filter(!(group_size > 1 & substrate == "NO"),
                dplyr::case_when(
                  any(type == "wood" & group_size > 1 & substrate  == "wood") ~ 
                    substrate != "wood",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type == "wood" & group_size > 1 & substrate %in% coniferous) ~ 
                    substrate != "coniferous",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type == "wood" & group_size > 1 & substrate %in% deciduous) ~ 
                    substrate != "deciduous",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type == "ferns" & group_size > 1 & substrate  == "ferns") ~ 
                    substrate != "ferns",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type == "herbs" & group_size > 1 & substrate  == "herbs") ~ 
                    substrate != "herbs",
                  TRUE ~ all()
                ),
                dplyr::case_when(
                  any(type == "basidiomata" & group_size > 1 & substrate  == "basidiomata") ~ 
                    substrate != "basidiomata",
                  TRUE ~ all()
                )
  ) %>% 
  dplyr::summarize(substrate = stringr::str_c("*", substrate, "*"),
                   substrate_groups = stringr::str_c(substrate,
                                                     collapse = ", ")
                   ) %>% 
  dplyr::arrange(acceptedNameUsage,
                 factor(type,
                        levels = c("wood",
                                   "litter",
                                   "ferns",
                                   "herbs",
                                   "mosses",
                                   "basidiomata",
                                   "ascomata",
                                   "lichens")
                 )
  ) %>% 
  dplyr::mutate(substrates = stringr::str_c(type, " (", substrate_groups, ")"),
                substrates = tidyr::replace_na(
                  dplyr::case_when(substrate_groups == "*algae*" ~ "algae",
                                   substrate_groups == "*soil*" ~ "soil",
                                   substrate_groups == "*stones*" ~ "stones",
                                   substrate_groups == "*No data about substrate*" ~ 
                                     "No data about substrate",
                                   TRUE ~ substrates)
                )
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::select(-type, -substrate, -substrate_groups) %>% 
  dplyr::filter(!is.na(substrates)) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(substrates = stringr::str_replace_all(
    string = substrates,
    stringr::coll(
      c("wood (*wood*)" = "wood",
        "litter (*litter*)" = "litter",
        "litter (*debris*)" = "litter (debris)",
        "litter (*cones*)" = "litter (cones)",
        "litter (*leaves*)" = "litter (leaves)",
        "ferns (*ferns*)" = "ferns",
        "herbs (*herbs*)" = "herbs",
        "mosses (*mosses*)" = "mosses",
        "basidiomata (*basidiomata*)" = "basidiomata",
        "No data about substrate (*No data about substrate*)" = "No data about substrate")
    ))
  )

# Assembly substrates ----------------------------------------------------------

substrates <- 
  substrates_grouped %>%
  dplyr::group_by(acceptedNameUsage) %>% 
  dplyr::summarise(substrates = stringr::str_c(substrates,
                                               collapse = ", "),
                   substrates = dplyr::case_when(
                     substrates == "No data about substrate" ~ stringr::str_c(substrates, "."),
                     TRUE ~ stringr::str_c("On ", substrates, ".")
                   )
  )
