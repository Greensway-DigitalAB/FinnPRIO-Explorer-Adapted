## prepare cleanfinnprioresults
species_data <- simulations |> 
  # left_join(simulationSummaries) |> 
  left_join(assessments) |> 
  left_join(pests |> 
              left_join(quaran) |> 
              left_join(taxa, by = "idTaxa", suffix = c(".q",".t"))
  ) |> 
  # mutate(inEurope = as.logical(inEurope)) |> 
  mutate(inEurope = as.logical(inEurope)) |> 
  select(idSimulation, scientificName, name.t, endDate, name.q, eppoCode, inEurope) |> 
  rename("pest" = scientificName,
         "assessment_date" = endDate,
         "eppo_code" = eppoCode,
         "presence_in_europe" = inEurope, 
         "quarantine_status" = name.q,
         "taxonomic_group" = name.t)

sim_data <- simulations |> 
  ## pick the latest
  group_by(idAssessment) |> 
  arrange(desc(date), desc(idSimulation)) |> 
  slice(1) |> 
  ungroup() |> 
  ## complete the data
  left_join(simulationSummaries) |> 
  select(idSimulation, variable, q25, median, q75) |> 
  # 2. Pivot wider so each variable becomes columns with suffixes
  pivot_wider(
    names_from = variable,
    values_from = c(q25, median, q75),
    names_glue = "{variable}_{.value}"
  ) |> 
  select(idSimulation, ENTRYA_q25, ENTRYA_median, ENTRYA_q75,
         ESTABLISHMENT_q25, ESTABLISHMENT_median, ESTABLISHMENT_q75,
         INVASIONA_q25, INVASIONA_median, INVASIONA_q75,
         IMPACT_q25, IMPACT_median, IMPACT_q75,
         PREVENTABILITY_median, CONTROLLABILITY_median, MANAGEABILITY_median)
# "RISKA_q25"              "RISKB_q25" 

cleanfinnprioresults <- species_data |> 
  left_join(sim_data) |>
  select(-idSimulation) |> 
  # 3. Rename columns to match your desired format
  rename("entry_25perc" = ENTRYA_q25, 
         "entry_median" = ENTRYA_median, 
         "entry_75perc" = ENTRYA_q75,
         "establishment_and_spread_25perc" = ESTABLISHMENT_q25, 
         "establishment_and_spread_median" = ESTABLISHMENT_median, 
         "establishment_and_spread_75perc" = ESTABLISHMENT_q75,
         "invasion_25perc" = INVASIONA_q25, 
         "invasion_median" = INVASIONA_median, 
         "invasion_75perc" = INVASIONA_q75,
         "impact_25perc" = IMPACT_q25, 
         "impact_median" = IMPACT_median, 
         "impact_75perc" = IMPACT_q75,
         "preventability_median" = PREVENTABILITY_median, 
         "controlability_median" = CONTROLLABILITY_median, 
         "manageability_median" = MANAGEABILITY_median)


# print(cleanfinnprioresults)
## Prepare pestquestions
questions_opt <- lapply(seq(1,nrow(questions_main)), function(i) {
  df <- fromJSON(questions_main$list[i])
  df$idQuestion <- questions_main$idQuestion[i]
  df |> select(opt, text, idQuestion)
}) |> bind_rows()
# 
# questions_entry
# 
# assessments
# answers_main


full_grid <- expand.grid(idAssessment = assessments$idAssessment,
                         idQuestion = questions_main$idQuestion)

# 2. Identify missing rows
missing_rows <- full_grid  |> 
  anti_join(answers_main, by = c("idAssessment","idQuestion")) |> 
  mutate(
    idAnswer = NA,  # or generate new IDs later
    min = NA,
    likely = NA,
    max = NA,
    justification = ""
  )

# 3. Append missing rows to answers
answers_complete <- bind_rows(answers_main, missing_rows) |> 
  arrange(idAssessment, idQuestion)

# # Optional: generate new idAnswer for missing rows
# answers_complete <- answers_complete |> 
#   mutate(idAnswer = ifelse(is.na(idAnswer),
#                            max(answers_main$idAnswer, na.rm = TRUE) + row_number(),
#                            idAnswer))


# Assume you have data frames: questions, answers, assessments, pests
# 1. Join answers with questions
answers_long <- answers_complete |> 
  select(-idAnswer ) |> 
  left_join(questions_main, by = "idQuestion") |> 
  # 2. Join with assessments to get pest IDs
  left_join(assessments, by = "idAssessment") |> 
  # 3. Join with pests to get pest names
  left_join(pests, by = "idPest") |> 
  # 4. Create a column for answer type (most likely, min, max)
  pivot_longer(cols = c(min, likely, max),
               names_to = "Answer for",
               values_to = "Answer")

# 5. Build the final table: group by question and spread pests as columns
final_table <- answers_long  |> 
  select(group, idQuestion, `Answer for`, scientificName , Answer)  |> 
  pivot_wider(names_from = scientificName , values_from = Answer) |> 
  arrange(group, idQuestion) |> 
  rename("Codes" = group) |> 
  as.data.frame()


replace_opts <- function(column, idQuestion, lookup, questions) {
  # Join each value with its description
  sapply(seq_along(column), function(i) {
    val <- column[i]
    qid <- idQuestion[i]
    type <- questions$type[questions$idQuestion == qid]
    if (type == "minmax") {
      desc <- lookup$text[lookup$opt == val & lookup$idQuestion == qid]
      res <- if (length(desc) == 1) paste0(val, ". ", desc) else val
    } else {
      desc <- lookup$text[lookup$idQuestion == qid]
      res <- if (!is.na(val)) paste0(desc ,": Yes") else paste0(desc ,": No")
    }
    return(res)
  })
}

# Apply to pest columns
for (col in pests$scientificName) {
  if (col %in% colnames(final_table))
    final_table[[col]] <- replace_opts(final_table[[col]], final_table$idQuestion, questions_opt, questions_main)
}

pestquestions <- final_table |> 
  left_join(questions_main |> 
              select(idQuestion, group, number, question)) |> 
  mutate(Question = paste0(group, number, ": ", question)) |> 
  arrange(number)
pestquestions$Question <- ifelse(pestquestions$'Answer for' != "min", "", pestquestions$Question)
# select(Codes, idQuestion, Question, 'Answer for')