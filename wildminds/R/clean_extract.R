#' Clean datasets after they come out of Filemaker
#'
#'
#' @param data data frame as extracted from Filemaker
#' @param clean.value.levels TRUE or FALSE; should value names be cleaned to be more easily comprehensible for all columns?
#' @param clean.col.names TRUE or FALSE; should the column names be cleaned to more easily understandable
#' @param exclude.unknown.inds TRUE or FALSE; should unknown Signaller be excluded from the data?
#' @param exclude.unknown.gestures TRUE or FALSE; should cases be excluded if the Gesture_record is ["None","Unk","Time_ex", "no_value", "Unclear", "Pot_new"], or if the Directed_to is ["No", "Sev_pot","Unk"]
#' @param exclude.incomplete.coding TRUE or FALSE; should incomplete coding be excluded from the data?
#' @param exclude.unknown.direction TRUE or FALSE; should unknown direction be excluded from the data?
#' @param exclude.goal.unmet TRUE or FALSE; should unmet goals be excluded from the data?
#' @param exclude.no.response.waiting TRUE or FALSE; should lacking response waiting be excluded from the data?
#' @param exclude.no.persistence TRUE or FALSE; should lacking persistence be excluded from the data?
#' @param exclude.unknown.goal TRUE or FALSE; should unknown goal be excluded from the data?
#'
#' @return Function returns a cleaned version of the dataframe that is entered
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all group_by mutate_at
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom tidytext unnest_tokens
#'
#' @export


clean_function <- function(data,
                           clean.value.levels = TRUE,
                           clean.col.names = TRUE,
                           exclude.unknown.inds = FALSE,
                           exclude.unknown.gestures = FALSE,
                           exclude.incomplete.coding = FALSE,
                           exclude.unknown.direction = FALSE,
                           exclude.goal.unmet = FALSE,
                           exclude.no.response.waiting = FALSE,
                           exclude.no.persistence = FALSE,
                           exclude.unknown.goal = FALSE) {
  # Replace NA and NV -------------------------------------------------------

  data <- data %>%
    mutate_all(str_replace_all, "^NA$", "Not_applicable") %>%
    mutate_all(str_replace_all, "^NV$", "No_value")


  # If there are still old column names, rename them ------------------------
  if('Latency_1' %in% colnames(data)){
  data <- data %>%
    rename('Outcome' = 'BhvCh_start') %>%
    rename('Outcome_T' = 'BhvCh_start_T') %>%
    rename('BhvCh1' = 'Latency_1') %>%
    rename('BhvCh1_T' = 'Latency_1_T') %>%
    rename('BhvCh2' = 'Latency_2') %>%
    rename('BhvCh2_T' = 'Latency_2_T')
  }
  if('Sgn_lat' %in% colnames(data)){
    data <- data %>%
      mutate(Sgn_Lat = Sgn_lat)
  }
  if('Con_lat' %in% colnames(data)){
    data <- data %>%
      mutate(Con_Lat = Con_lat)
  }


  # Exclude unknown ---------------------------------------------------------

  if (exclude.unknown.inds) {
    data <- data %>%
      filter(!(
        .data$Signaller %in% c(
          "A_F",
          "A_M",
          "I",
          "J",
          "SA_M",
          "SA_F",
          "StrF",
          "StrM",
          "Unk",
          "Unk_M",
          "Unk_F"
        )
      ))
  }

  # Exclude cases -----------------------------------------------------------

  if (exclude.unknown.gestures) {
    data <- data %>%
      filter(!(
        .data$Gesture_record %in% c("None",
                                    "Unk",
                                    "Time_ex",
                                    "no_value",
                                    "Unclear",
                                    "Pot_new")
      ))
  }
  if (exclude.unknown.direction) {
    data <- data %>%
      filter(!(.data$Directed_to %in% c("No",
                                        "Sev_pot",
                                        "Unk")))
  }
  if (exclude.incomplete.coding) {
    data <- data %>%
      filter((.data$Cod_status %in% c("Complete")))
  }
  if (exclude.goal.unmet) {
    data <- data %>%
      filter(!(.data$Goal_met %in% c("No",
                                     "Unk")))
  }
  if (exclude.no.response.waiting) {
    data <- data %>%
      filter(!(.data$Response_waiting %in% c("No",
                                     "Unk")))
  }
  if (exclude.no.persistence) {
    data <- data %>%
      filter(!(.data$Persistence %in% c("No",
                                             "Unk")))
  }
  if (exclude.unknown.goal) {
    data <- data %>%
      filter(!(.data$Goal %in% c("Unk", "NA")) &
               !(is.na(.data$Goal)))
  }



  if (clean.value.levels) {
    # Cleaning Values ---------------------------------------------------------

    ## Arousal -----------------------------------------------------------------

    data$Sgn_arousal %>%
      str_replace_all(pattern = "Unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Sgn_arousal

    data$Rcp_arousal %>%
      str_replace_all(pattern = "Unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_arousal

    ## Directionality ----------------------------------------------------------

    data$Directed_to %>%
      str_to_lower() %>%
      str_replace_all(pattern = "one_dir", replacement = "one_directed") %>%
      str_replace_all(pattern = "one_potential", replacement = "one_potential") %>%
      str_replace_all(pattern = "sev_dir", replacement = "several_directed") %>%
      str_replace_all(pattern = "sev_pot", replacement = "several_potential") %>%
      str_replace_all(pattern = "unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Directed_to

    data$Direct_G %>%
      str_to_lower() %>%
      str_replace_all(pattern = "app_undir", replacement = "apparently_undirected") %>%
      str_replace_all(pattern = "self_loc", replacement = "self_location") %>%
      str_replace_all(pattern = "unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Direct_G

    ## Goal --------------------------------------------------------------------

    data$Goal %>%
      str_to_lower() %>%
      str_replace_all(pattern = "gv_objfd", replacement = "give_me") %>%
      str_replace_all(pattern = "_ns", replacement = "_nurse") %>%
      str_replace_all(pattern = "tk_objfd", replacement = "take_object") %>%
      str_replace_all(pattern = "aff_", replacement = "affilation_") %>%
      str_replace_all(pattern = "_unc", replacement = "_unclear") %>%
      str_replace_all(pattern = "_con", replacement = "_contact") %>%
      str_replace_all(pattern = "_dis", replacement = "_distant") %>%
      str_replace_all(pattern = "_rest", replacement = "_rest") %>%
      str_replace_all(pattern = "^att_", replacement = "attend_") %>%
      str_replace_all(pattern = "_sgn", replacement = "_me") %>%
      str_replace_all(pattern = "_rcp", replacement = "_you") %>%
      str_replace_all(pattern = "dir_", replacement = "direct_") %>%
      str_replace_all(pattern = "_att", replacement = "_attention") %>%
      str_replace_all(pattern = "cl_", replacement = "climb_on_") %>%
      str_replace_all(pattern = "fw_", replacement = "follow_") %>%
      str_replace_all(pattern = "grm", replacement = "groom") %>%
      str_replace_all(pattern = "sex_", replacement = "sexual_") %>%
      str_replace_all(pattern = "pl_", replacement = "play_") %>%
      str_replace_all(pattern = "_st$", replacement = "_start") %>%
      str_replace_all(pattern = "_co$", replacement = "_continue") %>%
      str_replace_all(pattern = "_ch", replacement = "_change") %>%
      str_replace_all(pattern = "_concha", replacement = "_contact_chase") %>%
      str_replace_all(pattern = "_chacon", replacement = "_chase_contact") %>%
      str_replace_all(pattern = "trv", replacement = "travel") %>%
      str_replace_all(pattern = "trv_", replacement = "travel_with_") %>%
      str_replace_all(pattern = "stp_", replacement = "stop_") %>%
      str_replace_all(pattern = "_bh", replacement = "_behaviour") %>%
      str_replace_all(pattern = "spp", replacement = "support") %>%
      str_replace_all(pattern = "unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Goal

    data$Goal_met %>%
      str_to_lower() %>%
      str_replace_all(pattern = "unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Goal_met

    ## Gesture Record ----------------------------------------------------------

    data$Gesture_record %>%
      str_replace_all(pattern = "Tap_rcp", replacement = "Hit_rcp_sft") %>%
      str_replace_all(pattern = "Tapp_rcp", replacement = "Hitt_rcp_sft") %>%
      str_replace_all(pattern = "Tapp_obj", replacement = "Hitt_objgd_sft") %>%
      str_replace_all(pattern = "Time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "Bg_ld_scr", replacement = "big_loud_scratch") %>%
      str_replace_all(pattern = "_ks", replacement = "_kiss") %>%
      str_replace_all(pattern = "_thr", replacement = "_threat") %>%
      str_replace_all(pattern = "_obj$", replacement = "_object") %>%
      str_replace_all(pattern = "_obj_", replacement = "_object_") %>%
      str_replace_all(pattern = "_slf", replacement = "_self") %>%
      str_replace_all(pattern = "ChBt", replacement = "chest_beat") %>%
      str_replace_all(pattern = "_inf_", replacement = "_informal_") %>%
      str_replace_all(pattern = "_oth", replacement = "_other") %>%
      str_replace_all(pattern = "_std", replacement = "_standing") %>%
      str_replace_all(pattern = "_shk", replacement = "_shake") %>%
      str_replace_all(pattern = "_slf", replacement = "_self") %>%
      str_replace_all(pattern = "_mth", replacement = "_mouth") %>%
      str_replace_all(pattern = "_objgd", replacement = "_object") %>%
      str_replace_all(pattern = "_rcp", replacement = "_other") %>%
      str_replace_all(pattern = "Hitt_", replacement = "hitting_") %>%
      str_replace_all(pattern = "Stmp_", replacement = "stomp_") %>%
      str_replace_all(pattern = "Stmpg_", replacement = "stomping_") %>%
      str_replace_all(pattern = "Tapp_", replacement = "tapping_") %>%
      str_replace_all(pattern = "_clp", replacement = "_clap") %>%
      str_replace_all(pattern = "_nonrcp", replacement = "_bystander") %>%
      str_replace_all(pattern = "_sft", replacement = "_soft") %>%
      str_replace_all(pattern = "Lf_", replacement = "leaf_") %>%
      str_replace_all(pattern = "^Loc", replacement = "Locomote") %>%
      str_replace_all(pattern = "_bip", replacement = "_bipedal") %>%
      str_replace_all(pattern = "_gal", replacement = "_gallop") %>%
      str_replace_all(pattern = "^Obj_", replacement = "Object_") %>%
      str_replace_all(pattern = "_mth", replacement = "_mouth") %>%
      str_replace_all(pattern = "_att$", replacement = "_attached") %>%
      str_replace_all(pattern = "_unatt", replacement = "_unattached") %>%
      str_replace_all(pattern = "_mv", replacement = "_move") %>%
      str_replace_all(pattern = "Prs", replacement = "present") %>%
      str_replace_all(pattern = "_gn", replacement = "_genitals") %>%
      str_replace_all(pattern = "_bwd", replacement = "_backwards") %>%
      str_replace_all(pattern = "_fwd", replacement = "_forwards") %>%
      str_replace_all(pattern = "_dir", replacement = "_directed") %>%
      str_replace_all(pattern = "_pir", replacement = "_pirouette") %>%
      str_replace_all(pattern = "_sr", replacement = "_roulade") %>%
      str_replace_all(pattern = "_sosa", replacement = "_somersault") %>%
      str_replace_all(pattern = "Tch", replacement = "touch") %>%
      str_replace_all(pattern = "_lg", replacement = "_long") %>%
      str_replace_all(pattern = "Wtr_", replacement = "Water") %>%
      str_replace_all(pattern = "Pot_new", replacement = "potentially_new") %>%
      str_replace_all(pattern = "_dtch", replacement = "_unattached") %>%
      str_replace_all(pattern = "Grab_pull", replacement = "Pull") %>%
      str_replace_all(pattern = "Unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Gesture_record

    data$Rcp_G %>%
      str_replace_all(pattern = "Tap_rcp", replacement = "Hit_rcp_sft") %>%
      str_replace_all(pattern = "Tapp_rcp", replacement = "Hitt_rcp_sft") %>%
      str_replace_all(pattern = "Tapp_obj", replacement = "Hitt_objgd_sft") %>%
      str_replace_all(pattern = "Time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "Bg_ld_scr", replacement = "big_loud_scratch") %>%
      str_replace_all(pattern = "_ks", replacement = "_kiss") %>%
      str_replace_all(pattern = "_thr", replacement = "_threat") %>%
      str_replace_all(pattern = "_obj$", replacement = "_object") %>%
      str_replace_all(pattern = "_obj_", replacement = "_object_") %>%
      str_replace_all(pattern = "_slf", replacement = "_self") %>%
      str_replace_all(pattern = "ChBt", replacement = "chest_beat") %>%
      str_replace_all(pattern = "_inf_", replacement = "_informal_") %>%
      str_replace_all(pattern = "_oth", replacement = "_other") %>%
      str_replace_all(pattern = "_std", replacement = "_standing") %>%
      str_replace_all(pattern = "_shk", replacement = "_shake") %>%
      str_replace_all(pattern = "_slf", replacement = "_self") %>%
      str_replace_all(pattern = "_mth", replacement = "_mouth") %>%
      str_replace_all(pattern = "_objgd", replacement = "_object") %>%
      str_replace_all(pattern = "_rcp", replacement = "_other") %>%
      str_replace_all(pattern = "Hitt_", replacement = "hitting_") %>%
      str_replace_all(pattern = "_clp", replacement = "_clap") %>%
      str_replace_all(pattern = "_nonrcp", replacement = "_bystander") %>%
      str_replace_all(pattern = "_sft", replacement = "_soft") %>%
      str_replace_all(pattern = "Lf_", replacement = "leaf_") %>%
      str_replace_all(pattern = "^Loc", replacement = "Locomote") %>%
      str_replace_all(pattern = "_bip", replacement = "_bipedal") %>%
      str_replace_all(pattern = "_gal", replacement = "_gallop") %>%
      str_replace_all(pattern = "^Obj_", replacement = "Object_") %>%
      str_replace_all(pattern = "_mth", replacement = "_mouth") %>%
      str_replace_all(pattern = "_att$", replacement = "_attached") %>%
      str_replace_all(pattern = "_unatt", replacement = "_unattached") %>%
      str_replace_all(pattern = "_mv", replacement = "_move") %>%
      str_replace_all(pattern = "Prs", replacement = "present") %>%
      str_replace_all(pattern = "Stmp_", replacement = "stomp_") %>%
      str_replace_all(pattern = "Stmpg_", replacement = "stomping_") %>%
      str_replace_all(pattern = "Tapp_", replacement = "tapping_") %>%
      str_replace_all(pattern = "_gn", replacement = "_genitals") %>%
      str_replace_all(pattern = "_bwd", replacement = "_backwards") %>%
      str_replace_all(pattern = "_fwd", replacement = "_forwards") %>%
      str_replace_all(pattern = "_dir", replacement = "_directed") %>%
      str_replace_all(pattern = "_pir", replacement = "_pirouette") %>%
      str_replace_all(pattern = "_sr", replacement = "_roulade") %>%
      str_replace_all(pattern = "_sosa", replacement = "_somersault") %>%
      str_replace_all(pattern = "Tch", replacement = "touch") %>%
      str_replace_all(pattern = "_lg", replacement = "_long") %>%
      str_replace_all(pattern = "Wtr_", replacement = "Water") %>%
      str_replace_all(pattern = "Pot_new", replacement = "potentially_new") %>%
      str_replace_all(pattern = "Unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_G

    data$G_rcp %>%
      str_replace_all(pattern = "Time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "Bg_ld_scr", replacement = "big_loud_scratch") %>%
      str_replace_all(pattern = "_ks", replacement = "_kiss") %>%
      str_replace_all(pattern = "_thr", replacement = "_threat") %>%
      str_replace_all(pattern = "_obj$", replacement = "_object") %>%
      str_replace_all(pattern = "_obj_", replacement = "_object_") %>%
      str_replace_all(pattern = "_slf", replacement = "_self") %>%
      str_replace_all(pattern = "ChBt", replacement = "chest_beat") %>%
      str_replace_all(pattern = "_inf_", replacement = "_informal_") %>%
      str_replace_all(pattern = "_oth", replacement = "_other") %>%
      str_replace_all(pattern = "_std", replacement = "_standing") %>%
      str_replace_all(pattern = "_shk", replacement = "_shake") %>%
      str_replace_all(pattern = "_slf", replacement = "_self") %>%
      str_replace_all(pattern = "_mth", replacement = "_mouth") %>%
      str_replace_all(pattern = "_objgd", replacement = "_object") %>%
      str_replace_all(pattern = "_rcp", replacement = "_other") %>%
      str_replace_all(pattern = "Hitt_", replacement = "hitting_") %>%
      str_replace_all(pattern = "_clp", replacement = "_clap") %>%
      str_replace_all(pattern = "_nonrcp", replacement = "_bystander") %>%
      str_replace_all(pattern = "_sft", replacement = "_soft") %>%
      str_replace_all(pattern = "Lf_", replacement = "leaf_") %>%
      str_replace_all(pattern = "^Loc", replacement = "Locomote") %>%
      str_replace_all(pattern = "_bip", replacement = "_bipedal") %>%
      str_replace_all(pattern = "_gal", replacement = "_gallop") %>%
      str_replace_all(pattern = "^Obj_", replacement = "Object_") %>%
      str_replace_all(pattern = "_mth", replacement = "_mouth") %>%
      str_replace_all(pattern = "_att$", replacement = "_attached") %>%
      str_replace_all(pattern = "_unatt", replacement = "_unattached") %>%
      str_replace_all(pattern = "_mv", replacement = "_move") %>%
      str_replace_all(pattern = "Prs", replacement = "present") %>%
      str_replace_all(pattern = "Stmp_", replacement = "stomp_") %>%
      str_replace_all(pattern = "Stmpg_", replacement = "stomping_") %>%
      str_replace_all(pattern = "Tapp_", replacement = "tapping_") %>%
      str_replace_all(pattern = "_gn", replacement = "_genitals") %>%
      str_replace_all(pattern = "_bwd", replacement = "_backwards") %>%
      str_replace_all(pattern = "_fwd", replacement = "_forwards") %>%
      str_replace_all(pattern = "_dir", replacement = "_directed") %>%
      str_replace_all(pattern = "_pir", replacement = "_pirouette") %>%
      str_replace_all(pattern = "_sr", replacement = "_roulade") %>%
      str_replace_all(pattern = "_sosa", replacement = "_somersault") %>%
      str_replace_all(pattern = "Tch", replacement = "touch") %>%
      str_replace_all(pattern = "_lg", replacement = "_long") %>%
      str_replace_all(pattern = "Wtr_", replacement = "Water") %>%
      str_replace_all(pattern = "Pot_new", replacement = "potentially_new") %>%
      str_replace_all(pattern = "Unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$G_rcp

    ## Body Parts --------------------------------------------------------------

    data$Body_sgn %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_tht", replacement = "_throat") %>%
      str_replace_all(pattern = "_hd", replacement = "_head") %>%
      str_replace_all(pattern = "_nck", replacement = "_neck") %>%
      str_replace_all(pattern = "_shd", replacement = "_shoulder") %>%
      str_replace_all(pattern = "_ch", replacement = "_chest") %>%
      str_replace_all(pattern = "_fr", replacement = "_front") %>%
      str_replace_all(pattern = "_gnsk", replacement = "_genitalskin") %>%
      str_replace_all(pattern = "unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Body_sgn

    data$Body_con %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_tht", replacement = "_throat") %>%
      str_replace_all(pattern = "_hd", replacement = "_head") %>%
      str_replace_all(pattern = "_nck", replacement = "_neck") %>%
      str_replace_all(pattern = "_shd", replacement = "_shoulder") %>%
      str_replace_all(pattern = "_ch", replacement = "_chest") %>%
      str_replace_all(pattern = "_fr", replacement = "_front") %>%
      str_replace_all(pattern = "_gnsk", replacement = "_genitalskin") %>%
      str_replace_all(pattern = "unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Body_con

    ## Flexion -----------------------------------------------------------------

    data$Flexion %>%
      str_replace_all(pattern = "Fg", replacement = "Finger") %>%
      str_replace_all(pattern = "Wr", replacement = "Wrist") %>%
      str_replace_all(pattern = "El", replacement = "Elbow") %>%
      str_replace_all(pattern = "Unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Flexion

    ## Orientation -------------------------------------------------------------

    data$Orientation %>%
      str_replace_all(pattern = "Unk", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Orientation

    ## Repetition Count --------------------------------------------------------

    data$Rep_count %>%
      str_replace_all(pattern = "NV", replacement = "0") %>%
      str_replace_all(pattern = "nv", replacement = "0") %>%
      str_replace_all(pattern = "No_value", replacement = "0") %>%
      str_replace_all(pattern = "[+]", replacement = "") %>%
      str_replace_all(pattern = "Unk", replacement = "Unknown") ->
      data$Rep_count


    ## Object Use --------------------------------------------------------------

    data$with_Object %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_vg", replacement = "_vegetation") %>%
      str_replace_all(pattern = "_sw", replacement = "_swamp") %>%
      str_replace_all(pattern = "^no$", replacement = "none") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$with_Object

    data$Object_contacted %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_vg", replacement = "_vegetation") %>%
      str_replace_all(pattern = "_sw", replacement = "_swamp") %>%
      str_replace_all(pattern = "^no$", replacement = "none") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Object_contacted

    ## Laterality --------------------------------------------------------------

    data$Lat_bim %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^e$", replacement = "equally") %>%
      str_replace_all(pattern = "^r$", replacement = "right") %>%
      str_replace_all(pattern = "^l$", replacement = "left") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Lat_bim

    data$Sgn_Lat %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^b$", replacement = "both") %>%
      str_replace_all(pattern = "^ba$", replacement = "alternating") %>%
      str_replace_all(pattern = "^r$", replacement = "right") %>%
      str_replace_all(pattern = "^l$", replacement = "left") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Sgn_Lat

    data$Con_Lat %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^b$", replacement = "both") %>%
      str_replace_all(pattern = "^ba$", replacement = "both_alternating") %>%
      str_replace_all(pattern = "^r$", replacement = "right") %>%
      str_replace_all(pattern = "^l$", replacement = "left") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Con_Lat

    ## Emphasis ----------------------------------------------------------------

    data$Emphasis %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Emphasis

    ## Context -----------------------------------------------------------------

    data$Rcp_prior_context %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_oe", replacement = "_swelling") %>%
      str_replace_all(pattern = "_mpoli", replacement = "_monopolizable") %>%
      str_replace_all(pattern = "_soc", replacement = "_social") %>%
      str_replace_all(pattern = "_sol", replacement = "_solitary") %>%
      str_replace_all(pattern = "_unk", replacement = "_unknown") %>%
      str_replace_all(pattern = "_con", replacement = "_contact") %>%
      str_replace_all(pattern = "_ff", replacement = "_focal") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_prior_context

    data$Rcp_post_context %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_oe", replacement = "_swelling") %>%
      str_replace_all(pattern = "_mpoli", replacement = "_monopolizable") %>%
      str_replace_all(pattern = "_soc", replacement = "_social") %>%
      str_replace_all(pattern = "_sol", replacement = "_solitary") %>%
      str_replace_all(pattern = "_unk", replacement = "_unknown") %>%
      str_replace_all(pattern = "_con", replacement = "_contact") %>%
      str_replace_all(pattern = "_ff", replacement = "_focal") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_post_context

    data$Sgn_prior_context %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_oe", replacement = "_swelling") %>%
      str_replace_all(pattern = "_mpoli", replacement = "_monopolizable") %>%
      str_replace_all(pattern = "_soc", replacement = "_social") %>%
      str_replace_all(pattern = "_sol", replacement = "_solitary") %>%
      str_replace_all(pattern = "_unk", replacement = "_unknown") %>%
      str_replace_all(pattern = "_con", replacement = "_contact") %>%
      str_replace_all(pattern = "_ff", replacement = "_focal") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Sgn_prior_context

    data$Sgn_post_context %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_oe", replacement = "_swelling") %>%
      str_replace_all(pattern = "_mpoli", replacement = "_monopolizable") %>%
      str_replace_all(pattern = "_soc", replacement = "_social") %>%
      str_replace_all(pattern = "_sol", replacement = "_solitary") %>%
      str_replace_all(pattern = "_unk", replacement = "_unknown") %>%
      str_replace_all(pattern = "_con", replacement = "_contact") %>%
      str_replace_all(pattern = "_ff", replacement = "_focal") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Sgn_post_context

    ## Response Waiting --------------------------------------------------------

    data$Response_waiting %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Response_waiting

    ## Effectiveness -----------------------------------------------------------

    data$Effectiveness %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_cng", replacement = "_congruent") %>%
      str_replace_all(pattern = "_mech", replacement = "_mechanical") %>%
      str_replace_all(pattern = "_comp", replacement = "_component") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Effectiveness


    ## Persistence -------------------------------------------------------------

    data$Persistence %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Persistence


    ## Sequence Information ----------------------------------------------------

    data$Sequence_part %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "_") %>%
      str_replace_all(pattern = "Not_", replacement = "Not") ->
      data$Sequence_part

    data$Bout_part %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "_") %>%
      str_replace_all(pattern = "Not_", replacement = "Not") ->
      data$Bout_part

    data$Exchange_part %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "_") %>%
      str_replace_all(pattern = "Not_", replacement = "Not") ->
      data$Exchange_part

    ## Distance ----------------------------------------------------------------

    data$Distance_rcp %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "_") %>%
      str_replace_all(pattern = "Not_", replacement = "Not") ->
      data$Distance_rcp

    ## Gaze --------------------------------------------------------------------

    data$Gaze_before %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Gaze_before

    data$Gaze_during %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Gaze_during

    ## Audible -----------------------------------------------------------------

    data$Audible %>%
      str_to_lower() %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Audible

    ## Facial Expressions ------------------------------------------------------

    data$Sgn_FE %>%
      str_replace_all(pattern = "_undef", replacement = "_undefined") %>%
      str_replace_all(pattern = "BdTh_dsp", replacement = "bared_teeth_display") %>%
      str_replace_all(pattern = "Ph_face", replacement = "pant_hoot_face") %>%
      str_replace_all(pattern = "AmbF_dsp", replacement = "ambiguous") %>%
      str_replace_all(pattern = "Time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "_cont", replacement = "_continued") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Sgn_FE

    data$Rcp_FE %>%
      str_replace_all(pattern = "_undef", replacement = "_undefined") %>%
      str_replace_all(pattern = "BdTh_dsp", replacement = "bared_teeth_display") %>%
      str_replace_all(pattern = "Ph_face", replacement = "pant_hoot_face") %>%
      str_replace_all(pattern = "AmbF_dsp", replacement = "ambiguous") %>%
      str_replace_all(pattern = "Time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "_cont", replacement = "_continued") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_FE

    ## Vocalisations -----------------------------------------------------------

    data$Sgn_Voc %>%
      str_replace_all(pattern = "_undef", replacement = "_undefined") %>%
      str_replace_all(pattern = "Voc_", replacement = "vocalisation_") %>%
      str_replace_all(pattern = "Gt_", replacement = "grunt_") %>%
      str_replace_all(pattern = "_gt", replacement = "_grunt") %>%
      str_replace_all(pattern = "_sg", replacement = "_single") %>%
      str_replace_all(pattern = "Time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "_cont", replacement = "_continued") %>%
      str_replace_all(pattern = "_db", replacement = "_double") %>%
      str_replace_all(pattern = "_cop", replacement = "_copulation") %>%
      str_replace_all(pattern = "Gmb", replacement = "grumble") %>%
      str_replace_all(pattern = "Wh_", replacement = "whinny_") %>%
      str_replace_all(pattern = "Ht_", replacement = "hoot_") %>%
      str_replace_all(pattern = "_ht", replacement = "_hoot") %>%
      str_replace_all(pattern = "_ChBt", replacement = "_chest_beat") %>%
      str_replace_all(pattern = "Scrm_", replacement = "scream_") %>%
      str_replace_all(pattern = "_vic", replacement = "_victim") %>%
      str_replace_all(pattern = "Pnt", replacement = "pant") %>%
      str_replace_all(pattern = "_str", replacement = "_start") %>%
      str_replace_all(pattern = "_chk", replacement = "_choke") %>%
      str_replace_all(pattern = "_agg", replacement = "_aggression") %>%
      str_replace_all(pattern = "Brk", replacement = "bark") %>%
      str_replace_all(pattern = "_trv", replacement = "_travel") %>%
      str_replace_all(pattern = "_wor", replacement = "_worried") %>%
      str_replace_all(pattern = "_con$", replacement = "_contact") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Sgn_Voc

    data$Rcp_Voc %>%
      str_replace_all(pattern = "_undef", replacement = "_undefined") %>%
      str_replace_all(pattern = "Voc_", replacement = "vocalisation_") %>%
      str_replace_all(pattern = "Gt_", replacement = "grunt_") %>%
      str_replace_all(pattern = "_gt", replacement = "_grunt") %>%
      str_replace_all(pattern = "_sg", replacement = "_single") %>%
      str_replace_all(pattern = "Time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "_cont", replacement = "_continued") %>%
      str_replace_all(pattern = "_db", replacement = "_double") %>%
      str_replace_all(pattern = "_cop", replacement = "_copulation") %>%
      str_replace_all(pattern = "Gmb", replacement = "grumble") %>%
      str_replace_all(pattern = "Wh_", replacement = "whinny_") %>%
      str_replace_all(pattern = "Ht_", replacement = "hoot_") %>%
      str_replace_all(pattern = "_ht", replacement = "_hoot") %>%
      str_replace_all(pattern = "_ChBt", replacement = "_chest_beat") %>%
      str_replace_all(pattern = "Scrm_", replacement = "scream_") %>%
      str_replace_all(pattern = "_vic", replacement = "_victim") %>%
      str_replace_all(pattern = "Pnt", replacement = "pant") %>%
      str_replace_all(pattern = "_str", replacement = "_start") %>%
      str_replace_all(pattern = "_chk", replacement = "_choke") %>%
      str_replace_all(pattern = "_agg", replacement = "_aggression") %>%
      str_replace_all(pattern = "Brk", replacement = "bark") %>%
      str_replace_all(pattern = "_trv", replacement = "_travel") %>%
      str_replace_all(pattern = "_wor", replacement = "_worried") %>%
      str_replace_all(pattern = "_con$", replacement = "_contact") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_Voc

    data$Voc_rcp %>%
      str_replace_all(pattern = "_undef", replacement = "_undefined") %>%
      str_replace_all(pattern = "Voc_", replacement = "vocalisation_") %>%
      str_replace_all(pattern = "Gt_", replacement = "grunt_") %>%
      str_replace_all(pattern = "_gt", replacement = "_grunt") %>%
      str_replace_all(pattern = "_sg", replacement = "_single") %>%
      str_replace_all(pattern = "Time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "_cont", replacement = "_continued") %>%
      str_replace_all(pattern = "_db", replacement = "_double") %>%
      str_replace_all(pattern = "_cop", replacement = "_copulation") %>%
      str_replace_all(pattern = "Gmb", replacement = "grumble") %>%
      str_replace_all(pattern = "Wh_", replacement = "whinny_") %>%
      str_replace_all(pattern = "Ht_", replacement = "hoot_") %>%
      str_replace_all(pattern = "_ht", replacement = "_hoot") %>%
      str_replace_all(pattern = "_ChBt", replacement = "_chest_beat") %>%
      str_replace_all(pattern = "Scrm_", replacement = "scream_") %>%
      str_replace_all(pattern = "_vic", replacement = "_victim") %>%
      str_replace_all(pattern = "Pnt", replacement = "pant") %>%
      str_replace_all(pattern = "_str", replacement = "_start") %>%
      str_replace_all(pattern = "_chk", replacement = "_choke") %>%
      str_replace_all(pattern = "_agg", replacement = "_aggression") %>%
      str_replace_all(pattern = "Brk", replacement = "bark") %>%
      str_replace_all(pattern = "_trv", replacement = "_travel") %>%
      str_replace_all(pattern = "_wor", replacement = "_worried") %>%
      str_replace_all(pattern = "_con$", replacement = "_contact") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Voc_rcp

    ## Visual Attention --------------------------------------------------------

    data$Rcp_VA %>%
      str_replace_all(pattern = "VA", replacement = "visual_attention") %>%
      str_replace_all(pattern = "poss", replacement = "possible") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_VA

    ## Location ----------------------------------------------------------------

    data$Rcp_location %>%
      str_replace_all(pattern = "Grd", replacement = "ground") %>%
      str_replace_all(pattern = "vg", replacement = "vegetation") %>%
      str_replace_all(pattern = "sw", replacement = "swamp") %>%
      str_replace_all(pattern = "Tr", replacement = "tree") %>%
      str_replace_all(pattern = "Ind", replacement = "individual") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_location

    data$Sgn_location %>%
      str_replace_all(pattern = "Grd", replacement = "ground") %>%
      str_replace_all(pattern = "vg", replacement = "vegetation") %>%
      str_replace_all(pattern = "sw", replacement = "swamp") %>%
      str_replace_all(pattern = "Tr", replacement = "tree") %>%
      str_replace_all(pattern = "Ind", replacement = "individual") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Sgn_location

    ## Visibility --------------------------------------------------------------

    data$Vis_during %>%
      str_replace_all(pattern = "_bth", replacement = "_both") %>%
      str_replace_all(pattern = "_sgn", replacement = "_signaller") %>%
      str_replace_all(pattern = "_rcp", replacement = "_recipient") %>%
      str_replace_all(pattern = "^Unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Vis_during

    ## Behaviour Change --------------------------------------------------------

    data$Outcome %>%
      str_to_lower() %>%
      str_replace_all(pattern = "_amb", replacement = "_ambiguous") %>%
      str_replace_all(pattern = "sgn", replacement = "signaller") %>%
      str_replace_all(pattern = "rcp", replacement = "recipient") %>%
      str_replace_all(pattern = "_in", replacement = "_include") %>%
      str_replace_all(pattern = "time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "_ex$", replacement = "_exclude") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Outcome


    # Recipient Reaction ------------------------------------------------------

    data$Rcp_reaction %>%
      str_to_lower() %>%
      str_replace_all(pattern = "gv_objfd", replacement = "give_me") %>%
      str_replace_all(pattern = "gv_", replacement = "give_") %>%
      str_replace_all(pattern = "_ns", replacement = "_nurse") %>%
      str_replace_all(pattern = "tk_objfd", replacement = "take_object") %>%
      str_replace_all(pattern = "tk_", replacement = "take_") %>%
      str_replace_all(pattern = "aff_", replacement = "affilation_") %>%
      str_replace_all(pattern = "_unc", replacement = "_unclear") %>%
      str_replace_all(pattern = "_con", replacement = "_contact") %>%
      str_replace_all(pattern = "_dis", replacement = "_distant") %>%
      str_replace_all(pattern = "^att_", replacement = "attend_") %>%
      str_replace_all(pattern = "_sgn", replacement = "_me") %>%
      str_replace_all(pattern = "_rcp", replacement = "_you") %>%
      str_replace_all(pattern = "dir_", replacement = "direct_") %>%
      str_replace_all(pattern = "_att", replacement = "_attention") %>%
      str_replace_all(pattern = "^cl_", replacement = "climb_") %>%
      str_replace_all(pattern = "_cl$", replacement = "_climb") %>%
      str_replace_all(pattern = "_cl_", replacement = "_climb_") %>%
      str_replace_all(pattern = "fw_", replacement = "follow_") %>%
      str_replace_all(pattern = "grm", replacement = "groom") %>%
      str_replace_all(pattern = "aw_", replacement = "allow_") %>%
      str_replace_all(pattern = "sex", replacement = "sexual") %>%
      str_replace_all(pattern = "pl_", replacement = "play_") %>%
      str_replace_all(pattern = "_st$", replacement = "_start") %>%
      str_replace_all(pattern = "_co$", replacement = "_continue") %>%
      str_replace_all(pattern = "_ch", replacement = "_change") %>%
      str_replace_all(pattern = "_concha", replacement = "_contact_chase") %>%
      str_replace_all(pattern = "_chacon", replacement = "_chase_contact") %>%
      str_replace_all(pattern = "trv$", replacement = "travel") %>%
      str_replace_all(pattern = "trv_", replacement = "travel_with_") %>%
      str_replace_all(pattern = "stp_", replacement = "stop_") %>%
      str_replace_all(pattern = "_bh", replacement = "_behaviour") %>%
      str_replace_all(pattern = "spp", replacement = "support") %>%
      str_replace_all(pattern = "time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "dlb_ign", replacement = "deliberately_ignores") %>%
      str_replace_all(pattern = "dsp_", replacement = "display") %>%
      str_replace_all(pattern = "_dm", replacement = "_dominance") %>%
      str_replace_all(pattern = "_3p", replacement = "_bystander") %>%
      str_replace_all(pattern = "^agg", replacement = "aggression") %>%
      str_replace_all(pattern = "_agg", replacement = "_aggression") %>%
      str_replace_all(pattern = "_mv", replacement = "_move") %>%
      str_replace_all(pattern = "_unk", replacement = "_unknown") %>%
      str_replace_all(pattern = "_oe", replacement = "_swelling") %>%
      str_replace_all(pattern = "_mpoli", replacement = "_monopolizable") %>%
      str_replace_all(pattern = "_soc$", replacement = "_social") %>%
      str_replace_all(pattern = "_soc_", replacement = "_social_") %>%
      str_replace_all(pattern = "_sol", replacement = "_solitary") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Rcp_reaction


    ## Latency -----------------------------------------------------------------

    data$BhvCh1 %>%
      str_to_lower() %>%
      str_replace_all(pattern = "gv_objfd", replacement = "give_me") %>%
      str_replace_all(pattern = "gv_", replacement = "give_") %>%
      str_replace_all(pattern = "_ns", replacement = "_nurse") %>%
      str_replace_all(pattern = "tk_objfd", replacement = "take_object") %>%
      str_replace_all(pattern = "tk_", replacement = "take_") %>%
      str_replace_all(pattern = "aff_", replacement = "affilation_") %>%
      str_replace_all(pattern = "_unc", replacement = "_unclear") %>%
      str_replace_all(pattern = "_con", replacement = "_contact") %>%
      str_replace_all(pattern = "_dis", replacement = "_distant") %>%
      str_replace_all(pattern = "^att_", replacement = "attend_") %>%
      str_replace_all(pattern = "_sgn", replacement = "_me") %>%
      str_replace_all(pattern = "_rcp", replacement = "_you") %>%
      str_replace_all(pattern = "dir_", replacement = "direct_") %>%
      str_replace_all(pattern = "_att", replacement = "_attention") %>%
      str_replace_all(pattern = "^cl_", replacement = "climb_") %>%
      str_replace_all(pattern = "_cl$", replacement = "_climb") %>%
      str_replace_all(pattern = "_cl_", replacement = "_climb_") %>%
      str_replace_all(pattern = "fw_", replacement = "follow_") %>%
      str_replace_all(pattern = "grm", replacement = "groom") %>%
      str_replace_all(pattern = "aw_", replacement = "allow_") %>%
      str_replace_all(pattern = "sex", replacement = "sexual") %>%
      str_replace_all(pattern = "pl_", replacement = "play_") %>%
      str_replace_all(pattern = "_st$", replacement = "_start") %>%
      str_replace_all(pattern = "_co$", replacement = "_continue") %>%
      str_replace_all(pattern = "_ch", replacement = "_change") %>%
      str_replace_all(pattern = "_concha", replacement = "_contact_chase") %>%
      str_replace_all(pattern = "_chacon", replacement = "_chase_contact") %>%
      str_replace_all(pattern = "trv$", replacement = "travel") %>%
      str_replace_all(pattern = "trv_", replacement = "travel_with_") %>%
      str_replace_all(pattern = "stp_", replacement = "stop_") %>%
      str_replace_all(pattern = "_bh", replacement = "_behaviour") %>%
      str_replace_all(pattern = "spp", replacement = "support") %>%
      str_replace_all(pattern = "time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "dlb_ign", replacement = "deliberately_ignores") %>%
      str_replace_all(pattern = "dsp_", replacement = "display") %>%
      str_replace_all(pattern = "_dm", replacement = "_dominance") %>%
      str_replace_all(pattern = "_3p", replacement = "_bystander") %>%
      str_replace_all(pattern = "^agg", replacement = "aggression") %>%
      str_replace_all(pattern = "_agg", replacement = "_aggression") %>%
      str_replace_all(pattern = "_mv", replacement = "_move") %>%
      str_replace_all(pattern = "_unk", replacement = "_unknown") %>%
      str_replace_all(pattern = "_oe", replacement = "_swelling") %>%
      str_replace_all(pattern = "_mpoli", replacement = "_monopolizable") %>%
      str_replace_all(pattern = "_soc$", replacement = "_social") %>%
      str_replace_all(pattern = "_soc_", replacement = "_social_") %>%
      str_replace_all(pattern = "_sol", replacement = "_solitary") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$BhvCh1

    data$BhvCh2 %>%
      str_to_lower() %>%
      str_replace_all(pattern = "gv_objfd", replacement = "give_me") %>%
      str_replace_all(pattern = "gv_", replacement = "give_") %>%
      str_replace_all(pattern = "_ns", replacement = "_nurse") %>%
      str_replace_all(pattern = "tk_objfd", replacement = "take_object") %>%
      str_replace_all(pattern = "tk_", replacement = "take_") %>%
      str_replace_all(pattern = "aff_", replacement = "affilation_") %>%
      str_replace_all(pattern = "_unc", replacement = "_unclear") %>%
      str_replace_all(pattern = "_con", replacement = "_contact") %>%
      str_replace_all(pattern = "_dis", replacement = "_distant") %>%
      str_replace_all(pattern = "^att_", replacement = "attend_") %>%
      str_replace_all(pattern = "_sgn", replacement = "_me") %>%
      str_replace_all(pattern = "_rcp", replacement = "_you") %>%
      str_replace_all(pattern = "dir_", replacement = "direct_") %>%
      str_replace_all(pattern = "_att", replacement = "_attention") %>%
      str_replace_all(pattern = "^cl_", replacement = "climb_") %>%
      str_replace_all(pattern = "_cl$", replacement = "_climb") %>%
      str_replace_all(pattern = "_cl_", replacement = "_climb_") %>%
      str_replace_all(pattern = "fw_", replacement = "follow_") %>%
      str_replace_all(pattern = "grm", replacement = "groom") %>%
      str_replace_all(pattern = "aw_", replacement = "allow_") %>%
      str_replace_all(pattern = "sex", replacement = "sexual") %>%
      str_replace_all(pattern = "pl_", replacement = "play_") %>%
      str_replace_all(pattern = "_st$", replacement = "_start") %>%
      str_replace_all(pattern = "_co$", replacement = "_continue") %>%
      str_replace_all(pattern = "_ch", replacement = "_change") %>%
      str_replace_all(pattern = "_concha", replacement = "_contact_chase") %>%
      str_replace_all(pattern = "_chacon", replacement = "_chase_contact") %>%
      str_replace_all(pattern = "trv$", replacement = "travel") %>%
      str_replace_all(pattern = "trv_", replacement = "travel_with_") %>%
      str_replace_all(pattern = "stp_", replacement = "stop_") %>%
      str_replace_all(pattern = "_bh", replacement = "_behaviour") %>%
      str_replace_all(pattern = "spp", replacement = "support") %>%
      str_replace_all(pattern = "time_ex", replacement = "time_exclude") %>%
      str_replace_all(pattern = "dlb_ign", replacement = "deliberately_ignores") %>%
      str_replace_all(pattern = "dsp_", replacement = "display") %>%
      str_replace_all(pattern = "_dm", replacement = "_dominance") %>%
      str_replace_all(pattern = "_3p", replacement = "_bystander") %>%
      str_replace_all(pattern = "^agg", replacement = "aggression") %>%
      str_replace_all(pattern = "_agg", replacement = "_aggression") %>%
      str_replace_all(pattern = "_mv", replacement = "_move") %>%
      str_replace_all(pattern = "_unk", replacement = "_unknown") %>%
      str_replace_all(pattern = "_oe", replacement = "_swelling") %>%
      str_replace_all(pattern = "_mpoli", replacement = "_monopolizable") %>%
      str_replace_all(pattern = "_soc$", replacement = "_social") %>%
      str_replace_all(pattern = "_soc_", replacement = "_social_") %>%
      str_replace_all(pattern = "_sol", replacement = "_solitary") %>%
      str_replace_all(pattern = "^unk$", replacement = "unknown") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$BhvCh2

    ## Other -------------------------------------------------------------------
    data$Dur_ana %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Dur_ana

    data$Cod_status %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() %>%
      str_replace_all(pattern = " ", replacement = "") ->
      data$Cod_status

    ## Numeric Variables -------------------------------------------------------

    data %>% mutate_at(
      c(
        "Voc_rcp_T",
        "G_start_T",
        # "Bout_start_T",
        # "Bout_end_T",
        "Outcome_T",
        "BhvCh1_T",
        "BhvCh2_T",
        "MAU_duration",
        "MAU_end_T",
        "G_duration",
        "G_end_T",
        "G_rcp_T",
        #"GA_duration",
        "GA_end_T"
        # "Sgn_age",
        # "Rcp_age"
      ),
      as.numeric
    ) -> data
  }

  # Rename columns ----------------------------------------------------------
  old.names <- colnames(data)
  if (clean.col.names) {
    data %>%
      rename(
        "Recording_number" = .data$Rec_number,
        "Communication_number" = .data$Com_number,
        # "Signaller_age" = .data$Sgn_age,
        "Signaller_arousal" = .data$Sgn_arousal,
        # "Signaller_rank" = .data$Sgn_rank,
        # "Signaller_sex" = .data$Sgn_sex,
        "Signaller_facial_expression" = .data$Sgn_FE,
        "Signaller_vocalisation" = .data$Sgn_Voc,
        "Signaller_location" = .data$Sgn_location,
        # "Recipient_age" = .data$Rcp_age,
        "Recipient_arousal" = .data$Rcp_arousal,
        # "Recipient_rank" = .data$Rcp_rank,
        # "Recipient_sex" = .data$Rcp_sex,
        "Recipient_distance" = .data$Distance_rcp,
        "Recipient_facial_expression" = .data$Rcp_FE,
        "Recipient_gesture" = .data$Rcp_G,
        "Recipient_visusal_attention" = .data$Rcp_VA,
        "Recipient_location" = .data$Rcp_location,
        "Recipient_reaction" = .data$Rcp_reaction,
        "Recipient_vocalisation_sequence" = .data$Rcp_Voc,
        "Vocalisation_recipient" = .data$Voc_rcp,
        "Vocalisation_recipient_time" = .data$Voc_rcp_T,
        "Directed_towards" = .data$Directed_to,
        "Directionality_gesture" = .data$Direct_G,
        "Body_part_signaller" = .data$Body_sgn,
        "Body_part_contact" = .data$Body_con,
        "Repetition_count" = .data$Rep_count,
        "Object_used" = .data$with_Object,
        "Object_contacted" = .data$Object_contacted,
        "Laterality_bimanual" = .data$Lat_bim,
        "Laterality_signaller" = .data$Sgn_Lat,
        "Laterality_contact" = .data$Con_Lat,
        "Context_prior_recipient" = .data$Rcp_prior_context,
        "Context_post_recipient" = .data$Rcp_post_context,
        "Context_prior_signaller" = .data$Sgn_prior_context,
        "Context_post_signaller" = .data$Sgn_post_context,
        "Gesture_start_time" = .data$G_start_T,
        "Gesture_end_time" = .data$G_end_T,
        "Gesture_duration" = .data$G_duration,
        "Part_sequence" = .data$Sequence_part,
        "Part_bout" = .data$Bout_part,
        "Part_exchange" = .data$Exchange_part,
        # "Bout_start_time" = .data$Bout_start_T,
        # "Bout_end_time" = .data$Bout_end_T,
        "Visibility_during" = .data$Vis_during,
        "Duration_analysis_include" = .data$Dur_ana,
        "Outcome" = .data$Outcome,
        "Outcome_time" = .data$Outcome_T,
        "Behaviour_change1" = .data$BhvCh1,
        "Behaviour_change2" = .data$BhvCh2,
        "Behaviour_change1_time" = .data$BhvCh1_T,
        "Behaviour_change2_time" = .data$BhvCh2_T,
        "MAU_end_time" = .data$MAU_end_T,
        "Gesture_recipient" = .data$G_rcp,
        "Gesture_recipient_time" = .data$G_rcp_T,
        # "Gesture_action_duration" = .data$GA_duration,
        "Gesture_action_end_time" = .data$GA_end_T,
        "Coding_status" = .data$Cod_status,
        "Comment_recording" = .data$Comment_rec,
        "Comment_communication" = .data$Comment_com
      ) -> data

    colnames(data) <- colnames(data) %>%
      str_to_title()
  }

  return(data)
}
