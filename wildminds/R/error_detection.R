#' Function takes the gesture definition file as produced by Kirsty with everybody's input, and detects cases where those definitions for possible modifiers are violated
#'
#' Potential errors here only concern modifiers that are not defined in the protocols but occur. These should be reported back to the coder, to be changed in ELAN and FileMaker
#'
#' @param data data frame as extracted from Filemaker
#' @param col_names defines whether the 'original' names were used (i.e., as the come out of Filemaker) or it was 'renamed' using other functions I wrote
#'
#' @return Function returns data frame with potential errors and their description, including coder, communication number, and a description of what should be checked
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all across
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom utils read.csv
#'
#' @export
#'

error_detection <- function(data, col_names = "renamed") {
  # if col_names is set to 'original', additional columns need to be added that represent the renamed columns I tend to use
  if (col_names == "original") {
    data <- clean_function(
      data = data,
      clean.value.levels = TRUE,
      clean.col.names = TRUE
    ) %>%
      reduce_levels(reduce.nas = TRUE,
                    reduce.body.part.signaller = TRUE,
                    reduce.body.part.contact = TRUE) %>%
      suppressMessages()
  }

  data <- data %>%
    mutate(row.number = row_number())

  data$Coder <-
    info_from_Com_number(data$Communication_number, output = "coder")

  # Read definitions --------------------------------------------------------

  gesture.definitions <-
    gesture_definitions


  # Exclude unnecessary info ------------------------------------------------

  gesture.definitions <- gesture.definitions %>%
    select(
      -.data$Definition
    )


  # Clean gesture actions ---------------------------------------------------

  gesture.definitions$Gesture_action <-
    gesture.definitions$Gesture_action %>%
    str_replace_all(pattern = ":", replacement = " ") %>%
    str_replace_all(pattern = "_", replacement = " ") %>%
    str_replace_all(pattern = "\\(", replacement = " ") %>%
    str_replace_all(pattern = "\\)", replacement = " ") %>%
    str_replace_all(pattern = "\\/", replacement = " ") %>%
    str_replace_all(pattern = "  ", replacement = " ") %>%
    str_to_title() %>%
    str_replace_all(pattern = "With", replacement = "") %>%
    str_replace_all(pattern = "  ", replacement = " ") %>%
    str_replace_all(pattern = "Object Ground", replacement = "Object") %>%
    str_replace_all(pattern = "Ground Object", replacement = "Object") %>%
    str_replace_all(pattern = "Bipedal", replacement = "Standing") %>%
    str_replace_all(pattern = "Clap On", replacement = "Clap") %>%
    str_replace_all(pattern = "Clipping", replacement = "Clip") %>%
    str_replace_all(pattern = "Leaf Drop", replacement = "Drop") %>%
    str_replace_all(pattern = "Soft Other", replacement = "Other Soft") %>%
    str_replace_all(pattern = "Soft Self", replacement = "Self Soft") %>%
    str_replace_all(pattern = "Soft Object", replacement = "Object Soft") %>%
    str_replace_all(pattern = "Recipient", replacement = "Other") %>%
    str_replace_all(pattern = " S ", replacement = " ") %>%
    str_replace_all(pattern = "Rocking Sitting", replacement = "Rocking Sit") %>%
    str_replace_all(pattern = "Rocking Standing", replacement = "Rocking Bipedal") %>%
    str_replace_all(pattern = "Locomote Standing", replacement = "Locomote Bipedal") %>%
    str_replace_all(pattern = "Stiff Walk", replacement = "Stiff") %>%
    str_replace_all(pattern = "In Mouth", replacement = "Mouth") %>%
    str_replace_all(pattern = "Detached", replacement = "Unattached") %>%
    str_replace_all(pattern = "Stance Standing", replacement = "Stance Bipedal") %>%
    str_replace_all(pattern = "Hitting Object Other", replacement = "Hitting Other Object") %>%
    str_replace_all(pattern = "Hitting Soft Non Other", replacement = "Hitting Bystander Soft") %>%
    str_replace_all(pattern = "Hitting Non Other", replacement = "Hitting Bystander") %>%
    str_replace_all(pattern = "Hit Soft Other", replacement = "Hit Other Soft") %>%
    str_replace_all(pattern = "Hitting Soft Other", replacement = "Hitting Other Soft") %>%
    str_replace_all(pattern = "Spin Side Roulade", replacement = "Spin Roulade") %>%
    str_replace_all(pattern = "Water Splash", replacement = "Watersplash") %>%
    str_replace_all(pattern = "On Head", replacement = "Head") %>%
    str_replace_all(pattern = "Locomote Over", replacement = "Locomote Other") %>%
    str_replace_all(pattern = " ", replacement = "")


  # Repetition --------------------------------------------------------------

  gesture.definitions$Rep_count[gesture.definitions$Rep_count == "CODE"] <-
    "Any"
  gesture.definitions$Rep_count[gesture.definitions$Rep_count == "NV"] <-
    '0'


  # Object contact ----------------------------------------------------------

  gesture.definitions$Object_Contact[gesture.definitions$Object_Contact == "CODE"] <-
    "Value"
  gesture.definitions$Object_Contact[gesture.definitions$Object_Contact == "None"] <-
    'None'


  # With Object -------------------------------------------------------------

  gesture.definitions$With_object[gesture.definitions$With_object == "CODE (can be None)"] <-
    "Ambiguous"
  gesture.definitions$With_object[gesture.definitions$With_object == "CODE (usually None)"] <-
    "Ambiguous"
  gesture.definitions$With_object[gesture.definitions$With_object == "CODE"] <-
    "Value"
  gesture.definitions$With_object[gesture.definitions$With_object == "None"] <-
    'None'

  # Body Signaller ----------------------------------------------------------

  gesture.definitions$Body_sgn <- gesture.definitions$Body_sgn %>%
    str_replace_all(pattern = "rarely", replacement = "") %>%
    str_replace_all(pattern = "typically", replacement = "") %>%
    str_replace_all(pattern = "\\(", replacement = "") %>%
    str_replace_all(pattern = "\\)", replacement = "") %>%
    str_replace_all(pattern = "_tht", replacement = "_throat") %>%
    str_replace_all(pattern = "_hd", replacement = "_head") %>%
    str_replace_all(pattern = "_nck", replacement = "_neck") %>%
    str_replace_all(pattern = "_shd", replacement = "_shoulder") %>%
    str_replace_all(pattern = "_ch", replacement = "_chest") %>%
    str_replace_all(pattern = "_fr", replacement = "_front") %>%
    str_replace_all(pattern = "_gnsk", replacement = "_genitalskin")

  gesture.definitions$Body_sgn <-
    sapply(gesture.definitions$Body_sgn, function(x) {
      x <- ifelse(test = str_detect(x, "Any"),
                  yes = data$Body_part_signaller %>% 
                    unlist() %>% 
                    unique() %>% 
                    sort %>% 
                    paste(collapse = ', '),
                  no = x
      )
      xx <- str_split(x, pattern = ", ", simplify = TRUE)
      xx <- ifelse(
        test = str_detect(xx, "Head"),
        yes = "Head, HeadThroatNeck, HeadHead",
        no = xx
      )
      xx <- ifelse(
        test = str_detect(xx, "Face")|str_detect(xx, "Mouth"),
        yes = "Face, Mouth, Face_Mouth, FaceMouth, Mouth_Teeth, MouthTeeth",
        no = xx
      )
      xx <- ifelse(test = str_detect(xx, "Shoulder"),
                   yes = "Arm, BackShoulder, Shoulder, MouthTeeth",
                   no = xx
      )
      xx <- ifelse(
        test = str_detect(xx, "Genital") |
          str_detect(xx, "Penis") |
          str_detect(xx, "Testes") |
          str_detect(xx, "Swelling") |
          str_detect(xx, "Bottom"),
        yes = "Genital, Genitals, Penis, Testes, Swelling, Bottom, BottomGenitalskin",
        no = xx
      )
      xx <- ifelse(
        test = str_detect(xx, "Hand"),
        yes = "Hand, HandFoot, HandWrist",
        no = xx
      )
      xx <- ifelse(
        test = str_detect(xx, "Fingers"),
        yes = "Fingers",
        no = xx
      )
      xx <- ifelse(
        test = str_detect(xx, "Knuckles")|
          str_detect(xx, "Fist"),
        yes = "Fist, Knuckles",
        no = xx
      )
      xx <- unique(xx %>% str_split(', ') %>% unlist())
      return(str_c(xx, collapse = ", "))
    })


  # Body Contact ------------------------------------------------------------

  gesture.definitions$Body_con[gesture.definitions$Body_con == "None"] <-
    'None'
  gesture.definitions$Body_con[gesture.definitions$Body_con == "NV"] <-
    'None'
  gesture.definitions$Body_con[gesture.definitions$Body_con == "CODE (can be None)"] <-
    "Ambiguous"
  gesture.definitions$Body_con[gesture.definitions$Body_con == "CODE"] <-
    "Value"
  gesture.definitions$Body_con[gesture.definitions$Body_con == "Mouth, Teeth"] <-
    "Value"

  # Flexion -------------------------------------------------------------

  gesture.definitions$Flexion[gesture.definitions$Flexion == "CODE (can be None or NV, depending on Body_sgn)"] <-
    "Ambiguous_with_NA"
  gesture.definitions$Flexion[gesture.definitions$Flexion == "CODE (can be None)"] <-
    "Ambiguous_without_NA"
  gesture.definitions$Flexion[gesture.definitions$Flexion == "NV"] <-
    'None'

  # Orientation -------------------------------------------------------------

  gesture.definitions$Orientation[gesture.definitions$Orientation == "NV"] <-
    'None'
  gesture.definitions$Orientation[gesture.definitions$Orientation == "(Depending on Body_sgn) NV, Palm side, Palm up, Palm down, Unknown"] <-
    'Ambiguous_with_NA'
  gesture.definitions$Orientation[gesture.definitions$Orientation == "Palm side, Palm up, Palm down, Unknown CODE"] <-
    'Ambiguous_without_NA'
  gesture.definitions$Orientation[gesture.definitions$Orientation == "Palm side, Palm up, Palm down, Unknown"] <-
    'Ambiguous_without_NA'
  gesture.definitions$Orientation[gesture.definitions$Orientation == "CODE (can be None)"] <-
    'Ambiguous_without_NA'
  gesture.definitions$Orientation[gesture.definitions$Orientation != "None" &
                                    gesture.definitions$Orientation != "Ambiguous_with_NA" &
                                    gesture.definitions$Orientation != "Ambiguous_without_NA"] <-
    "Value"


  # Sgn_lat -------------------------------------------------------------

  gesture.definitions$Sgn_lat[gesture.definitions$Sgn_lat == "CODE"] <-
    "Value"
  gesture.definitions$Sgn_lat[gesture.definitions$Sgn_lat == "NV"] <-
    'None'
  gesture.definitions$Sgn_lat[gesture.definitions$Sgn_lat != "Value" &
                                gesture.definitions$Sgn_lat != "None"] <-
    "Ambiguous"


  error.list <- lapply(1:nrow(gesture.definitions), function(x) {
    sel.data <- gesture.definitions[x, ]
    # repetition count
    if (is.na(sel.data$Rep_count) |
        sel.data$Rep_count == 0) {
      Repetition_count <- c(0, 'NotApplicable', 'NoValue')
    }
    if (!is.na(sel.data$Rep_count) &
        sel.data$Rep_count == "Any") {
      Repetition_count <- 0:100
    }
    # object contact
    if (is.na(sel.data$Object_Contact)|
        sel.data$Object_Contact == 'None') {
      Object_contacted <- c("None", NA, 'NotApplicable')
    }
    if (!is.na(sel.data$Object_Contact) &
        sel.data$Object_Contact != 'None') {
      Object_contacted <- data %>%
        filter(.data$Object_contacted != "None" &
                 .data$Object_contacted != "NotApplicable") %>%
        select(.data$Object_contacted) %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }
    # with Object
    if (is.na(sel.data$With_object)|
        sel.data$With_object == 'None') {
      Object_used <- c("None", NA, 'NotApplicable')
    }
    if (!is.na(sel.data$With_object) &
        sel.data$With_object == "Value") {
      Object_used <- data %>%
        filter(.data$Object_used != "None" &
                 .data$Object_used != "NotApplicable") %>%
        select(.data$Object_used) %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }
    if (!is.na(sel.data$With_object) &
        sel.data$With_object == "Ambiguous") {
      Object_used <- data %>%
        select(.data$Object_used) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }
    # Body_sgn
    Body_part_signaller <-
      str_split(sel.data$Body_sgn, pattern = ", ") %>% unlist()
    Body_part_signaller <- c(Body_part_signaller, NA)

    # Flexion
    if (is.na(sel.data$Flexion)|
        sel.data$Flexion == 'None') {
      Flexion <- c(NA, 'NotApplicable', 'NoValue', 'Injury')
    }
    if (!is.na(sel.data$Flexion) &
        sel.data$Flexion == 'Ambiguous_with_NA') {
      Flexion <- data %>%
        filter(.data$Flexion != 'NotApplicable') %>%
        select(.data$Flexion) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }
    if (!is.na(sel.data$Flexion) &
        sel.data$Flexion == 'Ambiguous') {
      Flexion <- data %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }
    if (!is.na(sel.data$Flexion) &
        sel.data$Flexion == 'Ambiguous_without_NA') {
      Flexion <- data %>%
        filter(.data$Flexion != 'NotApplicable') %>%
        select(.data$Flexion) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE)
    }
    # Orientation
    if (is.na(sel.data$Orientation)|
        sel.data$Orientation == 'None') {
      Orientation <- c(NA, 'NotApplicable', 'NoValue', 'Injury')
    }
    if (!is.na(sel.data$Orientation) &
        sel.data$Orientation == 'Ambiguous_with_NA') {
      Orientation <- data %>%
        filter(.data$Orientation != 'NotApplicable') %>%
        select(.data$Orientation) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }
    if (!is.na(sel.data$Orientation) &
        sel.data$Orientation == 'Ambiguous') {
      Orientation <- data %>%
        select(.data$Orientation) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }
    if (!is.na(sel.data$Orientation) &
        sel.data$Orientation == 'Ambiguous_without_NA') {
      Orientation <- data %>%
        filter(.data$Orientation != 'NotApplicable') %>%
        select(.data$Orientation) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE)
    }
    if (!is.na(sel.data$Orientation) &
        sel.data$Orientation == 'Value') {
      Orientation <- data %>%
        filter(.data$Orientation != 'NotApplicable' &
                 .data$Orientation != 'NoValue') %>%
        select(.data$Orientation) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE)
    }
    # Sgn_lat
    if (is.na(sel.data$Sgn_lat)|
        sel.data$Sgn_lat == 'None') {
      Sgn_lat <- c("None", NA, 'NotApplicable', 'NoValue')
    }
    if (!is.na(sel.data$Sgn_lat) &
        sel.data$Sgn_lat == 'Ambiguous') {
      Sgn_lat <- data %>%
        filter(.data$Laterality_signaller != 'NotApplicable' &
                 .data$Laterality_signaller != 'None') %>%
        select(.data$Laterality_signaller) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }
    if (!is.na(sel.data$Sgn_lat) &
        sel.data$Sgn_lat == 'Value') {
      Sgn_lat <- data %>%
        filter(.data$Laterality_signaller != 'NotApplicable' &
                 .data$Laterality_signaller != 'None') %>%
        select(.data$Laterality_signaller) %>%
        drop_na() %>%
        unique() %>%
        unlist(use.names = FALSE)
    }
    # Body_con
    if (is.na(sel.data$Body_con)|
        sel.data$Body_con == 'None') {
      Body_con <- c("None", NA, 'NotApplicable')
    }
    if (!is.na(sel.data$Body_con) &
        sel.data$Body_con == "Value") {
      Body_con <- data %>%
        filter(.data$Body_part_contact != "None" &
                 .data$Body_part_contact != "NotApplicable") %>%
        select(.data$Body_part_contact) %>%
        unique() %>%
        unlist(use.names = FALSE)
    }
    if (!is.na(sel.data$Body_con) &
        sel.data$Body_con == "Ambiguous") {
      Body_con <- data %>%
        select(.data$Body_part_contact) %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        c(NA)
    }

    return(
      list(
        Repetition_count = Repetition_count,
        Object_contacted = Object_contacted,
        Object_used = Object_used,
        Body_part_signaller = Body_part_signaller,
        Body_part_contact = Body_con,
        Flexion = Flexion,
        Orientation = Orientation,
        Laterality_signaller = Sgn_lat
      )
    )
  })
  names(error.list) <- gesture.definitions$Gesture_action

  possible.errors <- lapply(seq_along(error.list), function(x) {
    error.rows <- lapply(seq_along(error.list[[x]]), function(y) {
      values <- error.list[[x]][[y]]
      test.column <- unlist(data[, names(error.list[[x]])[y]], F, F)
      xx <- data %>%
        filter(.data$Gesture_record == names(error.list)[x] &
                 !(test.column %in% values)) %>%
        select(
          .data$Coder,
          .data$Communication_number,
          .data$Recording_number,
          .data$Date,
          .data$Signaller,
          .data$Clip_name,
          .data$Gesture_record,
          .data$row.number,
          names(error.list[[x]])[y]
        ) %>%
        mutate(problematic.column = names(error.list[[x]])[y]) %>%
        select(
          .data$Coder,
          .data$Communication_number,
          .data$Recording_number,
          .data$Gesture_record,
          .data$problematic.column,
          .data$row.number
        ) %>%
        data.frame(stringsAsFactors = FALSE)

      xx$level <- data[
        data$row.number %in%
          xx$row.number,
        names(error.list[[x]])[y]
      ] %>% unlist()
      xx <- lapply(xx, as.character) %>%
        bind_cols()
      return(xx)
    })
    error.rows <- bind_rows(error.rows) %>%
      mutate(across(everything(), as.character))
  })
  names(possible.errors) <- names(error.list)
  possible.errors <-
    possible.errors[sapply(possible.errors, nrow) > 0]
  possible.errors <- bind_rows(possible.errors)

  # check coding status
  cs <- data %>%
    filter(Coding_status!='Complete') %>%
    mutate(problematic.column = 'Coding_status',
           level = Coding_status,
           row.number = as.character(row.number)) %>%
    select(Coder,
           Communication_number,
           Recording_number,
           Gesture_record,
           problematic.column,
           row.number,
           level)

  possible.errors <-
    bind_rows(possible.errors,
              cs)

  # check Flexion and Orientation for other body parts

  fl <- data %>%
    filter((Body_part_signaller %in% c('Back',
                                       'Body',
                                       'BodyChest',
                                       'BodyFront',
                                       'Face_Mouth',
                                       'Genitals',
                                       'Head') &
              (!(is.na(Flexion)| Flexion == 'NoValue'))) |
             (!(Body_part_signaller %in% c('Back',
                                          'Body',
                                          'BodyChest',
                                          'BodyFront',
                                          'Face_Mouth',
                                          'Genitals',
                                          'Head')) &
                 (is.na(Flexion)|Flexion != 'NoValue') &
                Gesture_record %in% gesture.definitions$Gesture_action[gesture.definitions$Flexion == 'Value'])) %>%
    mutate(problematic.column = 'Flexion',
           level = Flexion,
           row.number = as.character(row.number)) %>%
    select(Coder,
           Communication_number,
           Recording_number,
           Gesture_record,
           problematic.column,
           row.number,
           level)

  or <- data %>%
    filter((Body_part_signaller %in% c('Back',
                                       'Body',
                                       'BodyChest',
                                       'BodyFront',
                                       'Face_Mouth',
                                       'Genitals',
                                       'Head') &
              (!(is.na(Orientation)| Orientation == 'NoValue'))) |
             (!(Body_part_signaller %in% c('Back',
                                           'Body',
                                           'BodyChest',
                                           'BodyFront',
                                           'Face_Mouth',
                                           'Genitals',
                                           'Head')) &
                is.na(Orientation) &
                Gesture_record %in% gesture.definitions$Gesture_action[gesture.definitions$Orientation == 'Value']))  %>%
    mutate(problematic.column = 'Orientation',
           level = Orientation,
           row.number = as.character(row.number)) %>%
    select(Coder,
           Communication_number,
           Recording_number,
           Gesture_record,
           problematic.column,
           row.number,
           level)

  possible.errors <-
    bind_rows(possible.errors,
              or,
              fl) %>%
    arrange(row.number) %>% 
    distinct(.keep_all = TRUE)

  return(possible.errors)
}
