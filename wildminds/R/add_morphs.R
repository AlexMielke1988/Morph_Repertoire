# add the table with the rules for the morphs (saved in morph_rules.RData) and your data set

assign_morph <- function(fill_data, morph_rules){

  # turn columns into character vectors
  fill_data$Bp_contact <- as.character(fill_data$Body_part_contact)
  fill_data$Bp_signaller <- as.character(fill_data$Body_part_signaller)
  fill_data$Laterality <- as.character(fill_data$Laterality_signaller)
  fill_data$Repet_count <- as.character(fill_data$Repetition_count)

  # change gesture actions

  fill_data$Gesture_record <- fill_data$Gesture_record %>%
    # Bite
    str_replace_all(pattern = "BiteThreat", replacement = "Bite") %>%
    str_replace_all(pattern = "BiteKiss", replacement = "Bite") %>%
    # Chest Beat
    str_replace_all(pattern = "ChestBeatInformalOther", replacement = "ChestBeat") %>%
    str_replace_all(pattern = "ChestBeatInformalStanding", replacement = "ChestBeat") %>%
    # Hit Object
    str_replace_all(pattern = "HitObjectObject", replacement = "HitObject") %>%
    str_replace_all(pattern = "HittingObjectObject", replacement = "HitObject") %>%
    str_replace_all(pattern = "HitObjectSoft", replacement = "HitObject") %>%
    str_replace_all(pattern = "HittingObjectSoft", replacement = "HitObject") %>%
    str_replace_all(pattern = "HittingObject", replacement = "HitObject") %>%

    # HitOther
    str_replace_all(pattern = "HitOtherSoft", replacement = "HitOther") %>%
    str_replace_all(pattern = "HittingOtherClap", replacement = "HitOther") %>%
    str_replace_all(pattern = "HittingOtherSoft", replacement = "HitOther") %>%
    str_replace_all(pattern = "HittingOther", replacement = "HitOther") %>%
    str_replace_all(pattern = "HittingObjectOther", replacement = "HitObject") %>%
    str_replace_all(pattern = "HitObjectOther", replacement = "HitObject") %>%

    # Hit Self
    str_replace_all(pattern = "HitSelfSoft", replacement = "HitSelf") %>%
    str_replace_all(pattern = "HittingSelfSoft", replacement = "HitSelf") %>%
    str_replace_all(pattern = "HittingSelf", replacement = "HitSelf") %>%

    # Hit Bystander
    str_replace_all(pattern = "HittingBystanderSoft", replacement = "HitBystander") %>%
    str_replace_all(pattern = "HittingBystander", replacement = "HitBystander") %>%
    str_replace_all(pattern = "HitBystanderSoft", replacement = "HitBystander") %>%

    # Knock Object
    str_replace_all(pattern = "KnockingObject", replacement = "KnockObject") %>%

    # Leaf Clip
    str_replace_all(pattern = "LeafClipDrop", replacement = "LeafClip") %>%

    # Object Mouth
    str_replace_all(pattern = "ObjectMouthAttached", replacement = "ObjectMouth") %>%
    str_replace_all(pattern = "ObjectMouthUnattached", replacement = "ObjectMouth") %>%

    # Object Move
    str_replace_all(pattern = "ObjectMoveAttached", replacement = "ObjectMove") %>%
    str_replace_all(pattern = "ObjectMoveUnattached", replacement = "ObjectMove") %>%
    str_replace_all(pattern = "RakeObject", replacement = "ObjectMove") %>%

    # Poke
    str_replace_all(pattern = "Poking", replacement = "Poke") %>%

    # Stomp
    str_replace_all(pattern = "Stomping", replacement = "Stomp") %>%

    # Present Genitals
    str_replace_all(pattern = "PresentGenitalsBackwards", replacement = "PresentGenitals") %>%
    str_replace_all(pattern = "PresentGenitalsForwards", replacement = "PresentGenitals") %>%

    # Rocking
    str_replace_all(pattern = "RockingBipedal", replacement = "Rocking") %>%
    str_replace_all(pattern = "RockingSit", replacement = "Rocking") %>%

    # Touch Long
    str_replace_all(pattern = "TouchLongObject", replacement = "TouchObject") %>%
    str_replace_all(pattern = "TouchLongOther", replacement = "Touch") %>%

    # Directed
    str_replace_all(pattern = "PresentDirected", replacement = "Present") %>%
    str_replace_all(pattern = "SwingDirected", replacement = "Swing") %>%
    str_replace_all(pattern = "PushDirected", replacement = "Push") %>%
    str_replace_all(pattern = "PullDirected", replacement = "Pull")

  # lump up Body Part Signaller to be at the same level as in the morph calculation

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Unknown',
                                             'Unclear'),
      yes = NA,
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('NoValue',
                                             'NotApplicable'),
      yes = 'None',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Arm', 'BackShoulder'),
      yes = 'Arm',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Back'),
      yes = 'Back',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Body'),
      yes = 'Body',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('BodyChest'),
      yes = 'BodyChest',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('BodyFront'),
      yes = 'BodyFront',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Bottom'),
      yes = 'Bottom',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('BottomGenitalskin', 'Penis', 'Testes', 'Swelling'),
      yes = 'Genitals',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Face', 'Mouth', 'MouthTeeth'),
      yes = 'Face_Mouth',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Fingers'),
      yes = 'Fingers',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Fist'),
      yes = 'Fist',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Foot'),
      yes = 'Foot',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Hand', 'HandWrist'),
      yes = 'Hand',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('HandFoot'),
      yes = 'HandFoot',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('HeadThroatNeck', 'HeadHead'),
      yes = 'Head',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Knuckles'),
      yes = 'Knuckles',
      no = fill_data$Bp_signaller
    )

  fill_data$Bp_signaller <-
    ifelse(
      test = fill_data$Bp_signaller %in% c('Leg'),
      yes = 'Leg',
      no = fill_data$Bp_signaller
    )

  # Lump Body Part contact to same level as morphs

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Unknown',
                                           'Unclear'),
      yes = NA,
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('NoValue',
                                           'NotApplicable'),
      yes = 'None',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Arm', 'BackShoulder'),
      yes = 'Arm',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Back'),
      yes = 'Back',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Body'),
      yes = 'Body',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('BodyChest'),
      yes = 'BodyChest',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('BodyFront'),
      yes = 'BodyFront',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Bottom'),
      yes = 'Bottom',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('BottomGenitalskin', 'Penis', 'Testes', 'Swelling'),
      yes = 'Genitals',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Face', 'Mouth', 'MouthTeeth'),
      yes = 'Face_Mouth',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Fingers'),
      yes = 'Fingers_Hand',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Fist'),
      yes = 'Fist',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Foot'),
      yes = 'Leg',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Hand', 'HandWrist'),
      yes = 'Fingers_Hand',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('HeadThroatNeck', 'HeadHead'),
      yes = 'Head',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Knuckles'),
      yes = 'Knuckles',
      no = fill_data$Bp_contact
    )

  fill_data$Bp_contact <-
    ifelse(
      test = fill_data$Bp_contact %in% c('Leg'),
      yes = 'Leg',
      no = fill_data$Bp_contact
    )

  # This line creates a new column called "Laterality" in the "fill_data" dataframe
  # It checks whether the values in the "Laterality" column are "Left" or "Right"
  # If they are, it assigns "unimanual" to the new "Laterality" column, otherwise it assigns the value in the "Laterality" column
  fill_data$Laterality <- ifelse(fill_data$Laterality %in% c("Left", "Right"), "unimanual", fill_data$Laterality)
  fill_data$Laterality <- ifelse(fill_data$Laterality %in% c("NoValue"), NA, fill_data$Laterality)
  fill_data$Laterality <- ifelse(fill_data$Laterality %in% c("Unknown"), NA, fill_data$Laterality)

  # This line creates a new column called "Repet_count" in the "fill_data" dataframe
  # It checks whether the values in the "Repet_count" column are greater than 0
  # If they are, it assigns "yes" to the new "Repet_count" column, otherwise it assigns "no"
  fill_data$Repet_count[fill_data$Repet_count == 'Unknown'] <- NA
  fill_data$Repet_count <- as.numeric(as.character(fill_data$Repet_count))
  fill_data$Repet_count <- ifelse(as.numeric(fill_data$Repet_count) > 0, "yes", "no")

  # This line creates a new column called "Bp_contact" in the "fill_data" dataframe
  # It checks the values in the "Bp_contact" column and assigns new values based on the following conditions:
  # If the value is "Fist", it assigns "Fingers_Hand" to the new column
  # If the value is "BodyChest" or "BodyFront", it assigns "Body" to the new column
  # If the value is "Bottom", it assigns "Leg" to the new column
  # Otherwise, it assigns the original value in the "Bp_contact" column to the new column
  fill_data$Bp_contact <- case_when(
    fill_data$Bp_contact == 'Fist' ~ 'Fingers_Hand',
    fill_data$Bp_contact %in% c('BodyChest', 'BodyFront') ~ 'Body',
    fill_data$Bp_contact == 'Bottom' ~ 'Leg',
    TRUE ~ fill_data$Bp_contact
  )

  # This line creates a new column called "Bp_signaller" in the "fill_data" dataframe
  # It checks the values in the "Bp_signaller" column and assigns new values based on the following conditions:
  # If the value is "Fist" or 'Knuckles', it assigns "Fingers_Hand" to the new column
  # If the value is "BodyChest" or "BodyFront", it assigns "Body" to the new column
  # If the value is "HandFoot", it assigns "Foot" to the new column
  # Otherwise, it assigns the original value in the "Bp_contact" column to the new column
  fill_data$Bp_signaller <- case_when(
    fill_data$Bp_signaller %in% c('Fist', 'Knuckles') ~ 'Hand',
    fill_data$Bp_signaller %in% c('BodyChest', 'BodyFront') ~ 'Body',
    fill_data$Bp_signaller == 'HandFoot' ~ 'Foot',
    TRUE ~ fill_data$Bp_signaller
  )

  # This line creates a new variable called "morph_assignment"
  # It applies a function to each row of the "fill_data" dataframe using the "lapply" function
  morph_assignment <- lapply(1:nrow(fill_data), function(i){

    # This line creates a new dataframe called "sub_ga"
    # It filters the "morph_rules" dataframe based on the values in the current row of "fill_data"
    # Specifically, it filters by the gesture action in the current row, and by the body part signaller, body part contact, repetition, and laterality columns in the current row
    # If a column value is missing (NA), the filter allows for any value in that column
    sub_ga <-
      morph_rules %>%
      filter(gesture_action == fill_data$Gesture_record[i]) %>%
      filter(str_detect(Body_part_signaller, fill_data$Bp_signaller[i]) | is.na(Body_part_signaller)) %>%
      filter(str_detect(Body_part_contact, fill_data$Bp_contact[i]) | is.na(Body_part_contact)) %>%
      filter(str_detect(Repetition, fill_data$Repet_count[i]) | is.na(Repetition)) %>%
      filter(str_detect(Laterality, fill_data$Laterality[i]) | is.na(Laterality))

    # If the "sub_ga" dataframe has no rows and laterality is missing (NA), this block of code is executed
    if(nrow(sub_ga) == 0 & is.na(fill_data$Laterality[i])){
      # This line filters the "morph_rules" dataframe again with similar criteria as before, but also includes a condition for 'NV' values for laterality column
      sub_ga <-
        morph_rules %>%
        filter(gesture_action == fill_data$Gesture_record[i]) %>%
        filter(str_detect(Body_part_signaller, fill_data$Bp_signaller[i]) | is.na(Body_part_signaller)) %>%
        filter(str_detect(Body_part_contact, fill_data$Bp_contact[i]) | is.na(Body_part_contact)) %>%
        filter(str_detect(Repetition, fill_data$Repet_count[i]) | is.na(Repetition)) %>%
        filter(str_detect(Laterality, fill_data$Laterality[i]) | is.na(Laterality) | Laterality == 'NV')
    }

    # If the "sub_ga" dataframe has more than one row, this block of code is executed
    if(nrow(sub_ga) > 1){
      # This line filters the "sub_ga" dataframe to exclude rows where all columns are missing (NA)
      sub_ga <-
        sub_ga %>%
        filter(!(is.na(Body_part_signaller) & is.na(Body_part_contact) &  is.na(Repetition) & is.na(Laterality)))
    }

    if(nrow(sub_ga) == 1){
      return(sub_ga$morph_name)
    } else {
      return(NA)
    }
  })

  return(morph_assignment)
}
