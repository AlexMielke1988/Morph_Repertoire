#' Takes the dataset and reduces some levels for a number of modifiers to facilitate further analyses. Main removes multiple NA, NotApplicable, NoValue etc
#'
#' Reduction of the following columns: Signaller Arousal, Recipient Arousal, Directed Towards, Goal_met, Directionality, Body Parts Signaller, Body Parts Contact, Flexion, Orientation, Object Used, Object contact, Laterality, Emphasis, Context Waiting, Effectiveness, Persistence, Sequences, Recipient Distance, Gaze, Audible, Recipient Reaction, Visual Attention, Location, Recipient Signals, Latency, Signaller Communication, Visibility, Behaviour change
#'
#' @param data data frame as extracted from Filemaker
#' @param col_names either 'renamed' or 'original'. If they are still original at this point, they will be renamed here
#' @param reduce.nas if TRUE, for most columns, NAs and NVs etc will be combined in NA
#' @param reduce.all if TRUE, you don't have to set all the others as TRUE
#' @param reduce.goals if TRUE, some goals will be combined (e.g., GroomMe and GroomYou become 'Groom')
#' @param reduce.gestures if TRUE, some gestures are combined (e.g., HittingSoft, HitSoft, and Hitting become 'Hit)
#' @param reduce.directed.to if TRUE, some Directed_towards will be combined
#' @param reduce.goal.met if TRUE, some Goal_met will be combined
#' @param reduce.body.part.signaller if TRUE, some Body_part_signaller will be combined
#' @param reduce.body.part.contact if TRUE, some Body_parts_contacted will be combined
#' @param reduce.object.used if TRUE, some Object_used will be combined
#' @param reduce.object.contacted if TRUE, some Object_contacted will be combined
#' @param reduce.laterality if TRUE, some Laterality will be combined
#' @param reduce.effectiveness if TRUE, some Effectiveness will be combined
#' @param reduce.distance if TRUE, some Distances will be combined
#' @param reduce.visual.attention if TRUE, some Visual_attention will be combined
#' @param reduce.location if TRUE, some Locations will be combined
#' @param reduce.visibility if TRUE, some Visibility will be combined
#'
#'
#' @return data frame containing all data in cleaned fashion
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#'
#'

reduce_levels <- function(data,
                          col_names = 'renamed',
                          reduce.all = FALSE,
                          reduce.nas = TRUE,
                          reduce.goals = FALSE,
                          reduce.gestures = FALSE,
                          reduce.directed.to = FALSE,
                          reduce.goal.met = FALSE,
                          reduce.body.part.signaller = FALSE,
                          reduce.body.part.contact = FALSE,
                          reduce.object.used = FALSE,
                          reduce.object.contacted = FALSE,
                          reduce.laterality = FALSE,
                          reduce.effectiveness = FALSE,
                          reduce.distance = FALSE,
                          reduce.visual.attention = FALSE,
                          reduce.location = FALSE,
                          reduce.visibility = FALSE) {
  if (col_names == 'original') {
    save.colnames <- colnames(data)
    data_original <- data
    data <- clean_function(
      data = data,
      clean.value.levels = TRUE,
      clean.col.names = TRUE
    )
  }

  if(reduce.all){
    reduce.nas = TRUE
    reduce.goals = TRUE
    reduce.gestures = TRUE
    reduce.directed.to = TRUE
    reduce.goal.met = TRUE
    reduce.body.part.signaller = TRUE
    reduce.body.part.contact = TRUE
    reduce.object.used = TRUE
    reduce.object.contacted = TRUE
    reduce.laterality = TRUE
    reduce.effectiveness = TRUE
    reduce.distance = TRUE
    reduce.visual.attention = TRUE
    reduce.location = TRUE
    reduce.visibility = TRUE
  }

  # Signaller Arousal -------------------------------------------------------
  if (reduce.nas) {
    data$Signaller_arousal <-
      ifelse(
        test = data$Signaller_arousal %in% c('NotApplicable', 'Unknown'),
        yes = NA,
        no = data$Signaller_arousal
      )
  }

  # Recipient Arousal -------------------------------------------------------
  if (reduce.nas) {
    data$Recipient_arousal <-
      ifelse(
        test = data$Recipient_arousal %in% c('NotApplicable', 'Unknown', 'NoValue'),
        yes = NA,
        no = data$Recipient_arousal
      )
  }
  # Directed_towards -------------------------------------------------------
  if (reduce.directed.to) {
    data$Directed_towards <-
      ifelse(
        test = data$Directed_towards %in% c('SeveralPotential', 'No'),
        yes = 'Undirected',
        no = data$Directed_towards
      )

    data$Directed_towards <-
      ifelse(
        test = data$Directed_towards %in% c('Unknown', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Directed_towards
      )

    data$Directed_towards <-
      ifelse(
        test = data$Directed_towards %in% c('OneDirected', 'OnePot', 'SeveralDirected'),
        yes = 'Directed',
        no = data$Directed_towards
      )
  }

  # Goal_met ----------------------------------------------------------------

  if (reduce.goal.met) {
    data$Goal_met <-
      ifelse(
        test = data$Goal_met %in% c('YesForced'),
        yes = 'Yes',
        no = data$Goal_met
      )

    data$Goal_met <-
      ifelse(
        test = data$Goal_met %in% c('NotApplicable', 'Unknown', 'NoValue'),
        yes = NA,
        no = data$Goal_met
      )
  }
  # Directionality ----------------------------------------------------------

  if (reduce.nas) {
    data$Directionality_gesture <-
      ifelse(
        test = data$Directionality_gesture %in% c('Unknown', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Directionality_gesture
      )
  }

  # Body Parts Signaller--------------------------------------------------------------
  if (reduce.body.part.signaller) {
    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Unknown',
                                               'Unclear'),
        yes = NA,
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('NoValue',
                                               'NotApplicable'),
        yes = 'None',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Arm', 'BackShoulder'),
        yes = 'Arm',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Back'),
        yes = 'Back',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Body'),
        yes = 'Body',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('BodyChest'),
        yes = 'BodyChest',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('BodyFront'),
        yes = 'BodyFront',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Bottom'),
        yes = 'Bottom',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('BottomGenitalskin', 'Penis', 'Testes', 'Swelling'),
        yes = 'Genitals',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Face', 'Mouth', 'MouthTeeth'),
        yes = 'Face_Mouth',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Fingers'),
        yes = 'Fingers',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Fist'),
        yes = 'Fist',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Foot'),
        yes = 'Foot',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Hand', 'HandWrist'),
        yes = 'Hand',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('HandFoot'),
        yes = 'HandFoot',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('HeadThroatNeck', 'HeadHead'),
        yes = 'Head',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Knuckles'),
        yes = 'Knuckles',
        no = data$Body_part_signaller
      )

    data$Body_part_signaller <-
      ifelse(
        test = data$Body_part_signaller %in% c('Leg'),
        yes = 'Leg',
        no = data$Body_part_signaller
      )

  }

  # Body Parts Contact--------------------------------------------------------------
  if (reduce.body.part.contact) {
    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Unknown',
                                               'Unclear'),
        yes = NA,
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('NoValue',
                                               'NotApplicable'),
        yes = 'None',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Arm', 'BackShoulder'),
        yes = 'Arm',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Back'),
        yes = 'Back',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Body'),
        yes = 'Body',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('BodyChest'),
        yes = 'BodyChest',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('BodyFront'),
        yes = 'BodyFront',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Bottom'),
        yes = 'Bottom',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('BottomGenitalskin', 'Penis', 'Testes', 'Swelling'),
        yes = 'Genitals',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Face', 'Mouth', 'MouthTeeth'),
        yes = 'Face_Mouth',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Fingers'),
        yes = 'Fingers_Hand',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Fist'),
        yes = 'Fist',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Foot'),
        yes = 'Leg',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Hand', 'HandWrist'),
        yes = 'Fingers_Hand',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('HeadThroatNeck', 'HeadHead'),
        yes = 'Head',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Knuckles'),
        yes = 'Knuckles',
        no = data$Body_part_contact
      )

    data$Body_part_contact <-
      ifelse(
        test = data$Body_part_contact %in% c('Leg'),
        yes = 'Leg',
        no = data$Body_part_contact
      )
  }

  # Flexion -----------------------------------------------------------------
  if (reduce.nas) {
    data$Flexion <-
      ifelse(
        test = data$Flexion %in% c('Unknown', 'Unclear', 'NoValue', 'Injury', 'NotApplicable'),
        yes = NA,
        no = data$Flexion
      )

    data$Flexion <-
      ifelse(
        test = data$Flexion %in% c('All'),
        yes = 'ElbowFingerWrist',
        no = data$Flexion
      )
  }
  # Orientation -------------------------------------------------------------
  if (reduce.nas) {
    data$Orientation <-
      ifelse(
        test = data$Orientation %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Orientation
      )
  }
  # Object Used -------------------------------------------------------------
  if (reduce.object.used) {
    data$Object_used <-
      ifelse(
        test = data$Object_used %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Object_used
      )

    data$Object_used <-
      ifelse(
        test = data$Object_used %in% c('Nonene'),
        yes = 'None',
        no = data$Object_used
      )

    data$Object_used <-
      ifelse(
        test = data$Object_used %in% c(
          'Branch',
          'FoodLeaf',
          'FoodNonleaf',
          'Leaf',
          'Leaves',
          'Liana',
          'Sapling',
          'Stick',
          'Stone',
          'Tool'
        ),
        yes = 'Moveable',
        no = data$Object_used
      )

    data$Object_used <-
      ifelse(
        test = data$Object_used %in% c('Buttress',
                                       'Ground',
                                       'Log',
                                       'TreeTrunk',
                                       'Treetrunk',
                                       'Water'),
        yes = 'Surface',
        no = data$Object_used
      )

    data$Object_used <-
      ifelse(
        test = data$Object_used %in% c('Other'),
        yes = 'Yes',
        no = data$Object_used
      )
  }
  # Object contact ----------------------------------------------------------

  if (reduce.object.contacted) {
    data$Object_contacted <-
      ifelse(
        test = data$Object_contacted %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Object_contacted
      )

    data$Object_contacted <-
      ifelse(
        test = data$Object_contacted %in% c(
          'Branch',
          'FoodLeaf',
          'FoodNonleaf',
          'GroundVegetation',
          'Leaf',
          'Leaves',
          'Liana',
          'Sapling',
          'Stick',
          'Stone'
        ),
        yes = 'Moveable',
        no = data$Object_contacted
      )

    data$Object_contacted <-
      ifelse(
        test = data$Object_contacted %in% c('Buttress',
                                            'Ground',
                                            'Log',
                                            'TreeTrunk',
                                            'Treetrunk',
                                            'Water'),
        yes = 'Surface',
        no = data$Object_contacted
      )

    data$Object_contacted <-
      ifelse(
        test = data$Object_contacted %in% c('Other'),
        yes = 'Yes',
        no = data$Object_contacted
      )
  }
  # Laterality  ----------------------------------------------------

  if (reduce.laterality) {
    data$Laterality_bimanual <-
      ifelse(
        test = data$Laterality_bimanual %in% c('Left', 'Right'),
        yes = 'Unilateral',
        no = data$Laterality_bimanual
      )
  }
  if (reduce.nas) {
    data$Laterality_bimanual <-
      ifelse(
        test = data$Laterality_bimanual %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Laterality_bimanual
      )

    data$Laterality_signaller <-
      ifelse(
        test = data$Laterality_signaller %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Laterality_signaller
      )

    data$Laterality_contact <-
      ifelse(
        test = data$Laterality_contact %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Laterality_contact
      )
  }
  # Emphasis ----------------------------------------------------------------
  if (reduce.nas) {
    data$Emphasis <-
      ifelse(
        test = data$Emphasis %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Emphasis
      )
  }
  # Context waiting ---------------------------------------------------------
  if (reduce.nas) {
    data$Response_waiting <-
      ifelse(
        test = data$Response_waiting %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Response_waiting
      )
  }
  # Effectiveness -----------------------------------------------------------
  if (reduce.effectiveness) {
    data$Effectiveness <-
      ifelse(
        test = data$Effectiveness %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Effectiveness
      )

    data$Effectiveness <-
      ifelse(
        test = data$Effectiveness %in% c('NoCongruent', 'NoMechanical', 'OtherComponent'),
        yes = 'No',
        no = data$Effectiveness
      )
  }
  # Persistence -------------------------------------------------------------
  if (reduce.nas) {
    data$Persistence <-
      ifelse(
        test = data$Persistence %in% c('Unknown',
                                       'Unclear',
                                       'NoValue',
                                       'NotApplicable',
                                       'Unable'),
        yes = NA,
        no = data$Persistence
      )
  }
  # Sequences ---------------------------------------------------------------
  if (reduce.nas) {
    data$Part_sequence <-
      ifelse(
        test = data$Part_sequence %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Part_sequence
      )

    data$Part_bout <-
      ifelse(
        test = data$Part_bout %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Part_bout
      )

    data$Part_exchange <-
      ifelse(
        test = data$Part_exchange %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Part_exchange
      )
  }
  # Recipient Distance ------------------------------------------------------
  if (reduce.distance) {
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Recipient_distance
      )
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('No_0'),
        yes = 0,
        no = data$Recipient_distance
      )
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('0_0.5', 'U_0.5'),
        yes = 0.5,
        no = data$Recipient_distance
      )
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('0.5_1'),
        yes = 1,
        no = data$Recipient_distance
      )
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('1_1.5'),
        yes = 1.5,
        no = data$Recipient_distance
      )
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('1.5_2'),
        yes = 2,
        no = data$Recipient_distance
      )
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('2_3', 'U_3'),
        yes = 3,
        no = data$Recipient_distance
      )
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('3_5', 'O_3'),
        yes = 5,
        no = data$Recipient_distance
      )
    data$Recipient_distance <-
      ifelse(
        test = data$Recipient_distance %in% c('Over_5'),
        yes = 6,
        no = data$Recipient_distance
      )

    data$Recipient_distance <- as.numeric(data$Recipient_distance)
  }


  # Gaze --------------------------------------------------------------------
  if (reduce.nas) {
    data$Gaze_before <-
      ifelse(
        test = data$Gaze_before %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Gaze_before
      )

    data$Gaze_during <-
      ifelse(
        test = data$Gaze_during %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Gaze_during
      )
  }
  # Audible -----------------------------------------------------------------
  if (reduce.nas) {
    data$Audible <-
      ifelse(
        test = data$Audible %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Audible
      )
  }
  # Recipient Reaction ------------------------------------------------------
  if (reduce.nas) {
    data$Recipient_facial_expression <-
      ifelse(
        test = data$Recipient_facial_expression %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Recipient_facial_expression
      )

    data$Vocalisation_recipient <-
      ifelse(
        test = data$Vocalisation_recipient %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Vocalisation_recipient
      )

    data$Recipient_gesture <-
      ifelse(
        test = data$Recipient_gesture %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Recipient_gesture
      )

    data$Recipient_vocalisation_sequence <-
      ifelse(
        test = data$Recipient_vocalisation_sequence %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Recipient_vocalisation_sequence
      )

    data$Gesture_recipient <-
      ifelse(
        test = data$Gesture_recipient %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Gesture_recipient
      )
  }

  # Visual Attention --------------------------------------------------------
  if (reduce.nas) {
    data$Recipient_visusal_attention <-
      ifelse(
        test = data$Recipient_visusal_attention %in% c(
          'Unknown',
          'Unclear',
          'NoValue',
          'NotApplicable',
          'OutOfSight'
        ),
        yes = NA,
        no = data$Recipient_visusal_attention
      )
  }
  if (reduce.visual.attention) {
    data$Recipient_visusal_attention <-
      ifelse(
        test = data$Recipient_visusal_attention %in% c(
          'VisualAttention90',
          'VisualAttentionClear',
          'VisualAttentionPossible'
        ),
        yes = 'Yes',
        no = data$Recipient_visusal_attention
      )

    data$Recipient_visusal_attention <-
      ifelse(
        test = data$Recipient_visusal_attention %in% c('NoVisualAttention'),
        yes = 'No',
        no = data$Recipient_visusal_attention
      )
  }

  # Location ----------------------------------------------------------------
  if (reduce.nas) {
    data$Recipient_location <-
      ifelse(
        test = data$Recipient_location %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Recipient_location
      )
    data$Signaller_location <-
      ifelse(
        test = data$Signaller_location %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Signaller_location
      )
  }
  if (reduce.location) {
    data$Recipient_location <-
      ifelse(
        test = str_detect(data$Recipient_location, 'Tree'),
        yes = 'Tree',
        no = data$Recipient_location
      )

    data$Recipient_location <-
      ifelse(
        test = str_detect(data$Recipient_location, 'Ground'),
        yes = 'Ground',
        no = data$Recipient_location
      )

    data$Signaller_location <-
      ifelse(
        test = str_detect(data$Signaller_location, 'Tree'),
        yes = 'Tree',
        no = data$Signaller_location
      )

    data$Signaller_location <-
      ifelse(
        test = str_detect(data$Signaller_location, 'Ground'),
        yes = 'Ground',
        no = data$Signaller_location
      )
  }
  # Recipient Reaction ------------------------------------------------------
  if (reduce.nas) {
    data$Recipient_reaction <-
      ifelse(
        test = data$Recipient_reaction %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Recipient_reaction
      )
  }
  # Latency -----------------------------------------------------------------
  if (reduce.nas) {
    data$Behaviour_change1 <-
      ifelse(
        test = data$Behaviour_change1 %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Behaviour_change1
      )

    data$Behaviour_change2 <-
      ifelse(
        test = data$Behaviour_change2 %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Behaviour_change2
      )
  }

  # Signaller Communication -------------------------------------------------
  if (reduce.nas) {
    data$Signaller_facial_expression <-
      ifelse(
        test = data$Signaller_facial_expression %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Signaller_facial_expression
      )

    data$Signaller_vocalisation <-
      ifelse(
        test = data$Signaller_vocalisation %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Signaller_vocalisation
      )
  }

  # Visibility --------------------------------------------------------------
  if (reduce.nas) {
    data$Visibility_during <-
      ifelse(
        test = data$Visibility_during %in% c('Unknown', 'Unclear', 'NoValue', 'NotApplicable'),
        yes = NA,
        no = data$Visibility_during
      )
  }
  if (reduce.visibility) {
    data$Visibility_during <-
      ifelse(
        test = str_detect(data$Visibility_during, 'Poor'),
        yes = 'Poor',
        no = data$Visibility_during
      )
  }


  # Behaviour_Change --------------------------------------------------------
  if (reduce.nas) {
    data$Outcome <-
      ifelse(
        test = data$Outcome %in% c('Unknown',
                                            'Unclear',
                                            'NoValue',
                                            'NotApplicable',
                                            'Unable'),
        yes = NA,
        no = data$Outcome
      )

    data$Outcome <-
      ifelse(
        test = str_detect(data$Outcome, 'Poor'),
        yes = NA,
        no = data$Outcome
      )
  }

  # Goals -------------------------------------------------------------------

  # if user wants to reduce Goals, the following are combined
  if (reduce.goals) {
    data$Goal <- data$Goal %>%
      str_replace_all(pattern = "GroomMe", replacement = "Groom") %>%
      str_replace_all(pattern = "GroomMe2", replacement = "Groom") %>%
      str_replace_all(pattern = "GroomYou", replacement = "Groom") %>%
      str_replace_all(pattern = "Groom2", replacement = "Groom") %>%
      str_replace_all(pattern = "AttendMe", replacement = "DirectAttention") %>%
      str_replace_all(pattern = "AffilationContact", replacement = "AffilationContact") %>%
      str_replace_all(pattern = "AffilationDistant", replacement = "AffiliationDistant") %>%
      str_replace_all(pattern = "AffilationMeContact", replacement = "AffilationContact") %>%
      str_replace_all(pattern = "AffilationRestContact", replacement = "AffiliationRestContact") %>%
      str_replace_all(pattern = "AffilationUnclear", replacement = "AffilationContact") %>%
      str_replace_all(pattern = "AffilationYouContact", replacement = "AffilationContact") %>%
      str_replace_all(pattern = "GiveMeActive", replacement = "GiveMe") %>%
      str_replace_all(pattern = "GiveMePassive", replacement = "GiveMe") %>%
      str_replace_all(pattern = "MoveAway2", replacement = "MoveAway") %>%
      str_replace_all(pattern = "PlayChangeChangeacon", replacement = "PlayChange") %>%
      str_replace_all(pattern = "PlayChangeContactcha", replacement = "PlayChange") %>%
      str_replace_all(pattern = "SexualAttentionMe", replacement = "SexualAttention") %>%
      str_replace_all(pattern = "SexualAttentionYou", replacement = "SexualAttention") %>%
      str_replace_all(pattern = "SocioSexualAttentionMe", replacement = "SocioSexualAttention") %>%
      str_replace_all(pattern = "SocioSexualAttentionYou", replacement = "SocioSexualAttention") %>%
      str_replace_all(pattern = "StopBehaviourHere", replacement = "StopBehaviour") %>%
      str_replace_all(pattern = "StopBehaviourStay", replacement = "StopBehaviour") %>%
      str_replace_all(pattern = "SupportMe", replacement = "Support") %>%
      str_replace_all(pattern = "SupportYou", replacement = "Support") %>%
      str_replace_all(pattern = "TakeObjectActive", replacement = "TakeObject") %>%
      str_replace_all(pattern = "TakeObjectNurse", replacement = "TakeObjectNurse") %>%
      str_replace_all(pattern = "TakeObjectPassive", replacement = "TakeObject") %>%
      str_replace_all(pattern = "TravelMe", replacement = "Travel") %>%
      str_replace_all(pattern = "TravelYou", replacement = "Travel")
  }


  # Gesture Actions ---------------------------------------------------------
  # if the user chooses to combine Gestures, the following are combined
  if (reduce.gestures) {
    data$Gesture_record <- data$Gesture_record %>%
      str_replace_all(pattern = "BiteThreat", replacement = "Bite") %>%
      str_replace_all(pattern = "BiteKiss", replacement = "Bite") %>%
      str_replace_all(pattern = "ChestBeatInformalOther", replacement = "ChestBeat") %>%
      str_replace_all(pattern = "ChestBeatInformalStanding", replacement = "ChestBeat") %>%
      str_replace_all(pattern = "Soft", replacement = "") %>%
      str_replace_all(pattern = "Hitting", replacement = "Hit") %>%
      str_replace_all(pattern = "OtherClap", replacement = "Other") %>%
      str_replace_all(pattern = "Knocking", replacement = "Knock") %>%
      str_replace_all(pattern = "LeafClipDrop", replacement = "LeafClip") %>%
      str_replace_all(pattern = "Poking", replacement = "Poke") %>%
      str_replace_all(pattern = "Stomping", replacement = "Stomp") %>%
      str_replace_all(pattern = "TouchLongObject", replacement = "TouchObject") %>%
      str_replace_all(pattern = "TouchLongOther", replacement = "Touch")
  }

  if (col_names == 'original') {
    colnames(data) = save.colnames
  }
  return(data)
}
