#Samples: 1) 10x10 armour array of adult scales (n = 1); 2) 10x10 armour array of mismatched adult SB with stripeling AP&DP (n = 1)

#Tests: for each sample model, three repetitive trials of bending and torsion tests were conducted along every scale row.

#Hypothesis: When scale morphology is controlled using mature scales, joint structures formed by elongated processes and pegs facilitate smoother motions between scales.

#What do we want to compare?
#Torsion test:
#1) Within each model: 
#a. Which scale row direction is stiffer? 
#b. Is it the same as bending test? (e.g., the model is the stiffest in both bending and torsion in certain direction, compared to other directions?)

#2) Across two models: 
#a. For same scale row direction, which model is stiffer? 
#b. Is the difference across scale rows consistent for both models (e.g., they both are the stiffest in certain direction, compared to other directions?)
#c. Is it the same as bending test? (e.g., the model is the stiffest in both bending and torsion in certain direction, compared to other directions?)




library(tidyverse) #Package bundle that includes packages we needed: dplyr, stringr, purrr, ggplot2 (can't believe ggplot2 is now part of tidyverse, it was not a few month ago...)
library(readxl) #Read excel sheets
library(janitor) #Simple tools for examining and cleaning dirty data

# Torsion Data organization ------------------------------------------------------
#Find all Excel files with "Torsion" in the file name
torsion_files <- list.files(
  path = ".",
  pattern = "Torsion.*\\.(xlsx|xls)$"
)

#Function to capture desired data columns from sub spreadsheet.
read_specimen_sheet <- function(file, sheet) {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, skip = 3) #read everything in the sub spreadsheet, except the first three rows with redundant column headings.
  
  raw <- raw[, 2:3] #Only keep the columns with desired data (the last two columns)
  
  names(raw) <- c("Torque_Nm", "Angle_Degree") #Rename the columns
  
  raw <- raw %>%
    mutate(
      Torque_Nm = as.numeric(Torque_Nm),
      Angle_Degree = as.numeric(Angle_Degree)
    ) %>% 
    filter(!is.na(Torque_Nm), !is.na(Angle_Degree))# convert values in the columns from strings to numeric (they are numbers but weirdly saved as texts)
  
  raw <- raw %>%
    mutate(
      File = basename(file),
      `Trial ID` = sheet
    )
}

#Function to read all desired sub spreadsheets from each Excel file at once, we only want data from those with "Specimen" in their names.
read_torsion_workbook <- function(file) {
  sheets <- excel_sheets(file) #List all sub spreadsheets from all Excel workbooks
  
  specimen_sheets <- sheets[str_detect(sheets, regex("Specimen"))] #We only pick those with "Specimen" in their names
  
  map_dfr(specimen_sheets, ~ read_specimen_sheet(file, .x)) #And then pull out the first two columns
}

#Use the functions above to combine all data into a large dataframe.
torsion_data <- map_dfr(torsion_files, read_torsion_workbook)

#There are some trials that need to be removed due to technical issues (e.g., wall started shaking and moved the machine since someone outside the lab pushed the floor entrance SO HARD!)
torsion_data <- torsion_data %>%
  filter(!(
    (File == "PolypArmor3DprintingTorsion_20260318.xls" & `Trial ID` %in% c("Specimen 1", "Specimen 5", "Specimen 6", "Specimen 11")) |
      (File == "PolypArmor3DprintingTorsion_20260319.xls" & `Trial ID` %in% c("Specimen 2", "Specimen 4", "Specimen 7"))
  ))

#We need to add experimental dates to each trials so we can further search and add more required info from "ArmourJoint_TrialLog.xlsx" to this dataframe.
torsion_data <- torsion_data %>%
  mutate(
    Date = as.Date(str_extract(File, "\\d{8}"), format = "%Y%m%d")
  )

#Based on Trial ID and Date, we will add important info such as Model ID and Condition from the trial log.
trial_log <- read_excel(
  "ArmourJoint_TrialLog.xlsx",
  sheet = "TorsionTest"
)

trial_log <- trial_log %>%
  select(Date, `Trial ID`, `Model ID`, Condition) #Keep only the columns needed for matching

torsion_data <- torsion_data %>%
  left_join(trial_log, by = c("Date", "Trial ID")) #Join onto torsion_data

torsion_data <- torsion_data %>%
  group_by(File, `Trial ID`) %>%
  mutate(Point_Order = row_number()) %>%
  ungroup() #Give each data point of each trial an unique order number, so they won't be mixed up when plotting.

torsion_data <- torsion_data %>%
  mutate(
    Condition = str_remove(Condition, "_61mm_45deg$")
  ) #Remove the "_61mm_45deg" part in the condition column, as they are the same for all trials.

torsion_data <- torsion_data %>%
  mutate(
    `Torsion direction` = str_extract(Condition, "CW|CCW"),
    `Torsion axis` = str_extract(Condition, "AntPost|Helical|JointlessHelical"),
  )

#For simplicity, we will change torsion axes to "Axis 1" "Axis 2" and "Axis 3"
torsion_data <- torsion_data %>%
  mutate(
    `Torsion axis` = recode(
      `Torsion axis`,
      "AntPost" = "Axis 1",
      "Helical" = "Axis 2",
      "JointlessHelical" = "Axis 3"
    )
  )

# Torsion test analysis ------------------------------------------------------------

#We use similar methods to analyze torsion test. First by calculating normalized stiffness (K_torsion) for each experimental trial. K_torsion is calculated as the slope of the loading curve.

#Keep only the loading curve of each torsion trial by keeping points up to the maximum torsion angles.
loading_data <- torsion_data %>%
  group_by(File, `Trial ID`, `Torsion direction`) %>%
  arrange(Point_Order, .by_group = TRUE) %>%
  mutate(
    abs_angle = abs(Angle_Degree), #Since values for CW torsion are recorded with positive sign but negative sign for CCW torsion. Signs here only describe direction but not value. So if we want to compare values, we need to remove the distraction of signs by finding the absolute values of torsion angles.
    d_abs_angle = abs_angle - lag(abs_angle),
    d_abs_angle = coalesce(d_abs_angle, 0),#Unlike how we isolate the loading curves for bending test where 1 bending cycle for each bending trial, there are 3 torsion cycles for each torsion trial, and we want to pick all three loading curve. So, instead of simply keeping the regions with torsion angle less than 45, we need to tell R that "there are three loading curves I want, and they are the regions that torsion angles are continuously moving father from zero (abs_angle keeps increasing), pick them out."
    loading = d_abs_angle > 0.001, #For loading curves, torsion angle of current point minus torsion angle of previous point (d_abs_angle) should be greater than 0.001 (remove any potential tiny noise on the loading curve). For those points with d_abs_angle >0.001, they will be marked as "TRUE".
    loading_start = loading & !lag(loading, default = FALSE), #start point of a loading curve: the moment that loading just starts, the previous point was not loading. For points that are counted as start point of a loading curve, they are marked as "TRUE".
    loading_cycle = cumsum(loading_start) #The points between two starting points are grouped as one torsion cycle (loading curves + unloading curves)
  ) %>%
  filter(loading) %>% #Then, for each torsion cycle, we only select those points with positive d_abs_angle.
  group_by(File, `Trial ID`, `Torsion direction`, loading_cycle) %>%
  mutate(n_in_cycle = n()) %>% #records number of value points for each loading curve, make sure they are similar 
  ungroup() %>%
  filter(n_in_cycle > 10) %>%   #tiny noises still exist and may be mistakenly counted as a torsion cycle. So we remove those torsion cycles with less than 10 points.
  group_by(File, `Trial ID`, `Torsion direction`) %>%
  mutate(cycle_in_direction = dense_rank(loading_cycle)) %>% #This gives order for each cycle for each torsion direction (e.g., cycle 1 of clockwise direction, cycle 2 of counter clockwise direction etc.)
  ungroup()

#We calculate slope for each loading curve by fitting it using a linear model and then find its slope.
trial_stiffness <- loading_data %>%
  group_by(File, `Trial ID`, Date, `Model ID`, Condition, `Torsion direction`, `Torsion axis`, cycle_in_direction) %>%
  
  #For each trial, we are doing the following things to calculate the slope:
  group_modify(~{
    dat <- .x
    fit <- lm(Torque_Nm ~ Angle_Degree, data = dat) #fit the loading curve to a straight line
    
    tibble(
      slope_N_per_mm = coef(fit)[2], #slope of the line
      intercept = coef(fit)[1], #intercept of the line
      r2 = summary(fit)$r.squared, #R^2
      n_points = nrow(dat) #Number of data points used in this fit
    )
  }) %>%
  ungroup()

#A quick plot of your calculation
#In terms of simplicity, we only illustrate the "treatment" data and remove the control group.
trial_stiffness_treatment <- trial_stiffness %>% 
  filter(!(`Model ID` == "Silicone_with_no_scales"))

ggplot(trial_stiffness_treatment,
       aes(x = `Torsion axis`, y = slope_N_per_mm, colour = `Model ID`)) +
  geom_jitter(width = 0.1, size = 5) +
  facet_wrap(~ `Torsion direction`) +
  scale_colour_manual(
    values = c(
      "Polyp2025-101_Loc1_ASB_AAP_ADP" = "#0072B2",
      "Polyp2025-101_Loc1_ASB_SAP_SDP" = "#D55E00"
    ),
    labels = c(
      "Polyp2025-101_Loc1_ASB_AAP_ADP" = "Adult scale body + Adult process and peg",
      "Polyp2025-101_Loc1_ASB_SAP_SDP" = "Adult scale body + Stripeling process and peg"
    )
  ) +
  theme_classic() +
  labs(
    x = "Torsion axis",
    y = "Stiffness (Nm/degree)",
    colour = "Model ID"
  )+
  guides(colour = guide_legend(override.aes = list(size = 5)))+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

#For the three repetitive trials along every scale row, we need to prove their consistency before comparing them with trials from other scale rows or other models. We can prove the consistency by calculating mean, SD, and coefficient of variations across the three repetitive trials.
stiffness_summary <- trial_stiffness_treatment %>%
  group_by(File, `Trial ID`, Date, `Model ID`, Condition, `Torsion direction`, `Torsion axis`) %>%
  summarise(
    mean_stiffness = mean(slope_N_per_mm, na.rm = TRUE),
    sd_stiffness   = sd(slope_N_per_mm, na.rm = TRUE),
    cv_stiffness = sd_stiffness/mean_stiffness,
    n              = n(),
    .groups = "drop"
  ) %>%
  arrange(`Model ID`, `Torsion axis`, `Torsion direction`)

#If we add the mean+/-SD values to the previous stiffness plot...
ggplot() +
  geom_jitter(
    data = trial_stiffness_treatment,
    aes(x = `Torsion axis`, y = slope_N_per_mm, colour = `Model ID`),
    width = 0.1,
    size = 2.5,
    alpha = 0.5
  ) +
  geom_point(
    data = stiffness_summary,
    aes(x = `Torsion axis`, y = mean_stiffness, colour = `Model ID`),
    position = position_dodge(width = 0.4),
    size = 5
  ) +
  geom_errorbar(
    data = stiffness_summary,
    aes(
      x = `Torsion axis`,
      y = mean_stiffness,
      ymin = mean_stiffness - sd_stiffness,
      ymax = mean_stiffness + sd_stiffness,
      colour = `Model ID`
    ),
    position = position_dodge(width = 0.4),
    width = 0.4,
    linewidth = 1
  ) +
  facet_wrap(~ `Torsion direction`) +
  scale_colour_manual(
    values = c(
      "Polyp2025-101_Loc1_ASB_AAP_ADP" = "#0072B2",
      "Polyp2025-101_Loc1_ASB_SAP_SDP" = "#D55E00"
    ),
    labels = c(
      "Polyp2025-101_Loc1_ASB_AAP_ADP" = "Adult scale",
      "Polyp2025-101_Loc1_ASB_SAP_SDP" = "Adult scale with stripeling joints"
    )
  ) +
  theme_classic() +
  labs(
    x = "Torsion axis",
    y = "Stiffness (Nm/degree)",
    colour = "Model ID"
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5)))+
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "none",
    strip.text = element_text(size = 20)
  )

#After proving data consistency, we are ready to compare the data.
#Torsion test:
#1) Within each model: 
#a. Which scale row direction is stiffer? 
#We can answer this question by the stiffness we just calculated, together with the torsion curves.
ggplot(
  torsion_data,
  aes(
    x = Angle_Degree,
    y = Torque_Nm,
    group = interaction(File, `Trial ID`),
    colour = `Torsion axis`
  )
) +
  geom_path(linewidth = 1, alpha = 1) +
  facet_grid(
    rows = vars(`Torsion direction`),
    cols = vars(`Model ID`),
    scales = "fixed",
    labeller = labeller(
      `Model ID` = c(
        "Polyp2025-101_Loc1_ASB_AAP_ADP" = "Adult scale body + Adult process and peg",
        "Polyp2025-101_Loc1_ASB_SAP_SDP" = "Adult scale body + Stripeling process and peg",
        "Silicone_with_no_scales" = "Silicone only"
      )
    )
  ) +
  scale_colour_manual(values = c(
    "AntPost" = "#CC79A7",
    "Helical" = "#E69F00",
    "JointlessHelical" = "#56B4E9"
  )) +
  theme_classic() +
  labs(
    x = "Angle (°)",
    y = "Torque (Nm)",
    colour = "Torsion axis"
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 5)))+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

#b. Is it the same as bending test? (e.g., the model is the stiffest in both bending and torsion in certain direction, compared to other directions?)


#2) Across two models: 
#a. For same scale row direction, which model is stiffer? 
#We can answer this question by the stiffness we just calculated, together with the bending curves.
#Plotting
ggplot(
  torsion_data,
  aes(
    x = Angle_Degree,
    y = Torque_Nm,
    group = interaction(File, `Trial ID`),
    colour = `Model ID`
  )
) +
  geom_path(linewidth = 1, alpha = 1) +
  facet_grid(
    rows = vars(`Torsion direction`),
    cols = vars(`Torsion axis`),
    scales = "fixed"
  ) +
  scale_colour_manual(
    values = c(
      "Polyp2025-101_Loc1_ASB_AAP_ADP" = "#0072B2",
      "Polyp2025-101_Loc1_ASB_SAP_SDP" = "#D55E00",
      "Silicone_with_no_scales"        = "#009E73"
    ),
    labels = c(
      "Polyp2025-101_Loc1_ASB_AAP_ADP" = "Adult scale body + Adult process and peg",
      "Polyp2025-101_Loc1_ASB_SAP_SDP" = "Adult scale body + Stripeling process and peg",
      "Silicone_with_no_scales"        = "Silicone only"
    )
  ) +
  theme_classic() +
  labs(
    x = "Angle (°)",
    y = "Torque (Nm)",
    colour = "Model Type"
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 5)))+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

#b. Is the difference across scale rows consistent for both models (e.g., they both are the stiffest in certain direction, compared to other directions?)


#c. Is it the same as bending test? (e.g., the model is the stiffest in both bending and torsion in certain direction, compared to other directions?)


