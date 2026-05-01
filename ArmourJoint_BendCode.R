#Samples: 1) 10x10 armour array of adult scales (n = 1); 2) 10x10 armour array of mismatched adult SB with stripeling AP&DP (n = 1)

#Tests: for each sample model, three repetitive trials of bending and torsion tests were conducted along every scale row.

#Hypothesis: When scale morphology is controlled using mature scales, joint structures formed by elongated processes and pegs facilitate smoother motions between scales.

#What do we want to compare?
#Bending test:
#1) Within each model: 
#a. Which scale row direction is stiffer?

#2) Across two models: 
#a. For same scale row direction, which model is stiffer? 
#b. Is the difference across scale rows consistent for both models? (e.g., they both are the stiffest in certain direction, compared to other directions?)





library(tidyverse) #Package bundle that includes packages we needed: dplyr, stringr, purrr, ggplot2 (can't believe ggplot2 is now part of tidyverse, it was not a few month ago...)
library(readxl) #Read excel sheets
library(janitor) #Simple tools for examining and cleaning dirty data

# Bending Data organization ------------------------------------------------------
#Find all Excel files with "Bending" in the file name
bending_files <- list.files(
  path = ".",
  pattern = "Bending.*\\.(xlsx|xls)$"
)

#Function to capture desired data columns from sub spreadsheet.
read_specimen_sheet <- function(file, sheet) {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, skip = 3) #read everything in the sub spreadsheet, except the first three rows with redundant column headings.
  
  raw <- raw[, 1:2] #Only keep the columns with data (the first two columns)
  
  names(raw) <- c("Displacement_mm", "Force_N") #Rename the columns
  
  raw <- raw %>%
    mutate(
      Displacement_mm = as.numeric(Displacement_mm),
      Force_N = as.numeric(Force_N)
    ) %>% 
    filter(!is.na(Displacement_mm), !is.na(Force_N))# convert values in the columns from strings to numeric (they are numbers but weirdly saved as texts)
  
  raw <- raw %>%
    mutate(
      File = basename(file),
      `Trial ID` = sheet
    )
}

#Function to read all desired sub spreadsheets from each Excel file at once, we only want data from those with "Specimen" in their names.
read_bending_workbook <- function(file) {
  sheets <- excel_sheets(file) #List all sub spreadsheets from all Excel workbooks
  
  specimen_sheets <- sheets[str_detect(sheets, regex("Specimen"))] #We only pick those with "Specimen" in their names
  
  map_dfr(specimen_sheets, ~ read_specimen_sheet(file, .x)) #And then pull out the first two columns
}

#Use the functions above to combine all data into a large dataframe.
bending_data <- map_dfr(bending_files, read_bending_workbook)

#There are some trials that need to be removed due to technical issues (e.g., wall started shaking and moved the machine since someone outside the lab pushed the floor entrance SO HARD!)
bending_data <- bending_data %>%
  filter(!(
    (File == "PolypArmor3Dprinting3ptBending_20260318.xls" & `Trial ID` %in% c("Specimen 1", "Specimen 9", "Specimen 28")) |
      (File == "PolypArmor3Dprinting3ptBending_20260319.xls" & `Trial ID` %in% c("Specimen 1", "Specimen 13", "Specimen 16", "Specimen 21"))
  ))

#We need to add experimental dates to each trials so we can further search and add more required info from "ArmourJoint_TrialLog.xlsx" to this dataframe.
bending_data <- bending_data %>%
  mutate(
    Date = as.Date(str_extract(File, "\\d{8}"), format = "%Y%m%d")
  )

#Based on Trial ID and Date, we will add important info such as Model ID and Condition from the trial log.
trial_log <- read_excel(
  "ArmourJoint_TrialLog.xlsx",
  sheet = "BendingTest"
)

trial_log <- trial_log %>%
  select(Date, `Trial ID`, `Model ID`, Condition) #Keep only the columns needed for matching

bending_data <- bending_data %>%
  left_join(trial_log, by = c("Date", "Trial ID")) #Join onto bending_data

bending_data <- bending_data %>%
  group_by(File, `Trial ID`) %>%
  mutate(Point_Order = row_number()) %>%
  ungroup() #Give each data point of each trial an unique order number, so they won't be mixed up when plotting.

bending_data <- bending_data %>%
  mutate(
    Condition = str_remove(Condition, "_61mm_15mm$")
  ) #Remove the "_61mm_15mm" part in the condition column, as they are the same for all trials.

bending_data <- bending_data %>%
  mutate(
    `Bending direction` = str_extract(Condition, "Concave|Convex|NA"),
    `Bending axis` = str_extract(Condition, "AntPost|Helical|JointlessHelical"),
  )

#Change the bending direction to "Compression", "Tension" and "NA" instead of "Convex" and "Concave" to avoid confusion.
bending_data <- bending_data %>%
  mutate(
    `Bending direction` = recode(
      `Bending direction`,
      "Convex" = "Compression",
      "Concave" = "Tension"
    )
  )

#Change the axes name to "Axis 1" "Axis 2" "Axis 3" to avoid complex jargons.
bending_data <- bending_data %>%
  mutate(
    `Bending axis` = recode(
      `Bending axis`,
      "AntPost" = "Axis 1",
      "Helical" = "Axis 2",
      "JointlessHelical" = "Axis 3"
    )
  )

# Bending test analysis ------------------------------------------------------------
#We start analyzing bending test first, by calculating normalized stiffness (K_bending) for each experimental trial. Defined in Zolotovsky et al. (2021), K_bending is calculated as the slope of the loading curve.

#Keep only the loading curve of each bending trial by keeping points up to the first maximum displacement.
loading_data <- bending_data %>%
  group_by(File, `Trial ID`) %>%
  arrange(Point_Order, .by_group = TRUE) %>%
  mutate(max_displacement = max(Displacement_mm, na.rm = TRUE), #find the largest displacement value in the trial
         peak_index = which.max(Displacement_mm)[1]) %>% #the UTM stops at the largest displacement for a few seconds, we only pick the moment that the largest displacement just happens.
  filter(Point_Order <= peak_index) %>% #For each trial, we only keep the rows from the start to where the largest displacement first reaches.
  ungroup()

#We calculate slope for each trial by fitting the loading curve using a linear model and then find its slope from the middle 10% to 90% region (there could be artifacts at both ends of the curve so we don't count them in).
trial_stiffness <- loading_data %>%
  group_by(File, `Trial ID`, Date, `Model ID`, Condition, `Bending direction`, `Bending axis`) %>%
  
  #For each trial, we are doing the following things to calculate the slope:
  group_modify(~{
    dat <- .x
    
    dmin <- min(dat$Displacement_mm, na.rm = TRUE) #find the smallest displacement in the trial
    dmax <- max(dat$Displacement_mm, na.rm = TRUE) #find the largest displacement in the trial
    
    dat_region <- dat %>%
      filter(
        Displacement_mm >= dmin + 0.1 * (dmax - dmin), #remove the first 10% of displacement
        Displacement_mm <= dmin + 0.9 * (dmax - dmin) #remove the last 10% of displacement
      )
    
    fit <- lm(Force_N ~ Displacement_mm, data = dat_region) #fit the loading curve to a straight line
    
    tibble(
      slope_N_per_mm = coef(fit)[2], #slope of the line
      intercept = coef(fit)[1], #intercept of the line
      r2 = summary(fit)$r.squared, #R^2
      n_points = nrow(dat_region) #Number of data points used in this fit
    )
  }) %>%
  ungroup()


#A quick plot of your calculation
#In terms of simplicity, we only illustrate the "treatment" data and remove the control group.
trial_stiffness_treatment <- trial_stiffness %>% 
  filter(!(`Model ID` == "Silicone_with_no_scales"))

ggplot(trial_stiffness_treatment,
       aes(x = `Bending axis`, y = slope_N_per_mm, colour = `Model ID`)) +
  geom_jitter(width = 0.1, size = 5) +
  facet_wrap(~ `Bending direction`) +
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
    x = "Bending axis",
    y = "Stiffness (N/mm)",
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
  group_by(`Model ID`, Condition, `Bending direction`, `Bending axis`) %>%
  summarise(
    mean_stiffness = mean(slope_N_per_mm, na.rm = TRUE),
    sd_stiffness   = sd(slope_N_per_mm, na.rm = TRUE),
    cv_stiffness = sd_stiffness/mean_stiffness,
    n              = n(),
    .groups = "drop"
  ) %>%
  arrange(`Model ID`, `Bending axis`, `Bending direction`)

#If we add the mean+/-SD values to the previous stiffness plot...
ggplot() +
  geom_jitter(
    data = trial_stiffness_treatment,
    aes(x = `Bending axis`, y = slope_N_per_mm, colour = `Model ID`),
    width = 0.1,
    size = 2.5,
    alpha = 0.5
  ) +
  geom_point(
    data = stiffness_summary,
    aes(x = `Bending axis`, y = mean_stiffness, colour = `Model ID`),
    position = position_dodge(width = 0.4),
    size = 5
  ) +
  geom_errorbar(
    data = stiffness_summary,
    aes(
      x = `Bending axis`,
      y = mean_stiffness,
      ymin = mean_stiffness - sd_stiffness,
      ymax = mean_stiffness + sd_stiffness,
      colour = `Model ID`
    ),
    position = position_dodge(width = 0.4),
    width = 0.4,
    linewidth = 1
  ) +
  facet_wrap(~ `Bending direction`) +
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
    x = "Bending axis",
    y = "Stiffness (N/mm)",
    colour = "Model ID"
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5)))+
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    strip.text = element_text(size = 20)
  )

#After proving data consistency, we are ready to compare the data.
#1) Within each model: 
#a. Which scale row direction is stiffer?
#We can answer this question by the stiffness we just calculated, together with the bending curves.
ggplot(
  bending_data,
  aes(
    x = Displacement_mm,
    y = Force_N,
    group = interaction(File, `Trial ID`),
    colour = `Bending axis`
  )
) +
  geom_path(linewidth = 1, alpha = 1) +
  facet_grid(
    rows = vars(`Bending direction`),
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
    x = "Displacement (mm)",
    y = "Force (N)",
    colour = "Bending axis"
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 5)))+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

#2) Across two models: 
#a. For same scale row direction, which model is stiffer? 
#We can answer this question by the stiffness we just calculated, together with the bending curves.
#Plotting
ggplot(
  bending_data,
  aes(
    x = Displacement_mm,
    y = Force_N,
    group = interaction(File, `Trial ID`),
    colour = `Model ID`
  )
) +
  geom_path(linewidth = 1, alpha = 1) +
  facet_grid(
    rows = vars(`Bending direction`),
    cols = vars(`Bending axis`),
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
    x = "Displacement (mm)",
    y = "Force (N)",
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

#b. Is the difference across scale rows consistent for both models? (e.g., they both are the stiffest in certain direction, compared to other directions?)


