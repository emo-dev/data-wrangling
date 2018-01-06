library(tidyverse)

ach_profile <- read_csv("data/achievement_profile_data_with_CORE.csv")

## Exercise 1
# Use filter() to find the number of districts with a 100% Algebra I proficiency rate.
alg1_proficient_filter <- filter(ach_profile, AlgI == 100)
alg1_proficient_pipe <- ach_profile %>%
    filter(AlgI == 100)

View(alg1_proficient_filter)
View(alg1_proficient_pipe)

## Exercise 2
# Create a new variable called `math_achievement` with a value of:
#     * `"High"` if a district's Math proficiency is 75% or higher;
#     * `"Medium"` if a district's Math proficiency is between 50% and 75%;
#     * `"Low"` if a district's Math proficiency is below 50%.
View(mutate(
    ach_profile,
   math_achievement = if_else(Math >= 75, "High", NA_character_),
   math_achievement = if_else(Math >= 50, "Medium", math_achievement),
   math_achievement = if_else(Math < 50, "Low", math_achievement)
))

View(mutate(
    ach_profile,
    math_achievement = case_when(
        Math >= 75 ~ "High",
        Math >= 50 ~ "Medium",
        Math < 50 ~ "Low",
        TRUE ~ "No Data"
    )
) %>% select(Math, math_achievement))


## Exercise 3
# Filter down to district 792 (Shelby County), then pipe the result to `View()`.
View(
    ach_profile %>%
        filter(system == 792)
)

ach_profile %>%
    filter(system == 792) %>%
    View()

## Exercise 4
# Do the following in one sequence of function calls, piped together:
# 1. Read in the `data/tvaas.csv` file.
# 2. Rename variables as follows:
#     * `District Name` to `system`.
#     * `District-Wide: Composite` to `TVAAS Composite`.
#     * `District-Wide: Literacy` to `TVAAS Literacy`.
#     * `District-Wide: Numeracy` to `TVAAS Numeracy`.
# 3. Drop the `District Name` variable.

tvaas <-
    read_csv("data/tvaas.csv") %>%
    rename(
        system = `District Number`,
        `TVAAS Composite` = `District-Wide: Composite`,
        `TVAAS Literacy` = `District-Wide: Literacy`,
        `TVAAS Numeracy` = `District-Wide: Numeracy`
    ) %>%
    select(-`District Name`)

## Exercise 5
# Sort alphabetically by CORE region, then by Algebra I proficiency in descending order.
# Then, keep just the district name, Algebra I proficiency, and CORE region variables.
View(
    ach_profile %>%
        arrange(CORE_region, desc(AlgI)) %>%
        select(system_name, AlgI, CORE_region)
)

## Exercise 6
# Use `summarise()` to find the mean, minimum, and maximum district grad rate.
# Assign variable names to the resulting data frame.
View(
    ach_profile %>%
        filter(!is.na(Graduation)) %>%
        summarise(
            mean_math = mean(Graduation),
            min_math = min(Graduation),
            max_math = max(Graduation)
        )
)

## Exercise 7
# Identify districts with a higher Percent ED than the median district, and a
# higher Math proficiency than the median district.
median_Pct_ED <- median(ach_profile$Pct_ED, na.rm = TRUE)
median_Math <- median(ach_profile$Math, na.rm = TRUE)
View(
    ach_profile %>%
        filter(Pct_ED > median_Pct_ED & Math > median_Math) %>%
        select(system_name, Pct_ED, Math)
)


# Exercise 8
# Identify districts with a higher dropout rate than the average of districts
# in the same CORE Region.
View(
    ach_profile %>%
        filter(!is.na(Dropout)) %>%
        group_by(CORE_region) %>%
        mutate(Dropout_Mean = mean(Dropout)) %>%
        ungroup() %>%
        filter(Dropout > Dropout_Mean) %>%
        select(system_name, CORE_region, Dropout, Dropout_Mean)
)

## Exercise 9
# Calculate three variables:
# * A district's average proficiency in math subjects (Math, Algebra I-II)
# * A district's average proficiency in English subjects (ELA, English I-III)
# * A district's average proficiency in science subjects (Science, Biology I, Chemistry)
# Then, reorder variables such that:
# * The math average is next to the individual math variables.
# * The English average is next to the individual English variables.
# * The science average is next to the individual science variables.
View(
    ach_profile %>%
        rowwise() %>%
        mutate(
            Math_Mean = mean(c(Math, AlgI, AlgII), na.rm = TRUE),
            English_Mean = mean(c(EngI, EngII, EngIII), na.rm = TRUE),
            Science_Mean = mean(c(Science, BioI, Chemistry), na.rm = TRUE)
        ) %>%
        select(
            system,
            Math_Mean, Math, starts_with("Alg"),
            English_Mean, starts_with("Eng"),
            Science_Mean, Science, BioI, Chemistry,
            everything()
        )
)

## Exercise 10
# Create a data frame with the number of districts at each TVAAS level, by CORE region.
View(
    inner_join(ach_profile, tvaas, by='system') %>%
        group_by(CORE_region, `TVAAS Composite`) %>%
        count() %>%
        ungroup()
)

## Exercise 11
# Reshape the `tvaas` data frame long by subject, then arrange by system.
View(
    tvaas %>%
        gather(subject, level, starts_with("TVAAS")) %>%
        arrange(system)
)

