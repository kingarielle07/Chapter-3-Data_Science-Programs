install.packages("readxl")
library(readxl)
library(dplyr)
library(stringr)

                 # Replace "path/to/your/file.xlsx" with the actual path to your Excel file
HBCU_master <- read_excel("C:/Users/anjones2/Documents/HBCU_data_programs_project/HBCU_data_programs.backup-20251118-113227.xlsx")

new_row <- read.csv("C:/Users/anjones2/Documents/HBCU_data_programs_project/incoming/staug.csv")
setequal(unique_values, unique_name)
setdiff(colnames(HBCU_master), colnames(new_row))
HBCU_master <- HBCU_master %>%
  rename(url_main = url_mail)

# Append rows from df2 to df1
HBCU_master <- rbind(HBCU_master, new_row)
#count institutions in df
HBCU_master %>%
  count(institution_name)

# Get unique values from 'my_column'
unique_values <- unique(HBCU_master$institution_name)
unique_name <- unique(hbcu_url$name)


#see if institutions are in diff dataframes
unique_name %in% HBCU_merged$institution_name

anti <- setdiff(unique_name, unique_values)
anti2 <- setdiff(unique_values, unique_name)

#which ones do not match
missing_1 <- unique_name[!(unique_name %in% unique_values)]
missing_2 <- unique_values[!(unique_values %in% unique_name)]
#combine and keep only unique elements
combined_unique <- unique(c(missing_1, missing_2))

HBCU_merged_ordered[]

which(hbcu_url_df$name == "Florida Agricultural and Mechanical University")
hbcu_url_df[16, 2] <- "North Carolina A&T State University"
hbcu_url_df[42, 2] <- "Alabama A&M University"
hbcu_url_df[19, 2] <- "University of Maryland Eastern Shore"
hbcu_url_df[45, 2] <- "Southern University and A&M College"
hbcu_url_df[63, 2] <- "Saint Augustine’s University"
hbcu_url_df[52, 2] <- "Florida A&M University"
hbcu_url_df[22, 2] <- "Lincoln University (MO)"
hbcu_url_df[14, 2] <- "Lincoln University (PA)"
HBCU_master[70, 1] <- "Lincoln University (PA)"
all.equal(HBCU_master$institution_name[168],hbcu_url_df$name[63])
hbcu_url <- hbcu_url_df %>% select(orgId, name, city, state, stateName,
                                   schoolSetting,schoolTypeByDesignation, schoolTypeByYears,
                                   schoolSize, averageNetPrice,graduationRate,url)
# Merge on user_id vs id, keep all columns
HBCU_merged <- merge(HBCU_master, hbcu_url, by.x = "institution_name", by.y = "name", all = TRUE)
#save changes to dfs
saveRDS(list(hbcu_url = hbcu_url, hbcu_url_df = hbcu_url_df, HBCU_master = HBCU_master,
             HBCU_merged_ordered=HBCU_merged, NA_program = NA_program, 
             titles_df= titles_df,degree_level=degree_level), "all_dfs.rds")
dfs <- readRDS("all_dfs.rds")
names(dfs)
hbcu_url       <- dfs[["hbcu_url"]]
hbcu_url_df    <- dfs[["hbcu_url_df"]]
HBCU_master    <- dfs[["HBCU_master"]]
HBCU_merged_ordered  <- dfs[["HBCU_merged"]]
degree_level <- dfs$degree_level
titles_df <- dfs$titles_df

saveRDS(HBCU_master, file = "HBCU_programs2_1.rds")
saveRDS(hbcu_df, file = "HBCU_names.rds")
saveRDS(df, file = "orig_ds_programs.rds")
write.csv(HBCU_master, file="HBCU_master.csv", row.names = FALSE)
write.csv(hbcu_df, file="HBCU_names.csv", row.names = FALSE)
write.csv(df, file="orig_ds_programs.csv", row.names = FALSE)

HBCU_merged_ordered  <- dfs$HBCU_merged_ordered
library(dplyr)

HBCU_merged_ordered <- HBCU_merged_ordered %>%
  select(-averageNetPrice, -graduationRate, -schoolSize)
#count # of na Cells in column
sum(is.na(HBCU_merged_ordered$city.y))
na_cols <- HBCU_merged_ordered[is.na(HBCU_merged_ordered$city.y),]
#paste city.x values in na_cols of city.y
HBCU_merged_ordered$city.y[is.na(HBCU_merged_ordered$city.y)] <-
  HBCU_merged_ordered$city.x[is.na(HBCU_merged_ordered$city.y)]
names(HBCU_merged_ordered)

HBCU_merged_ordered[c(66, 67, 77, 86, 90, 91, 92, 126, 127, 160),"state" ] 
rows <- c(66, 67, 77, 86, 90, 91, 92, 126, 127, 160)

states_to_paste <- c("AL", "AL", "AL", "TN", "TN", "TN", "TN", "AR", "KY", "DC")

HBCU_merged_ordered$state[rows] <- states_to_paste
library(dplyr)
library(stringr)

HBCU_merged_ordered <- HBCU_merged_ordered %>%
  mutate(city.y = str_replace(city.y, ",.*", ""))


#standardize columns year status
year_status_df <- HBCU_merged_ordered %>%
  select(institution_name, year_status,schoolTypeByYears)

HBCU_merged_ordered <- HBCU_merged_ordered %>%
  mutate(
    year_status = case_when(
      is.na(year_status) & str_detect(schoolTypeByYears, "2") ~ "2 year",
      is.na(year_status) & str_detect(schoolTypeByYears, "4") ~ "4 year",
      TRUE ~ year_status
    )
  )

sum(is.na(HBCU_merged_ordered$year_status))
#identify institutions that have or dont have data_science program
HBCU_merged_ordered <- HBCU_merged_ordered %>%
  mutate(
    data_program = if_else(
      !is.na(program_title) &
        !is.na(degree_level) &
        str_detect(
          str_to_lower(paste(program_title, keywords, sep = " ")),
          str_c(data_keywords, collapse = "|")
        ),
      "Yes",
      "No"
    )
  )



#Identify unique institutions and number
unique_names <- US_df %>% 
  distinct(school)
write.csv(unique_names, "unique_names.csv", row.names = FALSE)


#get unique institution names and values (exclude NA's)
length(unique(na.omit(HBCU_merged_ordered$institution_name)))

HBCU_merged_ordered %>%
  count(institution_name, sort = TRUE)

#Identify R2 HBCUs

library(dplyr)

HBCU_merged_ordered %>%
  filter(research_activity == "R2:High Spending and Doctorate Production") %>%
  distinct(institution_name) %>%
  pull(institution_name)

HBCU_merged_ordered %>%
  filter(research_activity == "R2:High Spending and Doctorate Production") %>%
  summarise(n = n_distinct(institution_name))


#Which HBCU's appear more than once
library(dplyr)

Hbcu_count <- HBCU_merged %>%
  count(institution_name) %>%
 # filter(n > 1) %>%
  arrange(desc(n))

HBCU_merged_ordered <- HBCU_merged %>%
  add_count(institution_name, name = "inst_count") %>%
  arrange(desc(inst_count), institution_name)

#add column for further degree level grouping  
 library(stringr) 
degree_level <- HBCU_merged_ordered %>%
  mutate(
    degree_group = case_when(
      str_detect(tolower(degree_level), "minor") ~ "undergraduate",
      str_detect(tolower(degree_level), "bachelor") ~ "bachelor",
      str_detect(tolower(degree_level), "master") ~ "master",
      str_detect(tolower(degree_level), "phd|doctor") ~ "doctoral",
      str_detect(tolower(degree_level), "certificate") ~ "certificate",
      TRUE ~ "other"
    )
  )


 
HBCU_merged_ordered <- degree_level
#get unique values and counts of program levels
HBCU_merged_ordered %>%
  count(degree_group) %>% 
  arrange(desc(n))

othr_deg <- HBCU_merged_ordered %>%
  filter(degree_group == "other") %>%
 distinct(degree_level)

#How to get proportion of a year/status
HBCU_merged_ordered %>%
  distinct(institution_name, .keep_all = TRUE) %>%
  pull(year_status) %>%
  table()

HBCU_merged_ordered %>%
  summarise(
    na_program_title = sum(is.na(program_title))
  )

#Rows with NA in program title
other_filt <- HBCU_merged_ordered %>%
  filter(is.na(program_title))

NA_program <- other_filt
#write to csv file
write.csv(degree_level, "degree_ordered.csv", row.names = FALSE)
write.csv(hbcu_prgs_R2, "hbcu_prgs.csv", row.names = FALSE)

getwd()


key_row <- which(!is.na(HBCU_merged$city.x)& is.na(HBCU_merged$city.y))
HBCU_merged[key_row] %>%
  filter(!is.na(`city.x`) & is.na(`city.y`))


HBCU_merged[key_row,"institution_name"]
na_cols <- is.na(HBCU_merged[122, ])
HBCU_merged[122, na_cols] <- HBCU_merged[121, na_cols]

HBCU_merged <- HBCU_merged %>% slice(-121)


#word frequency in program titles
#1clean analytical subset
library(dplyr)

titles_df <- HBCU_merged %>%
  filter(!is.na(program_title)) %>%
  select(
    institution_name,
    keywords,
    college,
    department,
    program_title,
    degree_level,
    city.y,
    state,
    research_activity,
    schoolTypeByYears,
    schoolSize,
    graduationRate
  )

#tokenize program titles (unigrams first)
#each row = one word from one program title.
library(tidytext)
library(stringr)

title_words <- titles_df %>%
  mutate(program_title = str_to_lower(program_title)) %>%
  unnest_tokens(word, program_title)

#3. Remove stopwords and domain-trivial terms

data("stop_words")



custom_stopwords <- tibble(word = c(
  "program", "degree", "science",
  "studies", "concentration", "track",
  "major", "minor", "certificate"
))

title_words_clean <- title_words %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stopwords, by = "word")

  #4. Compute most frequent words (core result)
word_freq <- title_words_clean %>%
  count(word, sort = TRUE)
#Bar plot (recommended over word cloud)
library(ggplot2)

bar_3 <- word_freq %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Most Frequent Terms in Data-Intensive Program Titles",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()
# Save the plot to a file
ggsave("bar_3.png", plot = bar_3, width = 7, height = 4, units = "in", dpi = 300)
# This will save 'my_scatterplot.png' in your current working directory.
#Create bigrams from program titles
library(tidytext)
library(stringr)
library(tidyr)
library(ggplot2)
bigrams <- titles_df %>%
  filter(!is.na(program_title)) %>%
  mutate(program_title = str_to_lower(program_title)) %>%
  unnest_tokens(bigram, program_title, token = "ngrams", n = 2)

#seperate and clean bigrams
bigrams_separated <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

bigrams_clean <- bigrams_separated %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word,
    !word1 %in% custom_stopwords$word,
    !word2 %in% custom_stopwords$word
  ) %>%
  unite(bigram, word1, word2, sep = " ")
#bigram frequency list
bigram_freq <- bigrams_clean %>%
  count(bigram, sort = TRUE)

bigram_freq %>% slice_max(n, n = 20)

bar_4 <- bigram_freq %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Most Frequent bigrams in Data-Intensive Program Titles",
    x = "Words",
    y = "Frequency"
  ) +
  theme_minimal()
ggsave("bar_4.png", plot = bar_4, width = 7, height = 4, units = "in", dpi = 300)
##February 16
library(readr)
#remove row 104+
df <- df[1:103, ]
R1R2_prgs <- R1R2_prgs[1:103,]
write.csv(R1R2_prgs, file="R1_program_compile.csv", row.names = FALSE)
hbcu_prgs <- read_csv("filtered_HBCU_master.csv")
write.csv(hbcu_prgs_filt, file="filt_hbcu_data_list2_17.csv", row.names = FALSE)
R1R2_prgs <- read_csv("R1_program_compile.csv")
Carn_R1R2 <- read_csv("ace-institutional-classifications_updated.csv")
write.csv(di_prgs, file="DI_US_program.csv", row.names = FALSE)
write.csv(di_prgs_filt, file="DI_degreegrant_program.csv", row.names = FALSE)
write.csv(di_prgs_filt_R1R2, file="DI_R1R2_program.csv", row.names = FALSE) #2/23/26
colnames(di_prgs_filt_R1R2)
colnames(hbcu_std)
Carn_R1R2 <- Carn_R1R2 %>% 
  select(unitid,name,city,state,`Research Activity Designation`)

di_prgs_filt_R1R2 <- di_prgs_filt %>%
  filter(research_level %in% c("R1", "R2"))

#standardize the colnames for hbcu list
hbcu_std <- hbcu_prgs_filt %>%
  rename(
    cur_url      = curriculum_url,
    credit_hrs   = total_credit_hours,
    research_level = research_activity
  )

# Split city into city and state
library(dplyr)
library(tidyr)

hbcu_std <- hbcu_std %>%
  separate(city, into = c("city", "state"), sep = ",\\s*")

#remove columns "keyword_id" and "keyword_title" in hbcu_std
hbcu_std <- hbcu_std %>%
  select(-keyword_id, -keyword_title)


hbcu_std <- hbcu_std %>%
  mutate(
    research_level = case_when(
      research_level == "R2:High Spending and Doctorate Production" ~ "R2",
      research_level == "Research Colleges and Universities (RCU)" ~ "RCU",
      TRUE ~ "Other"
    )
  )

library(dplyr)

hbcu_std <- hbcu_std %>%
  mutate(
    research_level = if_else(
      institution_name == "Howard University",
      "R1",
      research_level
    )
  )

#standardize degree_level
library(stringr)
clean_degree <- function(x) {
  x_lower <- tolower(x)
  
  dplyr::case_when(
    # Bachelors
    stringr::str_detect(x_lower, "bachelor") |
      stringr::str_detect(x_lower, "\\bbs\\b") |
      stringr::str_detect(x_lower, "\\bba\\b") ~ "Bachelors",
    
    # Masters
    stringr::str_detect(x_lower, "master") |
      stringr::str_detect(x_lower, "\\bms\\b") |
      stringr::str_detect(x_lower, "\\bma\\b") |
      stringr::str_detect(x_lower, "\\bmba\\b") ~ "Masters",
    
    # Doctoral
    stringr::str_detect(x_lower, "phd") |
      stringr::str_detect(x_lower, "doctoral") |
      stringr::str_detect(x_lower, "doctorate") |
      stringr::str_detect(x_lower, "\\bdsc\\b") |
      stringr::str_detect(x_lower, "\\bscd\\b") ~ "Doctoral",
    
    # Certificate
    stringr::str_detect(x_lower, "certificate") ~ "Certificate",
    
    # Default: keep original
    TRUE ~ x
  )
}

hbcu_std <- hbcu_std %>% mutate(degree_level = clean_degree(degree_level))
r12_std  <- r12_std  %>% mutate(degree_level = clean_degree(degree_level))

hbcu_std$pop_serve <- "HBCU"
hbcu_std$MSI_id <- "Yes"
hbcu_std <- hbcu_std %>%
  select(-url_main, -year_status)

#check column names
setdiff(names(hbcu_std), names(r12_std))
setdiff(names(r12_std), names(hbcu_std))

#bindrows
library(dplyr)

di_prgs <- bind_rows(hbcu_std, r12_std)


library(dplyr)

#unique 
hbcu_prgs_filt %>%
  distinct(institution_name) %>%
  pull(institution_name)
#69

#unique 
R1R2_prgs %>%
  distinct(institution_name) %>%
  pull(institution_name)
#25
library(dplyr)

R1_msi <- R1R2_prgs %>%
  filter(research_level == "R1",
         MSI_id == "no")

R2_msi <- R1R2_prgs %>%
  filter(research_level == "R2",
         MSI_id == "no")

#10 random rows
write.csv(samp_hbcu_std, file="samp_hbcuprgs.csv", row.names = FALSE)
write.csv(samp_R12, file="samp_r12.csv", row.names = FALSE)
write.csv(samp_di, file="samp_di_prgs.csv", row.names = FALSE)
library(dplyr)

samp_hbcu_std <- hbcu_std %>%
  slice_sample(n = 10)

samp_R12 <- R1R2_prgs %>%
  slice_sample(n = 10)
samp_di <- di_prgs %>%
  slice_sample(n = 10)
#Filter rows where keyword_id == 1 AND degree_level contains bachelor/master/doctor

library(dplyr)
library(stringr)

hbcu_prgs_filt <- hbcu_prgs %>%
  filter(
    keyword_title == TRUE,
    str_detect(degree_level, regex("bachelor|master|doctor", ignore_case = TRUE))
  )
#✅ Show selected columns from hbcu_prgs
library(dplyr)

snap_hbcu_prgs_filt <- hbcu_prgs_filt %>%
  filter(keyword_title == TRUE) %>%
  select(institution_name, program_title, keyword_title)





hbcu_prgs %>%
  select(institution_name, program_title, keyword_title)library(dplyr)
#Feb 18

library(dplyr)
R1R2_prgs <- R1R2_prgs %>%
  mutate(institution_name = if_else(
    institution_name == "UMD",
    "University of Maryland",
    institution_name
  ))

R1R2_prgs <- R1R2_prgs %>%
  mutate(institution_name = if_else(
    institution_name == "CMU",
    "Carnegie Mellon University",
    institution_name
  ))



#Create keyword_title column based on keywords in program_title
library(dplyr)
library(stringr)

hbcu_prgs <- hbcu_prgs %>%
  mutate(
    keyword_title = str_detect(
      program_title,
      regex("machine learning|analytics|informatics|artificial intelligence|data science|data", ignore_case = TRUE)
    )
  )


library(dplyr)
library(stringr)

hbcu_prgs <- hbcu_prgs %>%
  mutate(
    keyword_title = str_detect(
      program_title,
      regex("machine learning|analytics|informatics|artificial intelligence|data science|data", ignore_case = TRUE)
    )
  )

#hbcu column names
colnames(di_prgs_filt_R1R2)
#R1R1 colnames
colnames(R1R2_prgs)

di_prgs_filt_R1R2 %>%
  distinct(institution_name) %>%
  pull(institution_name)

#identify duplicates rows
library(dplyr)

R1R2_prgs %>%
  count(across(everything())) %>%
  filter(n > 1)

dup <- R1R2_prgs %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE))
#keep first occurence of duplicate rows


R1R2_prgs <- R1R2_prgs %>%
  distinct(.keep_all = TRUE)

#13
hbcu_prgs_R2 <- hbcu_prgs %>%
  filter(research_activity == "R2:High Spending and Doctorate Production")

library(dplyr)
library(stringr)
#remove cert/course rows
hbcu_prgs_R2 <- hbcu_prgs_R2 %>%
  #filter(!str_detect(degree_level, regex("minor|unspecified|course|certificate", ignore_case = TRUE)))
  filter(!str_detect(degree_level, regex("associate|nexus", ignore_case = TRUE)))
#Feb 18 update
library(dplyr)

di_prgs_filt <- di_prgs %>%
  filter(degree_level %in% c("Bachelors", "Masters", "Doctoral"))

library(dplyr)

di_prgs_filt_R1R2 %>%
  count(degree_level) %>%
  arrange(desc(n))

library(dplyr)

di_not_R1R2 <- di_prgs_filt %>%
  filter(!research_level %in% c("R1", "R2"))

#### Create department list
unique_departments <- sort(unique(di_prgs_filt$department))
unique_departments

library(dplyr)
library(stringr)

classify_department <- function(dept) {
  d <- str_to_lower(dept)
  
  case_when(
    # Computer Science / Computing / Information
    str_detect(d, "computer|computing|information|informatics|it|technology|software|cis|cise") ~ "Computing / Information Science",
    
    # Engineering
    str_detect(d, "engineering|aerospace|mechanical|electrical|civil|industrial") ~ "Engineering",
    
    # Mathematics / Statistics / Quantitative
    str_detect(d, "math|statistic|quantitative|actuarial") ~ "Mathematics / Statistics",
    
    # Business / Management / Operations
    str_detect(d, "business|management|marketing|accounting|finance|operations|decision|mis|irom") ~ "Business / Management",
    
    # Health / Biomedical / Bioinformatics
    str_detect(d, "bioinformatics|biomedical|health|nursing|clinical|public health") ~ "Health / Biomedical",
    
    # Interdisciplinary / Data Science
    str_detect(d, "data science|interdisciplinary|institute|center|ai|machine learning|augmented intelligence") ~ "Data Science / Interdisciplinary",
    
    # Natural Sciences
    str_detect(d, "biology|physics|chemistry|science") ~ "Natural Sciences",
    
    TRUE ~ "Other / Unknown"
  )
}

di_prgs_filt <- di_prgs_filt %>%
  mutate(department_class = classify_department(department))
library(dplyr)
library(ggplot2)


di_prgs_filt %>%
  filter(research_level %in% c("R1", "R2")) %>%   # <-- filter here
  count(research_level, department_class) %>%
  ggplot(aes(x = department_class, y = n, fill = research_level)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribution of Department Categories by Research Level (R1 & R2 Only)",
    x = "Department Category",
    y = "Number of Programs",
    fill = "Research Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#changes all msi_id to lowercase
di_prgs_filt_R1R2 <- di_prgs_filt_R1R2 %>%
  dplyr::mutate(MSI_id = tolower(MSI_id))
unique(di_prgs_filt_R1R2$MSI_id)
unique(di_prgs_filt_R1R2$research_level)

di_prgs_filt_R1R2 %>%
  distinct(institution_name, research_level) %>%
  count(institution_name) %>%
  filter(n > 1)

di_prgs_filt_R1R2 %>%
  distinct(institution_name, MSI_id) %>%
  count(institution_name) %>%
  filter(n > 1)


di_prgs_filt_R1R2 <- di_prgs_filt_R1R2 %>%
  filter(institution_name != "Illinois State University")
new_rows <- tibble::tribble(
  ~institution_name,              ~city,           ~state, ~keywords,     ~college,                         ~department,           ~program_title,                                                           ~degree_level, ~cur_url,                                                           ~credit_hrs, ~research_level, ~notes,                                                                                       ~pop_serve, ~MSI_id,
  "Central Michigan University",  "Mount Pleasant","MI",   "data science","College of Science & Engineering","Computer Science",    "B.S. in Data Science",                                                   "Bachelor’s",  "https://www.cmich.edu/program/data-science-bs",                     "120",       "R2",            "Strong programming + statistics core; housed in Computer Science; applied focus",          "PWI",       "no",
  "Central Michigan University",  "Mount Pleasant","MI",   "data science","College of Science & Engineering","Computer Science",    "M.S. in Data Science",                                                   "Master’s",    "https://www.cmich.edu/program/data-science-ms",                     "30–36",     "R2",            "Emphasis on big data, machine learning, analytics tools; industry-oriented",               "PWI",       "no",
  "Central Michigan University",  "Mount Pleasant","MI",   "data science","College of Science & Engineering","Computer Science",    "Ph.D. in Applied Mathematics (Data Science concentration)",             "Doctoral",    "https://www.cmich.edu/program/applied-mathematics-phd",             "Varies",     "R2",            "Research pathway; quantitative modeling + computational methods",                          "PWI",       "no",
  "Central Michigan University",  "Mount Pleasant","MI",   "analytics",   "College of Business Administration","Business Information Systems","B.S.B.A. in Business Analytics",                                "Bachelor’s",  "https://www.cmich.edu/program/business-analytics-bsba",            "120",       "R2",            "Business-centered analytics; ERP, BI tools, applied decision-making",                      "PWI",       "no",
  "Central Michigan University",  "Mount Pleasant","MI",   "analytics",   "College of Business Administration","Business Information Systems","M.S. in Information Systems (Business Analytics concentration)", "Master’s",    "https://www.cmich.edu/program/information-systems-ms",             "30–36",     "R2",            "Applied analytics + enterprise systems",                                                   "PWI",       "no",
  "Central Michigan University",  "Mount Pleasant","MI",   "informatics", "College of Health Professions",    "School of Health Sciences","M.S. in Health Informatics",                                      "Master’s",    "https://www.cmich.edu/program/health-informatics-ms",              "36",        "R2",            "Health data systems, clinical data management",                                            "PWI",       "no",
  "Central Michigan University",  "Mount Pleasant","MI",   "informatics", "College of Health Professions",    "School of Health Sciences","Graduate Certificate in Health Informatics",                       NA,            NA,                                                                   NA,          "R2",            "Certificate — exclude if filtering to degree-granting only",                              "PWI",       "no"
)
di_prgs_filt_R1R2 <- dplyr::bind_rows(di_prgs_filt_R1R2, new_rows)
di_prgs_filt_R1R2 <- di_prgs_filt_R1R2 %>%
  filter(!is.na(degree_level))

new_rows <- tibble::tribble(
  ~institution_name,         ~city,   ~state, ~keywords,     ~college,                               ~department,                 ~program_title,                 ~degree_level, ~cur_url,                                                             ~credit_hrs, ~research_level, ~notes,                                                                                      ~pop_serve, ~MSI_id,
  "Illinois State University","Normal","IL",   "data science","College of Applied Science & Technology","School of Information Technology","B.S. in Data Science",       "Bachelor’s",  "https://illinoisstate.edu/academics/data-science/",               "120",       "R2",            "Technical curriculum: programming, statistics, machine learning, data management; housed in IT rather than math or CS","PWI","no",
  "Illinois State University","Normal","IL",   "data science","College of Applied Science & Technology","School of Information Technology","M.S. in Data Science",       "Master’s",    "https://illinoisstate.edu/academics/data-science/",               "30–36",     "R2",            "Applied analytics, predictive modeling, big data tools; professional orientation","PWI","no",
  "Illinois State University","Normal","IL",   "analytics",   "College of Business",                   "Department of Marketing",   "B.S. in Marketing Analytics",   "Bachelor’s",  "https://illinoisstate.edu/academics/marketing-analytics/",         "120",       "R2",            "Business-focused analytics; consumer data, decision analytics, visualization","PWI","no",
  "Illinois State University","Normal","IL",   "analytics",   "College of Business",                   "Department of Marketing",   "M.S. in Marketing Analytics",   "Master’s",    "https://illinoisstate.edu/academics/marketing-analytics/",         "30",        "R2",            "Industry-facing analytics degree; applied predictive modeling","PWI","no"
)
