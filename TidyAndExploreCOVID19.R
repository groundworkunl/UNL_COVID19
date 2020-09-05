#install.packages("tidyverse")
library(tidyverse)

# Loading Distribution Class
# Cleaning the data
classDist <- read_csv("UNLDistribution2019/ClassLevelDistribution.csv")
classDist$Percentage = as.numeric(str_replace_all(classDist$Percentage, '%', ''))

genderDist <- read_csv("UNLDistribution2019/GenderDistribution.csv")
genderDist$Percentage = as.numeric(str_replace_all(genderDist$Percentage, '%', ''))

majorDist <- read_csv("UNLDistribution2019/MajorDistribution.csv")
majorDist <- majorDist %>%
  select(College, Percentage)
majorDist$Percentage = as.numeric(str_replace_all(majorDist$Percentage, '%', ''))

raceDist <- read_csv("UNLDistribution2019/RaceDistribution.csv")
raceDist$Percentage = as.numeric(str_replace_all(raceDist$Percentage, '%', ''))

# Loading COVID Dataset
covid <- read_csv("COVID19/covid_gw.csv")
covid <- covid %>%
  select(-X15,-X16,-X17,-X18,-X19,-X20,-X21,-X22,-X23)
write.csv(covid,"COVID19/covidgw.csv")
covid <- read_csv("COVID19/covidgw.csv")

# Summarizing Clean Data (No Errors in General)
covid$class[covid$class == "Graduate Student"] <- 'Graduate'

sampleClass <- covid %>%
  select(class) %>%
  mutate(count = 1) %>%
  group_by(class) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = 100*freq/sum(freq))

# Cleaning Data
unique(covid$race)
unique(raceDist$`Race/Ethnicity`)

# Correcting data entry
covid$race[covid$race == "American Indian/Alaskan Native"] <- "Alaskan/Native American"
covid$race[covid$race == "White (Non-hispanic)"] <- "White (Non-Hispanic)"
covid$race[covid$race == "Black (Non-hispanic)"] <- "Black (Non-Hispanic)"

# Dealing with outlier data (I don't think the right word is outlier btw)
covid$race[!(covid$race %in% raceDist$`Race/Ethnicity`)] <- "Other"

sampleRace <- covid %>%
  select(race) %>%
  mutate(count = 1) %>%
  group_by(race) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = 100*freq/sum(freq))

# Cleaning for Gender
unique(covid$gender)

covid$gender[!(covid$gender %in% genderDist$Gender)] <- "Other"

sampleGender <- covid %>%
  select(gender) %>%
  mutate(count = 1) %>%
  group_by(gender) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = 100*freq/sum(freq))

# Cleaning for College
unique(covid$college)
unique(majorDist$College)

# Hard coding for correction (you don't want to do this for large datasets)
covid$college[covid$college == "Business"] <- "College of Business"
covid$college[covid$college == "Ag economics"] <- "College of Agricultural Science and Natural Resources"
covid$college[covid$college == "Pre-Health"] <- "College of Arts and Sciences"
covid$college[covid$college == "Fashion Communications"] <- "College of Journalism and Mass Communication"
covid$college[covid$college == "College of Agricultural Science and Natural Resources"] <- "College of Agricultural Sciences and Natural Resources"

covid$college[!(covid$college %in% majorDist$College)] <- "Other"

sampleCollege <- covid %>%
  select(college) %>%
  mutate(count = 1) %>%
  group_by(college) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = 100*freq/sum(freq))

# Comparing Distributions (Class)
classDist <- classDist %>%
  mutate(Type = "Actual")

sampleClass <- sampleClass %>%
  select(class, percentage) %>%
  rename(`Class Level` = class, Percentage = percentage) %>%
  mutate(Type = "Sample")

classComp <- rbind(classDist, sampleClass)

ggplot(classComp, aes(fill=Type, y=Percentage, x=`Class Level`)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Class Level Distribution Comparison between Actual vs. Sample") +
  theme_minimal()


# Comparing Distributions (Gender)
genderDist <- genderDist %>%
  mutate(Type = "Actual")

sampleGender <- sampleGender %>%
  select(gender, percentage) %>%
  rename(Gender = gender, Percentage = percentage) %>%
  mutate(Type = "Sample")

genderComp <- rbind(genderDist, sampleGender)

ggplot(genderComp, aes(fill=Type, y=Percentage, x=Gender)) +
  geom_bar(position="dodge", stat="identity")  + 
  ggtitle("Gender Distribution Comparison between Actual vs. Sample") +
  theme_minimal()

# Comparing Distributions (Race)
raceDist <- raceDist %>%
  mutate(Type = "Actual")

sampleRace <- sampleRace %>%
  select(race, percentage) %>%
  rename(`Race/Ethnicity` = race, Percentage = percentage) %>%
  mutate(Type = "Sample")

raceComp <- rbind(raceDist, sampleRace)

ggplot(raceComp, aes(fill=Type, y=Percentage, x=`Race/Ethnicity`)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Race Distribution Comparison between Actual vs. Sample") +
  theme_minimal()


# Comparing Distributions (College)
majorDist <- majorDist %>%
  mutate(Type = "Actual")

sampleCollege <- sampleCollege %>%
  select(college, percentage) %>%
  rename(College = college, Percentage = percentage) %>%
  mutate(Type = "Sample")

collegeComp <- rbind(majorDist, sampleCollege)

# Relabelling for better visuals
collegeComp$College[collegeComp$College == "College of Agricultural Sciences and Natural Resources"] <- 'CASNR'
collegeComp$College[collegeComp$College == "College of Architecture"] <- "CA"
collegeComp$College[collegeComp$College == "College of Arts and Sciences"] <- "CAS"
collegeComp$College[collegeComp$College == "College of Business"] <- "COB"
collegeComp$College[collegeComp$College == "College of Education and Human Sciences"] <- "CEHS"
collegeComp$College[collegeComp$College == "College of Engineering"] <- "CE"
collegeComp$College[collegeComp$College == "College of Fine and Performing Arts"] <- "CFPA"
collegeComp$College[collegeComp$College == "College of Journalism and Mass Communication"] <- "CJMC"
collegeComp$College[collegeComp$College == "College of Law"] <- "Law"
collegeComp$College[collegeComp$College == "Graduate Studies"] <- "Grad"

ggplot(collegeComp, aes(fill=Type, y=Percentage, x=College)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("College Distribution Comparison between Actual vs. Sample") +
  theme_minimal()

# Looking at the Distribution of Results
# Awareness
awareness <- covid %>%
  select(awareness) %>%
  mutate(count = 1) %>%
  mutate(awareness = as.character(awareness)) %>%
  group_by(awareness) %>%
  summarize(freq = sum(count))

ggplot(awareness, aes(x = awareness, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="blue", fill="steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Awareness")

# Testing
testing <- covid %>%
  select(testing) %>%
  mutate(count = 1) %>%
  mutate(testing = as.character(testing)) %>%
  group_by(testing) %>%
  summarize(freq = sum(count))

ggplot(testing, aes(x = testing, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="blue", fill="steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Testing")

# Remove NA
testing <- testing[complete.cases(testing),]

# Tuition
tuition <- covid %>%
  select(tuition) %>%
  mutate(count = 1) %>%
  mutate(tuition = as.character(tuition)) %>%
  group_by(tuition) %>%
  summarize(freq = sum(count))

# Remove NA
tuition <- tuition[complete.cases(tuition),]

ggplot(tuition, aes(x = tuition, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="blue", fill="steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Testing")

# Academic
academic <- covid %>%
  select(academic) %>%
  mutate(count = 1) %>%
  mutate(academic = as.character(academic)) %>%
  group_by(academic) %>%
  summarize(freq = sum(count))

# Remove NA
academic <- academic[complete.cases(academic),]

ggplot(academic, aes(x = academic, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="blue", fill="steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Academic Performance")

# Fees
fee <- covid %>%
  select(fee) %>%
  mutate(count = 1) %>%
  mutate(fee = as.character(fee)) %>%
  group_by(fee) %>%
  summarize(freq = sum(count))

# Remove NA
fee <- fee[complete.cases(fee),]

ggplot(fee, aes(x = fee, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="blue", fill="steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Fees")

# Mask
mask <- covid %>%
  select(mask) %>%
  mutate(count = 1) %>%
  mutate(mask = as.character(mask)) %>%
  group_by(mask) %>%
  summarize(freq = sum(count))

# Remove NA
mask <- mask[complete.cases(mask),]

ggplot(mask, aes(x = mask, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="blue", fill="steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Mask")

# Hygiene
hygiene <- covid %>%
  select(hygiene) %>%
  mutate(count = 1) %>%
  mutate(hygiene = as.character(hygiene)) %>%
  group_by(hygiene) %>%
  summarize(freq = sum(count))

# Remove NA
hygiene <- hygiene[complete.cases(hygiene),]

ggplot(hygiene, aes(x = hygiene, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="blue", fill="steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Hygiene")

# Satisfaction
satisfaction <- covid %>%
  select(satisfaction) %>%
  mutate(count = 1) %>%
  mutate(satisfaction = as.character(satisfaction)) %>%
  group_by(satisfaction) %>%
  summarize(freq = sum(count))

# Remove NA
satisfaction <- satisfaction[complete.cases(satisfaction),]

ggplot(satisfaction, aes(x = satisfaction, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="blue", fill="steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Satisfaction")

