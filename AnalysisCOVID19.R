# Testing Freshman and Others difference
# 
source("TidyAndExploreCOVID19.R")
covid <- covid %>%
  mutate(isFreshman = (class == "Freshman"))

covid.f <- covid[covid$isFreshman == TRUE,]
covid.o <- covid[covid$isFreshman == FALSE,]

# Comparison between freshman and other
mean.f <- mean(covid.f$awareness)
mean.o <- mean(covid.o$awareness)
sd.f <- sd(covid.f$awareness)
sd.o <- sd(covid.o$awareness)
n.f <-length(covid.f$awareness)
n.o <-length(covid.o$awareness)

# Now construct the test statistic
se <-sqrt((sd.f^2)/n.f + (sd.o^2)/n.o)
t <-(mean.f-mean.o)/se
print(2*((1-pt(abs(t), df=min(n.f, n.o)))))

# Abstraction using functions
t_test <- function(data_a, data_b){
  # Remove NA from vector
  data_a <- data_a[!is.na(data_a)]
  data_b <- data_b[!is.na(data_b)]
  
  mean.1 <- mean(data_a)
  cat("Mean of A: ", mean.1, "\n")
  mean.2 <- mean(data_b)
  cat("Mean of B: ", mean.2, "\n")
  sd.1 <- sd(data_a)
  sd.2 <- sd(data_b)
  n.1 <-length(data_a)
  n.2 <-length(data_b)
  
  # Now construct the test statistic
  se <-sqrt((sd.1^2)/n.1 + (sd.2^2)/n.2)
  t <-(mean.1-mean.2)/se
  cat("t-score: ", t, "\n")
  p <- 2*((1-pt(abs(t), df=min(n.1, n.2))))
  cat("P(|t| >= ", abs(t), ") =", p ,"\n")
}

# testing all variables between freshman vs others
t_test(covid.f$awareness, covid.o$awareness)
t_test(covid.f$testing, covid.o$testing)
t_test(covid.f$tuition, covid.o$tuition)
t_test(covid.f$academic, covid.o$academic)
t_test(covid.f$fee, covid.o$fee)
t_test(covid.f$mask, covid.o$mask)
t_test(covid.f$hygiene, covid.o$hygiene)
t_test(covid.f$satisfaction, covid.o$satisfaction)

covid.female <- covid[covid$gender == "Female",]
covid.male <- covid[covid$gender == "Male",]

# testing all variables between female and male
t_test(covid.female$awareness, covid.male$awareness)
t_test(covid.female$testing, covid.male$testing)
t_test(covid.female$tuition, covid.male$tuition)
t_test(covid.female$academic, covid.male$academic)
t_test(covid.female$fee, covid.male$fee)
t_test(covid.female$mask, covid.male$mask)
t_test(covid.female$hygiene, covid.male$hygiene)
t_test(covid.female$satisfaction, covid.male$satisfaction)

# testing difference between colleges
covid.cas <- covid[covid$college == "College of Arts and Sciences",]
covid.ce <- covid[covid$college == "College of Engineering",]
covid.casnr <- covid[covid$college == "College of Agricultural Sciences and Natural Resources",]

t_test(covid.cas$academic, covid.ce$academic)

# testing difference between gender
covid.m <- covid[covid$gender == "Male",]
covid.fm <- covid[covid$gender == "Female",]

t_test(covid.m$satisfaction, covid.fm$satisfaction)

# Simple Linear Regressions
lmod <- lm(satisfaction ~ awareness + mask + testing + academic + tuition + fee + hygiene, covid)
summary(lmod)
plot(lmod)

lmod <- lm(academic ~ tuition, covid)
summary(lmod)

# Dummy Variable
isFreshman <- factor(covid$isFreshman)

# Multiple Regression with Dummy Variable
lmod <- lm(satisfaction ~ academic  + testing + awareness + isFreshman, covid)
summary(lmod)

