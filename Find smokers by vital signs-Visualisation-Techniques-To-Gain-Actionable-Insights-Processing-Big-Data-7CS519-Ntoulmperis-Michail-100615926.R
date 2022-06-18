# Loading Libraries -------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(cowplot)
# Data Import -------------------------------------------------------------
df <- read.csv("Find smokers by vital signs.csv", sep = ";")
# Missing values = 0 -------------------------------------------------------
sum(is.na(df))
# Pre-Processing ----------------------------------------------------------

# Removing ID -------------------------------------------------------------
df <- subset(df, select = -oral)
df <- df[, -1]
# Transforming Columns to Appropriate Type --------------------------------
df$gender <- replace(df$gender, df$gender=="M", 1)
df$gender <- replace(df$gender, df$gender=="F", 0)
df$gender <- as.factor(df$gender)

df$tartar <- replace(df$tartar, df$tartar=="Y", 1)
df$tartar <- replace(df$tartar, df$tartar=="N", 0)
df$tartar <- as.factor(df$tartar)
df$smoking <- as.factor(df$smoking)
df$age <- as.factor(df$age)
# Split the genders -------------------------------------------------------
dfMale <- df[which(df$gender == "1"),]
dfFemale <- df[which(df$gender == "0"),]
dfMale <- subset(dfMale, select = -gender)
dfFemale <- subset(dfFemale, select = -gender)
# Split Genders to Male Smokers and Non Smokers ---------------------------
dfMaleSmokers <- dfMale[which(dfMale$smoking == "1"),]
dfMaleNonSmokers <- dfMale[which(dfMale$smoking == "0"),]
dfMaleSmokers <- subset(dfMaleSmokers, select = -smoking)
dfMaleNonSmokers <- subset(dfMaleNonSmokers, select = -smoking)
# Split Genders to Female Smokers and Non Smokers -------------------------
dfFemaleSmokers <- dfFemale[which(dfFemale$smoking == "1"),]
dfFemaleNonSmokers <- dfFemale[which(dfFemale$smoking == "0"),]
dfFemaleSmokers <- subset(dfFemaleSmokers, select = -smoking)
dfFemaleNonSmokers <- subset(dfFemaleNonSmokers, select = -smoking)



# Visualization ------------------------------------------------------------



# Male Smokers, Non Smokers -----------------------------------------------
plt <- ggplot(data = dfMale, aes(smoking)) +
  geom_bar(col= "blue", fill="red")

plt <- plt + labs(title = "Amount of Smokers Among Males", subtitle = "Smokers Frequency",
                  y= "Frequency", x= "0 is for Non-Smokers, 1 is for Smokers")

# Female Smokers, Non Smokers ---------------------------------------------


plt2 <- ggplot(data = dfFemale, aes(smoking)) +
  geom_bar(col= "red", fill="blue")

plt2 <- plt2 + labs(title = "Amount of Smokers Among Females", subtitle = "Smokers Frequency",
                  y= "Frequency", x= "0 is for Non-Smokers, 1 is for Smokers")



# Cholesterol -------------------------------------------------------------
# Cholesterol Male --------------------------------------------------------


plt <- ggplot(data=dfMaleSmokers, aes(age, Cholesterol)) +
  geom_violin(col= "red")

plt <- plt + labs( title = "Cholesterol in Male Smokers",
                 subtitle = "Cholesterol vs. Age",
                 y="Male Smokers Age", x="Cholesterol Levels",
                 caption = "Cholesterol")


plt2 <- ggplot(data=dfMaleNonSmokers, aes(age, Cholesterol)) +
  geom_violin( col= "blue")

plt2 <- plt2 + labs( title = "Cholesterol in Male Non Smokers",
                   subtitle = "Cholesterol vs. Age",
                   y="Male Non Smokers Age", x="Cholesterol Levels",
                   caption = "Cholesterol")

# Cholesterol Female ------------------------------------------------------

plt3 <- ggplot(data=dfFemaleSmokers, aes(age, Cholesterol)) +
  geom_violin(col= "red")

plt3 <- plt3 + labs( title = "Cholesterol in Female Smokers",
                   subtitle = "Cholesterol vs. Age",
                   y="Female Smokers Age", x="Cholesterol Levels",
                   caption = "Cholesterol")


plt4 <- ggplot(data=dfFemaleNonSmokers, aes(age, Cholesterol)) +
  geom_violin(col= "blue")

plt4 <- plt4 + labs( title = "Cholesterol in Female Non Smokers",
                     subtitle = "Cholesterol vs. Age",
                     y="Female Non Smokers Age", x="Cholesterol Levels",
                     caption = "Cholesterol")

# --------------------------------------------------------------------------

plot_grid(plt, plt2, plt3, plt4, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)

# Triglyceride ------------------------------------------------------------
# Triglyceride Male -------------------------------------------------------
plt <- ggplot(data=dfMaleSmokers, aes(age, triglyceride)) +
  geom_violin(height = 2, width = 2, col= "red")

plt <- plt + labs( title = "Triglyceride in Male Smokers",
                   subtitle = "Triglyceride vs. Age",
                   y="Male Smokers Age", x="Triglyceride Levels",
                   caption = "Triglyceride")


plt2 <- ggplot(data=dfMaleNonSmokers, aes(age, triglyceride)) +
  geom_violin(height = 2, width = 2, col= "blue")

plt2 <- plt2 + labs( title = "Triglyceride in Male Non Smokers",
                     subtitle = "Triglyceride vs. Age",
                     y="Male Non Smokers Age", x="Triglyceride Levels",
                     caption = "Triglyceride")
# Triglyceride Female -----------------------------------------------------
plt3 <- ggplot(data=dfFemaleSmokers, aes(age, triglyceride)) +
  geom_violin(height = 2, width = 2, col= "red")

plt3 <- plt3 + labs( title = "Triglyceride in Female Smokers",
                     subtitle = "Triglyceride vs. Age",
                     y="Female Smokers Age", x="Triglyceride Levels",
                     caption = "Triglyceride")


plt4 <- ggplot(data=dfFemaleNonSmokers, aes(age, triglyceride)) +
  geom_violin(height = 2, width = 2, col= "blue")

plt4 <- plt4 + labs( title = "Triglyceride in Female Non Smokers",
                     subtitle = "Triglyceride vs. Age",
                     y="Female Non Smokers Age", x="Triglyceride Levels",
                     caption = "Triglyceride")

# -------------------------------------------------------------------------
plot_grid(plt, plt2, plt3, plt4, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)

# Blood Pressure ----------------------------------------------------------
# Blood Pressure Male -------------------------------------------------------
plt2 <- ggplot(data=dfMaleSmokers, aes(systolic, relaxation)) +
  geom_jitter(col= "red")

plt2 <- plt2 + labs( title = "Blood Pressure in Male Smokers",
                   y="Relaxation Blood Pressure Levels", x="Systolic Blood Pressure Levels",
                   caption = "Blood Pressure")


plt <- ggplot(data=dfMaleNonSmokers, aes(systolic, relaxation)) +
  geom_jitter(col= "blue")

plt <- plt + labs( title = "Blood Pressure in Male Non Smokers",
                     y="Relaxation Blood Pressure Levels", x="Systolic Blood Pressure Levels",
                     caption = "Blood Pressure")
# Blood Pressure Female -----------------------------------------------------
plt4 <- ggplot(data=dfFemaleSmokers, aes(systolic, relaxation)) +
  geom_jitter(col= "red")

plt4 <- plt4 + labs( title = "Blood Pressure in Female Smokers",
                     y="Relaxation Blood Pressure Levels", x="Systolic Blood Pressure Levels",
                     caption = "Blood Pressure")


plt3 <- ggplot(data=dfFemaleNonSmokers, aes(systolic, relaxation)) +
  geom_jitter(col= "blue")

plt3 <- plt3 + labs( title = "Blood Pressure in Female Non Smokers",
                     y="Relaxation Blood Pressure Levels", x="Systolic Blood Pressure Levels",
                     caption = "Blood Pressure")

# -------------------------------------------------------------------------
plot_grid(plt, plt2, plt3, plt4, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)



# Waist size Weight -------------------------------------------------------

# Male Smokers and Non Smokers --------------------------------------------


plt2 <- ggplot(data=dfMaleSmokers, aes(weight.kg., waist.cm.)) +
  geom_point(col= "red")

plt2 <- plt2 + labs( title = "Weight and Waist size in Male Smokers",
                   subtitle = "Weight vs Waist size",
                   y="Male Smokers Waist Size", x="Male Smokers Weight",
                   caption = "Weight in kg Waist in cm")


plt <- ggplot(data=dfMaleNonSmokers, aes(weight.kg., waist.cm.)) +
  geom_point(col= "blue")

plt <- plt + labs( title = "Weight and Waist size in Male Non Smokers",
                     subtitle = "Weight vs Waist size",
                     y="Male Non Smokers Waist Size", x="Male Non Smokers Weight",
                     caption = "Weight in kg Waist in cm")



# Female Smokers and Non smokers ------------------------------------------

plt4 <- ggplot(data=dfFemaleSmokers, aes(weight.kg., waist.cm.)) +
  geom_point(col= "red")

plt4 <- plt4 + labs( title = "Weight and Waist size in Female Smokers",
                   subtitle = "Weight vs Waist size",
                   y="Female Smokers Waist Size", x="Female Smokers Weight",
                   caption = "Weight in kg Waist in cm")


plt3 <- ggplot(data=dfFemaleNonSmokers, aes(weight.kg., waist.cm.)) +
  geom_point(col= "blue")

plt3 <- plt3 + labs( title = "Weight and Waist size in Female Non Smokers",
                     subtitle = "Weight vs Waist size",
                     y="Female Non Smokers Waist Size", x="Female Non Smokers Weight",
                     caption = "Weight in kg Waist in cm")


plot_grid(plt, plt2, plt3, plt4, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)


# ALT ---------------------------------------------------------------------

# ALT MALE ----------------------------------------------------------------
plt2 <- ggplot(data=dfMaleSmokers, aes(age, ALT)) +
  geom_col(col= "red")

plt2 <- plt2 + labs( title = "ALT in Male Smokers",
                   subtitle = "ALT vs. Age",
                   y="Male Smokers Age", x="ALT Levels",
                   caption = "ALT")


plt <- ggplot(data=dfMaleNonSmokers, aes(age, ALT)) +
  geom_col(col= "red")

plt <- plt + labs( title = "ALT in Male Non Smokers",
                     subtitle = "ALT vs. Age",
                     y="Male Non Smokers Age", x="ALT Levels",
                     caption = "ALT")

# ALT Female ------------------------------------------------------

plt4 <- ggplot(data=dfFemaleSmokers, aes(age, ALT)) +
  geom_col(col= "blue")

plt4 <- plt4 + labs( title = "ALT in Female Smokers",
                     subtitle = "ALT vs. Age",
                     y="Female Smokers Age", x="ALT Levels",
                     caption = "ALT")


plt3 <- ggplot(data=dfFemaleNonSmokers, aes(age, ALT)) +
  geom_col(col= "blue")

plt3 <- plt3 + labs( title = "ALT in Female Non Smokers",
                     subtitle = "ALT vs. Age",
                     y="Female Non Smokers Age", x="ALT Levels",
                     caption = "ALT")

# --------------------------------------------------------------------------

plot_grid(plt, plt2, plt3, plt4, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)


# AST ---------------------------------------------------------------------

# AST MALE ----------------------------------------------------------------
plt2 <- ggplot(data=dfMaleSmokers, aes(age, AST)) +
  geom_col(col= "red")

plt2 <- plt2 + labs( title = "AST in Male Smokers",
                     subtitle = "AST vs. Age",
                     y="Male Smokers Age", x="AST Levels",
                     caption = "AST")


plt <- ggplot(data=dfMaleNonSmokers, aes(age, AST)) +
  geom_col(col= "red")

plt <- plt + labs( title = "AST in Male Non Smokers",
                   subtitle = "AST vs. Age",
                   y="Male Non Smokers Age", x="AST Levels",
                   caption = "AST")

# AST Female ------------------------------------------------------

plt4 <- ggplot(data=dfFemaleSmokers, aes(age, AST)) +
  geom_col(col= "blue")

plt4 <- plt4 + labs( title = "AST in Female Smokers",
                     subtitle = "AST vs. Age",
                     y="Female Smokers Age", x="AST Levels",
                     caption = "AST")


plt3 <- ggplot(data=dfFemaleNonSmokers, aes(age, AST)) +
  geom_col(col= "blue")

plt3 <- plt3 + labs( title = "AST in Female Non Smokers",
                     subtitle = "AST vs. Age",
                     y="Female Non Smokers Age", x="AST Levels",
                     caption = "AST")

# --------------------------------------------------------------------------

plot_grid(plt, plt2, plt3, plt4, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)


# END ---------------------------------------------------------------------

