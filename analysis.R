# Part of this code has concepts developed by learning
# the R documentation provided on (https://www.rdocumentation.org/)

# Read the dataset into a data frame (df)
df <- read.csv("gpa.csv")

# Extract data from the relevant columns
studyweek <- df$studyweek # independent variable studyweek (Study Hours per week)
gpa <- df$gpa       # dependent variable gpa (Grade Point Average)

# There were mixed opinions about the normality of the GPA data. 
# So we were advised to conduct statistical test to confirm normality, 
# Hence we conducted shapiro-wilk test on the GPA data. 
# The results of the test indicated that the GPA data is normally distributed. 
# Therefore, Using Pearson’s test
print(cor.test(studyweek, gpa, method = "pearson"))