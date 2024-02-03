# Read the CSV file into R, assuming that it doesn't have a header
# df <- read.csv("housing.csv", header = FALSE) Add path of your file

# Set the column names of the data frame
colnames(df) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")



# Subset the data into two groups based on proportion of non-retail business acres
low_indus <- subset(df, INDUS <= median(df$INDUS))
high_indus <- subset(df, INDUS > median(df$INDUS))

# Variance test
var.test(low_indus$MEDV, high_indus$MEDV)


# Perform two-sample t-test
t.test(low_indus$MEDV, high_indus$MEDV, var.equal = TRUE)


# Perform a two-way ANOVA
fit <- aov(MEDV ~ as.factor(CHAS) + factor(INDUS), data = df)
summary(fit)


# Fit a multiple linear regression model
model <- lm(MEDV ~ ., data = df)

# Summarize the results
summary(model)

# Additionally, there's code for plotting, which seems to compare predicted vs actual values
plot(fitted(model), residuals(model), xlab = "Predicted Values", ylab = "Actual Values")
abline(h = 0, lty = 1, col = 'red')



# Perform the Shapiro-Wilk test
shapiro.test(medv)

# Perform chi-squared goodness-of-fit test
medv <- as.numeric(df$MEDV)
obs.freq <- hist(medv, breaks = 10, plot = FALSE)$counts
exp.freq <- chisq.test(obs.freq)
exp.freq

# Histogram for visual inspection
hist(medv)
