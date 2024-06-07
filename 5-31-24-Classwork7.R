
install.packages("rpart.plot") # install package rpart.plot


library("rpart")
library("rpart.plot")

# Read the data
banktrain  <- read.csv(file.choose()) # "bank-sample.csv"
summary(banktrain)


# Make a  decision tree by only keeping the categorical variables
fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + poutcome, 
             method="class", 
             data=banktrain,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))


# Plot the tree
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

#too detailed, let's simplify it by removing branches
fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + poutcome, 
             method="class", 
             data=banktrain,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))

# Plot the tree
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)


# include a numeric variable "duration" into the model
fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + duration + poutcome, 
             method="class", 
             data=banktrain,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(fit)

# Plot the tree
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

# Predict
newdata <- data.frame(job="retired", 
                      marital="married", 
                      education="secondary",
                      default="no",
                      housing="yes",
                      loan="no",
                      contact = "cellular",
                      duration = 598,
                      poutcome="unknown")
newdata
predict(fit,newdata=newdata,type=c("class"))




#Manual Naive Bayes 


## Read the data


banktrain <- read.csv(file.choose()) #bank-sample.csv

## drop a few columns to simplify the model
drops<-c("balance", "day", "campaign", "pdays", "previous", "month")
banktrain <- banktrain [,!(names(banktrain) %in% drops)]
summary(banktrain)




## manually compute the conditional probabilities
str(banktrain)
maritalCounts <- table(banktrain$subscribed, banktrain$marital)  # Using variable names

maritalCounts 
rowSums(maritalCounts)
maritalCounts <- maritalCounts/rowSums(maritalCounts)
maritalCounts

if ("yes" %in% rownames(maritalCounts) & "divorced" %in% colnames(maritalCounts)) {
  value <- maritalCounts["yes", "divorced"]
  print(value)  # Print the count (if it exists)
} else {
  print("Value 'yes' does not exist for marital status 'divorced'")
}


jobCounts <- table(banktrain$subscribed, banktrain$job)  # Using variable names

jobCounts <- jobCounts/rowSums(jobCounts)
jobCounts

educationCounts <- table(banktrain$subscribed, banktrain$education)

educationCounts <- educationCounts/rowSums(educationCounts)
educationCounts

defaultCounts <- table(banktrain$subscribed, banktrain$default)  # Using variable names

defaultCounts <- defaultCounts/rowSums(defaultCounts)
defaultCounts

housingCounts <- table(banktrain$subscribed, banktrain$housing)  # Using variable names

housingCounts <- housingCounts/rowSums(housingCounts)
housingCounts

subscribed_col <- grep("subscribed", colnames(banktrain), ignore.case = TRUE)[1]  # Assuming "subscribed" is the pattern
loan_col <- grep("loan", colnames(banktrain), ignore.case = TRUE)[1]  # Assuming "loan" is the pattern
# Check if 'subscription_status' or 'has_loan' exist
if (any(c("subscription_status", "has_loan") %in% colnames(banktrain))) {
  # Use columns if they exist
  loanCounts <- table(banktrain[, c("subscription_status", "has_loan")])
} else {
  # Handle situation where columns are missing (print message or adjust analysis)
  print("Columns 'subscription_status' and 'has_loan' not found in banktrain data frame. Consider using different variables or obtaining data with the desired information.")
}

if (exists("loanCounts")) {  # Check if loanCounts exists
  loanCounts <- loanCounts / rowSums(loanCounts)
  print(loanCounts)  # Print the table with row percentages
} else {
  print("loanCounts table not found. Please create the table before calculating row percentages.")
}

subscription_pattern <- grep("subscription|subscribed", colnames(banktrain), ignore.case = TRUE)
loan_pattern <- grep("loan", colnames(banktrain), ignore.case = TRUE)

# Check if any patterns were found
if (length(subscription_pattern) > 0 && length(loan_pattern) > 0) {
  subscription_col <- colnames(banktrain)[subscription_pattern[1]]
  loan_col <- colnames(banktrain)[loan_pattern[1]]
  loanCounts <- table(banktrain[, c(subscription_col, loan_col)])
  print(loanCounts)  # Print the table if created
} else {
  print("Couldn't find columns related to subscription or loan status")
}



subscription_pattern <- grep("subscription|subscribed", colnames(banktrain), ignore.case = TRUE)
contact_pattern <- grep("contact", colnames(banktrain), ignore.case = TRUE)

# Check if any patterns were found
if (length(subscription_pattern) > 0 && length(contact_pattern) > 0) {
  subscription_col <- colnames(banktrain)[subscription_pattern[1]]
  contact_col <- colnames(banktrain)[contact_pattern[1]]
  contactCounts <- table(banktrain[, c(subscription_col, contact_col)])
  print(contactCounts)  # Print the table if created
} else {
  print("Couldn't find columns related to subscription or contact method")
}

if (exists("contactCounts")) {  # Check if contactCounts exists
  contactCounts <- contactCounts / rowSums(contactCounts)
  print(contactCounts)  # Print the table with row percentages
} else {
  print("contactCounts table not found. Please create the table before calculating row percentages.")
}

if (exists("contactCounts")) {
  contactCounts <- contactCounts / rowSums(contactCounts)
  print(contactCounts)
} else {
  print("contactCounts table not found. Please create the table before calculating row percentages.")
}


subscription_pattern <- grep("subscription|subscribed", colnames(banktrain), ignore.case = TRUE)
poutcome_pattern <- grep("poutcome|outcome|campaign", colnames(banktrain), ignore.case = TRUE)

# Check if any patterns were found
if (length(subscription_pattern) > 0 && length(poutcome_pattern) > 0) {
  subscription_col <- colnames(banktrain)[subscription_pattern[1]]
  poutcome_col <- colnames(banktrain)[poutcome_pattern[1]]
  poutcomeCounts <- table(banktrain[, c(subscription_col, poutcome_col)])
  print(poutcomeCounts)  # Print the table if created
} else {
  print("Couldn't find columns related to subscription or poutcome")
}

if (exists("poutcomeCounts")) {  # Check if poutcomeCounts exists
  poutcomeCounts <- poutcomeCounts / rowSums(poutcomeCounts)
  print(poutcomeCounts)  # Print the table with row percentages
} else {
  print("poutcomeCounts table not found. Please create the table before calculating row percentages.")
}

if (exists("poutcomeCounts")) {
  poutcomeCounts <- poutcomeCounts / rowSums(poutcomeCounts)
  print(poutcomeCounts)  # Print table with row percentages
} else {
  print("poutcomeCounts table not found. Please create the table before calculating row percentages.")
}




install.packages("e1071") # install package e1071
library(e1071) # load the library

sample <- read.csv(file.choose()) #sample1.csv
print(sample)


# define the data frames for the NB classifier
traindata <- as.data.frame(sample[1:14,])
testdata <- as.data.frame(sample[15,])
traindata
testdata


model <- naiveBayes(Enrolls ~ Age+Income+JobSatisfaction+Desire,
                    traindata)

# display model
model

# predict with testdata
results <- predict (model,testdata)
# display results
results


