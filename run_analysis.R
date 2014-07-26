# get required packages
pkg <- c('data.table', 'reshape2', 'knitr', 'markdown')
sapply(pkg, require, character.only=T, quietly=F)

# set paths
basepath <- getwd()
datadir <- file.path(basepath, 'UCI-HAR-Dataset')

# read subject files
dt_trainSubject <- fread(file.path(datadir, 'train', 'subject_train.txt'))
dt_testSubject <- fread(file.path(datadir, 'test', 'subject_test.txt'))

# read activity files
dt_trainActivity <- fread(file.path(datadir, 'train', 'Y_train.txt'))
dt_testActivity <- fread(file.path(datadir, 'test', 'Y_test.txt'))

# read data files (this may take some time, so wrap in a conditional to prevent unnecessary reads)
if (!exists('dt_train')) {
    dt_train <- data.table(read.table(file.path(datadir, 'train', 'X_train.txt')))   
}
if (!exists('dt_test')) {
    dt_test <- data.table(read.table(file.path(datadir, 'test', 'X_test.txt')))
}

# merge the training and test sets
dt_subject <- rbind(dt_trainSubject, dt_testSubject)
setnames(dt_subject, 'V1', 'subject')
dt_activity <- rbind(dt_trainActivity, dt_testActivity)
setnames(dt_activity, 'V1', 'activityNum')
dt <- rbind(dt_train, dt_test)

# merge columns
dt_subject <- cbind(dt_subject, dt_activity)
dt <- cbind(dt_subject, dt)

# set key
setkey(dt, subject, activityNum)

# read in 'features.txt' file. This tells which vars are measurements for mean and standard deviation
dt_features <- fread(file.path(datadir, 'features.txt'))
setnames(dt_features, names(dt_features), c('featureNum', 'featureName'))

# subset dt_features using grepl for mean and standard deviation measurements only
dt_features <- dt_features[grepl('mean\\(\\)|std\\(\\)', featureName)]

# convert column numbers to a vector of variable names matching columns in dt
dt_features$featureCode <- dt_features[, paste0('V', featureNum)]

# subset the vars using var names
select <- c(key(dt), dt_features$featureCode)
dt <- dt[, select, with=F]

# read activity_labels.txt file to add descriptive names to activities
dt_activityNames <- fread(file.path(datadir, 'activity_labels.txt'))
setnames(dt_activityNames, names(dt_activityNames), c('activityNum', 'activityName'))

# merge activity labels
dt <- merge(dt, dt_activityNames, by='activityNum', all.x=T)

# activityName will also be a key
setkey(dt, subject, activityNum, activityName)

# melt data table and reshape from short/wide to tall/narrow format
dt <- data.table(melt(dt, key(dt), variable.name='featureCode'))

# merge activity name
dt <- merge(dt, dt_features[, list(featureNum, featureCode, featureName)], by='featureCode', all.x=T)

# var activity: equiv. to activityName as factor
# var feature: equiv. to featureName as factor
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

# helper function greplthis
greplthis <- function(pattern) {
    grepl(pattern, dt$feature)
}

# features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)

# feat domain
x <- matrix(c(greplthis('^t'), greplthis('^f')), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c('Time', 'Freq'))

# feat instrument
x <- matrix(c(greplthis('Acc'), greplthis('Gyro')), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c('Accelerometer', 'Gyroscope'))

# feat acceleration
x <- matrix(c(greplthis('BodyAcc'), greplthis('GravityAcc')), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, 'Body', 'Gravity'))

# feat variable
x <- matrix(c(greplthis('mean()'), greplthis('std()')), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c('Mean', 'SD'))

# features with 1 category
dt$featJerk <- factor(greplthis('Jerk'), labels=c(NA, 'Jerk'))
dt$featMagnitude <- factor(greplthis('Mag'), labels=c(NA, 'Magnitude'))

# features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(greplthis('-X'), greplthis('-Y'), greplthis('-Z')), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, 'X', 'Y', 'Z'))

# check that all possible combinations of feature are accounted for by all poss. combinations of factor class vars
r1 <- nrow(dt[, .N, by=c('feature')])
r2 <- nrow(dt[, .N, by=c('featDomain', 'featAcceleration', 'featInstrument', 'featJerk', 'featMagnitude', 'featVariable', 'featAxis')])
r1 == r2

# create data set with average of each var for each activity and each subject
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dt_tidy <- dt[, list(count=.N, average=mean(value)), by=key(dt)]

# make codebook
knit('makeCodebook.Rmd', output='codebook.md', encoding='ISO8859-1', quiet=F)
markdownToHTML('codebook.md', 'codebook.html')

