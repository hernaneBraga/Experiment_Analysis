# Loading dataset
dataset_2016 <- read.csv('data/imc_20162.csv', sep=',')
dataset_2017 <- read.csv('data/CS01_20172.csv', sep=';')


# Renaming columns
names(dataset_2017)[names(dataset_2017) == "Sex"] <- "Gender"
names(dataset_2017)[names(dataset_2017) == "height.m"] <- "Height.m"


# Selecting variables
model_var <- c("Height.m", "Weight.kg", "IMC")


# Calculating IMC
dataset_2016[,'IMC'] = dataset_2016[,'Weight.kg'] / (dataset_2016[,'Height.m'])^2
dataset_2017[,'IMC'] = dataset_2017[,'Weight.kg'] / (dataset_2017[,'Height.m'])^2


# Selecting PPGEE students
PPGEE_2016students <- dataset_2016[which(dataset_2016$Course == 'PPGEE'), ]


# Male students
male_students2016 <- PPGEE_2016students[which(PPGEE_2016students$Gender == 'M'), ]
male_students2017 <- dataset_2017[which(dataset_2017$Gender == 'M'), ]


# Female students
female_students2016 <- PPGEE_2016students[which(PPGEE_2016students$Gender == 'F'), ]
female_students2017 <- dataset_2017[which(dataset_2017$Gender == 'F'), ]


# Experiment dataset - Male x Female IMC
male_students <- rbind(male_students2016[model_var], male_students2017[model_var])
female_students <- rbind(female_students2016[model_var], female_students2017[model_var])


# ------------------------------

# Boxplot analysis

boxplot(male_students$IMC, female_students$IMC, main="IMC Analysis", col=(c("gold","darkgreen")),
        ylab="IMC Value")


legend("topright", inset=.02, title="Gender",
       c("Male","Female"), fill=(c("gold","darkgreen")), horiz=TRUE, cex=0.8)



hgA <- hist(male_students$IMC, breaks = 12)
hgB <- hist(female_students$IMC)

range(c(hgA$breaks, hgB$breaks)) # Get range for x-axis


max(c(hgA$count, hgB$count)) # Get range for y-axis


plot(hgA, col ='gold', xlim = c(13,40), xlab = 'IMC', main ='IMC Histogram')
plot(hgB, add = TRUE, col = 'darkgreen')

legend("topright", inset=.02, title="Gender",
       c("Male","Female"), fill=(c("gold","darkgreen")), horiz=TRUE, cex=0.75)
