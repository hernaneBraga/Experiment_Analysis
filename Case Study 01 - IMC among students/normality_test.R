library(car)

# Verify data normality for male and female students
p_female_stundents <- shapiro.test(female_students$IMC)$p.value #OK
qqPlot(female_students$IMC)
p_male_stundents <- shapiro.test(male_students$IMC)$p.value #OK
qqPlot(male_students$IMC)

# Verify equality of variances between data sets (Male vs Female)
FvsM= stack(list(Female=female_students$IMC, Male=male_students$IMC))
fligner.test(values~ind,FvsM) # OK
resid <- tapply(X = FvsM$values, INDEX = FvsM$ind, FUN = function(x){x - mean(x)})
stripchart(x = resid,vertical = TRUE,pch = 16,cex = 1.5,las = 1,xlab = "mean",ylab = "residuals")

# Verify data normality for male 2016 and 2017 students
p_male_stundents2016 <-shapiro.test(male_students2016$IMC)$p.value #OK
qqPlot(male_students2016$IMC)
p_male_stundents2017 <-shapiro.test(male_students2017$IMC)$p.value #OK
qqPlot(male_students2017$IMC)

# Verify equality of variances between data sets (Male 2016-2 vs Male 2017-2)
M1vsM2= stack(list("M2016-2"=male_students2016$IMC, "M2017-2"=male_students2017$IMC))
fligner.test(values~ind,M1vsM2) # OK
resid <- tapply(X = M1vsM2$values, INDEX = M1vsM2$ind, FUN = function(x){x - mean(x)})
stripchart(x = resid,vertical = TRUE,pch = 16,cex = 1.5,las = 1,xlab = "mean",ylab = "residuals")

# Verify data normality for female 2016 and 2017 students
p_female_stundents2016 <-shapiro.test(female_students2016$IMC)$p.value #OK
qqPlot(female_students2016$IMC)
p_female_stundents2017 <-shapiro.test(female_students2017$IMC)$p.value #NOT OK
qqPlot(female_students2017$IMC)

# Verify equality of variances between data sets (Female 2016-2 vs Female 2017-2)
F1vsF2= stack(list("F2016-2"=male_students2016$IMC, "F2017-2"=male_students2017$IMC))
fligner.test(values~ind,F1vsF2) # OK
resid <- tapply(X = F1vsF2$values, INDEX = F1vsF2$ind, FUN = function(x){x - mean(x)})
stripchart(x = resid,vertical = TRUE,pch = 16,cex = 1.5,las = 1,xlab = "mean",ylab = "residuals")

# Since female_students2017 cannot be assumed normal, a non-parametric test must be issued for F1vsF2:
wilcox.test(female_students2016$IMC,female_students2017$IMC)

