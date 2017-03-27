#----------------------------------------
# STAT 550 - Statistial Consulting I
#----------------------------------------
# STAT 550 Presentation - R Code.R
#----------------------------------------

# Required Libraries
library(SemiPar)
library(nlme)

#----------------------------------------

# Required Source Files
ldaUrl = "http://www.maths.lancs.ac.uk/~diggle/lda/Datasets/lda.dat"
sUrl = "http://www.bristol.ac.uk/cmm/media/migrated/jsp.zip"

#----------------------------------------

###################################
# Random Effect (Random Intercept)
###################################

# Loading the pig weight data
data(pig.weights)
head(pig.weights)

# Plotting the data
plot(1:9,pig.weights$weight[1:9], type = "l", main = "Pig Weight Vs. Time", xlab = "Time", ylab = "Weight", ylim = c(20, 90))
for(i in 2:48){
  
  lines(1:9,pig.weights$weight[((i-1)*9+1):((i-1)*9+9)])
  
}

# Fitting the Random Intercept Model
pigsLme = lme(y ~ time, random = ~1 | id, data = pigsLong)
# Summary of the fit
summary(pigsLme$modelStruct)


###############################
# Random Effect (Random Slope)
###############################

# Formatting the data
cowStart = c(barley = 101, mixed = 155, lupins = 213)
cowLen = c(barley = 25, mixed = 27, lupins = 27)
Stime = 0:18
timeCols = paste("t", Stime, sep = "")
cows = mapply(function(start, len, diet) {
  res = scan(ldaUrl, skip = start - 1, nlines = len *
             2, quiet = TRUE)
  res = as.data.frame(matrix(res, nrow = len, byrow = TRUE))
  colnames(res) = timeCols
  res$diet = diet
  res
  }, start = cowStart, len = cowLen, diet = names(cowStart),
  SIMPLIFY = FALSE)
cows <- do.call(rbind, cows)
row_sub <- apply(cows, 1, function(row) all(row !="0" ))
cows <- cows[row_sub,]
row_sub <- apply(cows, 1, function(row) all(row !="0.00" ))
cows <- cows[row_sub,]

# Plotting the data
plot(1:19, cows[1,1:19], type = "l", ylim = c(2.5,5), col = "red", main = "Cow Harvest", xlab = "Time", ylab = "Harvest")
for(i in 2:nrow(cows)){
  
  if(cows[i,20]=="barley")
    lines(1:19, cows[i,1:19], type = "l", col = "red") else
      if(cows[i,20]=="mixed")
        lines(1:19, cows[i,1:19], type = "l", col = "blue") else
          lines(1:19, cows[i,1:19], type = "l", col = "green")
}

legend(5, 5, legend=c("Barley", "Mixed", "Lupin"), 
       col=c('red', 'blue', 'green'), lwd=3)

# Long format for the cow data
cowLong = reshape(cows,
                  direction = "long",
                  varying = list(timeCols),
                  times = Stime, v.names = "protein")
cowLong = cowLong[cowLong$protein >
                    0, ]
cowLong$diet = factor(cowLong$diet)
cowLong$t = factor(cowLong$time)
cowLong$tLupins = cowLong$tMixed = cowLong$time
cowLong$tLupins[cowLong$diet != "lupins"] = 0
cowLong$tMixed[cowLong$diet != "mixed"] = 0
cowLong$t = factor(cowLong$time)
head(cowLong)

# Fitting the Random Slope Model
cowLmeRs = lme(protein ~ t + diet + tLupins + tMixed, random = ~1 + time | id, data = cowLong)
# Summary of the fit
summary(cowLmeRs$modelStruct)



#####################
# Hierarchical Model
#####################

# Reading in the data
school = read.fwf("JSP.DAT", widths = c(2, 1, 1,1, 2, 4, 2, 2, 1), 
                  col.names = c("school", "class",
                                "gender", "socialClass", "ravensTest", "student", "english",
                                "math", "year"))
# Formatting the data
school$socialClass = factor(school$socialClass, 
                            labels = c("I","II", "IIIn", "IIIm", "IV", "V", "longUnemp", "currUnemp","absent"))
school$gender = factor(school$gender, labels = c("f", "m"))
head(school)

# Fitting the Random Slope Model
schoolLme = lme(math ~ gender + socialClass, random = ~1 | school/class/student, data = school)
# Summary of the fit
summary(schoolLme$modelStruct)

#####################
# Serial Correlation
#####################

# Setting the pigs data in long format
pigsLong = reshape(as.data.frame(pigs),
                   varying = list(y=colnames(pigs)),
                   direction='long', v.names='y')
head(pigsLong)

# Fitting the Random Slope Model
pigsLmeTime = lme(y~time,
                  random = ~1|id, data=pigsLong,
                  correlation=corExp(form=~time|id, nugget=T))
summary(pigsLmeTime)











