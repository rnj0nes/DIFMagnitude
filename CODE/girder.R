library(rhdf5)
source("/Users/rnj/Dropbox/Work/Syntax/H5Results/results.R")
source("/Users/rnj/Dropbox/Work/Syntax/H5Results/Mplus.R")
# dif-girder.R
spelling <- readr::read_csv("~/Dropbox/Work/data/girder.csv")
spelling$n2 <- spelling$men 
spelling$n1 <- spelling$women
spelling$men <- NULL 
spelling$women <- NULL
# Reshape on n
spelling_long <- spelling |>
  tidyr::pivot_longer(
    cols = starts_with("n"), # select columns that start with "n"
    names_to = "sex",        # the new variable name
    names_prefix = "n",      # remove the prefix "n" from column names
    values_to = "count"      # name of the new column containing values
  )
# Expand
spelling_expanded <- spelling_long |>
  tidyr::uncount(weights = count, .remove = FALSE) |>
  dplyr::select(-count)
# Coerce to numeric 
spelling_expanded$sex <- as.numeric(spelling_expanded$sex)
# Examine
table(spelling_expanded$sex)

# Model 1  - constrained baseline 
# MplusAutomation
model1 <- "f by infidelity* panoramic succumb (l1-l3);
          f by girder (l4);
          [infidelity$1] (t1) ; 
          [panoramic$1] (t2) ;
          [succumb$1] (t3) ; 
          [girder$1] (t4) ; 
          infidelity@1 panoramic@1 succumb@1 girder@1 ; 
           f@1 ; 
          MODEL MEN: F*;"

# Run Mplus with mplusModeler

mgcfa1 <- MplusAutomation::mplusModeler(
  MplusAutomation::mplusObject(
    VARIABLE = "USEVARIABLES = infidelity panoramic succumb girder sex;
                CATEGORICAL = infidelity panoramic succumb girder ;
                GROUPING = sex(1 = women 2 = men);" ,
    ANALYSIS = "ESTIMATOR = WLSMV; PARAMETERIZATION = THETA;" ,
    MODEL = model1 ,
    SAVEDATA = "H5RESULTS = mgcfa1.h5; SAVE = FSCORES; FILE=fscoresgirder1.dat;"  ,
    OUTPUT = "TECH4;" ,
    rdata = spelling_expanded
  ),
  "girder.dat", 
  run = TRUE)

coef(mgcfa1)

# Model 2  - Free Girder
model2 <- "f by infidelity* panoramic succumb (l1-l3);
          f by girder (l#4);
          [infidelity$1] (t1) ; 
          [panoramic$1] (t2) ;
          [succumb$1] (t3) ; 
          [girder$1] (t#4) ; 
          infidelity@1 panoramic@1 succumb@1 girder@1 ; 
           f@1 ; 
          MODEL MEN: F*;"

# Run Mplus with mplusModeler

mgcfa2 <- MplusAutomation::mplusModeler(
  MplusAutomation::mplusObject(
    VARIABLE = "USEVARIABLES = infidelity panoramic succumb girder sex;
                CATEGORICAL = infidelity panoramic succumb girder ;
                GROUPING = sex(1 = women 2 = men);" ,
    ANALYSIS = "ESTIMATOR = WLSMV; PARAMETERIZATION = THETA;" ,
    MODEL = model2 ,
    SAVEDATA = "H5RESULTS = mgcfa2.h5; SAVE = FSCORES; FILE=fscoresgirder2.dat;" ,
    OUTPUT = "RESIDUAL; TECH4;",
    rdata = spelling_expanded
  ),
  "girder2.dat", # because gh5 file will be named girder2.gh5
  run = TRUE)

coef(mgcfa2)

# salientDIF



# read the fscores 
# retrieve and merge files with factor scores
# code adapted from factor scores example

m1scores <- read.table("fscoresgirder1.dat", header = FALSE)
m2scores <- read.table("fscoresgirder2.dat", header = FALSE)


# Look at out files to find what the column names should be
colnames(m1scores) <- c("infi", "pano", "succ", "gird", "F", "F_SE", "sex")
colnames(m2scores) <- c("infi", "pano", "succ", "gird", "F", "F_SE", "sex")

# extract the factor scores and place in same data frame
# Select and rename columns
m1scores <- m1scores |> dplyr::select(F, sex) |> dplyr::rename(Fm1 = F)
m2scores <- m2scores |> dplyr::select(F) |> dplyr::rename(Fm2 = F)

scores <- as.data.frame(c(m1scores,m2scores))
cor(scores$Fm1,scores$Fm2)
scores$D <- scores$Fm2-scores$Fm1


summarytools::freq(round(scores$D,2))
# pick a threshold between some observed values
threshold <- .33

# Create the binary column salientD based on the absolute value of D
scores$salientD <- as.numeric(abs(scores$D) > threshold)

scores |>  dplyr::filter(sex == 1) |>  dplyr::pull(salientD) |>  mean(na.rm = TRUE)
scores |>  dplyr::filter(sex == 2) |>  dplyr::pull(salientD) |>  mean(na.rm = TRUE)


# Using the function 

# Tech4 output from model 1
# women
m01 <- 0
V01 <- 1
# men
m11 <- 0.248
V11 <- 1.514
# Tech4 output from model 2
# women
m02 <- 0
V02 <- 1
# men
m12 <- 0.004
V12 <- 1.652



salientDIF <- function(m1, s1, m2, s2, threshold = threshold) {
  
  # Calculate the mean of the difference D = x2 - x1
  m_D <- m2 - m1
  
  # Calculate the standard deviation of the difference D = x2 - x1
  s_D <- abs(s2 - s1)
  
  # Calculate the Z-scores for the positive and negative thresholds
  Z_positive <- (threshold - m_D) / s_D
  Z_negative <- (-threshold - m_D) / s_D
  
  # Calculate the probability that D is outside the range (-threshold, threshold)
  probability <- pnorm(Z_negative) + (1 - pnorm(Z_positive))
  
  # Return the calculated probability
  return(probability)
}

salientDIF_women <- salientDIF(m01,sqrt(V01),m02,sqrt(V02), threshold = threshold)
salientDIF_men <- salientDIF(m11,sqrt(V11),m12,sqrt(V12), threshold = threshold)

print(c(salientDIF_women,salientDIF_men))

