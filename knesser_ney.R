library(data.table)

PKN <- function(df, n){
  
  ## This function takes data frame containing n-grams and frequency of each one
  ## and returs the same df with Knesser Ney probabilities attached to each n-gram
  ## for further filtering according to user entries
  
  
  ## Calculation of constant Y, given by: Y = n1/(n1 + 2*n2)
  ## where n1 is the number of n-grams with frequency equals 1
  ## and n2 is the number of n-grams with frequency equals 2
  
  Y <- nrow(df[Freq == 1]) / (nrow(df[Freq == 1]) + 2*nrow(df[Freq == 2]))
  
  
  ## Calculation of discount factor D, which depends on frequency of each n-gram
  
  D1 <- 1 - 2*Y*(nrow(df[Freq == 2]) / nrow(df[Freq == 1]))
  D2 <- 2 - 3*Y*(nrow(df[Freq == 3]) / nrow(df[Freq == 2]))
  D3 <- 3 - 4*Y*(nrow(df[Freq == 4]) / nrow(df[Freq == 3]))
  
  ## Applying D to each n-gram depending on its frequency
  
  df[, D := 0] ## Initializing this factor with 0 value
  df[Freq == 1]$D <- D1
  df[Freq == 2]$D <- D2
  df[Freq >  2]$D <- D3
  
  
  ## Calculation of each term of knesser-ney probability
  
  ## Count of complete n-grams, called count1
  df[, count1 := pmax(Freq - D, 0)]
  
  ## Count of all n-grams with same first (n-1) terms, called count2
  if (n == 1) {
    df[, count2 := sum(Freq)]
  } else if (n == 2){
    df[, count2 := sum(Freq), by = term.1]
  } else if (n == 3){
    df[, count2 := sum(Freq), by = list(term.1, term.2)]
  } else if (n == 4){
    df[, count2 := sum(Freq), by = list(term.1, term.2, term.3)]
  }
  
  
  ## Calculation of constant lambda that makes the distribution sum to 1
  ## lambda = (D1*N1 + D2*N2 + D3*3) / count2
  
  ## N1, N2 and N3
  if (n == 1) {
    df[, N1 := sum(Freq==1)]
    df[, N2 := sum(Freq==2)]
    df[, N3 := sum(Freq >2)]
  } else if (n == 2){
    df[, N1 := sum(Freq==1), by = term.1]
    df[, N2 := sum(Freq==2), by = term.1]
    df[, N3 := sum(Freq >2), by = term.1]
  } else if (n == 3){
    df[, N1 := sum(Freq==1), by = list(term.1, term.2)]
    df[, N2 := sum(Freq==2), by = list(term.1, term.2)]
    df[, N3 := sum(Freq >2), by = list(term.1, term.2)]
  } else if (n == 4){
    df[, N1 := sum(Freq==1), by = list(term.1, term.2, term.3)]
    df[, N2 := sum(Freq==2), by = list(term.1, term.2, term.3)]
    df[, N3 := sum(Freq >2), by = list(term.1, term.2, term.3)]
  }
  
  ## Calculation of lambda = (D1*N1 + D2*N2 + D3*3) / count2
  df[, lambda := (N1*D1 + N2*D2 + N3*D3)/count2]
  
  
  ## Count of all n-grams with same last (n-1) terms, called count3
  if (n == 1) {
    df[, count3 := 0]
    } else if (n == 2){
    df[, count3 := sum(Freq > 0), by = term.2]
  } else if (n == 3){
    df[, count3 := sum(Freq > 0), by = list(term.2, term.3)]
  } else if (n == 4){
    df[, count3 := sum(Freq > 0), by = list(term.2, term.3, term.4)]
  }
  
  
  ## Count of all n-grams with same middle terms, called count4
  if (n == 1) {
    df[, count4 := sum(Freq > 0)]
  } else if (n == 2){
    df[, count4 := sum(Freq > 0)]
  } else if (n == 3){
    df[, count4 := sum(Freq > 0), by = term.2]
  } else if (n == 4){
    df[, count4 := sum(Freq > 0), by = list(term.2, term.3)]
  }
  
  ## Finally, the calculation of Pkn for each n-gram
  df[, Pkn := (count1/count2) + lambda*(count3/count4)]
  
}

## Load the term frequency matrices
onegramdata <- as.data.table(readRDS("onegram_terms_df.Rda"))
twogramdata <- as.data.table(readRDS("bigram_terms_df.Rda"))
threegramdata <- as.data.table(readRDS("trigram_terms_df.Rda"))
fourgramdata <- as.data.table(readRDS("quadgram_terms_df.Rda"))

dt1 <- PKN(onegramdata,1)
dt2 <- PKN(twogramdata,2)
dt3 <- PKN(threegramdata,3)
dt4 <- PKN(fourgramdata,4)

## Changing names of collunms to make possible merge all in one data table
colnames(dt1)[which(names(dt1) == "Terms")] <- "term.4"

colnames(dt2)[which(names(dt2) == "term.2")] <- "term.4"
colnames(dt2)[which(names(dt2) == "term.1")] <- "term.3"

colnames(dt3)[which(names(dt3) == "term.3")] <- "term.4"
colnames(dt3)[which(names(dt3) == "term.2")] <- "term.3"
colnames(dt3)[which(names(dt3) == "term.1")] <- "term.2"

## Merge all data tables, filling spaces with NA
dt <- rbind(dt4, dt3, dt2, dt1, fill = TRUE)

## Removing all temporary collumns used in calculation of Pkn
dt <- subset(dt, select = -c(Freq, D, count1, count2, N1, N2, N3, lambda, count3, count4))

## Save file
saveRDS(dt, file = "dt_kn.Rda")