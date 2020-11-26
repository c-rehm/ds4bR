#### Set WD

#Assure the packages are installed
if (!suppressWarnings(require(ggplot2, quietly=TRUE)))
  install.packages("ggplot2"); library(ggplot2)
if (!suppressWarnings(require(readr, quietly=TRUE)))
  install.packages("readr"); library(readr)
if (!suppressWarnings(require(dplyr, quietly=TRUE)))
  install.packages("dplyr"); library(dplyr)

#Load Rawdata
rawdata <- read_csv("never_use_4R.csv", col_types = cols(`Age (or some other number)` = col_character()))
dat <- rawdata

#Data Cleaning
dat <- dat[- grep("^X[0-9]*$", colnames(dat))]
dat <- dat[! colnames(dat) %in% ("Date")]

colnames(dat)[colnames(dat) == "Age (or some other number)"] <- "Age"

dat$Outcome[startsWith(dat$Outcome, "a")] <- "alive"
dat$Outcome[startsWith(dat$Outcome, "d")] <- "dead"
dat$Outcome <- as.factor(dat$Outcome)

dat$Age <- gsub(",", ".", dat$Age)
dat$Age <- as.numeric(dat$Age)

dat <- subset(dat, Age < 120)


#Save as RDS
saveRDS(dat, file = "data.rds")

#Boxplot
dat.n <- dat
dat.n$n <- 1
dat.n <- aggregate(n~Outcome, dat.n, FUN = "sum")

ggplot(dat, aes(x = Outcome, y = Age)) + 
  geom_boxplot() + 
  geom_text(aes(x = Outcome, y = max(dat$Age) * 1.1, label = paste("N =", n)), data = dat.n)

ggsave("Results.pdf", plot=last_plot(), scale=1, units = c("mm"), dpi=300, limitsize=TRUE)

