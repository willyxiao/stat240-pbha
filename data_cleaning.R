install.packages("chron")

library(stringr)
library(dplyr)
library(chron)

pbha.raw <- read.csv("PBHA.csv", stringsAsFactors = FALSE)
pbha.raw$Treated <- c(rep(1,750), rep(0, nrow(pbha.raw)-750))
names <- read.csv("names.csv", stringsAsFactors = FALSE)
male.names <- unique(names$male)
male.names <- male.names[male.names != ""]
female.names <- unique(names$female)
female.names <- female.names[female.names != ""]

pbha <- transmute(pbha.raw,
                  Y = ifelse(is.na(Last.Gift.Amount), 0, (dates(Last.Gift.Date) >= dates("11/01/14")) * Last.Gift.Amount),
                  z = Treated,
                  sal = Salutation,
                  full.name = Full.Name,
                  no.grad.year = as.numeric(is.na(College.graduation.year)),
                  grad.year = pmax(College.graduation.year - min(College.graduation.year, na.rm = T), 0, na.rm = T),
                  grad.year2 = grad.year^2,
                  leader = as.numeric(PBHA.Leadership.Roles.Held != ""),
                  no.mail = Do.Not.Snail.Mail,
                  total.gift = Total.Gifts,
                  total.hh.gift = Total.Household.Gifts,
                  prev.gift = Total.Household.Gifts - y,
                  email = as.numeric(!Email.Opt.Out),
                  MA = as.numeric(Mailing.State.Province == "MA"),
                  CA = as.numeric(Mailing.State.Province == "CA"),
                  NY = as.numeric(Mailing.State.Province == "NY"),
                  is.dr = as.numeric(Salutation == "Dr." | Salutation == "Drs." | Salutation == "Dr. & Mrs."),
                  phone = as.numeric(nchar(as.character(Phone)) > 0),
                  first.name = NA,
                  is.male = NA,
                  is.female = NA
                  )

# Assign gender on salutation and name
for (ii in 1:nrow(pbha)) {
  pbha$first.name[ii] <- str_split(pbha$full.name[ii], " ")[[1]][1]
  pbha$is.male[ii] <- pbha$sal[ii] %in% c("Mr.", "Rev.", "Rev. Mr.", "The Reverend") || pbha$first.name[ii] %in% male.names 
  pbha$is.female[ii] <- pbha$sal[ii] %in% c("Ms.", "Mrs.") || pbha$first.name[ii] %in% female.names
}
# Use salutation to override name
for (ii in which(pbha$is.male & pbha$is.female)) {
  pbha$is.male[ii] <- pbha$sal[ii] %in% c("Mr.", "Rev.", "Rev. Mr.", "The Reverend", "Mr. Tam", "The Rev.")
  pbha$is.female[ii] <- pbha$sal[ii] %in% c("Ms.", "Mrs.")
}
# Hard-coding from internet creeping
pbha$is.female[which(pbha$full.name == "Morgan Bradylyons")] <- TRUE
pbha$is.female[which(pbha$full.name == "Hui Hua Wan")] <- TRUE
pbha$is.male[which(pbha$full.name == "Quan Cai")] <- TRUE
pbha$is.male[which(pbha$full.name == "W. Hamner")] <- TRUE
pbha$is.male[which(pbha$full.name == "K. Riew")] <- TRUE
pbha$is.male[which(pbha$full.name == "Jong Yung")] <- TRUE
pbha$is.male[which(pbha$full.name == "R. Ramnath")] <- TRUE

# Assign any remaining unclassified 0.5
pbha$sex <- with(pbha, 1*is.male + 0.5*(!is.male & !is.female))

save(pbha, file = "pbha.Rdata")
