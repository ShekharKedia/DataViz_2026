######################
# Data Viz  
# Tutorial 1:  
# Tidyverse and ggplot        
######################

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
    package.list <- setdiff(package.list, basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
    }
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg,  dependencies = TRUE)
    sapply(pkg,  require,  character.only = TRUE)
    }

# Load any necessary packages
lapply(c("tidyverse", "ggplot2"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################
### In class demo
#################

# load data
AB_ZIM <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/AB_ZIM.csv")
# reduce data
AB_ZIM <- AB_ZIM |> select(Q1, Q101, Q102, Q94A, Q97, Q98)
# organize data
AB_ZIM <- AB_ZIM |> 
  # rename some columns
  rename(age = `Q1`, 
         gender = `Q101`,
         interview_lang = `Q102`,
         employed = `Q94A`,
         religion = `Q97`,
         party_vote = `Q98`) 

str(AB_ZIM)

# histogram example
pdf("AB_ZIM_hist1.pdf")
ggplot(data = AB_ZIM, aes(x=age)) + 
  geom_histogram() 
dev.off()

pdf("AB_ZIM_hist2.pdf")
ggplot(data = AB_ZIM, aes(x=age)) +
  geom_histogram(binwidth = 10) + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100))
dev.off()

pdf("AB_ZIM_hist3.pdf")
ggplot(data = AB_ZIM, aes(x=age)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender))
dev.off()

pdf("AB_ZIM_hist4.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip()
dev.off()

pdf("AB_ZIM_hist5.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip() +
  labs(x="\nAge", y="\nCount", fill="Gender")
dev.off()

pdf("AB_ZIM_hist6.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip() +
  labs(x="\nAge", y="Count", fill="Gender") +
  theme_bw()
dev.off()

##############
### Group work
##############

# (1) Organize data yourself in groups using tidy
# (2) Create informative plots of example RQs
# (3) Start to add basic elements using ggplot

# Research questions: 
# What is the relationship between social demographic characteristics (education, employment, age, gender) & informal politics (official political parties vs traditional leaders)?

# download data from AfroBarometer (Malawi R10): https://www.afrobarometer.org/survey-resource/malawi-round-10-data-2024/
# here is the codebook: https://www.afrobarometer.org/survey-resource/malawi-round-10-codebook-2024/
AB_MALAWI <- read.csv("t1_data_WIP.csv")
View(df)

# reduce your data to these variables: 
# URBRUR - urban/rural respondent
# Q1 - age 
# Q101 - gender
# Q102 - interview language
# Q94A - employed
# Q97 - religion
# Q98 - voted for party in last election
# Q12B - contacted party official
# Q12C - contacted traditional leader

# rename your variables to informative/easy names
AB_MALAWI <- AB_MALAWI |> select(URBRUR, Q1, Q101, Q102, Q94A, Q97, Q98, Q12B, Q12C)

# rename some columns
AB_MALAWI <- AB_MALAWI |> 
rename(Rural_Urban = `URBRUR`, 
       age = `Q1`, 
       gender = `Q101`,
       interview_lang = `Q102`,
       employed = `Q94A`,
       religion = `Q97`,
       party_vote = `Q98`,
       party_off = `Q12B`,
       trad_off  = `Q12C`) 

str(AB_MALAWI)

AB_MALAWI$age <- as.integer(AB_MALAWI$age)

# create a couple of visualizations that shed light on our RQ

ggplot(data = AB_MALAWI, aes(x=age)) + 
  geom_histogram(binwidth = 10) 

# Example 1: How does the age distribution differ by political behaviour?
ggplot(AB_MALAWI, aes(x = age)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(vars(party_off))

# Example 2: Contacting traditional leaders by gender
ggplot(AB_MALAWI, aes(x = gender, fill = trad_off)) +
  geom_bar(position = "fill") +
  labs(
    title = "Contact with Traditional Leaders by Gender",
    x = "Gender",
    y = "Proportion"
  ) +
  theme_minimal()

#geom_bar() uses stat = "count" by default

## Whether men and women differ in their likelihood of contacting traditional leaders.

# Example 3: Contacting party officials by urban/rural status
ggplot(AB_MALAWI, aes(x = Rural_Urban, fill = party_off)) +
  geom_bar(position = "fill") +
  labs(
    title = "Contact with Party Officials by Urban/Rural Residence",
    y = "Proportion",
    x = "Urban / Rural"
  ) +
  theme_minimal()

## Differences in formal political engagement across space.

# Example 4: Age Distribution by Political Contact Type
ggplot(AB_MALAWI, aes(x = age, fill = party_off)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(
    title = "Age Distribution by Contact with Party Officials",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()

# geom_histogram(binwidth = 5(anything else), alpha = 0.6(0-1), position = "identity/stack/fill")

## Whether contacting party officials is more common among certain age groups.

# Example 5: Employment Status & Informal Politics
ggplot(AB_MALAWI, aes(x = employed, fill = trad_off)) +
  geom_bar(position = "fill") +
  labs(
    title = "Employment Status and Contact with Traditional Leaders",
    x = "Employment Status",
    y = "Proportion"
  ) +
  theme_minimal()

# Example 6: Voting Behaviour & Political Contact

ggplot(AB_MALAWI, aes(x = party_vote, fill = party_off)) +
  geom_bar(position = "fill") +
  labs(
    title = "Voting Behaviour and Contact with Party Officials",
    x = "Voted in Last Election",
    y = "Proportion"
  ) +
  theme_minimal()

## Whether voters are more likely to engage with party officials.


# (we will present your "findings" to the class)
