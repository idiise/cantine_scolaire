
# import package ----------------------------------------------------------

library(tidyverse)
library(xlsx)
library(writexl)


# Import dataset ----------------------------------------------------------

dataset <- read.xlsx("Data/School canteen Needs and Prioritization_ 18.12.2023.xlsx",sheetName = 1)
nb_school <- length(unique(dataset$Ecoles.))
# 91 école sont concernées par l'étude
nb_student <- sum(dataset$Effectif.)
# le nombre total d'élève est de 36135

# number of school and student by region
dataset <- dataset |> rename(
  ecole = Ecoles.
)
nb_sch_stud_by_region <- dataset |> select(Région,ecole) |> 
  group_by(
  Région
) |> summarise(nb_school = n())

nb_stud_by_region <- dataset |> select(Région,Effectif.) |> 
  group_by(
    Région
  ) |> summarise(nb_student = sum(Effectif.))

nb_stu_sch_by_region <- nb_sch_stud_by_region |> left_join(
  nb_stud_by_region, by = "Région"
)

min(dataset$Effectif.)
max(dataset$Effectif.)
mean(dataset$Effectif.)
median(dataset$Effectif.)

test <- dataset |> filter(
  Effectif. >= median(Effectif.)
)

# critere1 <- dataset$LOT..chose.which.lot.the.school.should.be.part.of...=="Lot1"
critere2 <- dataset$Effectif. >= median(dataset$Effectif.)
critere3 <- dataset$Proximity.of.school.to.WFP.Office %in% c("Less than 30mins","30mins to 1hr")
critere4 <- dataset$State.of.roads.to.School == "Good"
critere5 <- dataset$Accessibility == "Easy"
critere6 <- dataset$Priority.need == "Canteen"
#  je regarderai l'approvisonnement de l'eau

dataset2 <- dataset |> mutate(
  score_global = critere2 + critere3 + critere4 + critere5 + critere6
)

dataset2 <- dataset2 |> arrange(desc(score_global))
ecole_selectionne <- head(dataset2,15)
write_xlsx(ecole_selectionne, "Data/ecole.xlsx")
