# Loading the Required Packages
pacman::p_load(readr, dplyr, tableone, ggplot2, stringr,
               ggsci, tidyr, magrittr,patchwork)

# Creating a custom function for descriptive statistics for
# categorical variables

descriptives <- function(x){
  x <- na.omit(x)
  y <- table(x)
  z <- round(prop.table(table(x)),4)*100
  print(y)
  print(z)
}

# Creating a function for factoring variables and labelling to 
# Yes and No

myfun <- function(x){
  factor(x, levels = c(0,1), 
         labels = c("Yes","No"))
}


# Creating a function for labeling likert scale
# variables on attitude

attifun <- function(x){
  factor(x,
         levels = c(1,2,3,4,5), 
         labels = c("Strongly Agree","Agree",
                    "Neutral",
                    "Disagree","Strongly Disagree"))
}

attifun2 <- function(x){
  factor(x,
         levels = c(1,2,3,4,5), 
         labels = c("Strongly Disagree", "Disagree",
                    "Neutral",
                    "Agree","Strongly Agree"))
}
# Loading the dataset

ds <- read_csv("ds.csv",
               show_col_types = FALSE)

#Plotting theme

common_theme <- function(){
  theme(axis.title = element_text(face = "bold")) +
    theme(axis.text.x = element_text( size = 10, face = "bold")) +
    theme(legend.text = element_text(size = 10, face = "bold")) +
    theme(legend.title = element_text(size = 10, face = "bold"))
}
