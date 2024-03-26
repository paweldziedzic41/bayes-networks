install.packages("tidyverse")
library(tidyverse)

# Loading working directory and data
setwd("C:/Users/gosc/Documents/Niepewnosci_projekt")
data <- read.csv("ds_salaries.csv",header = TRUE,sep = ",")

# Shape of dataframe
dim(data)

# Formating data
data <- subset(data, select = -c(X,salary,salary_currency,
                                 employee_residence,company_location))

unique(data$experience_level)
data$experience_level[data$experience_level == "EN"] <- "Junior"
data$experience_level[data$experience_level == "MI"] <- "Mid"
data$experience_level[data$experience_level == "SE"] <- "Senior"
data$experience_level[data$experience_level == "EX"] <- "Director"

unique(data$employment_type)
data$employment_type[data$employment_type == "FT"] <- "Full-time"
data$employment_type[data$employment_type == "PT"] <- "Part-time"
data$employment_type[data$employment_type == "CT"] <- "Contract"
data$employment_type[data$employment_type == "FL"] <- "Freelance"

unique(data$remote_ratio)
data$remote_ratio <- as.character(data$remote_ratio)
data$remote_ratio[data$remote_ratio == "0"] <- "Office"
data$remote_ratio[data$remote_ratio == "50"] <- "Hybrid"
data$remote_ratio[data$remote_ratio == "100"] <- "Remote"

unique(data$company_size)
data$company_size[data$company_size== "L"] <- "Large"
data$company_size[data$company_size == "M"] <- "Medium"
data$company_size[data$company_size == "S"] <- "Small"

data$salary_in_usd <- round(data$salary_in_usd/12)
data$salary_usd_per_year[data$salary_in_usd < 5000] <- "<5000"
data$salary_usd_per_year[data$salary_in_usd >= 5000 & data$salary_in_usd < 12500] <- "5000-12500"
data$salary_usd_per_year[data$salary_in_usd >= 12500 & data$salary_in_usd <= 20000] <- "12500-20000"
data$salary_usd_per_year[data$salary_in_usd >= 20000] <- ">20000"
data <- subset(data, select = -c(salary_in_usd))


# Probabilities count
column_names <- names(data)
probabilities <- list(1,2,3,4,5,6,7)
names(probabilities) <- column_names


## Count all probabilities
for (j in 1:7){
  unique_value <- unique(data[,j])
  omega <- length(data[,j])
  
  for (i in 1:length(unique_value)){
    event <- length(data[,j][data[,j] == unique_value[i]])
    probabilities[[j]][i] <- event/omega
  }
}

probabilities

# Variable independence

ind_prob_work_year <- list(probabilities[[1]],2,3,4,5,6,7)
ind_prob_work_year

## conditional probabilities
### work_year
### Prawdopodobientswo work_year '2020' pod warunkiem exp_level 'Mid'
length(data$work_year[data$work_year == 2020 & data$experience_level == 'Mid'])/length(data$experience_level == 'Mid')
typeof(data$experience_level)

length(probabilities$job_title)


