# 0: Load the data in RStudio

  library(dplyr)

library(tidyr)
refine_original <- read.csv("refine_original.csv", check.names = FALSE)
refine_tidied <- refine_original
# 1: Clean up brand names
refine_tidied$company <- tolower(refine_tidied$company)
refine_tidied$company <- gsub(pattern = "Phillips|phillips|phllips|phillps|phillipS|fillips|phlips", replacement = "philips", x = refine_tidied$company)
refine_tidied$company <- gsub(pattern = "Akzo|AKZO|akz0|ak zo", replacement = "akzo", x = refine_tidied$company)
refine_tidied$company <- gsub(pattern = "unilver", replacement = "unilever", x = refine_tidied$company)

# 2: Separate product code and number
refine_tidied.df <- colnames(refine_tidied)[colnames(refine_tidied)=="Product code / number"] <- "product_code_and_number"
refine_tidied <- separate(refine_tidied, product_code_and_number, c("product_code", "product_number"), sep = "-")
View(refine_tidied)

# 3: Add product categories
refine_tidied <- refine_tidied %>% group_by(product_code)   %>%   mutate(product_category = product_code)
refine_tidied$product_category[refine_tidied$product_category == "p"] <- "Smartphone"
refine_tidied$product_category[refine_tidied$product_category == "v"] <- "TV"
refine_tidied$product_category[refine_tidied$product_category == "x"] <- "Laptop"
refine_tidied$product_category[refine_tidied$product_category == "q"] <- "Tablet"

# 4: Add full address for geocoding
refine_tidied_new <- refine_tidied %>% 
  +     unite(full_address, address:country, sep=",")

# 5: Create dummy variables for company and product category
refine_tidied_evennewer <- refine_tidied_new %>% mutate(company_philips = as.numeric(company == "philips")) %>% mutate(company_akzo = as.numeric(company == "akzo")) %>% mutate(company_van_houten = as.numeric(company == "van houten")) %>% mutate(company_unilever = as.numeric(company == "unilever"))
refine_clean <- refine_tidied_evennewer %>% mutate(product_smartphone = as.numeric(product_category == "Smarthphone")) %>% mutate(product_tv = as.numeric(product_category == "TV")) %>% mutate(product_laptop = as.numeric(product_category == "Laptop")) %>% mutate(product_tablet = as.numeric(product_category == "Tablet"))

# 6: Submit the project on Github
write.csv(refine_clean, file = "refine_clean.csv")