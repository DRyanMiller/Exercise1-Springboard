install.packages("RecordLinkage")
install.packages("stringr")

refine_df <- data.frame(refine_original)

library(dplyr)
library(tidyr)
library(RecordLinkage)

#convert company to lower case
refine_df$company <- tolower(refine_df$company)

#correct spelling errors in company name
refine_df$company <- sub(".*\\ps$", "philips", refine_df$company)
refine_df$company <- sub("ak.*$", "akzo", refine_df$company)
refine_df$company <- sub("van .*$", "van houten", refine_df$company)
refine_df$company <- sub("uni.*$", "unilever", refine_df$company)

#ALTERNATIVE code for correcting spelling
for(i in 1:length(refine_df$company)) {
  if(jarowinkler(refine_df$company[i], "philips") >0.8) {
    refine_df$company2[i] <- "philips"
  } else if(jarowinkler(refine_df$company[i], "akzo") >0.8) {
    refine_df$company2[i] <- "akzo"
  } else if(jarowinkler(refine_df$company[i], "van houten") >0.8) {
    refine_df$company2[i] <- "van houten"
  } else if(jarowinkler(refine_df$company[i], "unilever") >0.8) {
    refine_df$company2[i] <- "unilever"
  }
}

#create product_code column
refine_df$product_code <- (substring(as.character(refine_df$Product.code...number), 1, 1))
refine_df$product_number <- substring(as.character(refine_df$Product.code...number), 3,)
prod_codes <- prod_codes <- cbind(as.character(c("p", "v", "x", "q")), as.character(c("Smartphone", "TV", "Laptop", "Tablet")))
data.frame(prod_codes)
colnames(prod_codes) <- c("product_code", "product")
str(prod_codes)

for(i in 1:length(refine_df$product_code)){
  if(refine_df$product_code[i] == "p") {
  refine_df$product_cat[i] <- "Smartphone"
  next
} else if(refine_df$product_code[i] == "v") {
  refine_df$product_cat[i] <- "TV"
  next
} else if(refine_df$product_code[i] == "x") {
  refine_df$product_cat[i] <- "Laptop"
  next
} else if(refine_df$product_code[i] == "q") {
  refine_df$product_cat[i] <- "Tablet"
} 
}

#Create a full_address field
refine_df$full_address <- paste(refine_df$address, refine_df$city, refine_df$country, sep = ", ")

#Create dummy variables for company and product
for(i in 1:length(refine_df$company)) {
  if(refine_df$company[i] == "philips") {
  refine_df$company_philips[i] = 1
} else {
  refine_df$company_philips[i] = 0
}
}

for(i in 1:length(refine_df$company)) {
  if(refine_df$company[i] == "akzo") {
    refine_df$company_akzo[i] = 1
  } else {
    refine_df$company_akzo[i] = 0
  }
}

for(i in 1:length(refine_df$company)) {
  if(refine_df$company[i] == "van houten") {
    refine_df$company_van_houten[i] = 1
  } else {
    refine_df$company_van_houten[i] = 0
  }
}

for(i in 1:length(refine_df$company)) {
  if(refine_df$company[i] == "unilever") {
    refine_df$company_unilever[i] = 1
  } else {
    refine_df$company_unilever[i] = 0
  }
}

for(i in 1:length(refine_df$product_code)) {
  if(refine_df$product_code[i] == "p") {
    refine_df$product_smartphone[i] = 1
  } else {
    refine_df$product_smartphone[i] = 0
  }
}

for(i in 1:length(refine_df$product_code)) {
  if(refine_df$product_code[i] == "v") {
    refine_df$product_tv[i] = 1
  } else {
    refine_df$product_tv[i] = 0
  }
}

for(i in 1:length(refine_df$product_code)) {
  if(refine_df$product_code[i] == "x") {
    refine_df$product_laptop[i] = 1
  } else {
    refine_df$product_laptop[i] = 0
  }
}

for(i in 1:length(refine_df$product_code)) {
  if(refine_df$product_code[i] == "q") {
    refine_df$product_tablet[i] = 1
  } else {
    refine_df$product_tablet[i] = 0
  }
}