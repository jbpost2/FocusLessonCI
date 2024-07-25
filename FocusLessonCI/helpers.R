library(tidyverse)
library(tidycensus)
require(plotrix)

my_key <- "e267f117801b2ef741e54620602b0903c5f4d3c8"

##########################################
#Info about the data downloaded

state_codes <- c(
    "01"= "Alabama/AL",
    "49"= "Utah/UT",
    "21"= "Kentucky/KY",
    "26"= "Michigan/MI",
    "29"= "Missouri/MO",
    "32"= "Nevada/NV",
    "34"= "New Jersey/NJ",
    "08"= "Colorado/CO",
    "51"= "Virginia/VA",
    "39"= "Ohio/OH",
    "02"= "Alaska/AK",
    "46"= "South Dakota/SD",
    "04"= "Arizona/AZ",
    "06"= "California/CA",
    "55"= "Wisconsin/WI",
    "15"= "Hawaii/HI",
    "22"= "Louisiana/LA",
    "30"= "Montana/MT",
    "47"= "Tennessee/TN",
    "48"= "Texas/TX",
    "09"= "Connecticut/CT",
    "50"= "Vermont/VT",
    "53"= "Washington/WA",
    "17"= "Illinois/IL",
    "20"= "Kansas/KS",
    "72"= "Puerto Rico/PR",
    "35"= "New Mexico/NM",
    "36"= "New York/NY",
    "10"= "Delaware/DE",
    "11"= "District of Columbia/DC",
    "12"= "Florida/FL",
    "56"= "Wyoming/WY",
    "16"= "Idaho/ID",
    "25"= "Massachusetts/MA",
    "27"= "Minnesota/MN",
    "42"= "Pennsylvania/PA",
    "45"= "South Carolina/SC",
    "13"= "Georgia/GA",
    "23"= "Maine/ME",
    "24"= "Maryland/MD",
    "28"= "Mississippi/MS",
    "37"= "North Carolina/NC",
    "41"= "Oregon/OR",
    "05"= "Arkansas/AR",
    "19"= "Iowa/IA",
    "31"= "Nebraska/NE",
    "33"= "New Hampshire/NH",
    "44"= "Rhode Island/RI",
    "54"= "West Virginia/WV",
    "18"= "Indiana/IN",
    "38"= "North Dakota/ND",
    "40"= "Oklahoma/OK"
)

state_names <- sapply(str_split(state_codes, "/"), FUN = function(x){x[1]})
names(state_names) <- names(state_codes)

variable_info <- c(
  "HHL" = "Household language",
  "HHLANP" = "Detailed household language",
  "LANP" = "Language spoken at home",
  "LANX" = "Language other than English spoken at home",
  "FLANP" = "Language spoken at home allocation flag",
  "FLANXP" = "Language other than English allocation flag",
  "FFSP" = "Yearly food stamp/Supplemental Nutrition Assistance Program (SNAP) recipiency allocation flag",
  "WAOB" = "World area of birth",
  "FER" = "Gave birth to child within the past 12 months",
  "SCHL" = "Educational attainment",
  "SCH" = "School enrollment",
  "FSCHP" = "School enrollmetn allocation flag",
  "RAC1P" = "Recoded detailed race code"
)

RAC1Pvals <- c(
  "3"= "American Indian alone",
  "1"= "White alone",
  "6"= "Asian alone",
  "9"= "Two or More Races",
  "4"= "Alaska Native alone",
  "2"= "Black or African American alone",
  "7"= "Native Hawaiian and Other Pacific Islander alone",
  "5"= "American Indian and Alaska Native tribes specified; or American Indian or Alaska Native, not specified and no other races",
  "8"= "Some other race alone"
)

FSCHPvals <- c(
  "0"= "No",
  "1"= "Yes"
)      


SCHvals <- c(
  "0"= "N/A (less than 3 years old)",
  "3"= "Yes, private school or college or home school",
  "1"= "No, has not attended in the last 3 months",
  "2"= "Yes, public school or public college"
)      

SCHLvals <- c(
  "16"= "Regular high school diploma",
  "01"= "No schooling completed",
  "04"= "Grade 1",
  "03"= "Kindergarten",
  "07"= "Grade 4",
  "23"= "Professional degree beyond a bachelor's degree",
  "19"= "1 or more years of college credit, no degree",
  "22"= "Master's degree",
  "10"= "Grade 7",
  "20"= "Associate's degree",
  "0"= "N/A (less than 3 years old)",
  "02"= "Nursery school, preschool",
  "21"= "Bachelor's degree",
  "08"= "Grade 5",
  "24"= "Doctorate degree",
  "06"= "Grade 3",
  "14"= "Grade 11",
  "17"= "GED or alternative credential",
  "12"= "Grade 9",
  "15"= "12th grade - no diploma",
  "13"= "Grade 10",
  "05"= "Grade 2",
  "11"= "Grade 8",
  "18"= "Some college, but less than 1 year",
  "09"= "Grade 6"
)

FERvals <- c(
  "0" = "N/A (less than 15 years/greater than 50 years/ male)",
  "2" = "No",
  "1" = "Yes"
)

FFSPvals <- c(
  "1" = "Yes",
  "0" = "No"
)      


WAOBvals <- c(
  "8" = "Oceania and at Sea (POBP = 060,500-554)",
  "1" = "US state (POBP = 001-059)",
  "2" = "PR and US Island Areas (POBP = 061-099)",
  "6" = "Africa (POBP = 400-499)",
  "3" = "Latin America (POBP = 303,310-399)",
  "7" = "Northern America (POBP = 300-302,304-309)",
  "4" = "Asia (POBP = 158-159,161,200-299)",
  "5" = "Europe (POBP = 100-157,160,162-199)"
)

#language values
HHLvals <- c( #"Household language",
  "0" = "N/A (GQ/vacant)",
  "2" = "Spanish",
  "3" = "Other Indo-European languages",
  "1" = "English Only",
  "4" = "Asian and Pacific Island languages",
  "5" = "Other Language")

HHLANPvals <- c( #"Detailed household language",
  "1210"= "Portuguese",
  "1350"= "Hindi",
  "2560"= "Japanese",
  "5940"= "Fulah",
  "1765"= "Tamil",
  "1263"= "Slovak",
  "1134"= "Afrikaans",
  "1025"= "Other English-based Creole languages",
  "1540"= "Other Indo-Iranian languages",
  "2475"= "Lao",
  "2030"= "Min Nan Chinese",
  "1970"= "Chinese",
  "1737"= "Kannada",
  "7050"= "Cherokee",
  "1500"= "Nepali",
  "2715"= "Malay",
  "1900"= "Khmer",
  "1130"= "Yiddish",
  "1250"= "Russian",
  "1141"= "Danish",
  "1262"= "Czech",
  "1140"= "Swedish",
  "1273"= "Bulgarian",
  "2770"= "Indonesian",
  "1276"= "Bosnian",
  "1565"= "Finnish",
  "3500"= "Tongan",
  "1340"= "India N.E.C.",
  "1582"= "Hungarian",
  "6120"= "Akan (incl. Twi)",
  "1000"= "Jamaican Creole English",
  "3420"= "Samoan",
  "1750"= "Malayalam",
  "9999"= "Other and unspecified languages",
  "6300"= "Edoid languages",
  "1132"= "Dutch",
  "6933"= "Navajo",
  "1270"= "Polish",
  "1420"= "Punjabi",
  "6290"= "Yoruba",
  "3190"= "Other Philippine languages",
  "6500"= "Other Niger-Congo languages",
  "1450"= "Gujarati",
  "4830"= "Oromo",
  "5525"= "Shona",
  "6230"= "Gbe languages",
  "9500"= "English only household",
  "5950"= "Wolof",
  "1069"= "Kabuverdianu",
  "2050"= "Cantonese",
  "6370"= "Igbo",
  "1235"= "Greek",
  "6800"= "Aleut languages",
  "3150"= "Ilocano",
  "3270"= "Marshallese",
  "2575"= "Korean",
  "1380"= "Bengali",
  "1281"= "Lithuanian",
  "7032"= "Muskogean languages",
  "2270"= "Chin languages",
  "2160"= "Burmese",
  "4590"= "Amharic",
  "1120"= "Swiss German",
  "4640"= "Tigrinya",
  "2850"= "Other languages of Asia",
  "6795"= "Other languages of Africa",
  "1142"= "Norwegian",
  "2000"= "Mandarin",
  "3570"= "Hawaiian",
  "2910"= "Filipino",
  "7300"= "Other Central and South American languages",
  "1440"= "Marathi",
  "3220"= "Chamorro",
  "2920"= "Tagalog",
  "1155"= "Italian",
  "1292"= "Dari",
  "1290"= "Farsi",
  "1327"= "Pashto",
  "4840"= "Somali",
  "2950"= "Cebuano",
  "7060"= "Uto-Aztecan languages",
  "N"= "N/A (GQ/vacant)",
  "4880"= "Other Afro-Asiatic languages",
  "2100"= "Tibetan",
  "1125"= "Pennsylvania German",
  "5845"= "Manding languages",
  "7124"= "Other Native North American languages",
  "2350"= "Karen languages",
  "3600"= "Other Eastern Malayo-Polynesian languages",
  "5900"= "Other Mande languages",
  "1278"= "Serbian",
  "4545"= "Hebrew",
  "2535"= "Hmong",
  "1220"= "Romanian",
  "1231"= "Irish",
  "1242"= "Albanian",
  "1360"= "Urdu",
  "4500"= "Arabic",
  "5345"= "Ganda",
  "1274"= "Macedonian",
  "6839"= "Ojibwa",
  "2525"= "Iu Mien",
  "1675"= "Turkish",
  "7019"= "Dakota languages",
  "4560"= "Assyrian Neo-Aramaic",
  "1175"= "Cajun French",
  "1690"= "Mongolian",
  "1564"= "Other Indo-European languages",
  "1200"= "Spanish",
  "1288"= "Armenian",
  "2430"= "Thai",
  "1730"= "Telugu",
  "4565"= "Chaldean Neo-Aramaic",
  "1110"= "German",
  "5150"= "Swahili",
  "4900"= "Nilo-Saharan languages",
  "6930"= "Apache languages",
  "1260"= "Ukrainian",
  "1530"= "Sinhala",
  "1275"= "Serbocroatian",
  "1283"= "Latvian",
  "1315"= "Kurdish",
  "1435"= "Konkani",
  "1277"= "Croatian",
  "6205"= "Ga",
  "1055"= "Haitian",
  "3350"= "Chuukese",
  "1170"= "French",
  "1960"= "Vietnamese",
  "5645"= "Other Bantu languages"
)

LANPvals <- c( #"Language spoken at home",
  "1220"= "Romanian",
  "2030"= "Min Nan Chinese",
  "1730"= "Telugu",
  "1970"= "Chinese",
  "4565"= "Chaldean Neo-Aramaic",
  "1231"= "Irish",
  "3420"= "Samoan",
  "N"= "N/A (GQ/vacant)",
  "6800"= "Aleut languages",
  "4640"= "Tigrinya",
  "1132"= "Dutch",
  "2850"= "Other languages of Asia",
  "6839"= "Ojibwa",
  "5900"= "Other Mande languages",
  "7300"= "Other Central and South American languages",
  "7019"= "Dakota languages",
  "7032"= "Muskogean languages",
  "1450"= "Gujarati",
  "1565"= "Finnish",
  "6230"= "Gbe languages",
  "7050"= "Cherokee",
  "1000"= "Jamaican Creole English",
  "1242"= "Albanian",
  "6370"= "Igbo",
  "1130"= "Yiddish",
  "2000"= "Mandarin",
  "1273"= "Bulgarian",
  "1420"= "Punjabi",
  "2920"= "Tagalog",
  "1278"= "Serbian",
  "6500"= "Other Niger-Congo languages",
  "1170"= "French",
  "1200"= "Spanish",
  "1210"= "Portuguese",
  "1110"= "German",
  "2950"= "Cebuano",
  "5940"= "Fulah",
  "4590"= "Amharic",
  "1360"= "Urdu",
  "4880"= "Other Afro-Asiatic languages",
  "2575"= "Korean",
  "2910"= "Filipino",
  "3600"= "Other Eastern Malayo-Polynesian languages",
  "1540"= "Other Indo-Iranian languages",
  "6290"= "Yoruba",
  "4560"= "Assyrian Neo-Aramaic",
  "3500"= "Tongan",
  "4830"= "Oromo",
  "4840"= "Somali",
  "5950"= "Wolof",
  "1120"= "Swiss German",
  "1250"= "Russian",
  "1530"= "Sinhala",
  "1275"= "Serbocroatian",
  "2770"= "Indonesian",
  "1281"= "Lithuanian",
  "1277"= "Croatian",
  "6205"= "Ga",
  "4545"= "Hebrew",
  "3190"= "Other Philippine languages",
  "1175"= "Cajun French",
  "1327"= "Pashto",
  "6120"= "Akan (incl. Twi)",
  "1350"= "Hindi",
  "2160"= "Burmese",
  "5150"= "Swahili",
  "2100"= "Tibetan",
  "6795"= "Other languages of Africa",
  "7124"= "Other Native North American languages",
  "1140"= "Swedish",
  "1380"= "Bengali",
  "1134"= "Afrikaans",
  "1270"= "Polish",
  "3220"= "Chamorro",
  "1283"= "Latvian",
  "1155"= "Italian",
  "1290"= "Farsi",
  "1564"= "Other Indo-European languages",
  "1960"= "Vietnamese",
  "2560"= "Japanese",
  "1900"= "Khmer",
  "9999"= "Other and unspecified languages",
  "6300"= "Edoid languages",
  "3150"= "Ilocano",
  "3270"= "Marshallese",
  "1765"= "Tamil",
  "1125"= "Pennsylvania German",
  "2350"= "Karen languages",
  "2475"= "Lao",
  "1315"= "Kurdish",
  "1276"= "Bosnian",
  "1690"= "Mongolian",
  "1288"= "Armenian",
  "5525"= "Shona",
  "2430"= "Thai",
  "1340"= "India N.E.C.",
  "1582"= "Hungarian",
  "2270"= "Chin languages",
  "1737"= "Kannada",
  "2715"= "Malay",
  "2050"= "Cantonese",
  "1235"= "Greek",
  "1750"= "Malayalam",
  "4900"= "Nilo-Saharan languages",
  "6930"= "Apache languages",
  "6933"= "Navajo",
  "5345"= "Ganda",
  "1142"= "Norwegian",
  "1262"= "Czech",
  "1260"= "Ukrainian",
  "1274"= "Macedonian",
  "1025"= "Other English-based Creole languages",
  "1440"= "Marathi",
  "2525"= "Iu Mien",
  "1435"= "Konkani",
  "1675"= "Turkish",
  "1055"= "Haitian",
  "3350"= "Chuukese",
  "2535"= "Hmong",
  "1500"= "Nepali",
  "1069"= "Kabuverdianu",
  "7060"= "Uto-Aztecan languages",
  "4500"= "Arabic",
  "5845"= "Manding languages",
  "1263"= "Slovak",
  "1141"= "Danish",
  "3570"= "Hawaiian",
  "1292"= "Dari",
  "5645"= "Other Bantu languages"
)

LANXvals <- c(#"Language other than English spoken at home",
  "2"= "No, speaks only English",
  "0"= "N/A (less than 5 years old)",
  "1"= "Yes, speaks another language"
)

FLANPvals <- c( #"Language spoken at home allocation flag"
  "0"= "No",
  "1"= "Yes"
)

FLANXPvals <- c(#"Language other than English allocation flag",
  "0"= "No",
  "1"= "Yes"
  )

WAOBvals2 <- WAOBvals %>% 
  str_split(" \\(") %>% 
  sapply(FUN = function(x){x[1]})
names(WAOBvals2) <- names(WAOBvals)


#########################################
#functions to create intervals

#basic parametric interval
basicCI <- function(y, n, alpha = 0.05){
  p_hat <- y/n
  c(p_hat - qnorm(1-alpha/2) * sqrt(p_hat*(1-p_hat)/n), p_hat + qnorm(1-alpha/2) * sqrt(p_hat*(1-p_hat)/n))  
}

#find Score interval
scoreCI <- function(y, n, alpha = 0.05){
  c((y/n+qnorm(1-alpha/2)^2/(2*n)-qnorm(1-alpha/2)*sqrt((y/n*(1-y/n)+qnorm(1-alpha/2)^2/(4*n))/n))/(1+qnorm(1-alpha/2)^2/n), (y/n+qnorm(1-alpha/2)^2/(2*n)+qnorm(1-alpha/2)*sqrt((y/n*(1-y/n)+qnorm(1-alpha/2)^2/(4*n))/n))/(1+qnorm(1-alpha/2)^2/n))
}

#find bootstrap interval
bootInt <- function(y, n, alpha = 0.05, B = 500){
  bootMLEs <- rbinom(n = B, size = n, prob = y/n)/n
  return(quantile(bootMLEs, c(alpha/2, 1-alpha/2)))
}


#########################################
#functions for sampling data
get_sample <- function(var, ages, state, year){
  get_pums(
    variables = var, 
    variables_filter = list(AGEP = ages),
    state = state,
    recode = TRUE,
    year = year,
    survey = "acs1",
    key = my_key
  )
}

mycolor <- function(endpoints, par) {
  if (par < endpoints[1]) 
    "Red"  # if the mean is below the left endpoint of the confidence interval
  else if (par > endpoints[2]) 
    "Red"  # if the mean is above the right endpoint of the confidence interval
  else "Black"  # if the mean lies between the endpoints
}

add_color <- function(CIdf, truth){
  apply(FUN = mycolor, X = CIdf, MARGIN = 1, par = truth)
}

add_ci <- function(data_df, type = 'basic', alpha = 0.05, B = 500){
    y <- data_df$y
    n <- data_df$n[1]
    truth <- data_df$truth[1]

    if(type == "basic"){
      CIdf <- do.call(rbind, lapply(X = y, FUN = basicCI, n, alpha)) |> as.data.frame()
    } else if(type == "score"){
      CIdf <- do.call(rbind, lapply(X = y, FUN = scoreCI, n, alpha)) |> as.data.frame()
    } else if(type == "bootstrap"){
      ci_values <- bootInt(y, n, alpha, B)
      CIdf <- do.call(rbind, lapply(X = y, FUN = bootInt, n, alpha, B = 500)) |> as.data.frame()
    } 
    names(CIdf) <- c("lower", "upper")
    CIdf$col <- add_color(CIdf, truth)
    CIdf$type <- type
    
    return(cbind(data_df, CIdf))
}



#########################################
#function for plotting
plot_CI <- function(CIdf){
  if(CIdf$type[1] == "basic"){
    message <- paste0("Visualization of Basic Parametric CIs")
  } else if(CIdf$type[1] == "score"){
    message <- paste0("Visualization of Score Based CIs")
  } else {
    message <- paste0("Visualization of Bootstrapped CIs")
  }
  truth <- CIdf$truth[1]
  CIdf$phat <- CIdf$y/CIdf$n
  CIdf$sample_number <- 1:nrow(CIdf)
  CIdf$`Sample Proportion` <- round(CIdf$phat, 4)
  CIdf$`Lower Bound` <- round(CIdf$lower, 4)
  CIdf$`Upper Bound` <- round(CIdf$upper, 4)
  CIdf$`Group Name` <- as.character(CIdf$group)
  CIdf$`True Proportion` <- round(CIdf$truth, 4)
  CIdf$Capture <- ifelse(CIdf$col == "Black", "Yes", "No")
  
  if(nrow(CIdf) < 50){
    g <- ggplot(CIdf, aes(x = sample_number, label2 = `Group Name`, label3 = `Sample Proportion`, label4 = `Lower Bound`, label5 = `Upper Bound`, label6 = `True Proportion`)) +
      geom_point(aes(x = sample_number, y = phat, color = Capture), size = 3) +
      geom_segment(aes(x = sample_number, xend = sample_number, y = lower, yend = upper, color = Capture), lineend = "square", linewidth = 1) +
      scale_color_manual(values = c("Yes" = "black", "No" = "red")) +
      geom_hline(data = tibble(yintercept = CIdf$truth[1], `True Proportion` = CIdf$`True Proportion`[1]),
                 linewidth = 1.5,
                 mapping = aes(yintercept = yintercept, label7 = `True Proportion`)) +
      xlab("Sample Data Set") + 
      ylab("Intervals") +
      ggtitle(message) + 
      theme_light() 
  } else {
    g <- ggplot(CIdf, aes(x = sample_number, label2 = `Group Name`, label3 = `Sample Proportion`, label4 = `Lower Bound`, label5 = `Upper Bound`, label6 = `True Proportion`)) +
      geom_point(aes(x = sample_number, y = phat, color = Capture), size = 1) +
      geom_segment(aes(x = sample_number, xend = sample_number, y = lower, yend = upper, color = Capture), lineend = "square", linewidth = 0.5) +
      scale_color_manual(values = c("Yes" = "black", "No" = "red")) +
      geom_hline(data = tibble(yintercept = CIdf$truth[1], `True Proportion` = CIdf$`True Proportion`[1]),
                 linewidth = 1.5,
                 mapping = aes(yintercept = yintercept, label7 = `True Proportion`)) +
      xlab("Sample Data Set") + 
      ylab("Intervals") +
      ggtitle(message) + 
      theme_light() 
    
  }
  
  
  ggplotly(g, tooltip = c("label2", "label3", "label4", "label5", "label6", "label7"))
  # plotCI(x = 1:nrow(CIdf), 
  #        y = CIdf$phat,
  #        li = CIdf$lower, 
  #        ui = CIdf$upper,
  #        col = CIdf$col, 
  #        lwd = 1.5,
  #        ylim = c(min(c(CIdf[, "lower"], truth - 0.01)), max(c(CIdf[, "upper"], truth + 0.01))),
  #        ylab = "Intervals",
  #        xlab = "Sampled Data Set",
  #        main = message)
  # #draw a line for true mean
  # abline(h = truth, lwd = 2)
}

