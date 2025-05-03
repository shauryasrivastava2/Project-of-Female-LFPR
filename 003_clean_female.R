# Script -----
# Name: clean_female.R
# Author: Aarthi Iyer, Aparna G
# Purpose: Labelling of variables in the female survey

  
# Loading packages ----
library(lubridate)
library(Hmisc)
library(stringr)
library(here)
library(tidyverse)
library(janitor)

#Setting directories ----

rawdata <- paste0(here(),"/data/raw/main_raw/")
scripts <- paste0(here(), "/data/scripts/main_scripts/")
cleandata <- paste0(here(),"/data/clean/main_clean/")

# Loading data

# Loading female survey
df_female <- read.csv(paste0(rawdata, "IWWAGE FLFS Female_WIDE.csv"), stringsAsFactors = TRUE)

# Loading functions
source(paste0(scripts, "Functions.R"))


# Pilot Observations' removal----



# In mac and Windows, dates are being read differently; following conditional takes care of that
#if (.Platform$OS.type == "unix") {
  #df_female$SubmissionDate <- as.POSIXct(as.character(df_female$SubmissionDate), format = "%d/%m/%Y, %H:%M:%S" ,tz = "Asia/Calcutta")
#} else {
  #df_female$SubmissionDate <- as.POSIXct(df_female$SubmissionDate, format = "%m/%d/%Y, %H:%M:%S" ,tz = "Asia/Calcutta")
#}
  
df_female$SubmissionDate <- as.POSIXct(as.character(df_female$SubmissionDate), format = "%d/%m/%Y, %H:%M:%S" ,tz = "Asia/Calcutta")

df_female$Submission_date_only <- as.Date(df_female$SubmissionDate)
df_female <- subset(df_female, Submission_date_only > as.POSIXct("2021-11-18") )


#Removing observations that did not consent
df_female <- subset(df_female, consent == 1 )


# A1: (Survey level) ----

#Dropping unnecessary Variables
df_female <- df_female %>%
  select(-c(deviceid,subscriberid,simid,devicephonenum,username, uploadstamp)) %>% 
  select(-contains("note"))
         
#Renaming variables
df_female <- df_female %>%
  rename(main_id = a1_2,
         main_id_repeat = a1_2_repeat,
         participant_name = a1_3,
         state = a1_4,
         district = a1_5,
         block_ward = a1_6,
         area = a1_7,
         city_town_village= a1_8)



#Recoding Variables
df_female$state = recode_factor(df_female$state,"DL"="Delhi", "MP"="Madhya Pradesh","RJ"="Rajasthan","JH"="Jharkhand","KA"="Karnataka")

df_female$area_actual <- substr(df_female$id,3,4)
df_female$area_actual <- recode_factor(df_female$area_actual,
                                       "01" = "Rural",
                                       "02" = "Urban")

df_female$district <- substr(df_female$id,1,6)
df_female$district = recode_factor(df_female$district,
                                   "DL02KB" = "Central Delhi",
                                   "DL02SB" = "Central Delhi",
                                   "DL02BW" = "South West Delhi",
                                   "DL02ML" = "South West Delhi",
                                   "JH01GH" = "Gharwa Jharkhand",
                                   "JH01BP" = "Gharwa Jharkhand",
                                   "JH02BM" = "Dhanbad Jharkhand",
                                   "JH02DB" = "Dhanbad Jharkhand",
                                   "KA01MN" = "Mandya Karnataka",
                                   "KA01MD" = "Mandya Karnataka",
                                   "KA02BS" = "Banglore Karnataka",
                                   "KA02YP" = "Banglore Karnataka",
                                   "MP01TT" = "Rewa MadhyaPradesh",
                                   "MP01GR" = "Rewa MadhyaPradesh",
                                   "MP02RA" = "Indore MadhyaPradesh",
                                   "MP02AM" = "Indore MadhyaPradesh",
                                   "RJ01SI" = "Barmer Rajasthan",
                                   "RJ01CH" = "Barmer Rajasthan",
                                   "RJ02HM" = "Jaipur Rajasthan",
                                   "RJ02VN" = "Jaipur Rajasthan")

df_female$constituency <- substr(df_female$id,5,6)
df_female$constituency = recode_factor(df_female$constituency, 
                                       "KB" = "Karol Bagh",
                                       "SB" = "Sadar Bazar",
                                       "BW" = "Bijwasan", 
                                       "ML" = "Matiala",
                                       "GH" = "Gharwa",
                                       "BP" = "Bhawanathpur",
                                       "BM" = "Baghmara",
                                       "DB" = "Dhanbad",
                                       "MN" = "Mandya",
                                       "MD" = "Maddur",
                                       "BS" = "Bangalore South",
                                       "YP" = "Yeshwanthapura",
                                       "TT" = "Teonthar",
                                       "GR" = "Gurh",
                                       "RA" = "Rau",
                                       "AM" = "Dr Ambedkar Nagar Mhow",
                                       "SI" = "Siwana",
                                       "CH" = "Chohtan",
                                       "HM" = "Hawa Mahal",
                                       "VN" = "Vidyadhar Nagar")

df_female$polling_station <- substr(df_female$id, start = 5, stop = 9)

df_female$polling_station <- recode_factor(df_female$polling_station, 
                                           "BM001" = "Baghmara  001",
                                           "BM002" = "Baghmara  002",
                                           "BM101" = "Baghmara  101",
                                           "BM102" = "Baghmara  102",
                                           "BM202" = "Baghmara  202",
                                           "BM203" = "Baghmara  203",
                                           "BS001" = "Bangalore South  001",
                                           "BS002" = "Bangalore South  002",
                                           "BS149" = "Bangalore South  149",
                                           "BS297" = "Bangalore South  297",
                                           "BS298" = "Bangalore South  298",
                                           "BS447" = "Bangalore South  447",
                                           "BP001" = "Bhawanathpur  001",
                                           "BP002" = "Bhawanathpur  002",
                                           "BP143" = "Bhawanathpur  143",
                                           "BP144" = "Bhawanathpur  144",
                                           "BP286" = "Bhawanathpur  286",
                                           "BP287" = "Bhawanathpur  287",
                                           "BW015" = "Bijwasan  015",
                                           "BW016" = "Bijwasan  016",
                                           "BW017" = "Bijwasan  017",
                                           "BW018" = "Bijwasan  018",
                                           "BW055" = "Bijwasan  055",
                                           "BW056" = "Bijwasan  056",
                                           "BW110" = "Bijwasan  110",
                                           "BW111" = "Bijwasan  111",
                                           "BW149" = "Bijwasan  149",
                                           "BW150" = "Bijwasan  150",
                                           "CH002" = "Chohtan 002",
                                           "CH095" = "Chohtan 095",
                                           "CH096" = "Chohtan 096",
                                           "CH189" = "Chohtan 189",
                                           "CH190" = "Chohtan 190",
                                           "CH283" = "Chohtan 283",
                                           "DB001" = "Dhanbad 001",
                                           "DB002" = "Dhanbad 002",
                                           "DB131" = "Dhanbad 131",
                                           "DB132" = "Dhanbad 132",
                                           "DB262" = "Dhanbad 262",
                                           "DB263" = "Dhanbad 263",
                                           "AM001" = "Dr Ambedkar Nagar Mhow 001",
                                           "AM002" = "Dr Ambedkar Nagar Mhow 002",
                                           "AM066" = "Dr Ambedkar Nagar Mhow 066",
                                           "AM067" = "Dr Ambedkar Nagar Mhow 067",
                                           "AM132" = "Dr Ambedkar Nagar Mhow 132",
                                           "AM133" = "Dr Ambedkar Nagar Mhow 133",
                                           "GH001" = "Gharwa 001",
                                           "GH002" = "Gharwa 002",
                                           "GH130" = "Gharwa 130",
                                           "GH131" = "Gharwa 131",
                                           "GH260" = "Gharwa 260",
                                           "GH261" = "Gharwa 261",
                                           "GR001" = "Gurh 001",
                                           "GR002" = "Gurh 002",
                                           "GR066" = "Gurh 066",
                                           "GR067" = "Gurh 067",
                                           "GR132" = "Gurh 132",
                                           "GR133" = "Gurh 133",
                                           "HM001" = "Hawa Mahal 001",
                                           "HM002" = "Hawa Mahal 002",
                                           "HM063" = "Hawa Mahal 063",
                                           "HM064" = "Hawa Mahal 064",
                                           "HM125" = "Hawa Mahal 125",
                                           "HM126" = "Hawa Mahal 126",
                                           "HM187" = "Hawa Mahal 187",
                                           "KB001" = "Karol Bagh 001",
                                           "KB002" = "Karol Bagh 002",
                                           "KB051" = "Karol Bagh 051",
                                           "KB052" = "Karol Bagh 052",
                                           "KB102" = "Karol Bagh 102",
                                           "KB103" = "Karol Bagh 103",
                                           "KB112" = "Karol Bagh 112",
                                           "KB113" = "Karol Bagh 113",
                                           "MD072" = "Maddur 072",
                                           "MD073" = "Maddur 073",
                                           "MD144" = "Maddur 144",
                                           "MD145" = "Maddur 145",
                                           "MD216" = "Maddur 216",
                                           "MN001" = "Mandya 001",
                                           "MN002" = "Mandya 002",
                                           "MN074" = "Mandya 074",
                                           "MN075" = "Mandya 075",
                                           "MN148" = "Mandya 148",
                                           "MN149" = "Mandya 149",
                                           "MN222" = "Mandya 222",
                                           "MN223" = "Mandya 223",
                                           "ML001" = "Matiala 001",
                                           "ML002" = "Matiala 002",
                                           "ML214" = "Matiala 214",
                                           "ML215" = "Matiala 215",
                                           "RA001" = "Rau 001",
                                           "RA002" = "Rau 002",
                                           "RA046" = "Rau 046",
                                           "RA047" = "Rau 047",
                                           "RA121" = "Rau 121",
                                           "RA122" = "Rau 122",
                                           "RA211" = "Rau 211",
                                           "RA263" = "Rau 263",
                                           "SB001" = "Sadar Bazar 001",
                                           "SB002" = "Sadar Bazar 002",
                                           "SB050" = "Sadar Bazar 050",
                                           "SB051" = "Sadar Bazar 051",
                                           "SB100" = "Sadar Bazar 100",
                                           "SB101" = "Sadar Bazar 101",
                                           "SI001" = "Siwana 001",
                                           "SI002" = "Siwana 002",
                                           "SI081" = "Siwana 081",
                                           "SI082" = "Siwana 082",
                                           "SI161" = "Siwana 161",
                                           "SI162" = "Siwana 162",
                                           "TT001" = "Teonthar 001",
                                           "TT002" = "Teonthar 002",
                                           "TT066" = "Teonthar 066",
                                           "TT067" = "Teonthar 067",
                                           "TT132" = "Teonthar 132",
                                           "TT133" = "Teonthar 133",
                                           "VN001" = "Vidyadhar Nagar 001",
                                           "VN002" = "Vidyadhar Nagar 002",
                                           "VN084" = "Vidyadhar Nagar 084",
                                           "VN085" = "Vidyadhar Nagar 085",
                                           "VN167" = "Vidyadhar Nagar 167",
                                           "VN168" = "Vidyadhar Nagar 168",
                                           "VN250" = "Vidyadhar Nagar 250",
                                           "YP001" = "Yeshwanthapura 001",
                                           "YP002" = "Yeshwanthapura 002",
                                           "YP004" = "Yeshwanthapura 004",
                                           "YP005" = "Yeshwanthapura 005",
                                           "YP006" = "Yeshwanthapura 006",
                                           "YP132" = "Yeshwanthapura 132",
                                           "YP133" = "Yeshwanthapura 133",
                                           "YP264" = "Yeshwanthapura 264",
                                           "YP265" = "Yeshwanthapura 265",
                                           "YP396" = "Yeshwanthapura 396")

df_female$area = recode_factor(df_female$area,"1"="Urban","2"="Rural")

# A2: Identification and Personnel(Respondent Level)----

#Renaming variables
df_female <- df_female %>% 
  rename(mobile = a2_1,
         mobile_alternate= a2_1_alternate,
         gender = a2_2,
         age_sample = age,
         age_actual = a2_3,
         pursuing_education = a2_4,
         last_grade = a2_5,
         state_origin = a2_6,
         district_origin = a2_7,
         area_origin = a2_8,
         marital_status= a2_9,
         marital_status_others= a2_9_os,
         husband_household = a2_10,
         currently_native= a2_11,
         currently_native_other = a2_11_os,
         reasons_moving_other = a2_12_os,
         relocated_dependents = a2_13,
         relocated_dependents_other = a2_13_os)


#Recoding variables
#Note: Using function "yesNo" from FLFS_functions script. This script is sourced in 002_clean.  
df_female$pursuing_education = yesNo(df_female, "pursuing_education")
df_female$last_grade = recode_factor(df_female$last_grade,"0"="No Education","1"="Class 1","2"="Class 2","3"="Class 3","4"="Class 4",
                                     "5"="Class 5","6"="Class 6","7"="Class 7","8"="Class 8","9"="Class 9","10"="Class 10","11"="Class 11","12"="Class 12","13"="Diploma","14"="Graduate","15"="Post Graduate","16"="Higher","-999"="Dont know")
df_female$state_origin = recode_factor(df_female$state_origin, "1" = "Andaman Nicobar",
                                       "2" = "Andhra Pradesh",
                                       "3" = "Arunachal Pradesh",
                                       "4" = "Assam",
                                       "5" = "Bihar",
                                       "6" = "Chandigarh",
                                       "7" = "Dadra and Nagar Haveli and Daman and Diu",
                                       "8" = "Delhi",
                                       "9" = "Goa",
                                      "10" = "Gujarat",
                                      "11" = "Haryana",
                                      "12" = "Himachal Pradesh",
                                      "13" = "Jammu Kashmir",
                                      "14" = "Jharkhand",
                                      "15" = "Karnataka",
                                      "16" = "Kerala",
                                      "17" = "Ladakh",
                                      "18" = "Lakshadweep",
                                      "19" = "Madhya Pradesh",
                                      "20" = "Maharashtra",
                                      "21" = "Manipur",
                                      "22" = "Meghalaya",
                                      "23" = "Mizoram",
                                      "24" = "Nagaland",
                                      "25" = "Odisha",
                                      "26"="Puducherry",
                                      "27"="Punjab",
                                      "28"="Rajasthan",
                                      "29"="Sikkim",
                                      "30"="Tamil Nadu",
                                      "31"="Telangana",
                                      "32"="Tripura",
                                      "33"="Uttar Pradesh",
                                      "34"="Uttarakhand",
                                      "35"="West Bengal") 
                                      
df_female$district_origin = recode_factor(df_female$district_origin, "101" = "Nicobar",
                                          "102" = "North Middle Andaman",
                                          "103" = "South Andaman",
                                          "104" = "Anantapur",
                                          "105" = "Chittoor",
                                          "106" = "East Godavari",
                                          "107" = "Guntur",
                                          "108" = "Kadapa",
                                          "109" = "Krishna",
                                          "110" = "Kurnool",
                                          "111" = "Nellore",
                                          "112" = "Prakasam",
                                          "113" = "Srikakulam",
                                          "114" = "Visakhapatnam",
                                          "115" = "Vizianagaram",
                                          "116" = "West Godavari",
                                          "117" = "Anjaw",
                                          "118" = "Central Siang",
                                          "119" = "Changlang",
                                          "120" = "Dibang Valley",
                                          "121" = "East Kameng",
                                          "122" = "East Siang",
                                          "123" = "Kamle",
                                          "124" = "Kra Daadi",
                                          "125" = "Kurung Kumey",
                                          "126" = "Lepa Rada",
                                          "127" = "Lohit",
                                          "128" = "Longding",
                                          "129" = "Lower Dibang Valley",
                                          "130" = "Lower Siang",
                                          "131" = "Lower Subansiri",
                                          "132" = "Namsai",
                                          "133" = "Pakke Kessang",
                                          "134" = "Papum Pare",
                                          "135" = "Shi Yomi",
                                          "136" = "Tawang",
                                          "137" = "Tirap",
                                          "138" = "Upper Siang",
                                          "139" = "Upper Subansiri",
                                          "140" = "West Kameng",
                                          "141" = "West Siang",
                                          "142" = "Bajali",
                                          "143" = "Baksa",
                                          "144" = "Barpeta",
                                          "145" = "Biswanath",
                                          "146" = "Bongaigaon",
                                          "147" = "Cachar",
                                          "148" = "Charaideo",
                                          "149" = "Chirang",
                                          "150" = "Darrang",
                                          "151" = "Dhemaji",
                                          "152" = "Dhubri",
                                          "153" = "Dibrugarh",
                                          "154" = "Dima Hasao",
                                          "155" = "Goalpara",
                                          "156" = "Golaghat",
                                          "157" = "Hailakandi",
                                          "158" = "Hojai",
                                          "159" = "Jorhat",
                                          "160" = "Kamrup",
                                          "161" = "Kamrup Metropolitan",
                                          "162" = "Karbi Anglong",
                                          "163" = "Karimganj",
                                          "164" = "Kokrajhar",
                                          "165" = "Lakhimpur",
                                          "166" = "Majuli",
                                          "167" = "Morigaon",
                                          "168" = "Nagaon",
                                          "169" = "Nalbari",
                                          "170" = "Sivasagar",
                                          "171" = "Sonitpur",
                                          "172" = "South Salmara-Mankachar",
                                          "173" = "Tinsukia",
                                          "174" = "Udalguri",
                                          "175" = "West Karbi Anglong",
                                          "176" = "Araria",
                                          "177" = "Arwal",
                                          "178" = "Aurangabad",
                                          "179" = "Banka",
                                          "180" = "Begusarai",
                                          "181" = "Bhagalpur",
                                          "182" = "Bhojpur",
                                          "183" = "Buxar",
                                          "184" = "Darbhanga",
                                          "185" = "East Champaran",
                                          "186" = "Gaya",
                                          "187" = "Gopalganj",
                                          "188" = "Jamui",
                                          "189" = "Jehanabad",
                                          "190" = "Kaimur",
                                          "191" = "Katihar",
                                          "192" = "Khagaria",
                                          "193" = "Kishanganj",
                                          "194" = "Lakhisarai",
                                          "195" = "Madhepura",
                                          "196" = "Madhubani",
                                          "197" = "Munger",
                                          "198" = "Muzaffarpur",
                                          "199" = "Nalanda",
                                          "200" = "Nawada",
                                          "201" = "Patna",
                                          "202" = "Purnia",
                                          "203" = "Rohtas",
                                          "204" = "Saharsa",
                                          "205" = "Samastipur",
                                          "206" = "Saran",
                                          "207" = "Sheikhpura",
                                          "208" = "Sheohar",
                                          "209" = "Sitamarhi",
                                          "210" = "Siwan",
                                          "211" = "Supaul",
                                          "212" = "Vaishali",
                                          "213" = "West Champaran",
                                          "214" = "Chandigarh",
                                          "215" = "Balod",
                                          "216" = "Baloda Bazar",
                                          "217" = "Balrampur",
                                          "218" = "Bastar",
                                          "219" = "Bemetara",
                                          "220" = "Bijapur",
                                          "221" = "Bilaspur",
                                          "222" = "Dantewada",
                                          "223" = "Dhamtari",
                                          "224" = "Durg",
                                          "225" = "Gariaband",
                                          "226" = "Gaurela Pendra Marwahi",
                                          "227" = "Janjgir Champa",
                                          "228" = "Jashpur",
                                          "229" = "Kabirdham",
                                          "230" = "Kanker",
                                          "231" = "Kondagaon",
                                          "232" = "Korba",
                                          "233" = "Koriya",
                                          "234" = "Mahasamund",
                                          "235" = "Mungeli",
                                          "236" = "Narayanpur",
                                          "237" = "Raigarh",
                                          "238" = "Raipur",
                                          "239" = "Rajnandgaon",
                                          "240" = "Sukma",
                                          "241" = "Surajpur",
                                          "242" = "Surguja",
                                          "243" = "Dadra and Nagar Haveli",
                                          "244" = "Daman",
                                          "245" = "Diu",
                                          "246" = "Central Delhi",
                                          "247" = "East Delhi",
                                          "248" = "New Delhi",
                                          "249" = "North Delhi",
                                          "250" = "North East Delhi",
                                          "251" = "North West Delhi",
                                          "252" = "Shahdara",
                                          "253" = "South Delhi",
                                          "254" = "South East Delhi",
                                          "255" = "South West Delhi",
                                          "256" = "West Delhi",
                                          "257" = "North Goa",
                                          "258" = "South Goa",
                                          "259" = "Ahmedabad",
                                          "260" = "Amreli",
                                          "261" = "Anand",
                                          "262" = "Aravalli",
                                          "263" = "Banaskantha",
                                          "264" = "Bharuch",
                                          "265" = "Bhavnagar",
                                          "266" = "Botad",
                                          "267" = "Chhota Udaipur",
                                          "268" = "Dahod",
                                          "269" = "Dang",
                                          "270" = "Devbhoomi Dwarka",
                                          "271" = "Gandhinagar",
                                          "272" = "Gir Somnath",
                                          "273" = "Jamnagar",
                                          "274" = "Junagadh",
                                          "275" = "Kheda",
                                          "276" = "Kutch",
                                          "277" = "Mahisagar",
                                          "278" = "Mehsana",
                                          "279" = "Morbi",
                                          "280" = "Narmada",
                                          "281" = "Navsari",
                                          "282" = "Panchmahal",
                                          "283" = "Patan",
                                          "284" = "Porbandar",
                                          "285" = "Rajkot",
                                          "286" = "Sabarkantha",
                                          "287" = "Surat",
                                          "288" = "Surendranagar",
                                          "289" = "Tapi",
                                          "290" = "Vadodara",
                                          "291" = "Valsad",
                                          "292" = "Ambala",
                                          "293" = "Bhiwani",
                                          "294" = "Charkhi Dadri",
                                          "295" = "Faridabad",
                                          "296" = "Fatehabad",
                                          "297" = "Gurugram",
                                          "298" = "Hisar",
                                          "299" = "Jhajjar",
                                          "300" = "Jind",
                                          "301" = "Kaithal",
                                          "302" = "Karnal",
                                          "303" = "Kurukshetra",
                                          "304" = "Mahendragarh",
                                          "305" = "Mewat",
                                          "306" = "Palwal",
                                          "307" = "Panchkula",
                                          "308" = "Panipat",
                                          "309" = "Rewari",
                                          "310" = "Rohtak",
                                          "311" = "Sirsa",
                                          "312" = "Sonipat",
                                          "313" = "Yamunanagar",
                                          "314" = "Bilaspur",
                                          "315" = "Chamba",
                                          "316" = "Hamirpur",
                                          "317" = "Kangra",
                                          "318" = "Kinnaur",
                                          "319" = "Kullu",
                                          "320" = "Lahaul Spiti",
                                          "321" = "Mandi",
                                          "322" = "Shimla",
                                          "323" = "Sirmaur",
                                          "324" = "Solan",
                                          "325" = "Una",
                                          "326" = "Anantnag",
                                          "327" = "Bandipora",
                                          "328" = "Baramulla",
                                          "329" = "Budgam",
                                          "330" = "Doda",
                                          "331" = "Ganderbal",
                                          "332" = "Jammu",
                                          "333" = "Kathua",
                                          "334" = "Kishtwar",
                                          "335" = "Kulgam",
                                          "336" = "Kupwara",
                                          "337" = "Poonch",
                                          "338" = "Pulwama",
                                          "339" = "Rajouri",
                                          "340" = "Ramban",
                                          "341" = "Reasi",
                                          "342" = "Samba",
                                          "343" = "Shopian",
                                          "344" = "Srinagar",
                                          "345" = "Udhampur",
                                          "346" = "Bokaro",
                                          "347" = "Chatra",
                                          "348" = "Deoghar",
                                          "349" = "Dhanbad",
                                          "350" = "Dumka",
                                          "351" = "East Singhbhum",
                                          "352" = "Garhwa",
                                          "353" = "Giridih",
                                          "354" = "Godda",
                                          "355" = "Gumla",
                                          "356" = "Hazaribagh",
                                          "357" = "Jamtara",
                                          "358" = "Khunti",
                                          "359" = "Koderma",
                                          "360" = "Latehar",
                                          "361" = "Lohardaga",
                                          "362" = "Pakur",
                                          "363" = "Palamu",
                                          "364" = "Ramgarh",
                                          "365" = "Ranchi",
                                          "366" = "Sahebganj",
                                          "367" = "Seraikela Kharsawan",
                                          "368" = "Simdega",
                                          "369" = "West Singhbhum",
                                          "370" = "Bagalkot",
                                          "371" = "Bangalore Rural",
                                          "372" = "Bangalore Urban",
                                          "373" = "Belgaum",
                                          "374" = "Bellary",
                                          "375" = "Bidar",
                                          "376" = "Chamarajanagar",
                                          "377" = "Chikkaballapur",
                                          "378" = "Chikkamagaluru",
                                          "379" = "Chitradurga",
                                          "380" = "Dakshina Kannada",
                                          "381" = "Davanagere",
                                          "382" = "Dharwad",
                                          "383" = "Gadag",
                                          "384" = "Gulbarga",
                                          "385" = "Hassan",
                                          "386" = "Haveri",
                                          "387" = "Kodagu",
                                          "388" = "Kolar",
                                          "389" = "Koppal",
                                          "390" = "Mandya",
                                          "391" = "Mysore",
                                          "392" = "Raichur",
                                          "393" = "Ramanagara",
                                          "394" = "Shimoga",
                                          "395" = "Tumkur",
                                          "396" = "Udupi",
                                          "397" = "Uttara Kannada",
                                          "398" = "Vijayanagara",
                                          "399" = "Vijayapura",
                                          "400" = "Yadgir",
                                          "401" = "Alappuzha",
                                          "402" = "Ernakulam",
                                          "403" = "Idukki",
                                          "404" = "Kannur",
                                          "405" = "Kasaragod",
                                          "406" = "Kollam",
                                          "407" = "Kottayam",
                                          "408" = "Kozhikode",
                                          "409" = "Malappuram",
                                          "410" = "Palakkad",
                                          "411" = "Pathanamthitta",
                                          "412" = "Thiruvananthapuram",
                                          "413" = "Thrissur",
                                          "414" = "Wayanad",
                                          "415" = "Kargil",
                                          "416" = "Leh",
                                          "417" = "Lakshadweep",
                                          "418" = "Agar Malwa",
                                          "419" = "Alirajpur",
                                          "420" = "Anuppur",
                                          "421" = "Ashoknagar",
                                          "422" = "Balaghat",
                                          "423" = "Barwani",
                                          "424" = "Betul",
                                          "425" = "Bhind",
                                          "426" = "Bhopal",
                                          "427" = "Burhanpur",
                                          "428" = "Chachaura",
                                          "429" = "Chhatarpur",
                                          "430" = "Chhindwara",
                                          "431" = "Damoh",
                                          "432" = "Datia",
                                          "433" = "Dewas",
                                          "434" = "Dhar",
                                          "435" = "Dindori",
                                          "436" = "Guna",
                                          "437" = "Gwalior",
                                          "438" = "Harda",
                                          "439" = "Hoshangabad",
                                          "440" = "Indore",
                                          "441" = "Jabalpur",
                                          "442" = "Jhabua",
                                          "443" = "Katni",
                                          "444" = "Khandwa",
                                          "445" = "Khargone",
                                          "446" = "Maihar",
                                          "447" = "Mandla",
                                          "448" = "Mandsaur",
                                          "449" = "Morena",
                                          "450" = "Nagda",
                                          "451" = "Narsinghpur",
                                          "452" = "Neemuch",
                                          "453" = "Niwari",
                                          "454" = "Panna",
                                          "455" = "Raisen",
                                          "456" = "Rajgarh",
                                          "457" = "Ratlam",
                                          "458" = "Rewa",
                                          "459" = "Sagar",
                                          "460" = "Satna",
                                          "461" = "Sehore",
                                          "462" = "Seoni",
                                          "463" = "Shahdol",
                                          "464" = "Shajapur",
                                          "465" = "Sheopur",
                                          "466" = "Shivpuri",
                                          "467" = "Sidhi",
                                          "468" = "Singrauli",
                                          "469" = "Tikamgarh",
                                          "470" = "Ujjain",
                                          "471" = "Umaria",
                                          "472" = "Vidisha",
                                          "473" = "Ahmednagar",
                                          "474" = "Akola",
                                          "475" = "Amravati",
                                          "476" = "Aurangabad",
                                          "477" = "Beed",
                                          "478" = "Bhandara",
                                          "479" = "Buldhana",
                                          "480" = "Chandrapur",
                                          "481" = "Dhule",
                                          "482" = "Gadchiroli",
                                          "483" = "Gondia",
                                          "484" = "Hingoli",
                                          "485" = "Jalgaon",
                                          "486" = "Jalna",
                                          "487" = "Kolhapur",
                                          "488" = "Latur",
                                          "489" = "Mumbai City",
                                          "490" = "Mumbai Suburban",
                                          "491" = "Nagpur",
                                          "492" = "Nanded",
                                          "493" = "Nandurbar",
                                          "494" = "Nashik",
                                          "495" = "Osmanabad",
                                          "496" = "Palghar",
                                          "497" = "Parbhani",
                                          "498" = "Pune",
                                          "499" = "Raigad",
                                          "500" = "Ratnagiri",
                                          "501" = "Sangli",
                                          "502" = "Satara",
                                          "503" = "Sindhudurg",
                                          "504" = "Solapur",
                                          "505" = "Thane",
                                          "506" = "Wardha",
                                          "507" = "Washim",
                                          "508" = "Yavatmal",
                                          "509" = "Bishnupur",
                                          "510" = "Chandel",
                                          "511" = "Churachandpur",
                                          "512" = "Imphal East",
                                          "513" = "Imphal West",
                                          "514" = "Jiribam",
                                          "515" = "Kakching",
                                          "516" = "Kamjong",
                                          "517" = "Kangpokpi",
                                          "518" = "Noney",
                                          "519" = "Pherzawl",
                                          "520" = "Senapati",
                                          "521" = "Tamenglong",
                                          "522" = "Tengnoupal",
                                          "523" = "Thoubal",
                                          "524" = "Ukhrul",
                                          "525" = "East Garo Hills",
                                          "526" = "East Jaintia Hills",
                                          "527" = "East Khasi Hills",
                                          "528" = "North Garo Hills",
                                          "529" = "Ri Bhoi",
                                          "530" = "South Garo Hills",
                                          "531" = "South West Garo Hills",
                                          "532" = "South West Khasi Hills",
                                          "533" = "West Garo Hills",
                                          "534" = "West Jaintia Hills",
                                          "535" = "West Khasi Hills",
                                          "536" = "Aizawl",
                                          "537" = "Champhai",
                                          "538" = "Hnahthial",
                                          "539" = "Kolasib",
                                          "540" = "Khawzawl",
                                          "541" = "Lawngtlai",
                                          "542" = "Lunglei",
                                          "543" = "Mamit",
                                          "544" = "Saiha",
                                          "545" = "Serchhip",
                                          "546" = "Saitual",
                                          "547" = "Dimapur",
                                          "548" = "Kiphire",
                                          "549" = "Kohima",
                                          "550" = "Longleng",
                                          "551" = "Mokokchung",
                                          "552" = "Mon",
                                          "553" = "Noklak",
                                          "554" = "Peren",
                                          "555" = "Phek",
                                          "556" = "Tuensang",
                                          "557" = "Wokha",
                                          "558" = "Zunheboto",
                                          "559" = "Angul",
                                          "560" = "Balangir",
                                          "561" = "Balasore",
                                          "562" = "Bargarh",
                                          "563" = "Bhadrak",
                                          "564" = "Boudh",
                                          "565" = "Cuttack",
                                          "566" = "Debagarh",
                                          "567" = "Dhenkanal",
                                          "568" = "Gajapati",
                                          "569" = "Ganjam",
                                          "570" = "Jagatsinghpur",
                                          "571" = "Jajpur",
                                          "572" = "Jharsuguda",
                                          "573" = "Kalahandi",
                                          "574" = "Kandhamal",
                                          "575" = "Kendrapara",
                                          "576" = "Kendujhar",
                                          "577" = "Khordha",
                                          "578" = "Koraput",
                                          "579" = "Malkangiri",
                                          "580" = "Mayurbhanj",
                                          "581" = "Nabarangpur",
                                          "582" = "Nayagarh",
                                          "583" = "Nuapada",
                                          "584" = "Puri",
                                          "585" = "Rayagada",
                                          "586" = "Sambalpur",
                                          "587" = "Subarnapur",
                                          "588" = "Sundergarh",
                                          "589" = "Karaikal",
                                          "590" = "Mahe",
                                          "591" = "Puducherry",
                                          "592" = "Yanam",
                                          "593" = "Amritsar",
                                          "594" = "Barnala",
                                          "595" = "Bathinda",
                                          "596" = "Faridkot",
                                          "597" = "Fatehgarh Sahib",
                                          "598" = "Fazilka",
                                          "599" = "Firozpur",
                                          "600" = "Gurdaspur",
                                          "601" = "Hoshiarpur",
                                          "602" = "Jalandhar",
                                          "603" = "Kapurthala",
                                          "604" = "Ludhiana",
                                          "605" = "Malerkotla",
                                          "606" = "Mansa",
                                          "607" = "Moga",
                                          "608" = "Mohali",
                                          "609" = "Muktsar",
                                          "610" = "Pathankot",
                                          "611" = "Patiala",
                                          "612" = "Rupnagar",
                                          "613" = "Sangrur",
                                          "614" = "Shaheed Bhagat Singh Nagar",
                                          "615" = "Tarn Taran",
                                          "616" = "Ajmer",
                                          "617" = "Alwar",
                                          "618" = "Banswara",
                                          "619" = "Baran",
                                          "620" = "Barmer",
                                          "621" = "Bharatpur",
                                          "622" = "Bhilwara",
                                          "623" = "Bikaner",
                                          "624" = "Bundi",
                                          "625" = "Chittorgarh",
                                          "626" = "Churu",
                                          "627" = "Dausa",
                                          "628" = "Dholpur",
                                          "629" = "Dungarpur",
                                          "630" = "Hanumangarh",
                                          "631" = "Jaipur",
                                          "632" = "Jaisalmer",
                                          "633" = "Jalore",
                                          "634" = "Jhalawar",
                                          "635" = "Jhunjhunu",
                                          "636" = "Jodhpur",
                                          "637" = "Karauli",
                                          "638" = "Kota",
                                          "639" = "Nagaur",
                                          "640" = "Pali",
                                          "641" = "Pratapgarh",
                                          "642" = "Rajsamand",
                                          "643" = "Sawai Madhopur",
                                          "644" = "Sikar",
                                          "645" = "Sirohi",
                                          "646" = "Sri Ganganagar",
                                          "647" = "Tonk",
                                          "648" = "Udaipur",
                                          "649" = "East Sikkim",
                                          "650" = "North Sikkim",
                                          "651" = "South Sikkim",
                                          "652" = "West Sikkim",
                                          "653" = "Ariyalur",
                                          "654" = "Chengalpattu",
                                          "655" = "Chennai",
                                          "656" = "Coimbatore",
                                          "657" = "Cuddalore",
                                          "658" = "Dharmapuri",
                                          "659" = "Dindigul",
                                          "660" = "Erode",
                                          "661" = "Kallakurichi",
                                          "662" = "Kanchipuram",
                                          "663" = "Kanyakumari",
                                          "664" = "Karur",
                                          "665" = "Krishnagiri",
                                          "666" = "Madurai",
                                          "667" = "Mayiladuthurai",
                                          "668" = "Nagapattinam",
                                          "669" = "Namakkal",
                                          "670" = "Nilgiris",
                                          "671" = "Perambalur",
                                          "672" = "Pudukkottai",
                                          "673" = "Ramanathapuram",
                                          "674" = "Ranipet",
                                          "675" = "Salem",
                                          "676" = "Sivaganga",
                                          "677" = "Tenkasi",
                                          "678" = "Thanjavur",
                                          "679" = "Theni",
                                          "680" = "Thoothukudi",
                                          "681" = "Tiruchirappalli",
                                          "682" = "Tirunelveli",
                                          "683" = "Tirupattur",
                                          "684" = "Tiruppur",
                                          "685" = "Tiruvallur",
                                          "686" = "Tiruvannamalai",
                                          "687" = "Tiruvarur",
                                          "688" = "Vellore",
                                          "689" = "Viluppuram",
                                          "690" = "Virudhunagar",
                                          "691" = "Adilabad",
                                          "692" = "Bhadradri Kothagudem",
                                          "693" = "Hyderabad",
                                          "694" = "Jagtial",
                                          "695" = "Jangaon",
                                          "696" = "Jayashankar",
                                          "697" = "Jogulamba",
                                          "698" = "Kamareddy",
                                          "699" = "Karimnagar",
                                          "700" = "Khammam",
                                          "701" = "Komaram Bheem",
                                          "702" = "Mahabubabad",
                                          "703" = "Mahbubnagar",
                                          "704" = "Mancherial",
                                          "705" = "Medak",
                                          "706" = "Medchal",
                                          "707" = "Mulugu",
                                          "708" = "Nagarkurnool",
                                          "709" = "Nalgonda",
                                          "710" = "Narayanpet",
                                          "711" = "Nirmal",
                                          "712" = "Nizamabad",
                                          "713" = "Peddapalli",
                                          "714" = "Rajanna Sircilla",
                                          "715" = "Ranga Reddy",
                                          "716" = "Sangareddy",
                                          "717" = "Siddipet",
                                          "718" = "Suryapet",
                                          "719" = "Vikarabad",
                                          "720" = "Wanaparthy",
                                          "721" = "Warangal",
                                          "722" = "Hanamkonda",
                                          "723" = "Yadadri Bhuvanagiri",
                                          "724" = "Dhalai",
                                          "725" = "Gomati",
                                          "726" = "Khowai",
                                          "727" = "North Tripura",
                                          "728" = "Sepahijala",
                                          "729" = "South Tripura",
                                          "730" = "Unakoti",
                                          "731" = "West Tripura",
                                          "732" = "Agra",
                                          "733" = "Aligarh",
                                          "734" = "Ambedkar Nagar",
                                          "735" = "Amethi",
                                          "736" = "Amroha",
                                          "737" = "Auraiya",
                                          "738" = "Ayodhya",
                                          "739" = "Azamgarh",
                                          "740" = "Baghpat",
                                          "741" = "Bahraich",
                                          "742" = "Ballia",
                                          "743" = "Balrampur",
                                          "744" = "Banda",
                                          "745" = "Barabanki",
                                          "746" = "Bareilly",
                                          "747" = "Basti",
                                          "748" = "Bhadohi",
                                          "749" = "Bijnor",
                                          "750" = "Budaun",
                                          "751" = "Bulandshahr",
                                          "752" = "Chandauli",
                                          "753" = "Chitrakoot",
                                          "754" = "Deoria",
                                          "755" = "Etah",
                                          "756" = "Etawah",
                                          "757" = "Farrukhabad",
                                          "758" = "Fatehpur",
                                          "759" = "Firozabad",
                                          "760" = "Gautam Buddha Nagar",
                                          "761" = "Ghaziabad",
                                          "762" = "Ghazipur",
                                          "763" = "Gonda",
                                          "764" = "Gorakhpur",
                                          "765" = "Hamirpur",
                                          "766" = "Hapur",
                                          "767" = "Hardoi",
                                          "768" = "Hathras",
                                          "769" = "Jalaun",
                                          "770" = "Jaunpur",
                                          "771" = "Jhansi",
                                          "772" = "Kannauj",
                                          "773" = "Kanpur Dehat",
                                          "774" = "Kanpur Nagar",
                                          "775" = "Kasganj",
                                          "776" = "Kaushambi",
                                          "777" = "Kheri",
                                          "778" = "Kushinagar",
                                          "779" = "Lalitpur",
                                          "780" = "Lucknow",
                                          "781" = "Maharajganj",
                                          "782" = "Mahoba",
                                          "783" = "Mainpuri",
                                          "784" = "Mathura",
                                          "785" = "Mau",
                                          "786" = "Meerut",
                                          "787" = "Mirzapur",
                                          "788" = "Moradabad",
                                          "789" = "Muzaffarnagar",
                                          "790" = "Pilibhit",
                                          "791" = "Pratapgarh",
                                          "792" = "Prayagraj",
                                          "793" = "Raebareli",
                                          "794" = "Rampur",
                                          "795" = "Saharanpur",
                                          "796" = "Sambhal",
                                          "797" = "Sant Kabir Nagar",
                                          "798" = "Shahjahanpur",
                                          "799" = "Shamli",
                                          "800" = "Shravasti",
                                          "801" = "Siddharthnagar",
                                          "802" = "Sitapur",
                                          "803" = "Sonbhadra",
                                          "804" = "Sultanpur",
                                          "805" = "Unnao",
                                          "806" = "Varanasi",
                                          "807" = "Almora",
                                          "808" = "Bageshwar",
                                          "809" = "Chamoli",
                                          "810" = "Champawat",
                                          "811" = "Dehradun",
                                          "812" = "Haridwar",
                                          "813" = "Nainital",
                                          "814" = "Pauri",
                                          "815" = "Pithoragarh",
                                          "816" = "Rudraprayag",
                                          "817" = "Tehri",
                                          "818" = "Udham Singh Nagar",
                                          "819" = "Uttarkashi",
                                          "820" = "Alipurduar",
                                          "821" = "Bankura",
                                          "822" = "Birbhum",
                                          "823" = "Cooch Behar",
                                          "824" = "Dakshin Dinajpur",
                                          "825" = "Darjeeling",
                                          "826" = "Hooghly",
                                          "827" = "Howrah",
                                          "828" = "Jalpaiguri",
                                          "829" = "Jhargram",
                                          "830" = "Kalimpong",
                                          "831" = "Kolkata",
                                          "832" = "Malda",
                                          "833" = "Murshidabad",
                                          "834" = "Nadia",
                                          "835" = "North 24 Parganas",
                                          "836" = "Paschim Bardhaman",
                                          "837" = "Paschim Medinipur",
                                          "838" = "Purba Bardhaman",
                                          "839" = "Purba Medinipur",
                                          "840" = "Purulia",
                                          "841" = "South 24 Parganas",
                                          "842" = "Uttar Dinajpur",
                                          "-888" = "Other (Specify)")                                      
                                      
df_female$area_origin = recode_factor(df_female$area_origin,"1"="Urban","2"="Rural")
df_female$marital_status = recode_factor(df_female$marital_status,"1"="Married","2"="Unmarried","3"="Seperated","4"="Divorced","5"="Widowed","-777"="Refused to answer","-888"="Others")
df_female$husband_household = recode_factor(df_female$husband_household,"1"="Yes","0"="No")
df_female$currently_native = recode_factor(df_female$currently_native,"1"="Yes","0"="No","-999"="Don't know","-777"="Refused to Answer","-888"="Others Specify")
df_female$relocated_dependents = recode_factor(df_female$relocated_dependents,"1"="Moved Alone","2"="Moved with dependents","-777"="Refused to Answer","-888"="Others Specify")


#Renaming variables for A2.12(reasons for moving to current residence)
df_female <- df_female %>%
  rename(relocated_marriage = a2_12_1,
         relocated_education = a2_12_2,
         relocated_employment = a2_12_3,
         relocated_economic = a2_12_4,
         relocated_social = a2_12_5,
         relocated_husband_employ = a2_12_6,
         relocated_healthcare = a2_12_7,
         relocated_divorce_seperation = a2_12_8,
         relocated_transfer = a2_12_9,
         relocated_natural_disaster = a2_12_10,
         relocated_dont_know = a2_12__999,
         relocated_refused = a2_12__777,
         relocated_others = a2_12__888)


# B: Demographics (Household level)----
#Renaming variables
df_female <- df_female %>%
  rename(social_category = b1_1,
         caste = b1_2_new,
         religion = b1_2,
         religion_other = b1_2_os,
         family_type = b1_3,
         head_household = b1_4,
         live_household = b1_5,
         own_children = b1_6,
         male_household = b1_7,
         female_household = b1_8,
         children_household = b1_9,
         elder_household = b1_10,
         disability_household= b1_11)


#Recoding variables
df_female$social_category <- recode_factor(df_female$social_category,"1"="General","2"="OBC","3"="SC","4"="ST","-777"="Refused to Answer","-999"="Do Not Know")
df_female$religion <- recode_factor(df_female$religion,"1"="Hindu","2"="Muslim","3"="Buddist","4"="Christian","5"="Sikh","6"="Jain","-888"="Other","-777"="Refused to answer" )
df_female$family_type <- recode_factor(df_female$family_type,"1"="Nuclear","2"="Joint")
df_female$head_household <- recode_factor(df_female$head_household,"1"="Self","2"="Husband","3"="Father","4"="Mother","5"="Brother","6"="Sister","7"="Son","8"="Daughter","9"="Uncle",
                                          "10"="Aunt","11"="Cousin","12"="Son-in-law","13"="Daughter-in-law","14"="Father-in-law","15"="Mother-in-law","16"="Grandparent","17"="Brother-in-law","18"="Sister-in-law","19"="Other family","20"="Other non family")  
df_female$disability_household <- ynr(df_female, "disability_household")


# C: Employment particulars of household----
#Renaming variables
df_female <- df_female %>%
  rename(studying_household = c1_1_1,
         casual_daily_household = c1_1_2,
         casual_piece_household = c1_1_3,
         contract_work_household= c1_1_4,
         self_employed_household= c1_1_5,
         permanent_work_household= c1_1_6,
         primary_income_earner= c1_2,
         primary_income_earner_others = c1_2_os,
         highest_education = c1_3,
         primary_source_income =c1_4,
         primary_source_income_others = c1_4_os,
         secondary_source_income = c1_5,
         secondary_source_income_others = c1_5_os,
         total_income_before_covid = c1_6,
         total_income_after_covid = c1_7)


#455 NAs in highest education before recoding
#140 NAs in secondary_source_income

#Recoding variables
df_female$primary_income_earner = recode_factor(df_female$primary_income_earner,"1"="No-one","2"="Spouse","3"="Self","4"="Father/Father-in-law","5"="Mother/Mother-in-law","6"="Son","7"="Daughter","8"="Brother","9"="Sister","10"="Brother-in-law", "11" = "Sister-in-Law", "-888"="Other")
df_female$primary_source_income = recode_factor(df_female$primary_source_income,"1"="Casual Wage Labour - Agriculture and Allied","2"="Casual Wage Labour - Non-Agriculture","3"="Own Business (non-farm)","4"= "Own Business (farm)", "5"="Casual Piece Work", "6" = "Government Job","7"="Private Job","8"="Direct Cash Transfer","9"="Pension","10"="Remittances","0"="NOTA","-888"="Other","-777"="Refused to Answer")
df_female$highest_education = recode_factor(df_female$highest_education,"0"="No education","1"="class 1","2"="class 2","3"="class 3","4"="class 4",
                                            "5"="class 5","6"="class 6","7"="class 7","8"="class 8","9"="class 9","10"="class 10","11"="class 11","12"="class 12","13"="Diploma","14"="Graduate","15"="Post Graduate","16"="Higher","-999"="Dont know")
df_female$secondary_source_income = recode_factor(df_female$secondary_source_income,"1"="Casual Wage Labour - Agriculture and Allied","2"="Casual Wage Labour - Non-Agriculture","3"="Own Business (non-farm)","4"= "Own Business (farm)", "5"="Casual Piece Work", "6" = "Government Job","7"="Private Job","8"="Direct Cash Transfer","9"="Pension","10"="Remittances","0"="NOTA","-888"="Other","-777"="Refused to Answer")

# D: Household infrastructure----

#Renaming variables
df_female <- df_female %>%
  rename(house_type = d1_0,
         house_ownership = d1_1,
         house_ownership_others = d1_1_os,
         number_rooms = d1_2,
         house_toilet = d1_3,
         toilet_piped_water=d1_5,
         number_toilets = d1_4,
         toilet_use_household = d1_6,
         drainage_type = d1_7,
         separate_kitchen = d1_8,
         cooking_lpg_cylinder = d1_9_1,
         cooking_lpg_pipeline = d1_9_2,
         cooking_firewood = d1_9_3,
         cooking_dung = d1_9_4,
         cooking_charcoal = d1_9_5,
         cooking_kerosene =d1_9_6,
         cooking_biogas =d1_9_7,
         cooking_solar_cooker =d1_9_8,
         cooking_electric_heater = d1_9_9,
         cooking_others =d1_9__888,
         cooking_others_specify = d1_9_os,
         drinking_water=d1_10,
         drinking_water_other=d1_10_os,
         electricity_connection= d1_11,
         electricity_type = d1_12,
         transport_bus = d1_13_1,
         transport_motorcycle = d1_13_2,
         transport_rickshaw = d1_13_3,
         transport_cab = d1_13_4,
         transport_self_driven_cars = d1_13_5,
         transport_auto_rickshaw = d1_13_6,
         transport_e_rickshaw = d1_13_7,
         transport_metro = d1_13_8,
         transport_animal_driven_cart = d1_13_9,
         transport_nota = d1_13_0,
         transport_other = d1_13__888,
         transport_facility_others_specify=d1_13_os)


#Recoding variables
df_female$house_type= recode_factor(df_female$house_type,"1"="Kuccha","2"="Pucca","3"="Semi-pucca")
df_female$house_ownership = recode_factor(df_female$house_ownership,"1"="Owned","2"="Rented","3"="Provided by employer","-888"="Other")
df_female$house_toilet = yesNo(df_female, "house_toilet")
# df_female$number_toilets = yesNo(df_female, "number_toilets") This is a continuous variable
df_female$toilet_use_household =recode_factor(df_female$toilet_use_household,"1"="Neighbour's Latrine","2"="Public Latrine","3"="Pay and Use","4"="Open Defecation")
df_female$drainage_type = recode_factor(df_female$drainage_type,"1"="Piped","2"="Underground","3"="Covered Pucca","4"="Open Pucca","5"="Open Katcha","0"="No Drainage","-999"="Do not Know","-777"= "No Response")
df_female$separate_kitchen = yesNo(df_female, "separate_kitchen")
df_female$drinking_water = recode_factor(df_female$drinking_water,"1"="Piped","2"="Water Tanker","3"="Hand Pump","4"="Community Tap","5" = "Bottled Water", "6" = "Rain water Harvesting", "-999"="Do not Know")
df_female$electricity_type = recode_factor(df_female$electricity_type,"1"="Metered","2"="Lump-sum Payment","3"="Fixed Charge Payment","4"="Not Charged")
df_female$electricity_connection = yesNo(df_female, "electricity_connection")

# E1: Documents,Assets,Finance(Household Level) ----

#Renaming variables for E1.1
df_female <- df_female %>%
  rename(documents_driving_license = e1_1_1,
         documents_aadhar = e1_1_2,
         documents_passport = e1_1_3,
         documents_bank_ac = e1_1_4,
         documents_voter_id = e1_1_5,
         documents_mgnregs = e1_1_6,
         documents_ration_apl = e1_1_7,
         documents_ration_bpl = e1_1_8,
         documents_ration_antyodaya = e1_1_9,
         documents_none = e1_1_0)

#1191 NAs in ration_card_current_state. 

#Renaming variables from E1.2 to E1.4
df_female <- df_female %>%
  rename(ration_card_current_state = e1_2,
         adhar_household_member = e1_3,
         bank_ac_household_member = e1_4)

#Recoding variables
df_female$ration_card_current_state <- recode_factor(df_female$ration_card_current_state, "1" = "Yes", "0" = "No", "-999" = "Do not Know") 


#Renaming variables for E1.5
df_female <- df_female %>%
  rename(assets_electric_fan = e1_5_1,
         assets_electric_cooler = e1_5_2,
         assets_ac = e1_5_3,
         assets_computer = e1_5_4,
         assets_mobile_wo_internet = e1_5_5,
         assets_mobile_internet =e1_5_6,
         assets_smartphone = e1_5_7,
         assets_bicycle = e1_5_8,
         assets_scooter = e1_5_9,
         assets_car = e1_5_10,
         assets_microwave = e1_5_11,
         assets_tv =e1_5_12,
         assets_fridge = e1_5_13,
         assets_washing_machine = e1_5_14,
         assets_3_wheeler = e1_5_15,
         assets_cart = e1_5_16,
         assets_tempo = e1_5_17,
         assets_tractor = e1_5_18,
         assets_thresher = e1_5_19,
         assets_none= e1_5_0)


#Renaming variables for E1.6
df_female <- df_female %>%
  rename(animal_assets_poultry = e1_6_1,
         animal_assets_cattle = e1_6_2,
         animal_assets_fishery = e1_6_3,
         animal_assets_none = e1_6_0,
         animal_assets_others = e1_6__888,
         animal_assets_others_specify = e1_6_os)

# E2: Documents, Assets, Finance(Individual Level)----

#Renaming variables for E2.1
df_female <- df_female %>%
  rename(documents_ind_driving_licence = e2_1_1,
         documents_ind_aadhar = e2_1_2,
         documents_ind_passport = e2_1_3,
         documents_ind_bank_ac = e2_1_4,
         documents_ind_voter_id = e2_1_5,
         documents_ind_mgnregs = e2_1_6,
         documents_ind_pan_card =e2_1_7,
         documents_ind_none = e2_1_0)


#832 NAs in jan dhan ac
#Renaming variables from E2.2 to E2.4
df_female <- df_female %>%
  rename(bank_ac_single = e2_2_1,
         bank_ac_joint = e2_2_2,
         bank_ac_dont_know = e2_2__999,
         jan_dhan_ac = e2_3,
         land_quantity = e2_3_1,
         land_quantity_units = e2_3_1_unit,
         land_type = e2_3_2,
         frequency_save_money = e2_4)


df_female$jan_dhan_ac <- recode_factor(df_female$jan_dhan_ac, "1" = "Yes", "0" = "No", "-999" = "Do not Know")
df_female$land_quantity_units <- recode_factor(df_female$land_quantity_units, "1" = "Hectare", "2"= "Acre", "3"=  "Bhigha", "4" = "Gunta", "5"= "Cents", "7" = "Gaj", "6"= "Other", "8" = "Decimal", "0" = "Do not own any Land")
df_female$land_type <- recode_factor(df_female$land_type,"1" = "Residential land", "2" =  "Farm Land", "3" = "Commercial Land", "-888" = "Others Specify", "-999" = "Do not know" )
df_female$frequency_save_money <- recode_factor(df_female$frequency_save_money,"1" = "Daily",
                                                "2" = "Weekly",
                                                "3" = "Monthly",
                                                "4" = "Yearly",
                                                "5" = "Whenever possible",
                                                "6" = "Never")                                     
#Renaming variables for E2.5
df_female <- df_female %>%
  rename(savings_cash_home = e2_5_1,
         savings_bank_ac =e2_5_2,
         savings_term_deposits = e2_5_3,
         savings_mfi = e2_5_4,
         savings_ngo= e2_5_5,
         savings_post_office = e2_5_6,
         savings_cooperative = e2_5_7,
         savings_chit_fund= e2_5_8,
         savings_local_committee = e2_5_9,
         savings_shg_rosca = e2_5_10,
         savings_piggy_bank = e2_5_11,
         savings_others = e2_5__888,
         savings_others_specify = e2_5_os)


#Renaming variables from E2.6 to E2.7
df_female <- df_female %>%
  rename(access_bank_ac_deposit = e2_6_deposit,
         access_bank_ac_withdrawal = e2_6_withdrawal,
         access_bank_ac_transfer = e2_6_transfer,
         use_savings_household = e2_7_1,
         use_savings_children_marriage = e2_7_2,
         use_savings_children_edu = e2_7_3,
         use_savings_family_business= e2_7_4,
         use_savings_own_business = e2_7_5,
         use_savings_loans = e2_7_6,
         use_savings_buy_house = e2_7_7,
         use_savings_healthcare = e2_7_8,
         use_savings_emergencies = e2_7_9,
         use_savings_others = e2_7__888,
         use_savings_others_specify = e2_7_os)


#Renaming variables from E2.8 to E2.10
df_female <- df_female %>%
  rename(personal_assets_jewellery = e2_8_1,
         personal_assets_own_land= e2_8_2,
         personal_assets_cattle = e2_8_3,
         personal_assets_house = e2_8_4,
         personal_assets_none = e2_8_0,
         personal_assets_others = e2_8__888,
         personal_assets_others_specify = e2_8_os)


#Renaming variables from E2.12 and E2.13
df_female <- df_female %>%
  rename(loan_own_name = e2_12,
         loan_purpose_family_crisis = e2_13_1,
         loan_purpose_family_business = e2_13_2,
         loan_purpose_own_business = e2_13_3,
         loan_purpose_others = e2_13__888,
         loan_purpose_others_specify = e2_13_os)

df_female$loan_own_name <- recode_factor(df_female$loan_own_name, "1" = "Yes", "0" = "No")


#Renaming variables for E2.14
df_female <- df_female %>%
  rename(loan_source_chit_fund = e2_14_1,
         loan_source_shg = e2_14_2,
         loan_source_mfi = e2_14_3,
         loan_source_financial_inst = e2_14_4,
         loan_source_ngo = e2_14_5,
         loan_source_post_office = e2_14_6,
         loan_source_cooperatives = e2_14_7,
         loan_source_local_committees = e2_14_8,
         loan_source_bank_loan = e2_14_9,
         loan_source_local_money_lenders = e2_14_10,
         loan_source_family_members = e2_14_11,
         loan_source_friends = e2_14_12,
         loan_source_others = e2_14__888,
         loan_source_others_specify = e2_14_os)


#Renaming variables from E2.15 to E2.18
df_female <- df_female %>%
  rename(outstanding_loan = e2_15,
         repay_loan_own_income = e2_16_1,
         repay_loan_own_savings = e2_16_2,
         repay_loan_spouse_income = e2_16_3,
         repay_loan_spouse_savings = e2_16_4,
         repay_loan_selling_assets = e2_16_5,
         repay_loan_borrowing = e2_16_6,
         repay_loan_others = e2_16__888,
         repay_loan_others_specify = e2_16_os,
         personal_mobile_smartphone = e2_17,
         access_mobile_smartphone = e2_18)

#3199NAs in access to mobile smartphone before recoding

df_female$outstanding_loan <- yesNo(df_female, "outstanding_loan")
df_female$personal_mobile_smartphone <- yesNo(df_female, "personal_mobile_smartphone")
df_female$access_mobile_smartphone <- yesNo(df_female, "access_mobile_smartphone")

# WP: Work Barriers and Engagement----

#Renaming variables WP1 and WP2
df_female <- df_female %>%
  rename(personal_income = wp1,
         work_for_pay = wp2)


#Renaming variables for section WP2==Yes, currently working
# WP==Yes, Currently working----
#Renaming variables from WP2.1.1 to WP2.1.5
df_female <- df_female %>%
  rename(type_of_work = wp2_1_1,
         primary_occupation_business = wp2_1_2,
         number_days_per_week = wp2_1_3,
         nature_job_work = wp2_1_4,
         nature_job_work_others = wp2_1_4_os,
         payment_frequency = wp2_1_5,
         payment_frequency_others = wp2_1_5_os)



#Renaming variables for WP2.1.6
df_female <- df_female %>%
  rename(wp21_factors_pays_well = wp2_1_6_1,
         wp21_factors_flexible_time = wp2_1_6_2,
         wp21_factors_creche =wp2_1_6_3,
         wp21_factors_no_interfere_household =wp2_1_6_4,
         wp21_factors_home_close =wp2_1_6_5,
         wp21_factors_transport_available =wp2_1_6_6,
         wp21_factors_education_skill =wp2_1_6_7,
         wp21_factors_family_engaged=wp2_1_6_8,
         wp21_factors_family_support=wp2_1_6_9,
         wp21_factors_only_work_available=wp2_1_6_10,
         wp21_factors_others =wp2_1_6__888,
         wp21_factors_others_specify=wp2_1_6_os,
         wp21_factors_rank_1 =wp2_1_6_rank1 ,
         wp21_factors_rank_2=wp2_1_6_rank2,
         wp21_factors_rank_3=wp2_1_6_rank3)

df_female$personal_income <- yesNo(df_female, "personal_income")
df_female$work_for_pay <- recode_factor(df_female$work_for_pay, "1" = "Yes; working currently",
                                        "2" = "No; never worked",
                                        "3" = "No; used to work previously")
df_female$type_of_work <- recode_factor(df_female$type_of_work,"1" = "Employed in casual daily work",
                                        "2" = "Employed in casual piece work",
                                        "3" = "Employed in contract work < 1 yr",
                                        "4" = "Self-employed/ own enterprise",
                                        "5" = "Regular/ Permanent/ Longer Contract work")
df_female$nature_job_work <- recode_factor(df_female$nature_job_work, "1" = "Farming/ agriculture related",
                                           "2" = "Govt./ PSU",
                                           "3" = "Private firm ",
                                           "4" = "Private employer ",
                                           "5" = "Self-employed",
                                           "6" = "Other government program",
                                           "7" = "MGNREGS",
                                           "-888" = "Others Specify")

df_female$payment_frequency <- recode_factor(df_female$payment_frequency, "1" = "Daily",
                                             "2" = "Weekly",
                                             "3" = "Fortnightly",
                                             "4" = "Per Month",
                                             "-888" = "Others Specify")

df_female <- df_female %>%
  mutate_at(c("wp21_factors_rank_1",
           "wp21_factors_rank_2",
           "wp21_factors_rank_3"),
           funs(recode_factor(.,"1" = "The job pays well",
                              "2" = "Flexible work timings",
                              "4" = "The job does not interfere with housework and care responsibilities ",
                              "5" = "Location of workplace is close to home",
                              "7" = "Work is suitable for your education/ training/ skills",
                              "8" = "Your friends/ family are engaged in the same work",
                              "9" = "Your family allows it/ supports it",
                              "10" = "This was the only work opportunity available to you",
                              "6" = "Transport to and from workplace is available easily",
                              "3" = "Creche/ childcare facilities are available at the workplace ",
                              "-888" = "Others")))


#Renaming variables for WP2.1.7
df_female <- df_female %>%
  rename(wp21_challenges_shortage_time =wp2_1_7_1,
         wp21_challenges_lack_leisure=wp2_1_7_2,
         wp21_challenges_lack_socialise=wp2_1_7_3,
         wp21_challenges_health_issues=wp2_1_7_4,
         wp21_challenges_dissatisfaction_family=wp2_1_7_5,
         wp21_challenges_workplace_performance=wp2_1_7_6,
         wp21_challenges_no_challenge=wp2_1_7_0,
         wp21_challenges_others=wp2_1_7__888,
         wp21_challenges_others_specify=wp2_1_7_os,
         wp21_challenges_rank_1 = wp2_1_7_rank1,
         wp21_challenges_rank_2 = wp2_1_7_rank2,
         wp21_challenges_rank_3 = wp2_1_7_rank3)


df_female <- df_female %>%
  mutate_at(c("wp21_challenges_rank_1",
              "wp21_challenges_rank_2",
              "wp21_challenges_rank_3"),
            funs(recode_factor(.,"1" = "Shortage of time for personal care",
                               "2" = "Lack of leisure time/ rest",
                               "3" = "Lack of time for socialising with friends and family",
                               "4" = "Stress/ health issues",
                               "5" = "Dissatisfaction of family",
                               "6" = "Are unable to perform well at the workplace",
                               "0" = "No challenges faced",
                               "-888" = "Other (Specify)")))


#Renaming variables from WP2.1.8 to WP2.1.11
df_female <- df_female %>%
  rename(wp21_work_location = wp2_1_8,
         wp21_willing_work_outside=wp2_1_9,
         wp21_travel_time_walking =wp2_1_10,
         wp21_mode_of_transport = wp2_1_10_mode,
         wp21_self_employed_prefer=wp2_1_11)

df_female$wp21_work_location <- recode_factor(df_female$wp21_work_location, "1" = "Within Home",
                                              "2" = "Outside Home",
                                              "3" = "Both")
df_female$wp21_willing_work_outside <- yesNo(df_female, "wp21_willing_work_outside")
df_female$wp21_mode_of_transport <- recode_factor(df_female$wp21_mode_of_transport,"1"="Walking", "2"="Transport" )
df_female$wp21_self_employed_prefer <- yesNo(df_female, "wp21_self_employed_prefer")

#Renaming variables for WP2.1.12
df_female <- df_female %>%
  rename(wp21_prevents_household =wp2_1_12_1,
         wp21_prevents_fear_succeed=wp2_1_12_2,
         wp21_prevents_negative_attitude =wp2_1_12_3,
         wp21_prevents_lack_capital=wp2_1_12_4,
         wp21_prevents_lack_workplace=wp2_1_12_5,
         wp21_prevents_afford_workplace=wp2_1_12_6,
         wp21_prevents_lack_savings=wp2_1_12_7,
         wp21_prevents_lack_bank_loans=wp2_1_12_8,
         wp21_prevents_lack_education=wp2_1_12_9,
         wp21_prevents_lack_work_exp=wp2_1_12_10,
         wp21_prevents_lack_knowledge=wp2_1_12_11,
         wp21_prevents_covid=wp2_1_12_12,
         wp21_prevents_nota=wp2_1_12_0,
         wp21_prevents_others=wp2_1_12__888,
         wp21_prevents_other_specify=wp2_1_12_os,
         wp21_prevents_rank_1=wp2_1_12_rank1,
         wp21_prevents_rank_2=wp2_1_12_rank2,
         wp21_prevents_rank_3=wp2_1_12_rank3)

df_female <- df_female %>%
  mutate_at(c("wp21_prevents_rank_1",
              "wp21_prevents_rank_2",
              "wp21_prevents_rank_3"),
            funs(recode_factor(.,"1" = "Household duties",
                               "4" = "Lack of start-up capital",
                               "2" = "Fear of failure to succeed/ Fear of incurring financial loss",
                               "10" = "Lack of previous formal work experience/ business experience",
                               "11" = "Lack of knowledge in market demand or consumer preferences",
                               "3" = "Negative perception or attitude from family or others against female led ventures",
                               "5" = "Unavailability of suitable work-space",
                               "6" = "Cannot afford suitable workspace",
                               "7" = "Lack of savings/ financial or physical assets",
                               "8" = "Inability to access or ineligibility for bank loans or government finance schemes",
                               "9" = "Lack of skills/ education",
                               "12" = "Fear of shut-down due to Covid-19",
                               "-888" = "Others Specify")))


#Renaming variables from WP2.1.13 to WP2.1.15
df_female <- df_female %>%
  rename(wp21_skill_no_training_centers=wp2_1_13_1,
         wp21_skill_not_affordable=wp2_1_13_2,
         wp21_skill_no_impart=wp2_1_13_3,
         wp21_skill_poor_quality=wp2_1_13_4,
         wp21_skill_others=wp2_1_13__888,
         wp21_skill_others_specify=wp2_1_13_os,
         wp21_employ_others=wp2_1_14,
         wp21_skill_number_employed=wp2_1_15)

df_female$wp21_employ_others <- yesNo(df_female, "wp21_employ_others")




#Renaming variables from WP2.1.16 to WP2.1.17
df_female <- df_female %>%
  rename(wp21_activity_1 =wp2_1_16_activity_1,
         wp21_nic_1=wp2_1_16_nic_1,
         wp21_nco_1=wp2_1_16_nco_1,
         wp21_wages_1=wp2_1_16_wages_1,
         wp21_status_1=wp2_1_16_status_1,
         wp21_time_hours_1=wp2_1_17_hour_1,
         wp21_time_min_1=wp2_1_17_minutes_1,
         wp21_time_week_1=wp2_1_17_weekday_1,
         wp21_activity_2 =wp2_1_16_activity_2,
         wp21_nic_2=wp2_1_16_nic_2,
         wp21_nco_2=wp2_1_16_nco_2,
         wp21_wages_2=wp2_1_16_wages_2,
         wp21_status_2=wp2_1_16_status_2,
         wp21_time_hours_2=wp2_1_17_hour_2,
         wp21_time_min_2=wp2_1_17_minutes_2,
         wp21_time_week_2=wp2_1_17_weekday_2,
         wp21_activity_3 =wp2_1_16_activity_3,
         wp21_nic_3=wp2_1_16_nic_3,
         wp21_nco_3=wp2_1_16_nco_3,
         wp21_wages_3=wp2_1_16_wages_3,
         wp21_status_3=wp2_1_16_status_3,
         wp21_time_hours_3=wp2_1_17_hour_3,
         wp21_time_min_3=wp2_1_17_minutes_3,
         wp21_time_week_3=wp2_1_17_weekday_3,
         wp21_activity_1_description=wp2_1_16_description_1,
         wp21_activity_2_description=wp2_1_16_description_2,
         wp21_activity_3_description=wp2_1_16_description_3)

#Renaming variables from WP2.1.18 to WP2.1.22
df_female <- df_female %>%
  rename(wp21_work_mgnregs = wp2_1_18,
         wp21_provisions_job_contract=wp2_1_19_1,
         wp21_provisions_leave_with_pay=wp2_1_19_2,
         wp21_provisions_epf=wp2_1_19_3,
         wp21_provisions_health_insurance=wp2_1_19_4,
         wp21_provisions_paid_maternity=wp2_1_19_5,
         wp21_provisions_creche=wp2_1_19_6,
         wp21_provisions_none=wp2_1_19_0,
         wp21_provisions_not_wage_employee=wp2_1_19_7,
         wp21_support_family_husband=wp2_1_20,
         wp21_difficulty_loan=wp2_1_21,
         wp21_fail_loan_lack_collateral=wp2_1_22_1,
         wp21_fail_loan_belief_unviable_venture=wp2_1_22_2,
         wp21_fail_loan_lack_documents=wp2_1_22_3,
         wp21_fail_loan_bribe=wp2_1_22_4,
         wp21_fail_loan_other=wp2_1_22__888,
         wp21_fail_loan_reason_others_specify=wp2_1_22_os)

#Recoding variables
df_female$wp21_work_mgnregs <- recode_factor(df_female$wp21_work_mgnregs, "1" = "Yes, have worked under MGNREGS",
                                             "2" = "No, not worked but have a MGNREGS job card",
                                             "3" = "No, but have applied for a job card ",
                                             "4" = "No and have not applied for any card ",
                                             "5" = "Not aware of MGNREGS",
                                             "-999" = "Do not know",)
df_female$wp21_support_family_husband <- yesNo(df_female, "wp21_support_family_husband")
df_female$wp21_difficulty_loan <- recode_factor(df_female$wp21_difficulty_loan,"1" = "Yes",
                                                "0" = "No",
                                                "-999" = "Do not know",
                                                "2" = "I have never applied for loan")

# WP==No,Never Worked----

#Renaming variables from WP2.2.1 to WP2.2.2
df_female <- df_female %>%
  rename(wp22_work_if_available=wp2_2_1,
         wp22_work_not_willing_education=wp2_2_2_1,
         wp22_work_not_willing_safety=wp2_2_2_2,
         wp22_work_not_willing_distance=wp2_2_2_3,
         wp22_work_not_willing_restrictions=wp2_2_2_4,
         wp22_work_not_willing_relevance=wp2_2_2_5,
         wp22_work_not_willing_no_skills=wp2_2_2_6,
         wp22_work_not_willing_pay=wp2_2_2_7,
         wp22_work_not_willing_illness=wp2_2_2_12,
         wp22_work_not_willing_carework=wp2_2_2_8,
         wp22_work_not_willing_not_required=wp2_2_2_9,
         wp22_work_not_willing_less_pay_men=wp2_2_2_10,
         wp22_work_not_willing_less_pay_women=wp2_2_2_11,
         wp22_work_not_willing_religious=wp2_2_2_13,
         wp22_work_not_willing_others=wp2_2_2__888,
         wp22_work_not_willing_others_specify=wp2_2_2_os,
         wp22_work_not_willing_rank_1=wp2_2_2_rank1,
         wp22_work_not_willing_rank_2=wp2_2_2_rank2,
         wp22_work_not_willing_rank_3=wp2_2_2_rank3)

#Recoding variables
df_female$wp22_work_if_available <- yesNo(df_female, "wp22_work_if_available" )

df_female <- df_female %>%
  mutate_at(c("wp22_work_not_willing_rank_1",
              "wp22_work_not_willing_rank_2",
              "wp22_work_not_willing_rank_3"),
            funs(recode_factor(.,"1" = "Pursuing a degree/ getting education presently",
                               "8" = "Household and care work take up too much time",
                               "4" = "Are not allowed to take up any work outside home",
                               "5" = "Jobs relevant to your skillset are not available",
                               "6" = "You do not have skills/ training required for available jobs",
                               "7" = "Jobs available do not pay as expected",
                               "9" = "Do not require a job/ No need for extra income",
                               "10" = "I will get paid less than men for the same job",
                               "11" = "I will get paid less than other women for the same job",
                               "3" = "Do not want to work outside of home due to distance of workplace",
                               "2" = "Do not want to work outside of home due to safety reasons",
                               "12" = "Illness/ injury",
                               "13" = "Religious/ cultural norms ",
                               "-888" = "Other")))

#Renaming variables from WP2.2.3 to WP2.2.7
df_female <- df_female %>%
  rename(wp22_challenges=wp2_2_3,
         wp22_challenges_others = wp2_2_3_os,
         wp22_looking_work = wp2_2_4,
         wp22_work_kind = wp2_2_5,
         wp22_work_type = wp2_2_6,
         wp22_work_outside= wp2_2_7)

#Recoding Variables
df_female$wp22_challenges <- recode_factor(df_female$wp22_challenges, "1" = "No training/ skilling centres in the vicinity",
                                           "2" = "Training/ skilling/ re-skilling centres not affordable",
                                           "3" = "Available training/ skilling centres do not impart skills relevant to your interests/ available work",
                                           "4" = "Available training/ skilling centres offer poor quality of training/skilling",
                                           "5" = "Family will not invest in women's education/skill",
                                           "-888" = "Others Specify")
df_female$wp22_looking_work <- yesNo(df_female, "wp22_looking_work")
df_female$wp22_work_kind  <- recode_factor(df_female$wp22_work_kind,"1" = "Casual daily work",
                                             "2" = "Casual piece work",
                                             "3" = "Self-employed/ own enterprise",
                                             "4" = "Contract work <1 year",
                                             "5" = "Regular/ Permanent/ Longer Contract work", )
df_female$wp22_work_type <- recode_factor(df_female$wp22_work_type, "1" = "Full time work",
                                          "2" = "Part time work")
df_female$wp22_work_outside <- yesNo(df_female, "wp22_work_outside")




#Renaming variables for WP2.2.8
df_female <- df_female %>%
  rename(wp22_not_working_no_work=wp2_2_8_1,
         wp22_not_working_overqualified=wp2_2_8_2,
         wp22_not_working_underqualified=wp2_2_8_3,
         wp22_not_working_far_home=wp2_2_8_4,
         wp22_not_working_pay_issues=wp2_2_8_5,
         wp22_not_working_less_pay_men=wp2_2_8_6,
         wp22_not_working_less_pay_women=wp2_2_8_7,
         wp22_not_working_no_flexible_hours=wp2_2_8_8,
         wp22_not_working_no_creche=wp2_2_8_9,
         wp22_not_working_interfere_housework=wp2_2_8_10,
         wp22_not_working_interfere_childcare=wp2_2_8_11,
         wp22_not_working_safety_issues=wp2_2_8_12,
         wp22_not_working_illness=wp2_2_8_13,
         wp22_not_working_others=wp2_2_8__888,
         wp22_not_working_others_specify=wp2_2_8_os)


#Renaming variables from WP2.2.9 to WP2.2.10
df_female <- df_female %>%
  rename(#wp22_prefer_selfemployment =wp2_2_9,
         wp22_prevents_household =wp2_2_10_1,
         wp22_prevents_fear_succeed=wp2_2_10_2,
         wp22_prevents_negative_attitude =wp2_2_10_3,
         wp22_prevents_lack_capital=wp2_2_10_4,
         wp22_prevents_lack_workplace=wp2_2_10_5,
         wp22_prevents_afford_workplace=wp2_2_10_6,
         wp22_prevents_lack_savings=wp2_2_10_7,
         wp22_prevents_lack_bank_loans=wp2_2_10_8,
         wp22_prevents_lack_education=wp2_2_10_9,
         wp22_prevents_lack_work_exp=wp2_2_10_10,
         wp22_prevents_lack_knowledge=wp2_2_10_11,
         wp22_prevents_covid=wp2_2_10_12,
         wp22_prevents_nota=wp2_2_10_0,
         wp22_prevents_other=wp2_2_10__888,
         wp22_prevents_others_specify=wp2_2_10_os)


#Renaming variables from WP2.2.11 to WP2.2.13
df_female <- df_female %>%
  rename(wp22_support_family_husband=wp2_2_11,
         wp22_difficulty_loan=wp2_2_12,
         wp22_fail_loan_lack_collateral=wp2_2_13_1,
         wp22_fail_loan_belief_unviable_venture=wp2_2_13_2,
         wp22_fail_loan_lack_documents=wp2_2_13_3,
         wp22_fail_loan_bribe=wp2_2_13_4,
         wp22_fail_loan_other=wp2_2_13__888,
         wp22_fail_loan_reason_others_specify=wp2_2_13_os)
         


#Recoding variables
df_female$wp22_support_family_husband <- yesNo(df_female, "wp22_support_family_husband")
df_female$wp22_difficulty_loan <-  recode_factor(df_female$wp22_difficulty_loan, "1" = "Yes",
                                               "0" = "No",
                                               "-999" = "Do not know",
                                               "2" = "I have never applied for loan")

# WP= No,Used to work----
#Renaming variables from WP2.3.1 to WP2.3.2
df_female <- df_female %>%
  rename(wp23_work_location = wp2_3_1,
         wp23_factors_pays_well = wp2_3_2_1,
         wp23_factors_flexible_time = wp2_3_2_2,
         wp23_factors_creche =wp2_3_2_3,
         wp23_factors_no_interfere_household =wp2_3_2_4,
         wp23_factors_home_close =wp2_3_2_5,
         wp23_factors_transport_available =wp2_3_2_6,
         wp23_factors_education_skill =wp2_3_2_7,
         wp23_factors_family_engaged=wp2_3_2_8,
         wp23_factors_family_support=wp2_3_2_9,
         wp23_factors_only_work_available=wp2_3_2_10,
         wp23_factors_others =wp2_3_2__888,
         wp23_factors_others_specify=wp2_3_2_os,
         wp23_factors_rank_1 =wp2_3_2_rank1 ,
         wp23_factors_rank_2=wp2_3_2_rank2,
         wp23_factors_rank_3=wp2_3_2_rank3)

df_female <- df_female %>%
  mutate_at(c("wp23_factors_rank_1",
              "wp23_factors_rank_2",
              "wp23_factors_rank_3"),
            funs(recode_factor(.,"1" = "The job pays well",
                               "4" = "The job does not interfere with housework and care responsibilities ",
                               "7" = "Work is suitable for your education/ training/ skills",
                               "8" = "Your friends/ family are engaged in the same work",
                               "6" = "Transport to and from workplace is available easily",
                               "2" = "Flexible work timings",
                               "5" = "Location of workplace is close to home",
                               "9" = "Your family allows it/ supports it",
                               "10" = "This was the only work opportunity available to you",
                               "3" = "Creche/ childcare facilities are available at the workplace ",
                               "-888" = "Other")))





#Recoding Variables

df_female$wp23_work_location <- recode_factor(df_female$wp23_work_location, "1" = "Within Home",
                                              "2" = "Outside Home",
                                              "3" = "Both")



#Renaming variables for WP2.3.3
df_female <- df_female %>%
  rename(wp23_challenges_shortage_time =wp2_3_3_1,
         wp23_challenges_lack_leisure=wp2_3_3_2,
         wp23_challenges_lack_socialise=wp2_3_3_3,
         wp23_challenges_health_issues=wp2_3_3_4,
         wp23_challenges_dissatisfaction_family=wp2_3_3_5,
         wp23_challenges_workplace_performance=wp2_3_3_6,
         wp23_challenges_others=wp2_3_3__888,
         wp23_challenges_nota=wp2_3_3_0,
         wp23_challenges_others_specify=wp2_3_3_os,
         wp23_challenges_rank_1 = wp2_3_3_rank1,
         wp23_challenges_rank_2  =wp2_3_3_rank2)

df_female <- df_female %>%
  mutate_at(c("wp23_challenges_rank_1",
              "wp23_challenges_rank_2"),
            funs(recode_factor(.,"1" = "Shortage of time for personal care",
                               "2" = "Lack of leisure time/ rest",
                               "3" = "Lack of time for socialising with friends and family",
                               "4" = "Stress/ health issues",
                               "5" = "Dissatisfaction of family",
                               "6" = "Are unable to perform well at the workplace",
                               "-888" = "Other")))



#Renaming variables for WP2.3.4
df_female <- df_female %>%
  rename(wp23_quit_married =wp2_3_4_1,
         wp23_quit_children=wp2_3_4_2,
         wp23_quit_burden_housework=wp2_3_4_3,
         wp23_quit_elderly_care=wp2_3_4_4,
         wp23_quit_health_issues=wp2_3_4_5,
         wp23_quit_higher_education=wp2_3_4_6,
         wp23_quit_wages_inadequate=wp2_3_4_7,
         wp23_quit_less_pay_men=wp2_3_4_8,
         wp23_quit_less_pay_women=wp2_3_4_9,
         wp23_quit_no_social_security=wp2_3_4_10,
         wp23_quit_lack_basic_facilities=wp2_3_4_11,
         wp23_quit_lon_distance_commute=wp2_3_4_12,
         wp23_quit_lack_transport=wp2_3_4_13,
         wp23_quit_migrated=wp2_3_4_14,
         wp23_quit_long_working_hours=wp2_3_4_15,
         wp23_quit_safety_issues=wp2_3_4_16,
         wp23_quit_workplace_harrassment=wp2_3_4_17,
         wp23_quit_lack_family_support=wp2_3_4_18,
         wp23_quit_left_covid=wp2_3_4_19,
         wp23_quit_employer_covid=wp2_3_4_20,
         wp23_quit_company_shut_covid=wp2_3_4_21,
         wp23_quit_others=wp2_3_4__888,
         wp23_quit_other_specify=wp2_3_4_os,
         wp23_quit_rank_1=wp2_3_4_rank1,
         wp23_quit_rank_2=wp2_3_4_rank2,
         wp23_quit_rank_3=wp2_3_4_rank3)

#Recoding variables:
df_female <- df_female %>%
  mutate_at(c("wp23_quit_rank_1",
              "wp23_quit_rank_2",
              "wp23_quit_rank_3"),
            funs(recode_factor(.,"1" = "Got married",
                               "2" = "Had children",
                               "3" = "Could not continue work due to the burden of household chores and responsibilities",
                               "4" = "Had to care for elderly/ disabled family members",
                               "5" = "Own health issues",
                               "6" = "Started pursuing higher education (+2 level, undergraduate, graduate)",
                               "7" = "Wages were inadequate",
                               "8" = "Was getting paid less than men for the same job",
                               "9" = "Was getting paid less than other women for the same job",
                               "10" = "There were no social security benefits",
                               "11" = "Workplace lacked basic facilities (drinking water/ washroom etc.)",
                               "12" = "Long distance/ commuting hours to work location",
                               "13" = "Lack of transport",
                               "14" = "Migrated",
                               "15" = "Working hours were too long",
                               "16" = "Safety issues",
                               "17" = "Workplace harassment experience",
                               "18" = "Lack of support/ objections to work from parent/ husband/ other family members",
                               "19" = "Left the job myself due to COVID",
                               "20" = "Was fired from the job by employer/company due to COVID",
                               "21" = "Due to covid company shut down",
                               "-888" = "Other")))

#Renaming variables from WP2.3.5 to WP2.3.8

df_female <- df_female %>%
  rename(wp23_work_if_available=wp2_3_5,
         wp23_looking_work = wp2_3_6,
         wp23_type_of_work=wp2_3_6_new,
         wp23_work_kind = wp2_3_7,
         wp23_work_outside= wp2_3_8)

#Recoding Variables
df_female$wp23_work_if_available <- yesNo(df_female, "wp23_work_if_available")
df_female$wp23_looking_work <- yesNo(df_female, "wp23_looking_work") 
df_female$wp23_type_of_work <- recode_factor(df_female$wp23_type_of_work, "1" = "Casual daily work",
                                             "2" = "Casual piece work",
                                             "3" = "Self-employed/ own enterprise",
                                             "4" = "Contract work <1 year",
                                             "5" = "Regular/ Permanent/ Longer Contract work")
df_female$wp23_work_kind <- recode_factor(df_female$wp23_work_kind, "1" = "full time work ",
                                          "2" = "part time work")
df_female$wp23_work_outside <- yesNo(df_female, "wp23_work_outside")




#Renaming variables for WP2.3.9
df_female <- df_female %>%
  rename(wp23_not_working_no_work=wp2_3_9_1,
         wp23_not_working_overqualified=wp2_3_9_2,
         wp23_not_working_underqualified=wp2_3_9_3,
         wp23_not_working_far_home=wp2_3_9_4,
         wp23_not_working_pay_issues=wp2_3_9_5,
         wp23_not_working_less_pay_men=wp2_3_9_6,
         wp23_not_working_less_pay_women=wp2_3_9_7,
         wp23_not_working_no_flexible_hours=wp2_3_9_8,
         wp23_not_working_no_creche=wp2_3_9_9,
         wp23_not_working_interfere_housework=wp2_3_9_10,
         wp23_not_working_interfere_childcare=wp2_3_9_11,
         wp23_not_working_safety_issues=wp2_3_9_12,
         wp23_not_working_illness=wp2_3_9_13,
         wp23_not_working_others=wp2_3_9__888,
         wp23_not_working_others_specify=wp2_3_9_os,
         wp23_not_working_rank_1=wp2_3_9_rank1,
         wp23_not_working_rank_2=wp2_3_9_rank2,
         wp23_not_working_rank_3=wp2_3_9_rank3)


df_female <- df_female %>%
  mutate_at(c("wp23_not_working_rank_1",
              "wp23_not_working_rank_2",
              "wp23_not_working_rank_3"),
            funs(recode_factor(.,"1" = "There was no work available",
                               "10" = "Available work interfered with your household chores and responsibilities",
                               "11" = "Available work interfered with child-care/ elderly care",
                               "2" = "Available work was not suitable for your education/ training/ skills; you were overqualified",
                               "3" = "You did not have the necessary skills/ training for the work available; you are underqualified",
                               "4" = "Available work was far from home",
                               "5" = "Available work did not pay well",
                               "6" = "Was getting offered less pay than men for the same job ",
                               "7" = "Was getting offered less pay than other women for the same job ",
                               "8" = "Available work did not have flexible working hours",
                               "9" = "Available work did not have creche/ childcare facilities",
                               "12" = "Worried about safety",
                               "13" = "Illness/ injury",
                               "-888" = "Other")))


#Renaming variables from WP2.3.10 to WP2.3.11
df_female <- df_female %>%
  rename(#wp23_prefer_selfemployment =wp2_3_10,
         wp23_prevents_household =wp2_3_11_1,
         wp23_prevents_fear_succeed=wp2_3_11_2,
         wp23_prevents_negative_attitude =wp2_3_11_3,
         wp23_prevents_lack_capital=wp2_3_11_4,
         wp23_prevents_lack_workplace=wp2_3_11_5,
         wp23_prevents_afford_workplace=wp2_3_11_6,
         wp23_prevents_lack_savings=wp2_3_11_7,
         wp23_prevents_lack_bank_loans=wp2_3_11_8,
         wp23_prevents_lack_education=wp2_3_11_9,
         wp23_prevents_lack_work_exp=wp2_3_11_10,
         wp23_prevents_lack_knowledge=wp2_3_11_11,
         wp23_prevents_covid=wp2_3_11_12,
         wp23_prevennts_nota=wp2_3_11_0,
         wp23_prevents_others =wp2_3_11__888 ,
         wp23_prevents_others_specify=wp2_3_11_os,
         wp23_prevents_rank_1=wp2_3_11_rank1,
         wp23_prevents_rank_2=wp2_3_11_rank2,
         wp23_prevents_rank_3=wp2_3_11_rank3)

df_female <- df_female %>%
  mutate_at(c("wp23_prevents_rank_1",
              "wp23_prevents_rank_2",
              "wp23_prevents_rank_3"),
            funs(recode_factor(.,"1" = "Household duties",
                               "4" = "Lack of start-up capital",
                               "2" = "Fear of failure to succeed/ Fear of incurring financial loss",
                               "10" = "Lack of previous formal work experience/ business experience",
                               "11" = "Lack of knowledge in market demand or consumer preferences",
                               "3" = "Negative perception or attitude from family or others against female led ventures",
                               "5" = "Unavailability of suitable work-space",
                               "6" = "Cannot afford suitable workspace",
                               "7" = "Lack of savings/ financial or physical assets",
                               "8" = "Inability to access or ineligibility for bank loans or government finance schemes",
                               "9" = "Lack of relevant skills/ education",
                               "12" = "Fear of shut-down due to Covid-19",
                               "-888" = "Other")))

#Renaming variables from WP2.3.12 to WP2.3.15
df_female <- df_female %>%
  rename(wp23_skill_no_training_centers=wp2_3_12_1,
         wp23_skill_not_affordable=wp2_3_12_2,
         wp23_skill_no_impart=wp2_3_12_3,
         wp23_skill_poor_quality=wp2_3_12_4,
         wp23_skill_others=wp2_3_12__888,
         wp23_skill_others_specify=wp2_3_12_os,
         wp23_work_mgnregs=wp2_3_12_new,
         wp23_difficulty_loan=wp2_3_13,
         wp23_fail_loan_lack_collateral=wp2_3_14_1,
         wp23_fail_loan_belief_unviable_venture=wp2_3_14_2,
         wp23_fail_loan_lack_documents=wp2_3_14_3,
         wp23_fail_loan_bribe=wp2_3_14_4,
         wp23_fail_loan_other=wp2_3_14__888,
         wp23_fail_loan_reason_others=wp2_3_14_os,
         wp23_support_family_husband=wp2_3_15)

#Recoding variables
df_female$wp23_work_mgnregs <- recode_factor(df_female$wp23_work_mgnregs, "1" = "Yes, have worked under MGNREGS",
                                             "2" = "No, not worked but have a MGNREGS job card",
                                             "3" = "No, but have applied for a job card ",
                                             "4" = "No and have not applied for any card ",
                                             "5" = "Not aware of MGNREGS",
                                             "-999" = "Do not know")
df_female$wp23_difficulty_loan <- recode_factor(df_female$wp23_difficulty_loan, "1" = "Yes",
                                                "0" = "No",
                                                "-999" = "Do not know",
                                                "2" = "I have never applied for loan")
df_female$wp23_support_family_husband <- yesNo(df_female, "wp23_support_family_husband")


#TUS-----

#Renaming variables for TUS 

df_female <- df_female %>%
  rename(tus_hhd_meal_prep_total = tus1_1_time,
         tus_hhd_meal_prep_time = tus1_1_travel,
         tus_hhd_meal_prep_sim_act = tus1_1_simact,
         tus_hhd_meal_prep_main_act = tus1_1_mainact,
         tus_hhd_cleaning_total = tus1_2_time,
         tus_hhd_cleaning_time = tus1_2_travel,
         tus_hhd_cleaning_sim_act = tus1_2_simact,
         tus_hhd_cleaning_main_act = tus1_2_mainact,
         tus_hhd_collecting_water_total = tus1_3_time,
         tus_hhd_collecting_water_time = tus1_3_travel,
         tus_hhd_collecting_water_sim_act = tus1_3_simact,
         tus_hhd_collecting_water_main_act = tus1_3_mainact,
         tus_hhd_collect_fuel_total = tus1_4_time,
         tus_hhd_collect_fuel_time = tus1_4_travel,
         tus_hhd_collect_fuel_sim_act = tus1_4_simact,
         tus_hhd_collect_fuel_main_act = tus1_4_mainact,
         tus_hhd_groceries_total = tus1_5_time,
         tus_hhd_groceries_time = tus1_5_travel,
         tus_hhd_groceries_sim_act = tus1_5_simact,
         tus_hhd_groceries_main_act = tus1_5_mainact,
         tus_hhd_child_care_total = tus1_6_time,
         tus_hhd_child_care_time = tus1_6_travel,
         tus_hhd_child_care_sim_act = tus1_6_simact,
         tus_hhd_child_care_main_act = tus1_6_mainact,
         tus_hhd_elder_care_total = tus1_7_time,
         tus_hhd_elder_care_time = tus1_7_travel,
         tus_hhd_elder_care_sim_act = tus1_7_simact,
         tus_hhd_elder_care_main_act = tus1_7_mainact,
         tus_hhd_repair_total = tus1_8_time,
         tus_hhd_repair_time = tus1_8_travel,
         tus_hhd_repair_sim_act = tus1_8_simact,
         tus_hhd_repair_main_act = tus1_8_mainact,
         tus_hhd_cattle_livestock_total = tus1_9_time,
         tus_hhd_cattle_livestock_time = tus1_9_travel,
         tus_hhd_cattle_livestock_sim_act = tus1_9_simact,
         tus_hhd_cattle_livestock_main_act = tus1_9_mainact,
         tus_hhd_produce_goods_total = tus1_10_time,
         tus_hhd_produce_goods_time = tus1_10_travel,
         tus_hhd_produce_goods_sim_act = tus1_10_simact,
         tus_hhd_produce_goods_main_act = tus1_10_mainact,
         tus_hhd_other_household_total = tus1_11_time,
         tus_hhd_other_household_time = tus1_11_travel,
         tus_hhd_other_household_sim_act = tus1_11_simact,
         tus_hhd_other_household_main_act = tus1_11_mainact,
         tus_hhd_voluntarty_work_total = tus1_12_time,
         tus_hhd_voluntarty_work_time = tus1_12_travel,
         tus_hhd_voluntarty_work_sim_act = tus1_12_simact,
         tus_hhd_voluntarty_work_main_act = tus1_12_mainact,
         tus_leisure_eating_drinking_total = tus1_13_time,
         tus_leisure_eating_drinking_time = tus1_13_travel,
         tus_leisure_eating_drinking_sim_act = tus1_13_simact,
         tus_leisure_eating_drinking_main_act = tus1_13_mainact,
         tus_leisure_socialise_total = tus1_14_time,
         tus_leisure_socialise_time = tus1_14_travel,
         tus_leisure_socialise_sim_act = tus1_14_simact,
         tus_leisure_socialise_main_act = tus1_14_mainact,
         tus_leisure_active_leisure_total = tus1_15_time,
         tus_leisure_active_leisure_time = tus1_15_travel,
         tus_leisure_active_leisure_sim_act = tus1_15_simact,
         tus_leisure_active_leisure_main_act = tus1_15_mainact,
         tus_leisure_media_social_media_total = tus1_16_time,
         tus_leisure_media_social_media_time = tus1_16_travel,
         tus_leisure_media_social_media_sim_act = tus1_16_simact,
         tus_leisure_media_social_media_main_act = tus1_16_mainact,
         tus_leisure_reading_total = tus1_17_time,
         tus_leisure_reading_time = tus1_17_travel,
         tus_leisure_reading_sim_act = tus1_17_simact,
         tus_leisure_reading_main_act = tus1_17_mainact,
         tus_leisure_other_leisure_total = tus1_18_time,
         tus_leisure_other_leisure_time = tus1_18_travel,
         tus_leisure_other_leisure_sim_act = tus1_18_simact,
         tus_leisure_other_leisure_main_act = tus1_18_mainact,
         tus_emp_work_own_enterprise_total = tus1_19_time,
         tus_emp_work_own_enterprise_time = tus1_19_travel,
         tus_emp_work_own_enterprise_sim_act = tus1_19_simact,
         tus_emp_work_own_enterprise_main_act = tus1_19_mainact,
         tus_emp_work_family_enterprise_total = tus1_20_time,
         tus_emp_work_family_enterprise_time = tus1_20_travel,
         tus_emp_work_family_enterprise_sim_act = tus1_20_simact,
         tus_emp_work_family_enterprise_main_act = tus1_20_mainact,
         tus_emp_work_wages_total = tus1_21_time,
         tus_emp_work_wages_time = tus1_21_travel,
         tus_emp_work_wages_sim_act = tus1_21_simact,
         tus_emp_work_wages_main_act = tus1_21_mainact,
         tus_edu_study_outside_home_total = tus1_24_time,
         tus_edu_study_outside_home_time = tus1_24_travel,
         tus_edu_study_outside_home_sim_act = tus1_24_simact,
         tus_edu_study_outside_home_main_act = tus1_24_mainact,
         tus_edu_study_inside_home_total = tus1_25_time,
         tus_edu_study_inside_home_time = tus1_25_travel,
         tus_edu_study_inside_home_sim_act = tus1_25_simact,
         tus_edu_study_inside_home_main_act = tus1_25_mainact,
         tus_care_healthcare_total = tus1_26_time,
         tus_care_healthcare_time = tus1_26_travel,
         tus_care_healthcare_sim_act = tus1_26_simact,
         tus_care_healthcare_main_act = tus1_26_mainact,
         tus_care_rest_sleep_day_total = tus1_27_time,
         tus_care_rest_sleep_day_time = tus1_27_travel,
         tus_care_rest_sleep_day_sim_act = tus1_27_simact,
         tus_care_rest_sleep_day_main_act = tus1_27_mainact,
         tus_care_rest_sleep_night_total = tus1_30_time,
         tus_care_rest_sleep_night_time = tus1_30_travel,
         tus_care_rest_sleep_night_sim_act = tus1_30_simact,
         tus_care_rest_sleep_night_main_act = tus1_30_mainact,
         tus_care_personal_care_total = tus1_28_time,
         tus_care_personal_care_time = tus1_28_travel,
         tus_care_personal_care_sim_act = tus1_28_simact,
         tus_care_personal_care_main_act = tus1_28_mainact)
         
df_female <- df_female %>% 
  mutate_at(c("tus_hhd_meal_prep_sim_act",
              "tus_hhd_cleaning_sim_act",
              "tus_hhd_collecting_water_sim_act",
              "tus_hhd_collect_fuel_sim_act",
              "tus_hhd_groceries_sim_act",
              "tus_hhd_child_care_sim_act",
              "tus_hhd_elder_care_sim_act",
              "tus_hhd_repair_sim_act",
              "tus_hhd_cattle_livestock_sim_act",
              "tus_hhd_produce_goods_sim_act",
              "tus_hhd_other_household_sim_act",
              "tus_hhd_voluntarty_work_sim_act",
              "tus_leisure_eating_drinking_sim_act",
              "tus_leisure_socialise_sim_act",
              "tus_leisure_active_leisure_sim_act",
              "tus_leisure_media_social_media_sim_act",
              "tus_leisure_reading_sim_act",
              "tus_leisure_other_leisure_sim_act",
              "tus_emp_work_own_enterprise_sim_act",
              "tus_emp_work_family_enterprise_sim_act",
              "tus_emp_work_wages_sim_act",
              "tus_edu_study_outside_home_sim_act",
              "tus_edu_study_inside_home_sim_act",
              "tus_care_healthcare_sim_act",
              "tus_care_rest_sleep_day_sim_act",
              "tus_care_rest_sleep_night_sim_act",
              "tus_care_personal_care_sim_act"), 
            funs(recode_factor(., "1" = "Yes", "0" = "No", "-666" = "NA")))


df_female <- df_female %>% 
  mutate_at(c("tus_hhd_meal_prep_main_act",
              "tus_hhd_cleaning_main_act",
              "tus_hhd_collecting_water_main_act",
              "tus_hhd_collect_fuel_main_act",
              "tus_hhd_groceries_main_act",
              "tus_hhd_child_care_main_act",
              "tus_hhd_elder_care_main_act",
              "tus_hhd_repair_main_act",
              "tus_hhd_cattle_livestock_main_act",
              "tus_hhd_produce_goods_main_act",
              "tus_hhd_other_household_main_act",
              "tus_hhd_voluntarty_work_main_act",
              "tus_leisure_eating_drinking_main_act",
              "tus_leisure_socialise_main_act",
              "tus_leisure_active_leisure_main_act",
              "tus_leisure_media_social_media_main_act",
              "tus_leisure_reading_main_act",
              "tus_leisure_other_leisure_main_act",
              "tus_emp_work_own_enterprise_main_act",
              "tus_emp_work_family_enterprise_main_act",
              "tus_emp_work_wages_main_act",
              "tus_edu_study_outside_home_main_act",
              "tus_edu_study_inside_home_main_act",
              "tus_care_healthcare_main_act",
              "tus_care_rest_sleep_day_main_act",
              "tus_care_rest_sleep_night_main_act",
              "tus_care_personal_care_main_act"), 
            funs(recode_factor(., "1" = "Yes", "0" = "No", "-666" = "NA")))


# F: Housework and Care Responsibilities----

#Renaming variables 
df_female <- df_female %>%
  rename(household_activities_cooking = f1_1,
         household_activities_ill_persons = f1_2,
         household_activities_children = f1_3,
         household_activities_most_time = f2,
         household_activities_most_time_why = f3,
         household_activities_most_time_why_others= f3_os,
         household_chores_help = f4,
         household_chores_help_husband = f5_1,
         household_chores_help_mother = f5_2,
         household_chores_help_father = f5_3,
         household_chores_help_sister = f5_4,
         household_chores_help_brother = f5_5,
         household_chores_help_domestic = f5_6,
         household_chores_help_friends = f5_7,
         household_chores_help_daughter = f5_8,
         household_chores_help_son = f5_9,
         household_chores_help_others = f5__888,
         household_chores_help_others_specify = f5_os,
         household_chores_help_male_members = f6)


df_female$household_activities_cooking <- recode_factor(df_female$household_activities_cooking, "1" = "Male members",
                                                        "2" = "Female members",
                                                        "3" = "Both")
df_female$household_activities_ill_persons <- recode_factor(df_female$household_activities_ill_persons, "1" = "Male members",
                                                            "2" = "Female members",
                                                            "3" = "Both")
df_female$household_activities_children <- recode_factor(df_female$household_activities_children, 
                                                         "0" = "There are no children in the household",
                                                         "1" = "Male members",
                                                         "2" = "Female members",
                                                         "3" = "Both")
df_female$household_activities_most_time <- yesNo(df_female, "household_activities_most_time")
df_female$household_activities_most_time_why <- recode_factor(df_female$household_activities_most_time_why,
                                                              "1" = "There is no other member who can do household work",
                                                              "2" = "Cannot afford to hire domestic help",
                                                              "3" = "Social/ religious constraints",
                                                              "-888" = "Other (Specify)")

df_female$household_chores_help_male_members <- yesNo(df_female, "household_chores_help_male_members")


# G: Skill and Digital Awareness----
#Renaming variables 

df_female <- df_female %>%
  rename(skill_dev_prog_aware = g1,
         skill_training_recieved = g2,
         skill_training_type = g3,
         skill_training_where = g4,
         skill_training_where_others = g4_os,
         knowledge_smartphone = g5_1,
         knowledge_computer = g5_2,
         knowledge_internet = g5_3)

df_female$skill_dev_prog_aware <- yesNo(df_female, "skill_dev_prog_aware")
df_female$skill_training_recieved <- yesNo(df_female, "skill_training_recieved")
df_female$skill_training_where <- recode_factor(df_female$skill_training_where, "1" = "From government skill development programme",
                                                "2" = "Private training centre",
                                                "3" = "On the job; training given by employer",
                                                "4" = "Family apprenticeship",
                                                "-888" = "Other (Specify)")
df_female$knowledge_smartphone <- recode_factor(df_female$knowledge_smartphone, "1" = "No knowledge",
                                                "2" = "Not very comfortable",
                                                "3" = "Somewhat comfortable",
                                                "4" = "Quite comfortable")
df_female$knowledge_computer <- recode_factor(df_female$knowledge_computer,"1" = "No knowledge",
                                              "2" = "Not very comfortable",
                                              "3" = "Somewhat comfortable",
                                              "4" = "Quite comfortable")
df_female$knowledge_internet <- recode_factor(df_female$knowledge_internet,"1" = "No knowledge",
                                              "2" = "Not very comfortable",
                                              "3" = "Somewhat comfortable",
                                              "4" = "Quite comfortable")

# H: Availing benefits from governments schemes/aid----

#Renaming variables for H1
df_female <- df_female %>%
  rename(schemes_bharatiya_mahila_bank = h1_1,
         schemes_annapurna = h1_2,
         schemes_stree_sakti = h1_3,
         schemes_orient = h1_4,
         schemes_dena = h1_5,
         schemes_udyogini = h1_6,
         schemes_cent_kalyani = h1_7,
         schemes_mahila = h1_8,
         schemes_mudra = h1_9,
         schemes_wep = h1_10,
         schemes_tread = h1_11,
         schemes_others = h1__888,
         schemes_none = h1_0,
         schemes_others_specify = h1_os)

#Renaming variables for H2
df_female <- df_female %>%
  rename(schemes_benefit_none= h2_0,
         schemes_benefit_bharatiya_mahila_bank = h2_1,
         schemes_benefit_annapurna = h2_2,
         schemes_benefit_stree_sakti = h2_3,
         schemes_benefit_orient = h2_4,
         schemes_benefit_dena = h2_5,
         schemes_benefit_udyogini = h2_6,
         schemes_benefit_cent_kalyani = h2_7,
         schemes_benefit_mahila = h2_8,
         schemes_benefit_mudra = h2_9,
         schemes_benefit_wep = h2_10,
         schemes_benefit_tread = h2_11,
         schemes_benefit_others = h2__888)


#Renaming variables  for H3
df_female <- df_female %>%
  rename(relief_measures_awareness = h3)

df_female$relief_measures_awareness <- yesNo(df_female, "relief_measures_awareness")

#Renaming variables  for H4
df_female <- df_female %>%
  rename(relief_meaures_pm_garib = h4_1,
         relief_meaures_pds = h4_2,
         relief_meaures_pm_ujjwala = h4_3,
         relief_meaures_cash_transfer_widow = h4_4,
         relief_meaures_cash_transfer_weak = h4_5,
         relief_meaures_ayushman_bharat = h4_6,
         relief_meaures_food_distribution = h4_7,
         relief_meaures_shelters = h4_8,
         relief_meaures_provision_migrant = h4_9,
         relief_meaures_nutritional_support = h4_10,
         relief_meaures_social_security = h4_11,
         relief_meaures_cash_transfer_bpl = h4_12,
         relief_meaures_interest_free = h4_13,
         relief_meaures_cash_transfer_anganwadi = h4_14,
         relief_meaures_none = h4_0,
         relief_meaures_others = h4__888,
         relief_meaures_others_specify = h4_os)

#Renaming variables  for H5 and H6
df_female <- df_female %>%
  rename(difficulties_availing_scheme = h5,
         challenges_not_recieved = h6_1,
         challenges_part_recieved = h6_2,
         challenges_difficulties_location = h6_3,
         challenges_no_documents = h6_4,
         challenges_ineligible = h6_5,
         challenges_others = h6__888,
         challenges_others_specify = h6_os)

df_female$difficulties_availing_scheme <- yesNo(df_female, "difficulties_availing_scheme")


#With team and LEAD
#Dropping personal information and responses with age_actual >60

df_female <- df_female %>%
  select(-c(enumeratorcode, enumeratorname, caseid, participant_name, fathername, houseno, mobile, mobile_alternate)) %>%
  filter(age_actual<=60)
  
# Removing one instance of duplicated row
df_female <- df_female[-which(df_female[,"main_id"]==13157)[1],] 

df_female <- df_female %>% group_by(main_id) %>%
  mutate(main_id_new = if(n( ) > 1) {paste0(main_id, "_",row_number())} 
         else {paste0(main_id)})

#NIC NCO script----
source(paste0(scripts, "NIC NCO analysis.R"))


#df_female %>%
#  select(wp21_nic_1,wp21_nco_1) %>%
#  filter(wp21_nco_1 > 10) %>%
#  view()

# NIC codes 0, 4 56 60 98 not in the list but have been entered


# Additional frequently used variables and addition of weights
df_female$district_name <- substr(df_female$id,1,6)
df_female$district_name = recode_factor(df_female$district_name,
                                        "DL02KB" = "Central Delhi",
                                        "DL02SB" = "Central Delhi",
                                        "DL02BW" = "South West Delhi",
                                        "DL02ML" = "South West Delhi",
                                        "JH01GH" = "Gharwa",
                                        "JH01BP" = "Gharwa",
                                        "JH02BM" = "Dhanbad",
                                        "JH02DB" = "Dhanbad",
                                        "KA01MN" = "Mandya",
                                        "KA01MD" = "Mandya",
                                        "KA02BS" = "Bangalore",
                                        "KA02YP" = "Bangalore",
                                        "MP01TT" = "Rewa",
                                        "MP01GR" = "Rewa",
                                        "MP02RA" = "Indore",
                                        "MP02AM" = "Indore",
                                        "RJ01SI" = "Barmer",
                                        "RJ01CH" = "Barmer",
                                        "RJ02HM" = "Jaipur",
                                        "RJ02VN" = "Jaipur")

# Loading weights | This is a csv from Vikash's excel file. Will send on the group
flfs_weights <- read.csv(here("data/raw/flfs_weights.csv"), stringsAsFactors = FALSE) %>% 
  clean_names()

# Merging weights----
df_female <- df_female %>% 
  mutate(
    booth_no = substr(id, 1,9)
  ) %>% 
  left_join(
    flfs_weights, by = c("booth_no" = "booth_code")
  ) %>% 
  mutate(
    comp_wt = case_when( age_actual <= 35 ~ dist_s_y,
                         TRUE~ dist_s_o)
  )

# Education
df_female <- df_female %>% 
  mutate(Education = case_when(last_grade %in% c("No Education")~"Not Literate",
                               last_grade %in% c("Class 1", "Class 2","Class 3","Class 4","Class 5")~"Primary", 
                               last_grade %in% c("Class 6","Class 7","Class 8")~"Secondary",
                               last_grade %in% c("Class 9","Class 10")~"Secondary",
                               last_grade %in% c("Class 11","Class 12")~"Higher Secondary",
                               last_grade %in% c("Graduate","Diploma")~"Graduate",
                               last_grade %in% c("Post Graduate","Higher")~"Post Graduate and Above",
                               TRUE~NA_character_),
         Education = factor(Education,  levels = c("Not Literate", "Primary", "Secondary", "Higher Secondary",
                                                   "Graduate","Post Graduate and Above"))) 

# Marital status
df_female <- df_female %>% 
  mutate(
    `Marital Status` = case_when( marital_status %in% c("Seperated","Divorced") ~ "Separated/Divorced",
                                  TRUE~as.character(marital_status)),
    `Marital Status` = factor(`Marital Status`, levels = c("Married", "Unmarried", "Separated/Divorced", "Widowed","Refused to answer"))
  )

# Age
df_female <- df_female %>% 
  mutate(
    `Age Group` = case_when( age_actual<=35 ~ "[18-35]",
                             TRUE~"(35-60]"),
    `Age Group` = factor(`Age Group`, levels = c("[18-35]","(35-60]"))
  )


# Assets
df_assets <- df_female %>% 
  ungroup() %>% 
  select(main_id_new, area_actual, comp_wt,starts_with("assets_")) %>% 
  mutate(
    assets_mobile_internet = case_when( assets_mobile_internet + assets_smartphone >0 ~ 1,
                                        TRUE~0)
  ) 

# We remove the assets that have an ownership of less than one percent in each sector's subset
assets_rural <- c("assets_electric_fan","assets_electric_cooler","assets_ac","assets_computer","assets_mobile_wo_internet",
                  "assets_mobile_internet","assets_bicycle","assets_scooter","assets_car","assets_tv","assets_fridge","assets_washing_machine",
                  "assets_3_wheeler","assets_cart","assets_tempo","assets_tractor","assets_thresher")

assets_urban <- c("assets_electric_fan","assets_electric_cooler","assets_ac","assets_computer","assets_mobile_wo_internet",
                  "assets_mobile_internet","assets_microwave","assets_bicycle","assets_scooter","assets_car","assets_tv","assets_fridge","assets_washing_machine",
                  "assets_3_wheeler")

df_assets_urban_wt <- df_assets %>% 
  filter(area_actual == "Urban") %>% 
  select(main_id_new, area_actual,comp_wt, assets_urban) %>% 
  mutate(
    across(
      assets_urban, ~(. * (1- sum(comp_wt[. == 1])/sum(comp_wt))))) %>% 
  rowwise() %>% 
  mutate(
    asset_index = rowSums(across(starts_with("assets_"))),
  )

df_assets_rural_wt <- df_assets %>% 
  filter(area_actual == "Rural") %>% 
  select(main_id_new, area_actual,comp_wt, assets_rural) %>% 
  mutate(
    across(
      assets_rural, ~(. * (1- sum(comp_wt[. == 1])/sum(comp_wt))))) %>% 
  rowwise() %>% 
  mutate(
    asset_index = rowSums(across(starts_with("assets_"))),
  )

# Unweighted ----
df_assets_urban <- df_assets %>% 
  filter(area_actual == "Urban") %>% 
  select(main_id_new, area_actual,comp_wt, assets_urban) %>% 
  mutate(
    across(
      assets_urban, ~(. * (1- sum(. == 1)/n())))) %>% 
  rowwise() %>% 
  mutate(
    asset_index = rowSums(across(starts_with("assets_"))),
  )

df_assets_rural <- df_assets %>% 
  filter(area_actual == "Rural") %>% 
  select(main_id_new, area_actual,comp_wt, assets_rural) %>% 
  mutate(
    across(
      assets_rural, ~(. * (1- sum(. == 1)/n())))) %>% 
  rowwise() %>% 
  mutate(
    asset_index = rowSums(across(starts_with("assets_"))),
  )

df_female <- df_female %>% 
  left_join(rbind.data.frame(df_assets_urban[,c("main_id_new", "asset_index")],
                             df_assets_rural[,c("main_id_new", "asset_index")]), by = "main_id_new") 

# Saving data
write.csv(df_female, paste0(cleandata,"female_clean_internal.csv"),row.names = FALSE)

#External
#Dropping personal information as well as geo-spatial variables

#Saving for external purpose
write.csv(df_female %>%
            select(-c(gpslocation.Accuracy, gpslocation.Latitude, gpslocation.Longitude, gpslocation.Altitude)) 
          , paste0(cleandata,"female_clean_external.csv"),row.names = FALSE)


