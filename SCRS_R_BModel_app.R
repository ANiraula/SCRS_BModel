####################################
# SCRS Normal Cost/Benefit Model #
####################################

## Class Three employees*

rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)
library(dplyr)
library(profvis)
library(shiny)
library(plotly)
library(data.table)
#setwd(getwd())

#FileName <- 'NDPERS_BM_Inputs.xlsx'
library(readxl)
library(httr)
url1 <- "https://github.com/ANiraula/SCRS_BModel/blob/main/SCRS_BM_Inputs.xlsx?raw=true"
GET(url1, write_disk(FileName <- tempfile(fileext = ".xlsx")))
#FileName <- "https://github.com/ANiraula/NDPERS_BModel/blob/main/NDPERS_BM_Inputs.xlsx?raw=true"

#urlfile="https://github.com/ANiraula/NDPERS_BModel/blob/main/NDPERS_BM_Inputs.xlsx?raw=true"
#inputs<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
#inputs <- setDT(inputs)
YearStart <- 2021
Age <- 20:120
YOS <- 0:100
RetirementAge <- 20:120
Years <- 2011:2121    #(why 2121? Because 120 - 20 + 2021 = 2121)
#Updated from 2010 to 2011

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}

#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')
#Updated* (to Pub-2010 Safety * Multipliers -> Actives)
#Updated* (to SCRS Table 2020 * 80% * MP-2019 Ultimate * Multipliers -> Retirees)
#Adding* capability to swithc between "Teachers", "General", "Blend" (weighted by 2021 membership)

#View(SurvivalRates)
#View(MaleMP)
MaleMP <- read_excel(FileName, sheet = 'MP-2019_Male') #Updated* (to MP-2019)
FemaleMP <- read_excel(FileName, sheet = 'MP-2019_Female')#Updated* (to MP-2019)
#SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")#Updated* (How to combined YOS & AGE increases?)

### Addition ###
SalaryGrowthYOS <- read_excel(FileName, sheet = "Salary Growth YOS")#Updated to SCRS* (YOS)
#View(SalaryGrowthYOS)

################
SalaryEntry <- read_excel(FileName, sheet = "Salary and Headcount") %>% #Updated*
  select(entry_age, start_sal, count_start)#Updated to SCRS*
#View(SalaryEntry)

##############
TerminationRateAfter10 <- read_excel(FileName, sheet = 'Termination Rates after 10')#Updated to SCRS*
TerminationRateBefore10 <- read_excel(FileName, sheet = 'Termination Rates before 10')#Updated to SCRS*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated to SCRS*
#View(TerminationRateBefore10)
#View(RetirementRates)

### Adding scaling factors
#scale.act.male <- 0.92 
#scale.ret.male <- 1.03
#scale.act.female <- 0.92 
#scale.ret.female <- 1.01 
#NormalRetRuleAge
#Function for determining retirement eligibility (including normal retirement, unreduced early retirement, and reduced early retirement)

################
# Main rule: Retirement Eligibility
################

IsRetirementEligible <- function(Age, YOS){
  Check = ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                   (YOS + Age >= NormalRetRule & YOS >= NormalYOSI) |
                   (Age >= ReduceRetAge & YOS >= NormalYOSI), TRUE, FALSE)
  return(Check)
}


################
# New rule: 3 Retirement Types
################

#Reduced: at Age 60 (before Age 65) w/ 8YOS
#Normal:
#       1. Age 65 w/ 8YOS
#       2. Rule of 90

RetirementType <- function(Age, YOS){
  Check = ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI), "Normal No Rule of 90",
                 ifelse((YOS + Age >= NormalRetRule & YOS >= NormalYOSI), "Normal With Rule of 90",
                        ifelse((Age >= ReduceRetAge & Age < NormalRetAgeI & YOS >= NormalYOSI), "Reduced","No")))
  
  return(Check)
}

#Age Based: Members retiring after age 60 will have their benefit reduced at the rate of 5% per year for each year of their retirement age precedes age 65.
#Service Based: 4% per year for each year of creditable service that is less than 28.

#These rates dont change so they're outside the function
#Transform base mortality rates and mortality improvement rates
MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)


##Mortality calculations
#Expand grid for ages 20-120 and years 2010 to 2121 (why 2121? Because 120 - 20 + 2021 = 2121)
MortalityTable <- expand_grid(Age, Years)

SurvivalRates <- SurvivalRates %>% mutate_all(as.numeric)   #why do we need this step?

######### Graphing SINGLE ENTRY AGE + RETENTION
palette_reason <- list(Orange="#FF6633",
                       LightOrange="#FF9900",
                       DarkGrey="#333333", 
                       LightGrey= "#CCCCCC", 
                       SpaceGrey ="#A69FA1",
                       DarkBlue="#0066CC", 
                       GreyBlue= "#6699CC", 
                       Yellow= "#FFCC33",
                       LightBlue = "#66B2FF", 
                       SatBlue = "#3366CC", 
                       Green = "#669900",LightGreen = "#00CC66", Red = "#CC0000",LightRed="#FF0000")


##########
##########
##########

ui <- fluidPage(
  titlePanel("SCRS NPV Pension Wealth Accrual (V1)"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/reason_logo.png"), width = 200, height = 50),
                 selectInput("e.age", "Entry Age", choices = c(20, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67)),
                 selectInput("ee","Employee Type", choices = c("Teachers", "General", "Blend"), selected = "Teachers"),
                 #sliderInput("interest", "Contribution Interest", min = 0.02, max = 0.08, step = 0.005, value = 0.065),
                 sliderInput("dr", "Discount Rate (%)", min = 4, max = 8, step = 0.5, value = 7),
                 sliderInput("cola", "Cost-of-Living Adjustment (%)", min = 0, max = 3, step = 0.5, value = 1),
                 sliderInput("mult", "Benefit Multiplier", min = 0.82, max = 2.82, step = 0.10, value = 1.82),
                 sliderInput("DCreturn", "DC Return Rate (%)",min = 3, max = 8, step = 0.5, value = 5),
                 sliderInput("DC_EEcontr", "DC EE Contribution (%)", min = 4, max = 15, step = 0.05, value = 9),
                 sliderInput("DC_ERcontr", "DC ER Contribution (%)", min = 0, max = 11, step = 0.05, value = 5),
    ),
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      
      plotly::plotlyOutput("plot_pwealth")
    )
  )
)

#wrap around secure_app for password protection
##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  output$plot_pwealth <- plotly::renderPlotly({
    
    ################## Set Employee type: General, Teacher, Blend
    employee <- as.character(input$ee) #"Teachers", "General"
    #Blend is weighted by 2021 Teacher vs. Other employee count (88,883 vs. 110,279)
    ##############################
    COLA <- input$cola/100
    BenMult <- input$mult/100
    ARR <- input$dr/100
    DC_return <- input$DCreturn/100
    DC_EE_cont <- input$DC_EEcontr/100
    DC_ER_cont <- input$DC_ERcontr/100
    #Interest <- input$interest
    
    #### Start the Timing
    #profvis({
    mortality <- function(data = MortalityTable,
                          SurvivalRates = SurvivalRates,
                          MaleMP = MaleMP,
                          FemaleMP = FemaleMP,
                          MaleMP_ultimate = MaleMP_ultimate,
                          FemaleMP_ultimate = FemaleMP_ultimate
    ){
      
      MortalityTable <- data %>% 
        left_join(SurvivalRates, by = "Age") %>% 
        left_join(MaleMP, by = c("Age", "Years")) %>% 
        left_join(FemaleMP, by = c("Age", "Years")) %>% 
        left_join(MaleMP_ultimate, by = "Age") %>% 
        left_join(FemaleMP_ultimate, by = "Age") %>% 
        mutate(MaleMP_final = MP_ultimate_male,
               FemaleMP_final = MP_ultimate_female,
               entry_age = Age - (Years - YearStart),
               YOS = Age - entry_age) %>% 
        group_by(Age) %>%
        
        #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
        mutate(MPcumprod_male = cumprod(1 - MaleMP_final*0.8*
                                          if(employee == "Blend"){ScaleMultipleMaleBlendRet}
                                        else if(employee == "Teachers"){ScaleMultipleMaleTeacherRet}
                                        else{ScaleMultipleMaleGeneralRet}),
               #Started mort. table from 2011 (instead of 2010) 
               #to cumsum over 2011+ & then multiply by 2010 MP-2019
               #removed /(1 - MaleMP_final[Years == 2010])
               MPcumprod_female = cumprod(1 - FemaleMP_final*0.8*
                                            if(employee == "Blend"){ScaleMultipleFeMaleBlendRet}
                                          else if(employee == "Teachers"){ScaleMultipleFeMaleTeacherRet}
                                          else{ScaleMultipleFeMaleGeneralRet}),
               mort_male = ifelse(IsRetirementEligible(Age, YOS)==F, 
                                  if(employee == "Blend"){PubS_2010_employee_male_blend}else if(employee == "Teachers"){PubS_2010_employee_male_teacher}else{PubS_2010_employee_male_general}, #Adding adj. facctors
                                  if(employee == "Blend"){SCRS_2020_employee_male_blend * ((ScaleMultipleMaleTeacherRet+ScaleMultipleMaleGeneralRet)/2)}
                                  else if(employee == "Teachers"){SCRS_2020_employee_male_teacher * ScaleMultipleMaleTeacherRet}
                                  else{SCRS_2020_employee_male_general * ScaleMultipleMaleGeneralRet}) * MPcumprod_male,
               mort_female = ifelse(IsRetirementEligible(Age, YOS)==F, 
                                    if(employee == "Blend"){PubS_2010_employee_female_blend}else if(employee == "Teachers"){PubS_2010_employee_female_teacher}else{PubS_2010_employee_female_general}, #Adding adj. facctors
                                    if(employee == "Blend"){SCRS_2020_employee_female_blend * ((ScaleMultipleFeMaleTeacherRet+ScaleMultipleFeMaleGeneralRet)/2)}
                                    else if(employee == "Teachers"){SCRS_2020_employee_female_teacher * ScaleMultipleFeMaleTeacherRet}
                                    else{SCRS_2020_employee_female_general * ScaleMultipleFeMaleGeneralRet}) * MPcumprod_female,
               mort = (mort_male + mort_female)/2) %>% 
        #Recalcualting average
        filter(Years >= 2021, entry_age >= 20) %>% 
        replace(is.na(.), 0) %>%
        ungroup()
      
      MortalityTable
      
    }
    ##############
    cumFV <- function(interest, cashflow){
      cumvalue <- double(length = length(cashflow))
      for (i in 2:length(cumvalue)) {
        cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
      }
      return(cumvalue)
    }
    
    ##############
    AnnuityF <- function(data = MortalityTable,
                         ColaType = "Simple"){
      
      AnnFactorData <- MortalityTable %>% 
        select(Age, entry_age, mort) %>%
        group_by(entry_age) %>% 
        mutate(surv = cumprod(1 - lag(mort, default = 0)),
               surv_DR = surv/(1+ARR)^(Age - entry_age),
               surv_DR_COLA = surv_DR * if(ColaType == "Compound"){1+(COLA * (Age - entry_age))}
               else{(1+COLA)^(Age - entry_age)},
               AnnuityFactor = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
        ungroup()
      
      AnnFactorData
      
    }
    
    ##############
    
    MortalityTable <- mortality(data = MortalityTable,
                                SurvivalRates = SurvivalRates,
                                MaleMP = MaleMP,
                                FemaleMP = FemaleMP,
                                MaleMP_ultimate = MaleMP_ultimate,
                                FemaleMP_ultimate = FemaleMP_ultimate)
    
    #Join base mortality table with mortality improvement table and calculate the final mortality rates
    # MortalityTable <- MortalityTable %>% 
    #   left_join(SurvivalRates, by = "Age") %>% 
    #   left_join(MaleMP, by = c("Age", "Years")) %>% 
    #   left_join(FemaleMP, by = c("Age", "Years")) %>% 
    #   left_join(MaleMP_ultimate, by = "Age") %>% 
    #   left_join(FemaleMP_ultimate, by = "Age") %>% 
    #   mutate(MaleMP_final = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male),
    #          FemaleMP_final = ifelse(Years > max(FemaleMP$Years),  MP_ultimate_female, MP_female),
    #          entry_age = Age - (Years - YearStart),
    #          YOS = Age - entry_age) %>% 
    #   group_by(Age) %>%
    #   
    #   #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
    #   mutate(MPcumprod_male = cumprod(1 - MaleMP_final),
    #          #Started mort. table from 2011 (instead of 2010) 
    #          #to cumsum over 2011+ & then multiply by 2010 MP-2019
    #          #removed /(1 - MaleMP_final[Years == 2010])
    #          MPcumprod_female = cumprod(1 - FemaleMP_final),
    #          mort_male = ifelse(IsRetirementEligible(Age, YOS)==F, PubG_2010_employee_male * ScaleMultipleMaleAct, #Adding adj. facctors
    #                             PubG_2010_healthy_retiree_male * ScaleMultipleMaleRet) * MPcumprod_male,
    #          mort_female = ifelse(IsRetirementEligible(Age, YOS)==F, PubG_2010_employee_female * ScaleMultipleFemaleAct,
    #                               PubG_2010_healthy_retiree_female * ScaleMultipleFemaleRet) * MPcumprod_female,
    #          mort = (mort_male + mort_female)/2) %>% 
    #          #Recalcualting average
    #   filter(Years >= 2021, entry_age >= 20) %>% 
    #   ungroup()
    
    #############
    #############
    
    #filter out the necessary variables
    MortalityTable <- MortalityTable %>% select(Age, Years, entry_age, mort) %>% 
      arrange(entry_age) 
    
    #View(MortalityTable)
    ######################
    ######################
      
    SeparationRates <- expand_grid(Age, YOS) %>% 
      mutate(entry_age = Age - YOS) %>% 
      filter(entry_age %in% SalaryEntry$entry_age) %>% 
      arrange(entry_age, Age)
    
    ### Creating column YearsFurstRetire
    SeparationRates <- SeparationRates %>% 
      mutate(first_retire = ifelse(RetirementType(Age, YOS) %in% c("Normal No Rule of 90", "Normal With Rule of 90", "Reduced"), Age, 0)) 
    
    x <- SeparationRates %>% 
      group_by(entry_age) %>%
      filter(first_retire > 0, .preserve=TRUE) %>%
      summarise(first_ret = first(first_retire))
    
    SeparationRates  <- SeparationRates %>% inner_join(x, by = "entry_age")
    SeparationRates  <- SeparationRates %>% select(-first_retire) %>% mutate(YearsFirstRetire = first_ret-Age)
    
    #Separation Rates
    SeparationRates <- SeparationRates %>% 
      left_join(TerminationRateAfter10, by = "YearsFirstRetire") %>% #Joining by new YearsFirstRetire
      left_join(TerminationRateBefore10, by = "YOS") %>% # Joining by YOS & AGE
      left_join(RetirementRates, by = c("Age")) %>%
      ### Additions ###
      mutate_all(as.numeric) %>% 
      replace(is.na(.), 0)
    
    ######################
    
    #View(SeparationRates %>% select(RetirementType(SeparationRates$Age,SeparationRates$YOS)[1]))
    
    #If you're retirement eligible, use the retirement rates, then checks YOS < 5 and use the regular termination rates
    SeparationRates <- SeparationRates %>% 
      mutate(retirement_type = RetirementType(Age,YOS),
             
             SepRateMale = ifelse(retirement_type == "Normal With Rule of 90", Rule90,
                                  ifelse(retirement_type == "Normal No Rule of 90", if(employee == "Blend"){NormalMaleBlend}
                                         else if(employee == "Teachers"){NormalMaleTeacher}
                                         else{NormalMaleGeneral},
                                         ifelse(retirement_type == "Reduced", if(employee == "Blend"){ReducedMaleBlend}
                                                else if(employee == "Teachers"){ReducedMaleTeacher}
                                                else{ReducedMaleGeneral},#Using 6 ifelse/if statements for 3 EE & 3 ret. types
                                                ifelse(YOS < 11, 
                                                       if(employee == "Blend"){TermBefore10BlendMale}
                                                       else if(employee == "Teachers"){TermBefore10TeacherMale}
                                                       else{TermBefore10GeneralMale}, 
                                                       if(employee == "Blend"){TermAfter10BlendMale}
                                                       else if(employee == "Teachers"){TermAfter10TeacherMale}
                                                       else{TermAfter10GeneralMale})))),
             SepRateFemale = ifelse(retirement_type == "Normal With Rule of 90", Rule90,
                                    ifelse(retirement_type == "Normal No Rule of 90", if(employee == "Blend"){NormalFeMaleBlend}
                                           else if(employee == "Teachers"){NormalFeMaleTeacher}
                                           else{NormalFeMaleGeneral},
                                           ifelse(retirement_type == "Reduced", if(employee == "Blend"){ReducedFeMaleBlend}
                                                  else if(employee == "Teachers"){ReducedFeMaleTeacher}
                                                  else{ReducedFeMaleGeneral},#Using 6 ifelse/if statements for 3 EE & 3 ret. types
                                                  ifelse(YOS < 11, 
                                                         if(employee == "Blend"){TermBefore10BlendFeMale}
                                                         else if(employee == "Teachers"){TermBefore10TeacherFeMale}
                                                         else{TermBefore10GeneralFeMale}, 
                                                         if(employee == "Blend"){TermAfter10BlendFeMale}
                                                         else if(employee == "Teachers"){TermAfter10TeacherFeMale}
                                                         else{TermAfter10GeneralFeMale})))),
             SepRate = ((SepRateMale+SepRateFemale)/2)) %>% 
      group_by(entry_age) %>% 
      mutate(RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
             SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
      ungroup()
    
    #Filter out unecessary values
    SeparationRates <- SeparationRates %>% select(Age, YOS, RemainingProb, SepProb)
    
    #View(SeparationRates)
    #Custom function to calculate cumulative future values
    
    ### Automate into a package ###
    account <- reactive({
    
    #colnames(SalaryGrowth)[2] <- "YOS"
    #Create a long-form table of Age and YOS and merge with salary data
    SalaryData <- expand_grid(Age, YOS) %>% 
      mutate(entry_age = Age - YOS) %>%    #Add entry age
      filter(entry_age %in% SalaryEntry$entry_age) %>% 
      arrange(entry_age) %>% 
      left_join(SalaryEntry, by = "entry_age") %>% 
      left_join(SalaryGrowthYOS, by = c("YOS")) %>%
      #left_join(SalaryGrowth, by = c("Age")) %>%
      ### Additions ###
      mutate(salary_increase = if(employee == "Blend"){salary_increase_yos_Blend}else if(employee == "Teacher"){
        salary_increase_yos_Teacher}else{salary_increase_yos_General})#Using 3 if statements for 3 EE types
    
    #############
    
    ##### Mortality Function #####
    ### Automate into a package ###
    
    #View(SurvivalRates)
    ########
    
    ## From SCRS val:
    ##Healthy retirees and beneficiaries – The gender-distinct South Carolina Retirees 2020 Mortality Tables. 
    #The rates are projected on a fully generational basis by the 80% of Scale UMP* 
    #to account for future mortality improvements and 
    #adjusted with multipliers* based on plan experience.
    
   
    
    #View(MortalityTable)
    ##### Mortality Function #####
    
    
   
    #######################################
    #################
    #################
    #################
    #View(SalaryData))
    #Calculate FAS and cumulative EE contributions
    #colnames(SalaryData)[7] <- "salary_increase"
    SalaryData <- SalaryData %>% 
      
      group_by(entry_age) %>% 
      mutate(Salary = start_sal*cumprod(1+lag(salary_increase,default = 0)),
             #Salary = pmin(Salary_gross, salary_cap),
             # IRSSalaryCap = pmin(Salary,IRSCompLimit),
             FinalAvgSalary = rollmean(lag(Salary), k = FinAvgSalaryYears, fill = NA, align = "right"),
             EEContrib = DB_EE_cont*Salary,
             DBEEBalance = cumFV(Interest, EEContrib),
             CumulativeWage = cumFV(ARR, Salary)) %>% 
      ungroup()
    
    
    #Survival Probability and Annuity Factor
    #View(MortalityTable)
    
    AnnFactorData <- AnnuityF(data = MortalityTable,
                              ColaType = "Compound")
    
    # AnnFactorData <- MortalityTable %>% 
    #   select(Age, entry_age, mort) %>%
    #   group_by(entry_age) %>% 
    #   mutate(surv = cumprod(1 - lag(mort, default = 0)),
    #          surv_DR = surv/(1+ARR)^(Age - entry_age),
    #          surv_DR_COLA = surv_DR * (1+COLA)^(Age - entry_age),
    #          AnnuityFactor = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
    #   ungroup()
    
    #View(data.frame(shift(AnnFactorData$surv_DR_COLA, n = 1:101, type = "lead")))
    
    ### Additions -> Calculating:
    ### 1. Earliest Age of Normal Retirement
    ### 2. Calculating Years between Early Retirement & Normal Retirement
    ### 3. Retirement Type
    ### 4. A benefit that begins before age 65 (or Rule of 90, if earlier) is reduced by 2/3 of one percent for each month before the earlier of age 65 or the age at which the Rule of 90 is met.
    
    #Add entry_age to AnnFactorData + keep toNormRetYears
    ########
    ReducedFactor <- expand_grid(Age, YOS) %>% 
      arrange(YOS) %>% 
      mutate(norm_retire = ifelse(RetirementType(Age, YOS) %in% c("Normal No Rule of 90", "Normal With Rule of 90"), 1, 0),
             first_retire = ifelse(RetirementType(Age, YOS) %in% c("Normal No Rule of 90", "Normal With Rule of 90", "Reduced"), Age, 0)) %>% 
      group_by(YOS) %>% 
      mutate(AgeNormRet = 120 - sum(norm_retire) + 1,     #This is the earliest age of normal retirement given the YOS
             YearsNormRet = AgeNormRet - Age,
             RetType = RetirementType(Age, YOS),
             RF = ifelse(RetType == "Reduced", 1 - (AgeRed)*YearsNormRet,#AgeRet is for Class Three EE
                         ifelse(RetType == "No", 0, 1)),
             RF = ifelse(RF <0,0,RF)) %>% 
      rename(RetirementAge = Age) %>% 
      ungroup() 
    
    x <- ReducedFactor %>% 
      group_by(YOS) %>%
      filter(first_retire > 0, .preserve=TRUE) %>%
      summarise(first_ret = first(first_retire))
    
    ReducedFactor  <- ReducedFactor %>% inner_join(x, by = "YOS")
    ReducedFactor  <- ReducedFactor %>% select(-first_retire) %>% mutate(YearsFirstRetire = first_ret-Age)
    
    #############
    BenefitsTable <- expand_grid(Age, YOS, RetirementAge) %>% 
      mutate(entry_age = Age - YOS) %>% 
      filter(entry_age %in% SalaryEntry$entry_age) %>% 
      arrange(entry_age, Age, RetirementAge) %>% 
      left_join(SalaryData, by = c("Age", "YOS", "entry_age")) %>% 
      left_join(ReducedFactor %>% select(RetirementAge, YOS, RF), by = c("RetirementAge", "YOS")) %>%
      left_join(AnnFactorData %>% select(Age, entry_age, surv_DR, AnnuityFactor), by = c("RetirementAge" = "Age", "entry_age")) %>%
      #Rename surv_DR and AF to make clear that these variables are at retirement
      rename(surv_DR_ret = surv_DR, AF_Ret = AnnuityFactor) %>% 
      #Rejoin the table to get the surv_DR for the termination age
      left_join(AnnFactorData %>% select(Age, entry_age, surv_DR), by = c("Age", "entry_age")) %>% 
      mutate(ReducedFactMult = RF*BenMult, 
             AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR,
             PensionBenefit = ReducedFactMult * FinalAvgSalary*YOS,
             PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj))
    
    #)
    
    #View(BenefitsTable)
    
    #For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
    OptimumBenefit <- BenefitsTable %>% 
      group_by(entry_age, Age) %>% 
      summarise(MaxBenefit = max(PresentValue)) %>%
      mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
      ungroup()
    
    ####### Benefit Accrual & Normal Cost #######
    #### Real Pension Wealth = Pension Wealth adjusted for inflation
    #### Actuarial PV of Pension Wealth = Pension Wealth 
    #Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
    #####################################
    SalaryData <- SalaryData %>% 
      left_join(OptimumBenefit, by = c("Age", "entry_age")) %>% 
      left_join(SeparationRates, by = c("Age", "YOS")) %>%
      mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),        #Members are assumed to elect the option with the greatest PV between a refund with interest and a deferred benefit
             RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
             PVPenWealth = PenWealth/(1 + ARR)^YOS * SepProb,
             PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb)
    
    #Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
    # SalaryData <- SalaryData %>% 
    #   left_join(OptimumBenefit, by = c("Age", "entry_age")) %>% 
    #   left_join(SeparationRates, by = c("Age", "YOS")) %>%
    #   mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),
    #          PVPenWealth = PenWealth/(1 + ARR)^YOS * SepProb,
    #          PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb) 
    
    
    SalaryData <- SalaryData %>% filter(entry_age == input$e.age & Age < 81)
    ####### DC Account Balance 
  
    SalaryData2 <- SalaryData %>% 
      #filter(entry_age == HiringAge) %>% 
      select(Age, YOS, entry_age, start_sal, salary_increase, Salary, RemainingProb) %>% 
      mutate(DC_EEContrib = Salary * DC_EE_cont,
             DC_ERContrib = Salary * DC_ER_cont,
             DC_Contrib = DC_EEContrib + DC_ERContrib,
             DC_balance = cumFV(DC_return, DC_Contrib),
             RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
      left_join(SalaryData %>% select(Age, YOS, RealPenWealth), by = c("Age", "YOS")) %>% 
      mutate(RealHybridWealth = RealDC_balance + RealPenWealth)
    

    ######### Graphing SINGLE ENTRY AGE + RETENTION
    
    colnames(SalaryData2)[13] <- "PVPenWealth"
    e.age <- unique(SalaryData2$entry_age)
    SalaryData2 <- data.frame(SalaryData2)
    SalaryData2$entry_age <- as.numeric(SalaryData2$entry_age)
    # #View(SalaryData2)
    
    SalaryData2
    })
    
    SalaryData2 <- data.table(account())
    
    
    SalaryData2$PVPenWealth <- as.numeric(SalaryData2$PVPenWealth)
    y_max <- max(SalaryData2$PVPenWealth)
    
    SalaryData2 <- data.frame(SalaryData2)
    

    #View(SalaryData2$DC_balance)
    
    ####
    pwealth <- ggplot(SalaryData2, aes(Age,PVPenWealth/1000))+
      geom_line(aes(group = 1,
                    text = paste0("Age: ", Age,
                                  "<br>DB Pension Wealth: $",round(PVPenWealth/1000,1), " Thousands")),size = 1.25, color = palette_reason$SatBlue)+
      geom_line(aes(Age, RealDC_balance/1000,
                    group = 2,
                    text = paste0("Age: ", Age,
                                  "<br>DC Wealth at ", DC_return*100,"% : $", round(RealDC_balance/1000,1), " Thousands")), size = 1, color = palette_reason$Orange)+
      geom_line(aes(Age, RemainingProb* (y_max/1000),
                    group = 3,
                    text = paste0("Age: ", Age,
                                  "<br>Members Remaining: ", round(RemainingProb*100,1), "%")), size = 1, color = palette_reason$LightBlue, linetype = "dashed")+
      scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x),
                         name = paste0("Age (Entry age at ", input$e.age,")"), expand = c(0,0)) +
      
      scale_y_continuous(breaks = seq(0, y_max/1000, by = 100),limits = c(0, y_max/1000*1.1), labels = function(x) paste0("$",x),
                         sec.axis = ggplot2::sec_axis(~./(y_max/100), 
                                                      breaks = scales::pretty_breaks(n = 10), name = "Percent of Members Remaining",
                                                      labels = function(b) paste0(round(b, 0), "%")),
                         name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) +
      theme_bw()+
      theme(   #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = margin(0.5, 0.5,0.5,0.5, "cm"),
        axis.text.y = element_text(size=11, color = "black"),
        axis.text.y.right = element_text(size=11, color = "black"),
        axis.text.y.left = element_text(size=11, color = "black"),
        axis.text.x = element_text(size=11, color = "black"),
        legend.title = element_text(size = 9, colour = "black", face = "bold"))
    
    
    ax2 <- list(
      overlaying = "y",
      side = "right",
      showticklabels = TRUE,
      range = c(0,110),
      title = "Percent of Employees Remaining (%)",
      automargin = T,
      showgrid = FALSE,
      titlefont = list(size = 15),
      tickvals = seq(0, 110, by = 25),
      tickfont = list(size = 15)) # I added this line
    
    library(plotly)
    ggplotly(pwealth, tooltip = c("text")) %>%
      add_markers(yaxis = "y2") %>% # new line
      layout(
        yaxis2 = ax2)
    
  })
  # #Calculate normal cost rate for each entry age
  # NormalCost <- SalaryData %>% 
  #   group_by(entry_age) %>% 
  #   summarise(normal_cost = sum(PVPenWealth, na.rm=T)/sum(PVCumWage, na.rm=T)) %>% 
  #   ungroup()
  # 
  # #View(NormalCost)
  # 
  # #Calculate the aggregate normal cost
  # NC_aggregate <- sum(NormalCost$normal_cost * SalaryEntry$start_sal * SalaryEntry$count_start, na.rm=T)/
  #   sum(SalaryEntry$start_sal * SalaryEntry$count_start, na.rm=T)
  # 
  # #Calculate the aggregate normal cost
  # NC_aggregate
  # 
  # ########################
  # ########################
  # library(parallel)
  # n.cores <- detectCores()
  # n.cores
  # 
  # x.list <- sapply(1:10000, list)
  # 
  # x.list
  # system.time({
  #   cluster <- makeCluster(n.cores)
  #   parLapply(cluster, x.list, function(x){sample(1:1000, 2000, replace = T)})}
  # )
  # 
  # system.time(
  #   
  #   lapply(x.list, function(x){sample(1:1000, 2000, replace = T)})
  # )
  # #########
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)
################################