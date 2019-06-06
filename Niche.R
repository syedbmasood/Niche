
####################################################################################################################
####################################################################################################################

rm(list=ls())
gc()
options(scipen=999)

####################################################################################################################
####################################################################################################################

library(data.table)
library(openxlsx)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(onewaytests)
library(ggplotly)

####################################################################################################################
#                                             FUNCTIONS
####################################################################################################################
convert_tolower <- function(y)
{
  
  cols = which(grepl("lit|service|service_group|sha|ons_cluster|age_group|cost_group", names(y))==T)
  
  y[, c(cols) := lapply(.SD, function(x){tolower(str_trim(x))}), .SDcols = cols]
  
  return(y)
  
}


extract_mental_health_service_ids <- function(x)
{
  # FILTER SERVICE GROUPS FOR MENTAL HEALTH
  df = unique(x$service)
  ids = which(str_detect(df, "(?i)\\b\\w*MENTAL\\w*\\b")==TRUE)
  mental_services = df[ids]
  return(mental_services)
  
}

normality_test <- function(x)
{
  
  return(shapiro.test(x))
  
}

read_data <- function()
{

  # READ FILES
  investment = data.table(read.xlsx("Niche Data Analyst Test March 2019 - Source Data.xlsx", sheet=1))
  areas = data.table(read.xlsx("Niche Data Analyst Test March 2019 - Source Data.xlsx", sheet=2))
  service = data.table(read.xlsx("Niche Data Analyst Test March 2019 - Source Data.xlsx", sheet=3))
  
  ####################################################################################################################
  ####################################################################################################################
  # RENAME COLUMNS
  
  names(investment) = c("lit","service","outturn")
  names(areas) = c("lit","weighted_pop_18_64","weighted_pop_65+","sha","ons_cluster")
  names(service) = c("service","age_group","cost_group","service_group")
  
  ####################################################################################################################
  ####################################################################################################################
  # REMOVE ANY DUPLICATED RECORDS
  
  investment = investment[!duplicated(investment)]
  areas = areas[!duplicated(areas)]
  service = service[!duplicated(service)]
  
  # CONVERT ALL TEXT COLUMNS TO LOWER CASE
  investment = convert_tolower(investment)
  areas = convert_tolower(areas)
  service = convert_tolower(service)
  
  data = list(investment, areas, service)
  
  return(data)
}
####################################################################################################################
# CHANGE DIRECTORY
setwd("C:/Users/Syed/Desktop/Niche")
####################################################################################################################

#                               TASK 1 - COMPARE BLACKPOOL TO STATISTICAL NEIGHBOURS

####################################################################################################################
# EXTRACT DATA FOR SELECTED LIT's 
task1 <- function()
{
  
  df = list()
  df = read_data()
  
  investment = df[[1]]
  areas = df[[2]]
  service = df[[3]]
  
  county_names = tolower(c("Blackpool", "Plymouth", "Sefton", "Wirral", "North Lincolnshire", "Redcar and Cleveland", "North Tyneside"))
  df = investment[lit%in%county_names]
  df = df[order(lit)]

  # SUBSET DATA FOR MENTAL HEALTH SERVICES ONLY AND SUM INVESTMENT FOR SIMILAR SERVICES
  mental_services = extract_mental_health_service_ids(df)
  df = df[service%in%mental_services]
  
  # SUM PLANNED OUTTURN BY LIT AND SERVICE TYPE
  df = df[, lapply(.SD, sum), by=c("lit","service")]
  
  # MERGE DATA
  df1 = merge(df,areas, by = "lit", all.x = T)
  df = merge(df1, service, by="service", all.x = T)

  #APPLY FINAL FILTER TO FETCH ROWS FOR ADULTS ONLY AGE GROUP
  df = df[age_group=="adults"]
  df = df[order(lit)]
  
  
  # CALCULATE TOTAL POPULATION BY ADDING AGE GROUPS
  df[, total_population := sum(weighted_pop_18_64, `weighted_pop_65+`), by=c("lit", "service")]
  cols_to_omit = c("cost_group", "age_group", "weighted_pop_18_64","weighted_pop_65+")
  df[,c(cols_to_omit):=NULL] # SET COLUMNS TO NULL
  
  
  # CALCULATE SPEND/INVESTMENT FOR EACH WEIGHTED ADULT PER LIT PER SERVICE TYPE. UNIT IN £'S
  df[, spend_per_adult := (outturn*1000)/total_population] # CONVERT TO ACTUAL NUMBERS
  df[, total_spend_per_lit:= sum(outturn), by = (lit)] 
  
  ####################################################################################################################
  #                                           PLOTS
  ####################################################################################################################
  
  plot1 = df %>% group_by(lit) %>% summarize(total_spend = sum(outturn))
  p = ggplot(plot1)+geom_bar(aes(lit, total_spend/1000, fill=lit), stat="identity")+
    ggtitle("Mental Health Spend - Blackpool vs Statistical Neighbours")+
    theme(legend.position ="none")+xlab("LIT Name")+ylab("Total Spend in £000's")
  
  p <- ggplotly(p)
  
  
  plot2 = df %>% group_by(lit, service) %>% summarise(percent_spend_per_service = (sum(outturn)/total_spend_per_lit)) 
  p1 = ggplot(plot2)+geom_bar(aes(lit, percent_spend_per_service, fill = lit), stat="identity")+facet_wrap(~service)+
    theme(axis.text.x = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))+ggtitle("Percentage Spend Per Service Across Lit's")+
    ylab("Percentage Spend")+xlab("")
  p1 <- ggplotly(p1)
  
  
  p2 = ggplot(df)+geom_bar(aes(lit, spend_per_adult, fill = lit), stat = "identity")+
    facet_wrap(~service)+theme(axis.text.x = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
    ggtitle("Amount of Spend Per Weighted Adult/Service/Lit")+
    ylab("Spend Amount is £000's")+xlab("")
  p2 = ggplotly(p2)
  
  
  p3 = ggplot(plot2)+geom_bar(aes(service, percent_spend_per_service, fill = service), stat="identity")+facet_wrap(~lit)+
    theme(axis.text.x = element_blank())+
    ggtitle("Percentage Spend on Services in LIT's")+
    ylab("")+xlab("")
  p3 <- ggplotly(p3)
  
  
  plot4 = df %>% group_by(lit) %>% summarize(total_spend_perAdult_perLIT = sum(spend_per_adult))
  p4 = ggplot(plot4)+geom_bar(aes(lit, total_spend_perAdult_perLIT, fill = lit), stat="Identity")+
    theme(legend.position ="none")+
    ggtitle("Mental Health - Overall Investment Per Weighted Adult Across Lit's")+
    ylab("Spend in £000's")
  p4 = ggplotly(p4)
}


####################################################################################################################
####################################################################################################################

task2 <- function()
{
  df = list()
  df = read_data()
  
  investment = df[[1]]
  areas = df[[2]]
  service = df[[3]]
  
  # MERGE DATA
  df1 = merge(investment,areas, by = "lit", all.x = T)
  df = merge(df1, service, by="service", all.x = T)
  setDT(df)
  
  # SUBSET FOR MENTAL HEALTH SERVICES ONLY
  mental_services = extract_mental_health_service_ids(investment)
  df = df[service%in%mental_services] 
  
  #APPLY FINAL FILTER TO FETCH ROWS FOR ADULTS ONLY AGE GROUP
  df = df[age_group=="adults"]
  
  # DRAW DISTRIBUTIONS
  x = ggplot(df, aes(x = outturn))+geom_histogram(aes(y = ..density..), bins = 40)+
    geom_density(alpha=.2, fill="#FF6666")+
    ggtitle("Planned Investment Distribution")+
    xlab("Investment in 000's")+
    theme(legend.position = "None", plot.title = element_text(hjust = 0.5))
    
  y = ggplot(df, aes(x = log(outturn)))+geom_histogram(aes(y = ..density..), bins = 40)+
    geom_density(alpha=.2, fill="#FF6689")+
    ggtitle("Planned Investment Distribution")+
    xlab("Log of Investment")+
    theme(legend.position = "None", plot.title = element_text(hjust = 0.5))
  
  
  grid.arrange(x, y, nrow=1)
  
  
  # PROCESS DATA
  data = df%>%group_by(sha, lit) %>% 
    summarize(total_pop = sum(weighted_pop_18_64, `weighted_pop_65+`), total_spend = sum(outturn), 
              total_spend_per_head = (total_spend/total_pop)*1000)
  setDT(data)
  data[, area_type := ifelse(sha=="nhs london", "london", "other_areas")]
  data[, sha := str_replace(sha, "nhs","")]
  
  # PLOTS 
  draw_plots <- function(data)
  {
    
    # Draw Distributions
    x1 = data %>% filter(area_type=="london") %>%
    ggplot(aes(x = total_spend_per_head))+geom_histogram(aes(y = ..density..), bins = 40)+
      geom_density(alpha=.2, fill="#FF6666")+
      ggtitle("Planned Investment Distribution - London")+
      xlab("Investment in £'s")+
      theme(legend.position = "None", plot.title = element_text(hjust = 0.5))
  
    x2 = data %>% filter(area_type=="london") %>%
    ggplot(aes(x = log(total_spend_per_head)))+geom_histogram(aes(y = ..density..), bins = 40)+
      geom_density(alpha=.2, fill="#FF6666")+
      ggtitle("Planned Investment Distribution - London")+
      xlab("Log Investment")+
      theme(legend.position = "None", plot.title = element_text(hjust = 0.5))
  
    y1 = data %>% filter(area_type=="other_areas") %>%
    ggplot(aes(x = total_spend_per_head))+geom_histogram(aes(y = ..density..), bins = 40)+
      geom_density(alpha=.2, fill="#FF6689")+
      ggtitle("Planned Investment Distribution - Other Areas")+
      xlab("Investment in £'s")+
      theme(legend.position = "None", plot.title = element_text(hjust = 0.5))
  
    y2 = data %>% filter(area_type=="other_areas") %>%
    ggplot(aes(x = log(total_spend_per_head)))+geom_histogram(aes(y = ..density..), bins = 40)+
      geom_density(alpha=.2, fill="#FF6689")+
      ggtitle("Planned Investment Distribution - Other Areas")+
      xlab("Log Investment")+
      theme(legend.position = "None", plot.title = element_text(hjust = 0.5))
  
    grid.arrange(x1, x2, y1, y2, nrow=2, ncol=2)
  
  # Boxplot for total spend per head across sha's
  ggplot(data)+geom_boxplot(aes(sha, total_spend_per_head, fill = sha))+
    ggtitle("Total Spend Per Adult Across SHA's")+
    theme(legend.position = "None", plot.title = element_text(hjust = 0.5))+
    xlab("Strategic Health Areas")+
    ylab("Total Spend Per Head in £'s")
  
  
  }
  
  
  draw_plots(data)
  
  london = data %>% filter(area_type=="london") %>% select(total_spend_per_head)
  other_areas = data %>% filter(area_type=="other_areas") %>% select(total_spend_per_head)
  
  
  
  # TEST FOR NORMALITY
  print(normality_test(data$total_spend_per_head))
  print(normality_test((london$total_spend_per_head)))
  print(normality_test((other_areas$total_spend_per_head)))
  
  # TEST FOR NORMALITY ON LOGGED INVESTMENT PER HEAD
  normality_test(log(london$total_spend_per_head))
  normality_test(log(other_areas$total_spend_per_head))
  
  # TEST FOR EQUAL VARIANCES
  var.test(london$total_spend_per_head, other_areas$total_spend_per_head)
  
  
  # TAKE LOG OF TOTAL SPEND
  data[, total_spend_per_head := log(total_spend_per_head)]
  
  # CONDUCT WELCH'S T-TEST
  result = t.test(data$total_spend_per_head~data$area_type, alternative="greater")
  print(result)
  
  
  # VALIDATE RESULTS
  df = data %>% group_by(area_type) %>% summarize(group_means = mean(total_spend_per_head))
  setDT(df)
  df[, group_means := exp(group_means)]
  print(df)
  
  
}


####################################################################################################################
####################################################################################################################
