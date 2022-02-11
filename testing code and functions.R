library(tidyverse)

#Received, FirstBooked and LastBooked?

x = c(NA, 1, 2, 3, NA, 0)
y = c(3, NA, 8, 3, NA, NA)

pmax(x, y, na.rm = T)

dim(jobsMaster)
jobsMaster %>% mutate( DateTest = pmax(JobReceivedDate, FirstBookedDate, LastBookedDate, LatestBookedDatetime, na.rm = T ))




jobsMaster %>% select( JobStatus) %>% group_by(JobStatus) %>% summarize(n = n())


# Check overlap of different JobStatus JobId.

jobStatus = "Pending"
jobsMaster %>% filter(get("JobStatus") == jobStatus) %>% dim()

Duplicate = jobsMaster %>% filter(get("JobStatus") == jobStatus) %>% select(c(get("JobId"), get("JobStatus")))

Duplicate = jobsMaster %>% filter(get("JobStatus") == jobStatus) %>% select( c("JobId", "JobStatus") )
Duplicate %>% dim()
head(Duplicate)

filter_cust_fun <- function(jobStatus) {
  jobsMaster %>% filter(get("JobStatus") == jobStatus) %>% select( "JobId" )
}

duplicateJobIds = filter_cust_fun("Duplicate")
pendingJobIds = filter_cust_fun("Pending")


intersect(duplicateJobIds, pendingJobIds)

find_common_ids <- function(jobStatus1, jobStatus2) {
  
  filter_cust_fun <- function(jobStatus) {
    jobsMaster %>% filter(get("JobStatus") == jobStatus) %>% pull( "JobId" )
  }
  
  df1 = filter_cust_fun(jobStatus1)
  df2 = filter_cust_fun(jobStatus2)
  
  intersect(df1, df2)
}

find_common_ids("Pending", "Duplicate")

find_common_ids("Pending", "Completed")

find_common_ids("Cancelled", "Completed")

find_common_ids("Completed", "Duplicate")

find_common_ids("Cancelled", "Duplicate")

find_common_ids("Pending", "Completed")

find_common_ids("Duplicate", "Completed")

find_common_ids("Pending", "Cancelled")

find_common_ids("Completed", "Cancelled")

intersect(1:10, 9:14)

find_common_ids("Completed", "New")

find_common_ids("Cancelled", "New")

find_common_ids("Unassigned", "New")

# Do a manual check:

(jobsMaster %>% filter(JobStatus == "New") %>% pull(JobId)) %in% (jobsMaster %>% filter(JobStatus == "Unassigned") %>% pull(JobId))

# this works
dfCancelled = 
  jobsMaster %>% 
  mutate( CancelledAfterTechnAccepted = ifelse(JobStatus != "Cancelled", NA, ifelse(AnyOffersAccepted == TRUE, 1, 0)) ) 

# Not working.
dfCancelled = 
  jobsMaster %>% 
  mutate( CancelledAfterTechnAccepted = case_when(JobStatus == "Cancelled" & AnyOffersAccepted == TRUE  ~ TRUE, 
                                                  JobStatus == "Cancelled" & AnyOffersAccepted == FALSE ~ FALSE,
                                                  TRUE ~ NA) )

colnames(jobsMaster)
colnames(dfCancelled)
dfCancelled %>% select(c("JobId", "JobStatus", "AnyOffersAccepted", "CancelledAfterTechnAccepted"))

dfComplaintReasonCode = 
  jobsMaster %>% mutate(ComplaintReason = case_when(ComplaintReasonCode == "Overage Dispute" ~ "Overage(s) Dispute"
                                                                          , ComplaintReasonCode == "Overages Dispute" ~ "Overage(s) Dispute"
                                                                          , TRUE ~ ComplaintReasonCode))
jobsMaster %>% select(ComplaintReasonCode) %>% group_by(ComplaintReasonCode) %>%  summarize(n=n())
dfComplaintReasonCode %>% select(ComplaintReason) %>%  group_by(ComplaintReason) %>% summarize(n=n())
dfComplaintReasonCode %>% select(ComplaintReason) %>%  group_by(ComplaintReason) %>% summarize(n=n())


dfRedFlagStatus = jobsMaster %>% mutate(RedFlagStatus = case_when(RedFlagStatus == "Not Ack"         ~ "Not Acknowledged"
                                                                   , RedFlagStatus == "Customer Missed" ~ "Customer Appointment Missed"
                                                                   , TRUE ~ RedFlagStatus))

jobsMaster %>% select(RedFlagStatus) %>% group_by(RedFlagStatus) %>% summarize(n=n())
jobsMaster %>% distinct(RedFlagStatus)
dfRedFlagStatus %>% distinct(RedFlagStatus)




# ISA project
ourNamesInd = read_csv("ColumnNameLookupInd.csv")
dd_raw = read_csv("Individual Survey.csv", col_names = ourNamesInd$InternalName, skip=3, na = c(""," ", "N / A"))

sensorA = c(1, 91, 6, 32, NA, NA)
sensorB = c(1, NA, 6, NA, NA, NA)
sensorC = c(1, 12, 6, 31, 20, NA)
coalesce(sensorA, sensorB, sensorC)


# ) %>% mutate_at(vars(contains("NumPubs")), list(~case_when(.==0.1 ~ 0  
#                                                            , TRUE ~ .x ) # This .x does not work. Tried simply "." also did not work. 


table(dd$Country)
table(dd[[2]])
table(dd[[3]])


table_plus <- function(n) {
  print(colnames(dd)[n-1])
  print(table(dd[[n-1]]))
  print(colnames(dd)[n-1])
}

table_plus(3) # Country... Done


fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", toupper)
str_replace_all(fruits, "b", NA_character_)

pretty_strings <- function(string) {
  
  blankCount = str_count(stringColumn, pattern = " ")
  
  # If only one space, replace with \n
  if(blankCount == 1){
    str_replace(stringColumn, " ", "\n")
    
  } else if (blankCount == 2) { # if 2 spaces, put beside longest word
    
    
  } else if (blankCount == 3) { # If 3 spaces, put after 2nd one
    
    
  } else if (blankCount > 3) { # If 4 or more, put every 2nd space
    
    
  }
  
}


dd = readRDS("ISA_Raw_Ind.rds")
up = dd %>% select(PeopleInfluenced) %>%  str_split(pattern = ";")

str_split(dd$PeopleInfluenced, pattern = ";")


de_coalesce <- function(column) {
  
}

dd %>% pull(PersonalEngagement) %>% unique() %>% str_split(pattern = ";", simplify = TRUE) %>% as.vector() %>% unique() #%>% str_split_fixed(pattern = ";", n = 5)

dd %>% pull(PersonalEngagement) %>% str_split(pattern = ";", simplify = TRUE)  %>%  as.vector() %>% unique() #%>% str_split_fixed(pattern = ";", n = 5)

dd %>% pull(PersonalEngagement) %>% str_split(pattern = ";", simplify = TRUE)

dd %>% select(PersonalEngagement) %>% separate(col="PersonalEngagement", into = paste0("PersonalEngagement", 1:3), sep = ";", fill = "right" ) %>% 
  mutate(EconomicOpportunity = case_when(PersonalEngagement1 == "Economic opportunity"  ))


dd %>% select(PersonalEngagement) %>% separate(col="PersonalEngagement", into = paste0("PersonalEngagement", 1:3), sep = ";", fill = "right" )

dd %>% 
  select(PersonalEngagement) %>% 
  mutate(EconomicOpportunity = case_when(str_detect(PersonalEngagement, pattern = "Economic opportunity") ~ 1
                                         , TRUE ~ 0)
  ) %>% str()

toto = dd %>% 
  select(PersonalEngagement) %>% 
  mutate(EconomicOpportunity = case_when(str_detect(PersonalEngagement, pattern = "Economic opportunity") ~ 1
                                         , is.na(PersonalEngagement) ~ NA_real_
                                         , TRUE ~ 0)
         
         , Conservation = case_when(str_detect(PersonalEngagement, pattern = "Conservation") ~ 1
                                    , is.na(PersonalEngagement) ~ NA_real_
                                    , TRUE ~ 0)
         
         , CapacityDevelopment = case_when(str_detect(PersonalEngagement, pattern = "Capacity development") ~ 1
                                           , is.na(PersonalEngagement) ~ NA_real_
                                           , TRUE ~ 0)
  )


dd %>% 
  select(PersonalEngagement) %>% 
  mutate(EconomicOpportunity = case_when(is.na(PersonalEngagement) ~ PersonalEngagement
                                         , str_detect(PersonalEngagement, pattern = "Economic opportunity") ~ 1
                                         , TRUE ~ 0)
  )

print(dd %>% select(PersonalEngagement) %>% 
        extract(col = PersonalEngagement, into = c("Economic opportunity", "Conservation", "Capacity development"), 
                regex = "E(.*)Cons(.*)Ca(.*)")
)

dd %>% select(PersonalEngagement) %>% 
  extract(col = PersonalEngagement, into = c("Economic opportunity", "Conservation", "Capacity development"), 
          regex = "Economic(Economic.*)Conservation(Conservation.*)Capacity(Capacity.*)")

# Count total number of responses per column
dd %>% summarise(across(everything(), ~ sum(!is.na(.)))) %>%  t() %>% write.csv(file = "n_per_column.csv")
dd %>% summarise(across(everything(), ~ sum(!is.na(.)))) %>%  pivot_longer() # Does not work. # Find working Tidyverse solution.





# de_coalesce_col()
sep_col <- function(dfr, colName = "PersonalEngagement") {
  
  dfr = dfr %>%
    pull(colName) %>%
    str_split(pattern = ";", simplify = TRUE) %>%
    as.data.frame() %>%
    mutate(ID = row_number()) %>%
    pivot_longer(starts_with("V")) %>%
    filter(value != "") %>%
    pivot_wider(id_cols = ID, names_from = value) %>% 
    select(-ID)
  
  # Correctly renaming column names should be a separate function.
  sepColumnNames = dfr %>% colnames() %>%  str_to_title() %>% str_replace_all(pattern = " ", replacement = "")
  colnames(dfr) = sepColumnNames
  
  dfr
}

sep_col <- function(dfr, colName = "PersonalEngagement") {
  
  new_columns_as_dataframe = dfr %>%
    pull(colName) %>%
    str_split(pattern = ";", simplify = TRUE) %>%
    as.data.frame() %>%
    mutate(ID = row_number()) %>%
    pivot_longer(starts_with("V")) %>%
    filter(value != "" | is.na(value)) %>%
    pivot_wider(id_cols = ID, names_from = value) %>% 
    select(-c("ID", "NA"))
  
  sepColumnNames = new_columns_as_dataframe %>% colnames() %>%  str_to_title() %>% str_replace_all(pattern = " ", replacement = "")
  colnames(new_columns_as_dataframe) = sepColumnNames
  
  dfr = bind_cols(dfr, new_columns_as_dataframe)
  dfr
}

# Version 2 for sep_col() function. Also, I think better name is de_coalesce_col()

sep_col <- function(dfr, colName = "PersonalEngagement") {
  
  new_columns_as_dataframe = dfr %>%
    pull(colName) %>%
    str_split(pattern = ";", simplify = TRUE) %>%
    as.data.frame() %>%
    mutate(ID = row_number()) %>%
    pivot_longer(starts_with("V")) %>%
    filter(value != "" | is.na(value)) %>%
    pivot_wider(id_cols = ID, names_from = value) %>% 
    select(-c("ID", "NA")) %>% 
    mutate(across(everything(), ~replace(., !is.na(.), 1))) %>% # using replace() & replace_na() instead of ifelse() or if_else() due to speed.
    mutate(across(everything(), .fns = ~replace_na(.,0))) 
  
  
  sepColumnNames = new_columns_as_dataframe %>% colnames() %>%  str_to_title() %>% str_replace_all(pattern = " ", replacement = "") %>% paste(colName, ., sep = "_")
  colnames(new_columns_as_dataframe) = sepColumnNames
  
  dfr = bind_cols(dfr, new_columns_as_dataframe)
  dfr
}

## Version 3 of sep_col() function. 

sep_col <- function(dfr, colName = "PersonalEngagement") {
  
  new_columns_as_dataframe = dfr %>%
    pull(colName) %>%
    str_split(pattern = ";", simplify = TRUE) %>%
    as.data.frame() %>%
    mutate(ID = row_number()) %>%
    pivot_longer(starts_with("V")) %>%
    filter(value != "" | is.na(value)) %>%
    pivot_wider(id_cols = ID, names_from = value) %>% 
    select(-c("ID", "NA")) %>% 
    mutate(across(everything(), ~replace(., !is.na(.), 1))) %>% # using replace() & replace_na() instead of ifelse() or if_else() due to speed.
    mutate(across(everything(), .fns = ~replace_na(.,0))) 
  
  sepColumnNames = new_columns_as_dataframe %>% # Using Tyler the Great's naming convention. 
    colnames() %>%  str_to_title() %>% str_replace_all(pattern = " ", replacement = "_") %>% 
    paste(str_to_upper(colName), ., sep = "_")
  
  colnames(new_columns_as_dataframe) = sepColumnNames
  
  dfr = bind_cols(dfr, new_columns_as_dataframe)
  dfr
}



### Checking how replace_na() works

# Replace NAs in a data frame
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% replace_na(list(x = 0, y = "unknown"))

# Replace NAs in a vector
df %>% dplyr::mutate(x = replace_na(x, 0))
# OR
df$x %>% replace_na(0)
df$y %>% replace_na("unknown")

# Replace NULLs in a list: NULLs are the list-col equivalent of NAs
df_list <- tibble(z = list(1:5, NULL, 10:20))
df_list %>% replace_na(list(z = list(5)))




## Renaming data columns

iris <- as_tibble(iris) # so it prints a little nicer
rename(iris, petal_length = Petal.Length)

rename_with(iris, toupper)
rename_with(iris, toupper, starts_with("Petal"))
rename_with(iris, ~ tolower(gsub(".", "_", .x, fixed = TRUE)))



## Function pretty_strings() version 1, does not work correctly for columns or dataframe
pretty_strings <- function(string) {
  
  pretty_new_lines = function(x) gsub("([^ ]+ [^ ]+) ", "\\1\n", x)
  
  blankCount = str_count(string, pattern = " ")
  
  # If only one space, replace with \n
  if (blankCount == 1) {
    string = str_replace(string, " ", "\n")
    
  } else if (blankCount == 2) { # if 2 spaces, put beside longest word
    string = pretty_new_lines(string)
    
  } else if (blankCount == 3) { # If 3 spaces, put after 2nd one
    string = pretty_new_lines(string)
    
  } else if (blankCount > 3) { # If 4 or more, put every 2nd space
    string = pretty_new_lines(string)
    
  }
  return(string)
}


## Function pretty_strings() Version 2 properly working by Professor.


pretty_strings <- function(string) {
  # string = "Marine Engineering and Technology" # Testing
  pretty_new_lines = function(x) gsub("([^ ]+ [^ ]+) ", "\\1\n", x)
  
  blankCount = str_count(string, pattern = " ")
  
  for(i in 1:length(blankCount)){
    thisBlankCount = blankCount[i]
    # If only one space, replace with \n
    if (thisBlankCount == 1) {
      string[i] = str_replace(string[i], " ", "\n")
      
    } else if (thisBlankCount == 2) { # if 2 spaces, put beside longest word
      string[i] = pretty_new_lines(string[i])
      
    } else if (thisBlankCount == 3) { # If 3 spaces, put after 2nd one
      string[i] = pretty_new_lines(string[i])
      
    } else if (thisBlankCount > 3) { # If 4 or more, put every 2nd space
      string[i] = pretty_new_lines(string[i])
    }
    
    
  }
  return(string)
}



#######################################################################

dd %>% select(contains("Grant"))
