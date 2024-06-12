## ----setup, include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)


## ----libraries------------------------------------------------------------------
if(!require(haven)) install.packages('haven', repos = 'http://cran.us.r-project.org')
if(!require(tidyverse)) install.packages('tidyverse', repos = 'http://cran.us.r-project.org')
if(!require(caret)) install.packages('caret', repos = 'http://cran.us.r-project.org')
if(!require(ggrepel)) install.packages('ggrepel', repos = 'http://cran.us.r-project.org')
if(!require(doParallel)) install.packages('doParallel', repos = 'http://cran.us.r-project.org')
if(!require(rsample)) install.packages('kableExtra', repos = 'http://cran.us.r-project.org')
if(!require(ConfusionTableR)) install.packages('ConfusionTableR', repos = 'http://cran.us.r-project.org')
if(!require(kableExtra)) install.packages('kableExtra', repos = 'http://cran.us.r-project.org')
if(!require(readxl)) install.packages('readxl', repos = 'http://cran.us.r-project.org')
if(!require(pander)) install.packages('pander', repos = 'http://cran.us.r-project.org')


## ----theme_setting--------------------------------------------------------------
# set theme for ggplot
theme_set(theme_minimal() + 
            theme(legend.position = 'bottom',
                  panel.grid = element_blank(),
                  text = element_text(family = 'serif'),
                  panel.border = element_rect(color = 'grey66', 
                                               fill = NA, 
                                          linewidth = .5),
          plot.title = element_text(size = 11), 
          plot.subtitle = element_text(size = 9))) 


## ----archigos-------------------------------------------------------------------
# load first database, archigos
archigos <- 
  read_stata('Archigos_4.1_stata14.dta',
             encoding = 'latin1',
             col_select = NULL,
             skip = 0,
             n_max = Inf,
             .name_repair = 'unique')
# transform some predictors
archigos <- archigos |> 
  mutate(
    startdate = ymd(startdate),
    enddate = ymd(enddate),
# create a feature (Years in office) for the approximate number of         
# years leader holds power
    `Years in office` = year(enddate) - year(startdate))
# approximate number of years Fidel Castro held power
years_castro <- archigos |>
  select(`Years in office`, leader, idacr) |> 
  filter(leader == 'Castro' & idacr == 'CUB')

archigos <- archigos |> 
  mutate(fties = ifelse(fties =='NA', 'No', 'Yes'))

# select the most relevant potential variables. For details 
# on the predictors below, please refer to Annex II
archigos <- archigos |> 
  select(obsid, yrborn, leader, startdate, enddate, 
         entry, exit, exitcode, prevtimesinoffice, gender, 
         numentry, numposttenurefate, fties, `Years in office`) 
# glimpse(archigos)


## ----chisols, eval = T----------------------------------------------------------
chisols <- read_csv('CHISOLSll5_0.csv')

# rename 'archigosobsid' (observation ID) to 'obsid' to 
# facilitate dataset joining
chisols <- chisols |> 
  rename(obsid = archigosobsid) |> 
  mutate(year = as.integer(str_extract(obsid, '\\d{4}')),
  idacr = str_sub(obsid, 1, 3)) 

chisols <- chisols |>
  mutate(year = as.integer(str_extract(obsid, '\\d{4}')))

# join archigos and chisols

chisols <- chisols |> 
  select(-leader) # removed to avoid duplicate
archigos_chisols <- left_join(chisols, archigos, by = 'obsid') 

# select some relevant predictors
archigos_chisols <- archigos_chisols |> 
  select(obsid, year, leader, yrborn, startdate, entry, exit, exitcode,
prevtimesinoffice, gender, fties, statename, leaderpos, 
affiliation, `Years in office`) |> 
  mutate(country_abb = str_sub(obsid, 1, 3))

# glimpse(archigos_chisols)
archigos_chisols <- archigos_chisols |> 
  filter(year <= 2015) # max year


## ----function_for_filling_missing_years, eval = T-------------------------------
# function for filling missing years within each country ('statename')
fill_missing_years <- function(data, year_column, group_column) {
  # extract the numeric year part from the year_column
  data <- data  |> 
    mutate(numeric_year = as.numeric(sub('-.*', '', .data[[year_column]])))
  # generate a complete dataset with all possible combinations of statename and years
  complete_data <- expand.grid(
    country_abb = unique(data[[group_column]]),
    year = seq(min(data$numeric_year, na.rm = TRUE), max(data$numeric_year, na.rm = TRUE))
  ) |> 
    mutate(year = as.character(year))  # Ensure the year is character type for merging
  # generate all possible year variations based on existing suffixes
  unique_suffixes <- unique(sub('^[^-]*', '', data[[year_column]]))
  complete_data_expanded <- complete_data |> 
    crossing(suffix = unique_suffixes) |> 
    mutate(year = paste0(year, suffix)) |> 
    select(-suffix)
# merge the complete dataset with the original data
  merged_data <- merge(
    complete_data_expanded, 
    data, 
    by.x = c(group_column, 'year'), 
    by.y = c(group_column, year_column), 
    all.x = TRUE
  )
  # Fill missing values by replicating data from the previous 
  # available year within each group
  filled_data <- merged_data |> 
    group_by(.data[[group_column]]) |> 
    fill(everything(), .direction = 'down') |>
    ungroup()
  
  return(filled_data)
}

# fill missing years in the 'year' column, grouped by location and selecting relevant predictors
archigos_chisols_filled <- fill_missing_years(archigos_chisols, 'year', 'country_abb')
archigos_chisols_filled <- archigos_chisols_filled |> 
  filter(year >= 1950 & year <= 2015)



## ----table_1--------------------------------------------------------------------
table_1 <- tribble(
~Period,                       ~Years,      ~Classification,
'1902 - 1905, 1916 - 1933',    22,         'Personalist rule',
'1940 - 1952',                 13,         'Semi-presidentialism', 
'1934 - 1939, 1952 - 1959',    14,         'Military rule',
'1909 - 1915',                 7,          'Presidentialism',
'1906 - 1908',                 3,          'US Military occupation') |> 
  arrange(desc(Years)) |> 
  kbl(caption = 'Cuba: Forms of Governance (1902 - 1959)') |> 
  kable_styling(latex_options = c('striped', 'hold_position')) |> 
  footnote('Based on the Anckar and Fredriksson (2020) dataset', 
           footnote_as_chunk = TRUE)
table_1


## ----table_2, eval = T----------------------------------------------------------
# create table with top five longest political tenures since the 1840s
table_2 <- archigos |> 
  select(leader, startdate, enddate, `Years in office`) |> 
  filter(`Years in office` >= 49) |> 
  kbl(col.names = c('Leader', 'Entry date', 'Exit date', 'Years in power'), caption = 'The world: Top Five Longest Political Tenures (1840 - 2015)') |> 
  kable_styling(latex_options = c('striped', 'hold_position')) |> 
  footnote('Based on the Goemans et al (2019) dataset', footnote_as_chunk = T)


## ----figure_1, eval = T---------------------------------------------------------
# figure that shows Castro's long tenure in perspective
figure_1 <- archigos |> 
  drop_na() |> 
  ggplot(aes(startdate, `Years in office`, label = leader)) +
  geom_hline(yintercept = 49, linewidth = .09, color = 'orange') +
  geom_point(size = .21) +
  geom_text_repel(min.segment.length = 0, 
                  data = subset(archigos, `Years in office` >= 49), size = 2.75) + 
  labs(
    title = 'Figure 1: The long tenure of Fidel Castro in historical context', 
    subtitle = '(The orange line intersects the vertical axis at 49, the approximate number of years Fidel Castro held power)', 
       x = 'Year the leader assumes power', y = 'Years in power', caption = 'Source: Goemans et al (2019)')

table_2
figure_1


## ----dataset_regimes, eval = T--------------------------------------------------
# regimes data set; source: Anckar and Fredriksson (2020)
regimes <- read_xlsx('anckarfredrikssonepsdatafinal2.0.xlsx')
regimes <- regimes |> 
  mutate(
         year = as.integer(year),
regimenarrowcat = case_when(
         regimenarrowcat == 0 ~ 'Parliamentarism',
         regimenarrowcat == 1 ~ 'Semi-presidentialism',
         regimenarrowcat == 2 ~ 'Presidentialism',
         regimenarrowcat == 3 ~ 'Semi-monarchy',
         regimenarrowcat == 4 ~ 'Single-party rule',
         regimenarrowcat == 5 ~ 'Multi-party autoritarian rule',
         regimenarrowcat == 6 ~ 'Personalist rule',
         regimenarrowcat == 7 ~ 'Military rule',
         regimenarrowcat == 8 ~ 'Absolute monarchy',
         regimenarrowcat == 9 ~ 'Monarchic oligarchy',
         regimenarrowcat == 10 ~ 'Other oligarchy',
         .default = 'Missing' 
       )) 
# select only the relevant variables from regimes
regimes <- regimes |> 
  select(year, abbreviation, democracy, regimenarrowcat) |> 
  mutate(abb_year = paste0(abbreviation, '-', year)) |> 
  rename(abb = abbreviation) |>  # to facilitate joining with COW
  filter(year >= 1950 & year <= 2015) 

archigos_chisols_filled <- archigos_chisols_filled |> 
  mutate(abb_year = paste0(country_abb, '-', year))
# join the three datasets: archigos, CHISOLS and regimes
arc_chi_reg <- left_join(
  distinct(archigos_chisols_filled) |> 
  select(-year), 
  distinct(regimes), 
  by = 'abb_year') |>
  drop_na()


## ----un_dataset, eval = T-------------------------------------------------------
# load UN dataset (demographics)
demographics <- read_csv(
  'WPP2022_Demographic_Indicators_Medium.csv')

demographics_1950_2015 <- demographics |> 
  filter(!is.na(ISO3_code)) |> 
  mutate(year = as.integer(Time), 
     ISO_year = paste0(str_trim(ISO3_code), '_', str_trim(year))) |> 
  filter(year >= 1950 & year <= 2015) 

# Remove some empty, almost empty and/or otherwise irrelevant variables
# and limit the span
demographics_1950_2015 <- demographics_1950_2015 |> 
  select(-c(Notes, ISO2_code, SortOrder, LocID, 
            SDMX_code, LocTypeID,LocTypeName, 
            ParentID, Location, VarID, Variant, 
            DoublingTime)) 

#glimpse(demographics_1950_2015)


## ----correlates_of_war, eval = T------------------------------------------------
# map 'Correlates of War' (COW) codes to ISO3 codes
# to facilitate dataset joining
COW <- read_csv('COW-country-codes.csv')
COW <- COW |># (StateAbb is not an ISO3 code; it's just a country name abbreviation)
rename(abb = StateAbb) |>
  mutate(abb = case_when(
    abb == 'ROM' ~ 'RUM', # Fix code for Romania, wrongly coded
    abb == 'RVN' ~ 'DRV', # Fix code for Viet Nam, wrongly coded 
    .default = abb))

df_1950_2015 <- arc_chi_reg |> 
left_join(COW) 
  
df_1950_2015 <- df_1950_2015 |> 
  mutate(ISO_year = paste0(str_trim(ISO3_code), '_', str_trim(year))) |> 
  select(-year, -ISO3_code)
  #glimpse(df_1950_2015)


## ----join_all_datasets, eval = T------------------------------------------------
# final join: the four datasets
df_1950_2015 <- left_join(df_1950_2015, demographics_1950_2015, 
                          by = 'ISO_year')


## ----brothers, eval = T---------------------------------------------------------
# get data for the Castros (brothers) 
brothers <- df_1950_2015 |> 
  filter(str_detect(leader, 'Castro') , 
         statename == 'Cuba')
# remove data for the Castros from dataset
df_1950_2015 <- df_1950_2015 |> 
  anti_join(brothers, df, by = 'obsid') |> 
  select(-obsid)

# removing obsid from brother
brothers <- brothers |> 
  select(-obsid)

# removing some irrelevant predictors
df_1950_2015 <- df_1950_2015 |> 
  select(-c(leader, startdate))

# convert all character columns to factor
convert_to_factor <- function(data) {
  data |> 
    mutate(across(where(is.character), as.factor))
}
df_1950_2015 <- convert_to_factor(df_1950_2015)

# (almost) final feature selection
df_1950_2015 <- df_1950_2015 |> 
  select(-c(exit, country_abb, StateNme, abb_year, ISO_year, abb, exit,
         exitcode, leaderpos, affiliation, year, InfantDeaths, statename)) |> 
  select(!starts_with('TPopulation')) |> 
  select(!starts_with('ISO3_code'))

# remove NAs
df_1950_2015 <- df_1950_2015 |> 
  drop_na()

# exclude gender, which shows near-zero variance (Refer to Annex...)
df_1950_2015_final <- df_1950_2015 |> 
  select(-gender)


## ----table_target_variable, eval = T--------------------------------------------
# table - target
table_target <- df_1950_2015_final |> 
  count(regimenarrowcat) |>
  mutate(Proportion = round(100 * n / sum(n))) |> 
  arrange(desc(n)) |> kbl(
    col.names = c('Classes', 'Frequency', 'Proportion (%)'), 
    caption = 'Distribution of the 12 classes in the target variable') |> 
  kable_styling(latex_options = c('striped', 'hold_position')) |> 
  footnote('Based on Anckar and Fredriksson (2020)', footnote_as_chunk = T)

table_target    


## ----predictors, eval = T-------------------------------------------------------
# table - dataset used
table_predictors <- df_1950_2015_final |> 
  map_df(~ class(.)) |> 
  pivot_longer(everything(), names_to ='Feature', values_to = 'Class') |> 
  mutate(Class = ifelse(Class == 'factor', 'categorical', Class))

UN_demographic_notes <- read_csv('WPP2022_Demographic_Indicators_notes.csv', 
                                 show_col_types = FALSE) |> 
                                 rename(Feature = Indicator)

table_predictors <- table_predictors |> 
left_join(UN_demographic_notes, by = 'Feature') |> 
mutate(Source = c(rep('Goemans et al (2009)', 5), 
                  rep('Mattes et al (2016)', 2), 
                  #rep('Anckar and Fredriksson (2020)', 5), 
                  rep('UN Population Division (2022)', 
                      nrow(table_predictors) -7))) |>
select(-c(Topic, IndicatorNo, Unit)) |> 
  rename(`Brief description` = IndicatorName) |>
  # complete some missing descriptions
  mutate(`Brief description` = case_when( 
    Feature == 'yrborn' ~ 'Year the leader was born', 
    Feature == 'entry' ~ 'How the leader reaches power',
    Feature == 'prevtimesinoffice' ~ 'Number of times a leader has previously been in office',
    Feature == 'fties' ~ 'Family ties to a previous or future leader',
    Feature == 'Years in office' ~'Years the leader held the position',
    Feature == 'year' ~ 'Year leader enters office',
    Feature == 'democracy' ~ 'Binary variable for democracy',
    Feature == 'leaderpos' ~  'Highest position the leader held',
    Feature == 'affiliation' ~ "Leader's Party affiliation",
    Feature == 'CCode' ~ 'Country code',
    .default = Feature)) |> 
  mutate(`Brief description` = ifelse(`Brief description` == 'numeric_year', 'Year leader enters office', `Brief description`)) |>
filter(Feature != 'regimenarrowcat' & Feature != 'Time') # these are not 
  # predictors; just the target variable and (part of) an indicator variable to join 
  # datasets, respectively


## ----X_and_y, eval = T----------------------------------------------------------
# define the predictors and target
predictors <- df_1950_2015_final |> 
  select(-regimenarrowcat)
target <- factor(df_1950_2015_final$regimenarrowcat)

# rename the levels of the target variable to ensure they are valid R variable names
levels(target) <- make.names(levels(target))

# split the data into training and testing sets
set.seed(1931, sample.kind = 'Rounding')
trainIndex <- createDataPartition(target, p = .8, list = FALSE)
X_train <- predictors[trainIndex, ]
X_test  <- predictors[-trainIndex, ]
y_train <- target[trainIndex]
y_test  <- target[-trainIndex]


## ----class_weights, eval = T----------------------------------------------------
# calculate weights to deal with imbalanced classes
class_counts  <- table(y_train)
total_counts  <- sum(class_counts)
class_weights <- total_counts / class_counts
weights <- class_weights[as.character(y_train)]


## ----naive_bayes, eval = T------------------------------------------------------
# combine predictors and target for training and testing
train_data <- data.frame(X_train, y_train)
test_data  <- data.frame(X_test, y_test)

# # define the control parameters for 5-fold CV
train_control <- trainControl(method = 'cv', number = 5)

# set up the parameters grid
nb_grid <- expand.grid(fL = seq(0, 1, by = .5), 
                usekernel = c(TRUE, FALSE), 
                   adjust = seq(.5, 1, by = .5))

# initialize parallel processing
cl_nb <- makeCluster(detectCores() - 1)
registerDoParallel(cl_nb)

# train the NB classifier
nb_model <- train(y_train ~ ., 
                  data = train_data, 
                  method = 'nb',
                  weights = weights,
                  trControl = train_control,
                  tuneGrid = nb_grid)
# stop parallel processing
stopCluster(cl_nb)

# tuned parameters
nb_tuned_parameters <- nb_model$bestTune

# predict on the test set using the NB model
nb_predictions <- predict(nb_model, newdata = test_data)

# evaluate the model performance
conf_matrix_nb <- confusionMatrix(nb_predictions, y_test)
conf_matrix_nb_overall <- as.matrix(conf_matrix_nb, what = "overall") 
conf_matrix_nb_classes <- as.matrix(conf_matrix_nb, what = "classes") 

# extract precision and recall for each class
precision_nb <- conf_matrix_nb$byClass[,'Precision']
recall_nb <- conf_matrix_nb$byClass[, 'Recall']

# calculate F1 for each class, while handling NA values
F1_nb <- ifelse(is.na(precision_nb) | is.na(recall_nb), 0, 
                2 * ((precision_nb * recall_nb) / (precision_nb + recall_nb)))

# get macro statistics
macro_precision_nb <- mean(precision_nb)
macro_recall_nb <- mean(recall_nb)
macro_F1_nb <- mean(F1_nb, na.rm = TRUE)

# standardize the column names in X_train and 'brothers'
names(X_train)  <- make.names(names(X_train))
names(brothers) <- make.names(names(brothers))

# predict on 'brothers' with tuned NB classifier
nb_predictions <- predict(nb_model, newdata = brothers)

# NB results
nb_results <- tibble(Years = 1959:2015, 
                     `Naive Bayes` = levels(y_train)[nb_predictions])


## ----RF_for_five_top_predictors, eval = T---------------------------------------
# define the control parameters for 5-fold CV
control <- trainControl(
  method = 'cv',                  
  number = 5)
# define grid for tuning mtry
rf_grid <- expand.grid(mtry = seq(2, 6, by = 1))

# initialize parallel processing
cl_rf <- makeCluster(detectCores() - 1)
registerDoParallel(cl_rf)
# train the RF with parameter tuning
model <- train(
  x = X_train,
  y = y_train,
  method = 'rf',
  trControl = control,
  weights = weights,
  ntree = 1000,
  importance = T,
  tuneGrid = rf_grid)
# stop parallel processing
stopCluster(cl_rf)

# extract the best value of mtry from the tuned model
tuned_mtry <- model$bestTune$mtry

# get variable importance
importance <- varImp(model, scale = F)
importance_df <- as.data.frame(importance$importance)

# add rownames as a column
importance_df <- rownames_to_column(
  importance_df, var = 'predictor')

# get overall importance score, which is either the
 #column entitled 'Overall', or the second column
if ('Overall' %in% names(importance_df)) {
  overall_col <- 'Overall'
} else {
  overall_col <- names(importance_df)[2]
}

# sort predictors by importance
top_predictors <- importance_df  |> 
  arrange(desc(!!sym(overall_col))) |> 
  pull(predictor)

# select the top 5 predictors
selected_predictors <- top_predictors[1:5]

# standardize the column names in X_train and X_test
names(X_train)  <- make.names(names(X_train))
names(X_test)   <- make.names(names(X_test))

# subset the training and testing data with the selected predictors
X_train_top <- X_train[, selected_predictors]
X_test_top  <- X_test[, selected_predictors]


## ----final_RF_model, eval = T---------------------------------------------------
# train a the RF model with top five predictors
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
model_top <- train(x = X_train_top, 
                   y = y_train, method = 'rf', 
                   trControl = control,
                   ntree = 1000,
                   weights = weights)
stopCluster(cl)

# predictions with the final model
predictions_top <- predict(model_top, X_test_top)

### evaluate the RF final classifier performance ###
conf_matrix <- confusionMatrix(predictions_top, y_test)
conf_matrix_classes <- as.matrix(conf_matrix, what = "classes")
# extract overall accuracy
accuracy <- conf_matrix$overall['Accuracy']

# extract precision, recall, and F1-score for each class
precision <- conf_matrix$byClass[, 'Pos Pred Value']
recall    <- conf_matrix$byClass[, 'Sensitivity']
F1 <- 2 * (precision * recall) / (precision + recall)
# handling NaN values in F1, if any
F1[is.nan(F1)] <- 0
# calculate macro averages
macro_precision <- mean(precision, na.rm = TRUE)
macro_recall <- mean(recall, na.rm = TRUE)
macro_f1 <- mean(F1, na.rm = TRUE)

# accuracy with 5 predictors
accuracy_top <- sum(predictions_top == y_test) / length(y_test)

# function to align factor levels
align_factor_levels <- function(train, brothers) {
  factor_columns <- names(which(sapply(train, is.factor)))
  brothers[factor_columns] <- map(factor_columns, ~ {
    factor(brothers[[.]], levels = levels(train[[.]]))
  })
  return(brothers)
}
# align factor levels in the 'brothers' data
brothers <- align_factor_levels(X_train_top, brothers)
# ensure 'brothers' data only has the selected top predictors
brother_top <- brothers[, selected_predictors]

# make predictions for the 'brothers'
new_prediction <- predict(model_top, newdata = brother_top)
# final results
final_results <- tibble(Years = 1959:2015, 
                        `Random forest` = levels(y_train)[new_prediction])


## ----eval = T-------------------------------------------------------------------
nb_tuned_parameters |> 
  as.matrix() |> 
  as_tibble() |> 
  kbl(format = 'latex', booktabs = TRUE, escape = FALSE, 
    digits = 0, 
    linesep = '', 
    caption = 'Tuned parameters for the Naive Bayes classifier') |>
  kable_styling(latex_options = c('hold_position'))


## ----eval = T-------------------------------------------------------------------
tibble(Feature = selected_predictors) |> 
  left_join(table_predictors, by = 'Feature') |> 
  mutate(Feature = str_replace_all(Feature, '\\.', ' ')) |> 
  mutate(Class = ifelse(is.na(Class), 'numeric', Class)) |> 
  mutate(`Brief description` = ifelse(Feature == 'PopSexRatio', 'Population Sex Ratio, as of 1 July', `Brief description`)) |>
  mutate(`Brief description` = ifelse(Feature == 'PopDensity', 'Population Density, as of 1 July', `Brief description`)) |>
  mutate(`Brief description` = ifelse(is.na(`Brief description`), 
                                      'Years the leader held the position', `Brief description`)) |> 
  mutate(Source = ifelse(is.na(Source), 'Goemans et al (2009)', Source)) |>
  kbl(format = 'latex', booktabs = TRUE, escape = FALSE, 
    digits = 0, 
    linesep = '', 
    caption = 'The top five most important predictors') |>
  kable_styling(latex_options = c('hold_position'))


## ----results, eval = T----------------------------------------------------------
both_results <- nb_results |> 
  left_join(final_results, by = 'Years')
# replace dots (".") with blank spaces
both_results <- both_results |>
  mutate(across(where(is.character), ~ str_replace_all(., '\\.', ' ')))
# apply conditional coloring cells to visually enhance the table of results 
set_background <- function(x, color = "white", format = "latex") {
    cell_spec(x, format = format, background = color)
}

table_of_results <- both_results |>
    mutate(
        across(`Naive Bayes`:`Random forest`, \(x)
            case_when(x == 'Military rule' ~ set_background(x, "#ccd64f"),
                      x == 'Parliamentarism' ~ set_background(x, "#11bbff"),
                      x == 'Presidentialism' ~ set_background(x, "#99ddff"),
               .default = set_background(x)
            ))) |>
    kbl(format = "latex", longtable = T, booktabs = TRUE, escape = FALSE, 
    digits = 0, 
    linesep = '', 
    caption = "Classifiers predictions: Cuba's Governance forms (1959-2015)") |>
  kable_styling(latex_options = c('hold_position', 'repeat_header'))


## ----eval = T-------------------------------------------------------------------
table_of_results 


## ----precision_nb, eval = T-----------------------------------------------------

t(conf_matrix_nb$overall) |> 
  as_tibble() |> 
  select(1:6) |> 
  kbl(caption = 'Naives Bayes: Accuracy and some related statistics', 
      digits = 3, col.names = c('Accuracy', 'Kappa', 'Lower bound', 'Upper bound', 
                                'Null', 'p-value')) |> 
  kable_styling(latex_options = c('striped', 'hold_position', 
                                  table.attr = "style='font-size: 10px'"))


## ----plot_conf_matrix_nb_classes, echo = F, eval = T----------------------------
# plot Naive Bayes' confusion matrix by metric (conf_matrix_nb)
# extract the metrics for each class and convert matrix into a data frame
class_stats_df <- as.matrix(conf_matrix_nb_classes) |> 
  as.data.frame()

# add a column for the class names
class_stats_df <- rownames_to_column(class_stats_df, 'Class')

# "pivotlong" the data frame for plot
class_stats_long <- class_stats_df |>
  pivot_longer(-Class, names_to = 'Metric', values_to = 'Score')
# plot
plot_cm_nb <- class_stats_long |>
  # plotting only the 9 most frequently-reported metrics
  filter(Class != 'Sensitivity') |> 
  filter(Class != 'Neg Pred Value') |> 
  ggplot(aes(x = Metric, y = Score, group = Class)) +
  geom_line(color = 'grey87', linewidth = .37) +
  geom_point(size = .51) +
  facet_wrap(~Class) +
  labs(title = 'Figure 2: Naive Bayes - Confusion matrix by metric',
       x = 'Class',
       y = 'Score') +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1), 
    legend.position = 'none')

plot_cm_nb


## ----precision_rf, eval = T-----------------------------------------------------
t(conf_matrix$overall) |> 
  as_tibble() |> 
  select(1:6) |> 
  kbl(caption = 'Random Forest: Accuracy and some related statistics', 
      digits = 3, col.names = c('Accuracy', 'Kappa', 'Lower bound', 'Upper bound', 
                                'Null', 'p-value')) |> 
  kable_styling(latex_options = c('striped', 'hold_position', 'scale_down'))


## ----plot_conf_matrix_rf_classes, eval = T--------------------------------------
# plot Random Forest' confusion matrix by metric (conf_matrix)
# extract the metrics for each class and convert matrix into a data frame
class_stats_rf_df <- as.matrix(conf_matrix_classes) |> 
  as.data.frame()

# add a column for the class names
class_stats_rf_df <- rownames_to_column(class_stats_rf_df, 'Class')

# "pivotlong" the data frame for plot
class_stats_rf_long <- class_stats_rf_df |>
  pivot_longer(-Class, names_to = 'Metric', values_to = 'Score')
# plot
plot_cm_rf <- class_stats_rf_long |>
  # plotting only the 9 most frequently-reported metrics
  filter(Class != 'Sensitivity') |> 
  filter(Class != 'Neg Pred Value') |> 
  ggplot(aes(x = Metric, y = Score, group = Class)) +
  geom_line(color = 'grey87', linewidth = .37) +
  geom_point(size = .51) +
  facet_wrap(~Class) +
  labs(title = 'Figure 3: Random Forest - Confusion matrix by metric',
       x = 'Class',
       y = 'Score') +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1), 
    legend.position = 'none')

plot_cm_rf


## ----eval = T-------------------------------------------------------------------
archigos_chisols |> 
  filter(country_abb == 'USA' & (leader == 'Nixon' | leader == 'Carter')) |> 
  select(2:6) |> 
  kbl(caption = 'Joining datasets without any adjustements') |> 
  kable_styling(latex_options = c('striped', 'hold_position')) 


## ----eval = T-------------------------------------------------------------------
archigos_chisols_filled |> 
  filter(country_abb == 'USA' & (leader == 'Nixon' | leader == 'Carter')) |>
  select(2, 4:7) |> 
  head() |> 
  kbl(caption = "Joining datasets using the function \n\\textit{fill\\_missing\\_years}") 


## ----near_zero, eval = T--------------------------------------------------------
# get near-zero variances using function from caret
nzv <- nearZeroVar(df_1950_2015, saveMetrics = T)


## ----eval = T-------------------------------------------------------------------
# table of near-zero variances
nzv[nzv$nzv, ] |> 
  kbl(caption = 'Near-zero variance in one feature: gender', digits = 2) |> 
  kable_styling(latex_options = c('striped', 'hold_position'))


## ----table_predictors, eval = T-------------------------------------------------
# table of predictors

kbl(table_predictors, col.names = c('Predictor', 'Class', 'Brief description', 'Source'), 
    caption = 'Selected predictors', longtable = T) |> 
  kable_styling(latex_options = c('striped', 'repeat_header'))


## ----eval = T-------------------------------------------------------------------
# confusion matrix for the NB classifier
conf_matrix_nb


## ----eval = T-------------------------------------------------------------------
# confusion matrix for the RF classifier
conf_matrix


## ----eval = F-------------------------------------------------------------------
## pander(sessionInfo())

