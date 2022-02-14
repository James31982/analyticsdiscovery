---
title: "Business Forecasting Project"
author: "Jaime Paz"
og:
  type: "Project"
  title: "Applied Time Series Forecasting in Business"

footer:
  
  - content: 'Created by Jaime Paz. Tools used: R Markdown, Tidymodels, Tidyverse & Timetk'
date: "2022-02-11"
output: markdowntemplates::bulma

---

<center><img
src="https://dantaanalytics.com/wp-content/uploads/2021/02/Forecasting-Methods-in-Marketing.jpg"
width="1000" 
height="200">
</center>


    
    
The goal of this project is to propose a machine learning model capable to predict the revenue in our current company. The goal is to develop a forecasting tool that estimates our revenue during the next months, until the end of the year. 

Our data was sampled from our Finance application tool using data from 2017 through the current year 2021. The data was summarized using our commercial business lines (LOBs). Our department from Finance provide us the data **for the last 2 months (August / September)**, so in short, our main goal is to find a model or models that can better predict these actual values.

**Business Value:** Recently, our team from finance is estimating the revenue every end of the month and providing to us the “Best Estimate projection”. However, our commercial team is proposing to create a new tool that should be capable to give better and accurate results during the next months. 


## Tools used

The tools used for this project are:
**R Programming & forecasting packages:** The “R programming language” provides us a vast utility specially for data science and forecasting models. But during the last years, there is a new set of packages called “Tidyverse”, “Tidymodels” & "Timetk".

These set of packages integrates several data science packages into one single universe, but also gives highly potential tools such as: creating workflows and recipes to compare several models; parallel programming (which gives the utility to forecast several time series at once) and a lot more. In regard to the forecasting tools, we have a great portfolio from traditional models (such as ARIMA and Moving Average) to advanced algorithms such as Deep Learning, XGBoost, Neural Nets, etc.
**R Markdown:** R also provides a set of tools to publish our work using documents and HTML sources, to provide a better and executive way and present our work to the audiences.

### 1. List of packages used


```r
rm(list=ls())

setwd("/home/analytics/R/Projects/Time Series 2")

# Forecasting Libraries ----
library(forecast)    
# Machine Learning Libraries ----
library(glmnet)       
library(randomForest) 
library(xgboost)     
# Machine Learning: Ensembles
library(modeltime.ensemble)
# Time Series ML ----
library(tidymodels)   
library(rules)        
library(modeltime)    
# Core Libraries ----
library(tidyverse)    
library(lubridate)    
library(timetk)       
# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)
# Extras
library(fs)
library(dplyr)
library(tidyquant)
library(readxl)
library(hrbrthemes)
library(plotly)
```


### 2. Collecting & Preparing Data

The fist step is to collect the data from a spreadsheet, which contains the pull from our finance database. 

The basic steps are: Collect data, convert nomenclatures, convert time-based columns to date & filtering the data. Our category “Forecast” contains the predictions from the traditional model from Finance. Our goal is to improve these numbers.



```r
# Reading the spreadsheet
financials <- read_excel('/home/analytics/R/Projects/Time Series 2/00_data/FinancialsV3.xlsx',
                         sheet = 1, col_names = TRUE)

financials$Category...1 <- NULL

# Pivoting our data and converting our data
fin_data <- financials %>%
  gather(key = "Month", value = "Value", c(Jan, Feb, Mar, Apr, May, 
                                           Jun, Jul, Aug, Sep, Oct, Nov, Dec))  %>%
    group_by(LOB, Year, Category...3, Metric, Month) %>%
      summarise(Value = sum(Value)) %>%
        mutate(Month = case_when(
        Month == "Jan" ~ "01",
        Month == "Feb" ~ "02",
        Month == "Mar" ~ "03",
        Month == "Apr" ~ "04",
        Month == "May" ~ "05",
        Month == "Jun" ~ "06",
        Month == "Jul" ~ "07",
        Month == "Aug" ~ "08",
        Month == "Sep" ~ "09",
        Month == "Oct" ~ "10",
        Month == "Nov" ~ "11",
        Month == "Dec" ~ "12",
        TRUE ~ Month)) %>%  
      mutate(Date = paste0(Year,"-",Month,"-","01") %>% as_date()) %>% ungroup() %>%
        rename(Category = Category...3) %>%
        mutate(Group = case_when(
        LOB == 'LOB 1' ~ 'G1',
        LOB == 'LOB 2' ~ 'G1',
        LOB == 'LOB 3' ~ 'G2',
        LOB == 'LOB 4' ~ 'G2',
        LOB == 'LOB 5' ~ 'G3',
        LOB == 'LOB 6' ~ 'G3',
        LOB == 'LOB 7' ~ 'G4',
        TRUE ~ LOB)) %>% 
  filter_by_time(.start_date = "2017-01-01", .end_date = "2021-09-01") %>% 
    group_by(LOB, Year, Category, Metric, Month, Group) %>%
      summarise_by_time(
        .by        = "month",
        Value      = last(Value),
        .type      = "ceiling") %>% 
        mutate(Date = Date %-time% "1 day")  %>% ungroup()

# fin_data will exclude traditional forecasting tool (we will replace a new method)
fin_data <- fin_data %>% filter(!(Date < "2021-07-31" &
                        Category == 'Forecast')  ) 
fin_data$Year <- NULL
fin_data$Month <- NULL

# Filter by Category and Metric (Actuals and Revenue)
fin_data_ac_rev <- fin_data %>% filter(Category == 'Actuals' & Metric == 'Revenue' ) 
```

Main data will be stored in fin_data_ac_rev dataframe. 

This is the output of our data structure:


```r
fin_data_ac_rev %>% head(5)
```

```
## # A tibble: 5 × 6
##   LOB   Category Metric  Group Date        Value
##   <chr> <chr>    <chr>   <chr> <date>      <dbl>
## 1 LOB 1 Actuals  Revenue G1    2017-01-31 23965.
## 2 LOB 1 Actuals  Revenue G1    2017-02-28 23056.
## 3 LOB 1 Actuals  Revenue G1    2017-03-31 29634.
## 4 LOB 1 Actuals  Revenue G1    2017-04-30 25861.
## 5 LOB 1 Actuals  Revenue G1    2017-05-31 27389.
```
How our data looks like:


```r
fin_data_ac_rev %>% head(5)
```

```
## # A tibble: 5 × 6
##   LOB   Category Metric  Group Date        Value
##   <chr> <chr>    <chr>   <chr> <date>      <dbl>
## 1 LOB 1 Actuals  Revenue G1    2017-01-31 23965.
## 2 LOB 1 Actuals  Revenue G1    2017-02-28 23056.
## 3 LOB 1 Actuals  Revenue G1    2017-03-31 29634.
## 4 LOB 1 Actuals  Revenue G1    2017-04-30 25861.
## 5 LOB 1 Actuals  Revenue G1    2017-05-31 27389.
```


### 3.0 Trend Analysis 


We them summarize our data to understand our trend:


```r
# Merging all LOBs

fin_data_ac_rev_bus <- fin_data %>% filter(Category == 'Actuals' & Metric == 'Revenue') %>%
  select(Date, Value) %>%
  group_by(Date) %>% summarize(Value = sum(Value)) 

# Plotting current revenue
fin_data_ac_rev_bus %>%
  plot_time_series(Date, Value, .smooth = FALSE, 
                   .facet_scales = "free",
                   .title = "Commercial Business Revenue (Actuals )" )  
```

```
## Error in path.expand(path): invalid 'path' argument
```

By the middle of 2020, our revenue trend seems to be stabilized, compared to the previous years. Also, our commercial revenue had been decreased from the middle of 2017 to the middle of 2020 in about **27.6 %** :


```r
detrend <- fin_data_ac_rev_bus %>% summarize_by_time(.date_var=Date, .by = 'month', value = mean(Value)) %>%
  filter(Date == '2017-06-01' | Date == '2020-06-01')

paste0(100 * (detrend$value[2] - detrend$value[1]) / detrend$value[1], " %")
```

```
## [1] "-27.6341759298646 %"
```

By the end of September 2021, we are reporting about **$204,194** on current revenue.

Now, let's compare our actual revenue agains the forecasted revenue (traditional way):



```r
fin_data_rev_bus <- fin_data %>% filter(Category %in% c('Actuals', 'Forecast') & Metric == 'Revenue') %>%
  select(Category, Date, Value) %>%
  group_by(Category, Date) %>%
  summarize(Value = sum(Value)) 

fin_data_rev_bus %>% ungroup() %>%
  plot_time_series(Date, Value, .color_var = Category, .smooth = FALSE,
                   .title = "Commercial Business Revenue (Actuals vs Forecast)")
```

```
## Error in path.expand(path): invalid 'path' argument
```

Clearly we can see a gap of 3% between the actual value and the forecasted value by the end of September 2021. 



```r
fin_data_rev_bus %>% filter_by_time(.date_var = Date, 
                                                  .start_date = '2021-06-30', .end_date = '2021-09-30') %>% ungroup() %>% 
  plot_time_series(Date, Value, .color_var = Category, .smooth = FALSE,
                   .title = "Commercial Business Revenue (Actuals vs Forecast)")
```

```
## Error in path.expand(path): invalid 'path' argument
```

The current forecasted value was merly accurate by the end of September 2021. However, this model doesn't seem to capture the current trend pretty well. For instance, we can a gap of 6.7% by the end of October 2021.

### 3.1 Correlation Analysis (between lags) 


```r
fin_data_ac_rev_bus %>% plot_acf_diagnostics(Value, log_interval_vec(Value, offset = 1000000),
                                           .lags = 100)  
```

```
## Error in path.expand(path): invalid 'path' argument
```

After executing some correlation analysis, we can spot some seasonality on the Partial Correlation Plot (2nd graph). Here we are showing all the lags list and the seasonality is spotted from lags 6 to 19. This means that the data is showing strong relationships between these points and we should assess if this variations are significant during our analysis.

*Note:* We are applying *log transformations* in order to make our data more smooth and so we can spot the variations more clearly.

### 3.2 Seasonality/trend/remainder diagnostics


```r
fin_data_ac_rev_bus %>%
  plot_stl_diagnostics(Date, log_interval_vec(Value, offset = 1000000), 
                       .facet_scales = "free_y",
                       .feature_set = c( "observed", "season", "trend", "remainder"))
```

```
## Error in path.expand(path): invalid 'path' argument
```

It's evident that seasonality is spotted once again. Since the data is presented in a monthly format and in order to capture this variations, we need to include the monthly seasonality in our models.

As previously stated, our trend has shown a strong decrease during the first 3 years, but it seems to be stabilized during the last 2 years. There are some errors introduced in the model and we can check it on the remainder. Our goal is to capture these variations and find a useful model to predict our future revenue.

More importantly, a better way to understand the seasonality is to plot the actual values, using an STL (seasonal & trend decomposition) diagnostics.

**Yearly Seasonality:**



```r
## YEARLY SEASONALITY

fin_data_ac_rev_bus %>% 

  plot_seasonal_diagnostics(Date, log_interval_vec(Value, offset = 1000000),
                            .feature_set = "year") 
```

```
## Error in path.expand(path): invalid 'path' argument
```


**Monthly Seasonality:**


```r
## MONTHLY SEASONALITY

fin_data_ac_rev_bus %>% 

  plot_seasonal_diagnostics(Date, log_interval_vec(Value, offset = 1000000),
                            .feature_set = "month.lbl") 
```

```
## Error in path.expand(path): invalid 'path' argument
```

Clearly, we can spot some seasonality on a monthly basis. There is quite a lot of opportunity in the commercial business during the last quarter of the year. We usually call it, "the busy months" since we are collecting most of the revenue during this period.


### 4. Data preparation (phase 1): Transformation / Feature Engineering

Next steps are to normalize and transform our data in order to facilitate the models to capture the variations (specially the sequential models). Basic transformations are used: **logarithm (base 10) & normalization transformation** (centralize the data to the mean and make it comparable).

**Feature Engineering** consists in adding “additional features” to the model and be able to make accurate models. For instance, we add Fourier Terms (i.e., monthly & yearly) in order to capture seasonality more accurate.

It's important to state that our assessment to validate our predictions will be for the last 3 months. **This is a unknown data so far** corresponding to the months of October, November and December of 2021.




```r
# Activating Parallel Processing (for fast computing)

registerDoFuture()
n_cores <- parallel::detectCores()
plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

# DATA PREPARATION: TRANSFORMATIONS / FEATURE ENGINEERING

full_data_tbl <- fin_data_ac_rev_bus %>%

  # 1. Log transformation  
  mutate(value_trans = Value %>% log_interval_vec(offset = 1000000) )  %>%
  # 2. Standard normalization
  mutate(value_trans = standardize_vec(value_trans)) %>%
  
  # 3. Creating future frame: we are going to asses the last 2 months (available)
  future_frame(Date, .length_out = 2, .bind_data = TRUE) %>% 
  
  # 4. Foutier series: including monthly / yearly seasonality. Order 2.
  tk_augment_fourier(Date, .periods = c(30, 365), .K = 2) %>%
  
  rowid_to_column(var = "rowid") # create rowid to keep track of our values

# DATA PREPARATION: Creating two sets: The data to train the model and data to be assessed (3 months)

# train data
data_prepared_tbl <- full_data_tbl %>%
  filter(!is.na(Value)) 

# Future data (future data corresponds to unseen data, that means: October, November and December 2021)
future_tbl <- full_data_tbl %>%
  filter(is.na(Value))
```

How the data looks like (after transformation steps):


```r
data_prepared_tbl %>% head(5)
```

```
## # A tibble: 5 × 12
##   rowid Date         Value value_trans Date_sin30_K1 Date_cos30_K1 Date_sin30_K2 Date_cos30_K2
##   <int> <date>       <dbl>       <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
## 1     1 2017-01-31 283845.       0.965        0.0540        -0.999        -0.108        0.994 
## 2     2 2017-02-28 288364.       1.17        -0.135         -0.991         0.267        0.964 
## 3     3 2017-03-31 313201.       3.16        -0.338         -0.941         0.636        0.772 
## 4     4 2017-04-30 288546.       1.18        -0.520         -0.854         0.889        0.458 
## 5     5 2017-05-31 297216.       1.66        -0.687         -0.727         0.998        0.0574
## # … with 4 more variables: Date_sin365_K1 <dbl>, Date_cos365_K1 <dbl>, Date_sin365_K2 <dbl>,
## #   Date_cos365_K2 <dbl>
```

**value_trans:** the actual values already transformed

**Date_sin & Date_cos features:** the fourier series features created to assess seasonality


### 5. Data splitting (train / test)

Before training a model, we need to split the data into two parts:

1. Training set: The data that is going to be used to train our models.
2. Testing set: Corresponds to the last 12 point values of our data. This is fixed number, depending on the size of the data we may choose a greater a value. 


```r
# TIME SPLIT

splits <- time_series_split(data_prepared_tbl, assess = 12, skip = 12, cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, value_trans)
```

```
## Error in path.expand(path): invalid 'path' argument
```

```r
# New cleaned version of the data:

train_cleaned <- training(splits) 
```


### 6. Data preparation (Phase 2): "The Recipe"


To be more concise, we divided the data preparation step into 2 portions. This last portion consist in creating a “recipe”. This term is used by the “Tidymodels” package to prepare the data and have it ready to be imputed to the future forecasting models.

Main additional points in this phase, consist in adding two **feature engineering** steps:

**1. Creating Time Series Signature:** In short, this step adds 12 additional variables, that correspond to binary features per each month (i.e. Date_month.lbl_12 = 1, this means that the data point corresponds the month of December).

**2. Natural Splines:** Splines are curves, which are usually required to be continuous and smooth. Splines are usually defined as piecewise polynomials of degree n with function values and first n-1 derivatives that agree at the points where they join. The abscissa values of the join points are called knots




```r
train_cleaned$Value <- NULL


recipe_spec <- recipe(value_trans ~ ., data = train_cleaned) %>%  
  update_role(rowid, new_role = "indicator") %>%
  # 1. Time series signature: Adding additional terms (monthly features) 
  step_timeseries_signature(Date) %>% 
  step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(week)|(mday)|(day)|(half)|(quarter)")) %>% 
  
  step_rm(ends_with('Date_month')) %>% 
  step_normalize(Date_index.num, Date_year) %>% #normalizing index value
  # 2. Adding natural splines features
  step_ns(ends_with("index.num"), deg_free = 3) %>%
  # convert all categorical features to factors
  step_dummy(all_nominal(), one_hot = TRUE)

# since this is a recipe, we display it by
recipe_spec %>% prep() %>% juice %>% glimpse()
```

```
## Rows: 45
## Columns: 27
## $ rowid               <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, …
## $ Date                <date> 2017-01-31, 2017-02-28, 2017-03-31, 2017-04-30, 2017-05-31, 2017-06-30, 20…
## $ Date_sin30_K1       <dbl> 0.05402259, -0.13471146, -0.33778424, -0.52033669, -0.68651460, -0.81882899…
## $ Date_cos30_K1       <dbl> -0.99853971, -0.99088487, -0.94122357, -0.85396120, -0.72711602, -0.5740375…
## $ Date_sin30_K2       <dbl> -0.10788741, 0.26696710, 0.63586098, 0.88869469, 0.99835153, 0.94007715, 0.…
## $ Date_cos30_K2       <dbl> 0.99416312, 0.96370564, 0.77180361, 0.45849946, 0.05739540, -0.34096182, -0…
## $ Date_sin365_K1      <dbl> -0.1243415, -0.1397535, -0.1567772, -0.1732076, -0.1901352, -0.2064630, -0.…
## $ Date_cos365_K1      <dbl> -0.9922395, -0.9901863, -0.9876340, -0.9848853, -0.9817579, -0.9784544, -0.…
## $ Date_sin365_K2      <dbl> 0.2467531, 0.2767640, 0.3096771, 0.3411793, 0.3733334, 0.4040293, 0.4352769…
## $ Date_cos365_K2      <dbl> 0.9690784, 0.9609379, 0.9508418, 0.9399982, 0.9276972, 0.9147460, 0.9002966…
## $ value_trans         <dbl> 0.964781080, 1.171775504, 3.156546783, 1.180634206, 1.661153632, 1.60201955…
## $ Date_year           <dbl> -1.2780193, -1.2780193, -1.2780193, -1.2780193, -1.2780193, -1.2780193, -1.…
## $ Date_index.num_ns_1 <dbl> 0.000000000, -0.015921599, -0.033116819, -0.048882412, -0.063796814, -0.076…
## $ Date_index.num_ns_2 <dbl> 0.00000000, 0.04784176, 0.10041556, 0.15049620, 0.20099176, 0.24823228, 0.2…
## $ Date_index.num_ns_3 <dbl> 0.00000000, -0.03187866, -0.06691044, -0.10028094, -0.13392793, -0.16540596…
## $ Date_month.lbl_01   <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, …
## $ Date_month.lbl_02   <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ Date_month.lbl_03   <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ Date_month.lbl_04   <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ Date_month.lbl_05   <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ Date_month.lbl_06   <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, …
## $ Date_month.lbl_07   <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, …
## $ Date_month.lbl_08   <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, …
## $ Date_month.lbl_09   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, …
## $ Date_month.lbl_10   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, …
## $ Date_month.lbl_11   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, …
## $ Date_month.lbl_12   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, …
```


### 7. Modelling Stage 1 (Sequential models)


We tested out several models during our training stage and finally come up with 4 models:

**Sequential**: It includes ARIMA and some of its variants
**Non-Sequential**: it includes Neural Network Model

Here, when we say **Sequential**, it means that the model considers the order of the data (otherwise, wrong results will be delivered). For** Non-Sequential models** we have included the best option, which is the NEURAL NETWORK model. This machine learning doesn’t necessary considers the order of the upcoming data.

*Model description summary:*
** 1. ARIMA + EXTERNAL REGRESSORS **: ARIMA means “Auto-Regressive + Integrated + Moving Average”. In general terms, this model considers each lag sequentially (monthly data) by providing a weight to each lag (according to their importance). This algorithm also includes an integrated part which basically takes a “derivate” to the derivate and make it more stationary (more efficient to predict). Finally, it includes the Moving Average (MA) part, that simply takes the average of our errors to continuously improve the accuracy. The term EXTERNAL REGRESSORS, means that it includes the additional features that we added previously in the feature engineering stage. 
So, ARIMA (1,1,1)(2,1,0)[12] means:  “1” AR component, “1” derivative, “1” MA component. Same logic applies to the seasonal component (2,1,0). Finally, “[12]” means the period of our data. 

** 2. ARIMA + SLT **: Though this variant doesn’t include the external regressors part, it has the capability to include a seasonal pattern (STL). This has the benefit to capture our monthly seasonality more efficiently.

** 3. XGBOOST ARIMA + EXTERNAL REGRESSORS **: Apart from adding the ARIMA modeling, this model also includes an **eXtreme Gradient Boosting** which implements the called “gradient boosting decision tree algorithm”. Boosting is an ensemble technique where new models are added to correct the errors made by existing models. 


Each model is composed with a RECIPE (adding additional features) + MODEL SPECIFICATION (Parameters) and creating a WORKFLOW.


```r
################################ 1. ARIMA + EXTERNAL REGRESSORS #########################################

# RECIPE
recipe_arima <- recipe_spec %>%
  step_rm(ends_with('rowid')) %>%
  step_rm(matches("(Date_month.lbl)")) %>%
  step_rm(contains('Date_sin')) %>%
  step_rm(contains('Date_year')) %>%
  step_rm(contains('Date_cos')) 

# MODEL SPEC

model_spec_arima <- arima_reg(
  mode = "regression",
  seasonal_period = 12,
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 1,
  seasonal_ar = 2,
  seasonal_differences = 1,
  seasonal_ma = 0
)  %>%
  set_engine("arima")

# WORKFLOW
workflow_fit_arima <- workflow() %>%
  add_model(model_spec_arima) %>%
  add_recipe(recipe_arima)  %>% 
  fit(training(splits))


################################ 2. ARIMA + STL (Seasonal) #########################################

# MODEL SPEC
model_spec_arima_STL <-  seasonal_reg(
  seasonal_period_1 = 12) %>%
  set_engine("stlm_arima") 

# WORKFLOW
workflow_fit_arima_STL <- workflow() %>%
  add_model(model_spec_arima_STL) %>%
  add_recipe(recipe_arima)  %>% 
  fit(training(splits))

################################# 3. ARIMA + XGBOOST #############################################

# MODEL SPEC

# arima parameters
model_arima_boost <- arima_boost(
  seasonal_period = 12,  
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 2,
  seasonal_ar = 2,
  seasonal_differences = 1,
  seasonal_ma = 0,

# xgboost parameters  
  mtry = 20,
  min_n = 1,
  tree_depth = 3,
  learn_rate = 0.25,
  trees = 15

) %>%
  set_engine("arima_xgboost")

# WORKFLOW
workflow_fit_arima_XG <- workflow() %>%
  add_model(model_arima_boost) %>%
  add_recipe(recipe_arima)  %>% 
  fit(training(splits))
```

Now, we combine all models into one single table:


```r
submodels_1_tbl <- modeltime_table(
  
  workflow_fit_arima,
  workflow_fit_arima_STL,
  workflow_fit_arima_XG
)

submodels_1_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse)
```

```
## # A tibble: 3 × 9
##   .model_id .model_desc                                         .type   mae  mape  mase smape  rmse   rsq
##       <int> <chr>                                               <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1         3 ARIMA(1,1,2)(2,1,0)[12] W/ XGBOOST ERRORS           Test  0.114  12.0 0.599  12.5 0.143 0.541
## 2         1 REGRESSION WITH ARIMA(1,1,1)(2,1,0)[12] ERRORS      Test  0.188  19.4 0.989  19.8 0.223 0.461
## 3         2 SEASONAL DECOMP: REGRESSION WITH ARIMA(0,0,0) ERRO… Test  0.186  25.9 0.973  29.6 0.283 0.827
```

### 8. Modelling Stage 2 (Non-Sequential models)


** 4. NEURAL NETWORK **: This algorithm basically simulates “a brain network” where one can provide several neurons that learn according to several iterations or epochs during the training stage. This is a black box model, which it can be difficult to understand but in exchange it provided greater results. Our model was trained using a method called “Hyper-parameter tunning” which basically finds the best parameters that provide better results. Our final version was: [24-5-1] Neural Network, which means: “24” inputs, “5” deep layers and “1” single output.

This stage also includes a sampling technique called: **V-fold cross-validation** (also known as k-fold cross-validation) which randomly splits the data into V groups of roughly equal size (called “folds”). A resample of the analysis data consisted of V-1 of the folds while the assessment set contains the final fold. In basic V-fold cross-validation (i.e. no repeats), the number of resamples is equal to V.



```r
################################# 4. NEURAL NETWORK #############################################

# K-FOLD Cross-Validation

set.seed(123)
resamples_kfold <- train_cleaned %>% vfold_cv(v = 5)  
resamples_kfold %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, value_trans, .facet_ncol = 2)
```

```
## Error in path.expand(path): invalid 'path' argument
```

```r
cell_folds <- vfold_cv(train_cleaned)

# MODEL
model_spec_nnet_tune <- mlp(
  mode = "regression",
  hidden_units = 5,
  penalty = 0.3,
  epochs = tune(), #epochs parameter will be tunned as a hyperparameter
  dropout = 0,
  activation = 'linear'
  
) %>%
  set_engine("nnet", trace = 0)


# MODEL SPEC
wflw_spec_nnet_tune <- workflow() %>%
  add_model(model_spec_nnet_tune) %>%
  add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator"))


# TUNNING STAGE

tic()
set.seed(123)
tune_results_nnet <- wflw_spec_nnet_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    param_info = parameters(wflw_spec_nnet_tune) %>%
      update(
        epochs = epochs(c(10, 1000))),
    grid = 10,
    control = control_grid(verbose = TRUE, allow_par = TRUE)
  )
toc()
```

```
## 44.463 sec elapsed
```

```r
# DISPLAY RESULTS (Best option)

tune_results_nnet %>% show_best("rmse", n = Inf)
```

```
## # A tibble: 10 × 7
##    epochs .metric .estimator  mean     n std_err .config              
##     <int> <chr>   <chr>      <dbl> <int>   <dbl> <fct>                
##  1    201 rmse    standard   0.366     5  0.0845 Preprocessor1_Model02
##  2    601 rmse    standard   0.368     5  0.0838 Preprocessor1_Model05
##  3    361 rmse    standard   0.368     5  0.0828 Preprocessor1_Model09
##  4    432 rmse    standard   0.369     5  0.0831 Preprocessor1_Model06
##  5     91 rmse    standard   0.369     5  0.0846 Preprocessor1_Model03
##  6    729 rmse    standard   0.369     5  0.0831 Preprocessor1_Model10
##  7    827 rmse    standard   0.369     5  0.0842 Preprocessor1_Model04
##  8    208 rmse    standard   0.370     5  0.0840 Preprocessor1_Model01
##  9    628 rmse    standard   0.370     5  0.0839 Preprocessor1_Model07
## 10    927 rmse    standard   0.370     5  0.0839 Preprocessor1_Model08
```

```r
# ** Finalize

wflw_fit_nnet_tuned <- wflw_spec_nnet_tune %>%
  finalize_workflow(select_best(tune_results_nnet, "rmse")) %>%
  fit(train_cleaned)
```


### 9. Model integration (Sequential Models + Non-Sequential)

The goal of integrating all models into one single table is to have a better approach to assess the accuracy of them all. We can also combine several models into one **(ENSEMBLES)** by average them and having a better prediction.



```r
submodels_2_tbl <- modeltime_table(
  wflw_fit_nnet_tuned) %>%
  update_model_description(1, "NEURAL NETWORK 24-5-1") %>%  
  combine_modeltime_tables(submodels_1_tbl) %>%
  update_model_description(2, "ARIMA (1,1,1)(2,1,0)[12] + XREGS") %>%
  update_model_description(3, "ARIMA(0,0,1) + STL") %>%
  update_model_description(4, "XGBOOST ARIMA(1,1,2)(2,1,0)[12] + XREGS") 

#Calibration ----
calibration_tbl <- submodels_2_tbl %>%
  modeltime_calibrate(testing(splits))

#Accuracy ----
calibration_tbl %>% 
  modeltime_accuracy() %>%
  arrange(rmse)
```

```
## # A tibble: 4 × 9
##   .model_id .model_desc       .type    mae  mape  mase smape  rmse   rsq
##       <int> <chr>             <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1         1 NEURAL NETWORK 2… Test  0.0947  10.9 0.497  10.8 0.114 0.869
## 2         4 XGBOOST ARIMA(1,… Test  0.114   12.0 0.599  12.5 0.143 0.541
## 3         2 ARIMA (1,1,1)(2,… Test  0.188   19.4 0.989  19.8 0.223 0.461
## 4         3 ARIMA(0,0,1) + S… Test  0.186   25.9 0.973  29.6 0.283 0.827
```

From this table, we can extract the value of **rmse**, which is one metric that assess better the accuracy of our prediction. Root Mean Square Error (RMSE) is the standard deviation of the residuals (prediction errors). Residuals are a measure of how far from the regression line data points are; RMSE is a measure of how spread out these residuals are. The shorter the value, the better. In other words, it tells you how concentrated the data is around the line of best fit. According to the results, the **NEURAL NETWORK** and the **XGBOOST ARIMA + XREGS** are the best models.

### 10. Model representation (Plotting results)


Lets take a look at the forecasted models:



```r
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data   = TRUE  
  ) %>%
  

  plot_modeltime_forecast(
    .facet_ncol         = 4, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE, 
    .title = "FORECASTING MODELS ASSESSMENT"
  )
```

```
## Error in path.expand(path): invalid 'path' argument
```


In order to visualize the data in the original values, we need to apply the inverse of the standardization and the logarithm:



```r
# EXECUTING THE INVERSE OF THE LOGARITHM AND THE STANDARDIZATION:

forecast_test_tbl <- submodels_2_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data   = TRUE) %>%
  mutate(
    value_trans= value_trans %>% standardize_inv_vec(mean = 2.88065437189917 , sd = 0.577155837177902))%>%
  mutate(
    .value = .value %>% standardize_inv_vec(mean = 2.88065437189917 , sd = 0.577155837177902)) %>% 
  mutate(
    value_trans= value_trans %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 1325114.92929216, offset = 1e+06)  ) %>%
  mutate(
    .value = .value %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 1325114.92929216, offset = 1e+06))


### PREDICTION

table_forecast_metrics <- forecast_test_tbl %>% 
  select(Date, .value, .model_desc, value_trans)

forecast_test_tbl %>%
  plot_modeltime_forecast(
    .facet_ncol = 4, .title = "FORECASTING: MODELS ASSESSMENT"
  )
```

```
## Error in path.expand(path): invalid 'path' argument
```


Lets take a closer look to the most recent data:



```r
table_forecast_metrics <- forecast_test_tbl %>% 
  select(Date, .value, .model_desc, value_trans)

forecast_test_tbl %>%
  filter_by_time(.date_var = Date, .start_date = '2021-01-01', .end_date = '2021-09-30') %>%
  plot_modeltime_forecast(
    .facet_ncol = 4, .title = "FORECASTING: MODELS ASSESSMENT (Year 2021)"
  )
```

```
## Error in path.expand(path): invalid 'path' argument
```



### 11. Proposed Forecasting Models vs Traditional Model (Best Estimate)

Next step is to compare the results against the traditional forecasting method **(BEST ESTIMATE)** that our department of Finance was providing to us. First, lets integrate the data.



```r
library(viridis)

fin_data_forecast <- fin_data %>% filter(Category %in% c('Forecast') & Metric == 'Revenue') %>%
  arrange(Date, ascending = TRUE) %>% group_by(Category, Metric, Date) %>% summarize(.value = sum(Value)) %>% filter_by_time(.date_var = Date, .start_date = "2021-08-01", .end_date = "2021-09-30") %>% ungroup() %>% 
  mutate(.model_desc = "Forecast (BE)")  

fin_data_forecast$Category <- NULL
fin_data_forecast$Metric <- NULL
fin_data_forecast$value_trans <- c(194056.4, 204194.1)

table_forecast_metrics <- rbind(table_forecast_metrics, fin_data_forecast)

table_forecast_metrics_ggplot <- table_forecast_metrics %>%
  ggplot( aes(x=Date, y=.value, group=.model_desc, color=.model_desc)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("PROPOSED FORECASTING MODELS VS BEST ESTIMATE (Original prediction)") +
  theme_ipsum() +
  ylab("Actuals ($revenue)")

ggplotly(table_forecast_metrics_ggplot)
```

```
## Error in path.expand(path): invalid 'path' argument
```



```r
table_forecast_metrics_ggplot <- table_forecast_metrics %>% 
  filter_by_time(.date_var = Date, .start_date = '2021-01-01', .end_date = '2021-09-30') %>%
  ggplot( aes(x=Date, y=.value, group=.model_desc, color=.model_desc)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("PROPOSED FORECASTING MODELS VS BEST ESTIMATE (Original prediction) - Year 2021") +
  theme_ipsum() +
  ylab("Actuals ($revenue)")

ggplotly(table_forecast_metrics_ggplot)
```

```
## Error in path.expand(path): invalid 'path' argument
```


Lets how our model generalizes for unseen data:




```r
model_refit_tbl <- submodels_2_tbl %>%
  modeltime_refit(data_prepared_tbl)

modeltime_forecast_tbl <- model_refit_tbl %>%
  modeltime_forecast(
    new_data    = future_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE 
  ) %>%
  mutate(
    value_trans= value_trans %>% standardize_inv_vec(mean = 2.88065437189917 , sd =0.577155837177902))%>%
  mutate(
    .value = .value %>% standardize_inv_vec(mean = 2.88065437189917 , sd = 0.577155837177902)) %>%
  mutate(
    value_trans= value_trans %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 1325114.92929216, offset = 1e+06)) %>%
  mutate(
    .value = .value %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 1325114.92929216, offset = 1e+06))

modeltime_forecast_tbl %>% 
  plot_modeltime_forecast(
    .facet_ncol   = 4,
    .y_intercept  = 0
  )
```

```
## Error in path.expand(path): invalid 'path' argument
```



### 12. ASSESSMENT: Proposed Forecasting Models vs Traditional Model (Best Estimate)

Next step, is to assess the model(s) that provide a better prediction.



```r
table_forecast_metrics %>% filter_by_time(.date_var = Date, .start_date = '2021-08-31', .end_date = '2021-09-30') %>%
  select(.model_desc, .value, value_trans) %>%
  group_by(.model_desc) %>% 
  summarize_accuracy_metrics(
    truth      = value_trans, 
    estimate   = .value,
    metric_set = metric_set(mae, rmse)
  )
```

```
## # A tibble: 6 × 3
##   .model_desc                                mae   rmse
##   <chr>                                    <dbl>  <dbl>
## 1 ACTUAL                                      0      0 
## 2 ARIMA (1,1,1)(2,1,0)[12] + XREGS        12760. 12977.
## 3 ARIMA(0,0,1) + STL                       8267. 10717.
## 4 Forecast (BE)                           10211. 10886.
## 5 NEURAL NETWORK 24-5-1                    7514.  9597.
## 6 XGBOOST ARIMA(1,1,2)(2,1,0)[12] + XREGS  9437.  9692.
```

In terms of **root mean square errors and mean absolute errors**, we can see that our proposed models over-passed the traditional method. It seems that on average, both models **Neural Network & ARIMA(0,0,1) + STL** output a better prediction. 

One thing to note here is that **RMSE** is used when small errors can be safely ignored and big errors must be penalized and reduced as much as possible. On the other hand, the **MAE** is a better choice where the errors get worse linearly. So in our case, this metric should be a better choice when comparing the models on the future.



### 13. ASSESSMENT: Ensemble Model (Proposed) vs Traditional Model (Best Estimate)

In machine learning there is a concept called **Ensemble Models** that in short, it combines the output of several models into one single output (model). The decision of our summary metric is arbitrary, but most useful options are **mean and median**.

Next, we are going to create an ensemble model wich contains the best models proposed in terms of **MAE**: 

**1. NEURAL NETWORK 24-5-1**

**2. XGBOOST ARIMA(1,1,2)(2,1,0)[12] + XREGS**



```r
# SELECTING MODELS TO SUMMARIZE
submodels_2_ids_to_keep <- c(1,4)

# CREATING ENSEMBLE TABLE

ensemble_fit <- submodels_2_tbl %>%
  filter(.model_id %in% submodels_2_ids_to_keep) %>%
  ensemble_average(type = "median")

model_ensemble_tbl <- modeltime_table(
  ensemble_fit
)

forecast_ensemble_test_tbl <- model_ensemble_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>% mutate(
    value_trans= value_trans %>% standardize_inv_vec(mean = 2.88065437189917 , sd =0.577155837177902))%>%
  mutate(
    .value = .value %>% standardize_inv_vec(mean = 2.88065437189917 , sd = 0.577155837177902)) %>%
  mutate(
    value_trans= value_trans %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 1325114.92929216, offset = 1e+06)) %>%
  mutate(
    .value = .value %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 1325114.92929216, offset = 1e+06))

forecast_ensemble_test_tbl %>%
  plot_modeltime_forecast(
    .facet_ncol = 4, .title = "ENSEMBLE MODEL (NNET + XGBOOST ARIMA) Vs Revenue (Actuals)"
  )
```

```
## Error in path.expand(path): invalid 'path' argument
```



```r
table_forecast_ensemble_metrics <- forecast_ensemble_test_tbl %>% 
  select(Date, .value, .model_desc, .key, value_trans) 

fin_data_forecast$.key = 'ACTUAL'

table_forecast_ensemble_metrics <- rbind(table_forecast_ensemble_metrics, fin_data_forecast)
table(table_forecast_ensemble_metrics$.model_desc)
```

```
## 
##                      ACTUAL ENSEMBLE (MEDIAN): 2 MODELS 
##                          57                          12 
##               Forecast (BE) 
##                           2
```

```r
table_forecast_ensemble_metrics_ggplot <- table_forecast_ensemble_metrics %>%
  ggplot(aes(x=Date, y=.value, group=.model_desc, color=.model_desc)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("ENSEMBLE MODEL VS TRADITIONAL METHOD (BEST ESTIMATE)") +
  theme_ipsum() +
  ylab("Revenue (Actuals)")

ggplotly(table_forecast_ensemble_metrics_ggplot)
```

```
## Error in path.expand(path): invalid 'path' argument
```

We can clearly see that our **ENSEMBLE MODEL** is a better approach, compared to the traditional method.


Next, we are displaying the **MAE** metric, comparing both Ensemble and traditional method (Best Estimate):


```r
table_forecast_ensemble_metrics %>%
  select(.model_desc, .value, value_trans) %>%
  group_by(.model_desc) %>%  
  summarize_accuracy_metrics(
    truth      = value_trans, 
    estimate   = .value,
    metric_set = metric_set(mae)
  )
```

```
## # A tibble: 3 × 2
##   .model_desc                    mae
##   <chr>                        <dbl>
## 1 ACTUAL                          0 
## 2 ENSEMBLE (MEDIAN): 2 MODELS  3746.
## 3 Forecast (BE)               10211.
```

```r
#Turn OFF Parallel Backend
plan(sequential)
```

These results are showing that our ensemble model is a better method for predicting the actual revenue. We had a MAE metric of 3,746.3 and it was reduced in about 6464.2.


### 14. FINAL WORDS AND RECOMMENDATIONS


**(1)** According to our proposal, the ensemble model have improved the forecasting accuracy in about 63%. These results were compared to the best estimate method provided by finance (traditional method) by taking the last 2 months (August / September 2021) of the data.

**(2)** The current proposal states that our new model will be a better approach to the traditional method. It’s recommended to test this proposal during the next 2 months.

**(3)** Machine Learning is a continuous process, so it’s recommended to continuously calibrate the proposed model (at least every month) from now on.

**(4)** Next steps are to publish the model using our tools such as POWER BI and Shiny and keep monitoring the performance during the next 2 months. Unless there are no changes in our current commercial structure, we shall continue with the proposal; otherwise, it’s recommended to run the process again and taking into account the new changes on the structure.







