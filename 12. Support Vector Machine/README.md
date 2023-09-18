Churn_SVM
================

For the chapter on Support Vector Machines we are going to continue
using Churn data, or customer leakage. Before loading the data, we
invoke the Tidyverse library. After loading the data, we cleaned it up a
bit and took a look at it.

``` r
#tidymodels
library(tidyverse)

data <- read_csv("Churn_Modelling.csv") %>% 
  mutate(is_female = ifelse(Gender == "Female",1,0),
         Exited = as.factor(Exited)) %>% 
        select(-RowNumber, -Surname, -Geography, -Gender, -CustomerId) %>% 
  relocate(Exited)
 
data %>% glimpse()
```

    ## Rows: 10,000
    ## Columns: 10
    ## $ Exited          <fct> 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, …
    ## $ CreditScore     <dbl> 619, 608, 502, 699, 850, 645, 822, 376, 501, 684, 528,…
    ## $ Age             <dbl> 42, 41, 42, 39, 43, 44, 50, 29, 44, 27, 31, 24, 34, 25…
    ## $ Tenure          <dbl> 2, 1, 8, 1, 2, 8, 7, 4, 4, 2, 6, 3, 10, 5, 7, 3, 1, 9,…
    ## $ Balance         <dbl> 0.00, 83807.86, 159660.80, 0.00, 125510.82, 113755.78,…
    ## $ NumOfProducts   <dbl> 1, 1, 3, 2, 1, 2, 2, 4, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, …
    ## $ HasCrCard       <dbl> 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, …
    ## $ IsActiveMember  <dbl> 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, …
    ## $ EstimatedSalary <dbl> 101348.88, 112542.58, 113931.57, 93826.63, 79084.10, 1…
    ## $ is_female       <dbl> 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, …

We are going to implement the SVMs using the tidymodels library, to
maintain the syntax that we have used until now.

``` r
library(tidymodels)
library(discrim) 
set.seed(42)
data_split <- initial_split(data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

nrow(test_data)
```

    ## [1] 2500

``` r
train_data %>% nrow()
```

    ## [1] 7500

We are going to create the recipe for the support vector machine, which
is the same as the last class.

For the model we use the svm_poly function that allows us to create
polynomial kernels. For the linear case it is equivalent to a polynomial
of degree 1.

``` r
receta <- 
  recipe(Exited ~ ., data = train_data) 

receta
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 9

``` r
modelo <- svm_poly(degree = 1) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()


modelo
```

    ## Polynomial Support Vector Machine Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   degree = 1
    ## 
    ## Computational engine: kernlab 
    ## 
    ## Model fit template:
    ## kernlab::ksvm(x = missing_arg(), data = missing_arg(), kernel = "polydot", 
    ##     prob.model = TRUE, kpar = list(degree = ~1))

Now we will test the fitea function that we created last class with this
polynomial model of degree 1 (linear)

``` r
fitea <- function(mod){
  
  modelo_fit <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(receta) %>% 
  fit(data = train_data)

model_pred <- 
  predict(modelo_fit, test_data, type = "prob") %>% 
  bind_cols(test_data) 

return(model_pred %>% 
  roc_auc(truth = Exited, .pred_0))
}

fitea(modelo)
```

    ## maximum number of iterations reached 0.01189402 0.01018144

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.668

Now let’s modify the fitea function to try different degrees of
polynomial, with degree 1, 2 or 3

``` r
fitea_polySVM <- function(grado){
  
  mod <- svm_poly(degree = grado) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()
  
  modelo_fit <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(receta) %>% 
  fit(data = train_data)

model_pred <- 
  predict(modelo_fit, test_data, type = "prob") %>% 
  bind_cols(test_data) 

return(model_pred %>% 
  roc_auc(truth = Exited, .pred_0))
}

fitea_polySVM(1)
```

    ## maximum number of iterations reached 0.01489115 0.01201625

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.668

``` r
fitea_polySVM(2)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.804

``` r
fitea_polySVM(3)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.789

We see that the grade 2 model has an AUC of .80, equivalent to that of
grade 3
