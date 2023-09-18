Churn_NeuralNetworks
================

For the neural networks chapter we are going to continue using Churn
data, or customer churn. Before loading the data, we invoke the
Tidyverse library. After loading the data, we cleaned it up a bit and
took a look at it.

``` r
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

We are going to implement the RRNN using the tidymodels library, to
maintain the syntax that we have used until now

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

We will also use the fitea function that we created in the last class

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
```

Now we create the recipe, the same as the previous cases, and finally
the multi-layer perceptron (mlp) model.

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
modelo <- mlp(hidden_units  = 5) %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  translate()


fitea(modelo)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.572

We see the AUC is very poor. This is because neural networks need the
input data to be scaled between -1 and 1, given the nature of their
activation functions. we added the scaling steps in the recipe and
tested the same model.

``` r
receta <- 
  recipe(Exited ~ ., data = train_data) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

receta
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 9

    ## 

    ## ── Operations

    ## • Centering for: all_predictors()

    ## • Scaling for: all_predictors()

``` r
modelo <- mlp(hidden_units  = 5) %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  translate()


fitea(modelo)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.837

The AUC improved quite a bit, now we will see if the number of neurons
allows us to improve the result obtained so far. For this, we will
create a function that fits the model depending on the number of neurons
in the hidden layer.

``` r
fiteaNN <- function(hid){
  
  mod <- mlp(hidden_units  = hid) %>% 
  set_engine("nnet") %>% 
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


fiteaNN(5)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.844

``` r
fiteaNN(6)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.849

``` r
fiteaNN(7)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.845

``` r
fiteaNN(8)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.848

``` r
fiteaNN(9)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.845

``` r
fiteaNN(10)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.836

We see that a model with 5 hidden layers obtains better results than
other more complex models, so we should preserve this model since it has
fewer parameters and therefore is more stable.

For more complex network configurations we should use the tensorflow
library, which I will leave as homework

``` r
library(tensorflow)
```

    ## Warning: package 'tensorflow' was built under R version 4.3.1

``` r
# https://colorado.rstudio.com/rsc/churn/modeling/tensorflow-w-r.nb.html
```
