Churn_Tree
================

For the Decision Trees chapter we are going to continue using Churn
data, or customer churn. Before loading the data, we invoke the
Tidyverse library. After loading the data, we cleaned it up a bit and
took a look at it.

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

We generate the same dataset from the previous class but with a single
line of code.

Now, to implement the model we are going to use a library called
tidymodels, which allows us to unify different Machine Learning
libraries present in R. We also load the discrim library, which has
complementary classification models to tidymodels.

The first step we do is to separate the data into training and test
sets, where tidymodels has the initial_split function.

``` r
library(tidymodels)
library(discrim) 

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

Next, we create the “recipe” of the model, which consists of the “black
box” relationship between the input variables and the output variables.
In this case, the recipe will be to model Exited based on all the
variables present in the data set.

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

Now if we create the model, where we will use a decision tree with 5
decision layers, and a minimum number of entities per leaf (pruning) of
10. The library used to calculate this model will be rpart, which comes
preloaded in the packages we are using. With this step we only define
the model, we still calculate it.

``` r
modelo <-
  decision_tree(tree_depth = 5, min_n = 10) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo
```

    ## Decision Tree Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   tree_depth = 5
    ##   min_n = 10
    ## 
    ## Computational engine: rpart

Now we fit the model, calculate its predictions and calculate the AUC
value

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

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.747

Now we will see the magic of tidymodels, we will make a comparison with
other models, such as the logistic regression model, naïve Bayes or Knn.
For this, the only thing we need to change is the model, since the
recipe is the same, and the validation flow is also the same. Therefore
we can use the function we created above to evaluate the different
models and compare them.

``` r
modelo_rl <- 
  logistic_reg() %>% 
  set_engine("glm")

fitea(modelo_rl)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.772

``` r
modelo_nb <-
  naive_Bayes(smoothness = .8) %>%
  set_engine("naivebayes")

fitea(modelo_nb)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.849

``` r
modelo_knn <-
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

fitea(modelo_knn)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.765

We can see that in this case the Naive Bayes model obtains the best
results when classifying with an AUC of .84.
