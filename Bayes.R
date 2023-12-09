
# Git Test 2

# Compute P(A)
p_A <- nrow(subset(where9am, location == "office")) / nrow(where9am)

# Compute P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / nrow(where9am)

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, location == "office" & daytype == "weekday")) / nrow(where9am)

# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
p_A_given_B

## Exercise 2
# Load the naivebayes package
library(naivebayes)

# Build the location prediction model
  locmodel <- naive_bayes(location ~ daytype, data = where9am)

# Predict Thursday's 9am location
  predict(locmodel, thursday9am)

# Predict Saturdays's 9am location
  predict(locmodel, saturday9am)


## Exercise 2
  # Examining "raw" probabilities

  # The naivebayes package offers several ways to peek inside a Naive Bayes model.

  # Typing the name of the model object provides the a priori (overall) and conditional probabilities
  # of each of the model's predictors. If one were so inclined, you might use these for calculating
  # posterior (predicted) probabilities by hand.

  # Alternatively, R will compute the posterior probabilities for you if the type = "prob" parameter is
  # supplied to the predict() function.

  # Using these methods, examine how the model's predicted 9am location probability varies from day-to-day.
  # The model locmodel that you fit in the previous exercise is in your workspace.

  # The 'naivebayes' package is loaded into the workspace
  # and the Naive Bayes 'locmodel' has been built

  # Examine the location prediction model
    locmodel

  # Obtain the predicted probabilities for Thursday at 9am
    predict(locmodel, thursday9am, type = "prob")

  # Obtain the predicted probabilities for Saturday at 9am
    predict(locmodel, saturday9am, type = "prob")


  ## Exercise
  # A more sophisticated location model
  # The locations dataset records Brett's location every hour for 13 weeks. Each hour, the tracking information
  # includes the daytype (weekend or weekday) as well as the hourtype (morning, afternoon, evening, or night).
  # Using this data, build a more sophisticated model to see how Brett's predicted location not only varies
  # by the day of week but also by the time of day. The dataset locations is already loaded in your workspace.
  # You can specify additional independent variables in your formula using the + sign (e.g. y ~ x + b).

  # The 'naivebayes' package is loaded into the workspace already

  # Build a NB model of location
    locmodel <- naive_bayes(location ~ daytype + hourtype, data = locations)

  # Predict Brett's location on a weekday afternoon
    predict(locmodel, weekday_afternoon)

  # Predict Brett's location on a weekday evening
    predict(locmodel, weekday_evening)

## Exercise
    Preparing for unforeseen circumstances

    While Brett was tracking his location over 13 weeks, he never went into the office during the weekend. Consequently, the joint probability of P(office and weekend) = 0.

    Explore how this impacts the predicted probability that Brett may go to work on the weekend in the future. Additionally, you can see how using the Laplace correction will allow a small chance for these types of unforeseen circumstances.

    The model locmodel is already in your workspace, along with the dataframe weekend_afternoon.

  # The 'naivebayes' package is loaded into the workspace already
  # The Naive Bayes location model (locmodel) has already been built

  # Observe the predicted probabilities for a weekend afternoon
    predict(locmodel, weekend_afternoon, type = "prob")

  # Build a new model using the Laplace correction
    locmodel2 <- naive_bayes(location ~ daytype + hourtype, data = locations, laplace = 1)

  # Observe the new predicted probabilities for a weekend afternoon
    predict(locmodel2, weekend_afternoon, type = "prob")




    k_1 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types)
    mean(signs_actual == k_1)
