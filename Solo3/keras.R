library(keras)

train_mat <- model.matrix(Y ~ . - 1, training_df)
test_mat <- model.matrix(Y ~ . - 1, test_df)
train_y <- as.numeric(training_df$Y)-1
test_y <-  as.numeric(test_df$Y)-1

# Normalize training data
train_mat <- scale(train_mat)
# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_mat, "scaled:center")
col_stddevs_train <- attr(train_mat, "scaled:scale")
test_mat <- scale(test_mat, center = col_means_train, scale = col_stddevs_train)

dim(train_mat); dim(test_mat)

model <- keras_model_sequential()

model %>%
    layer_dense(units = 64, activation = 'relu', input_shape = dim(train_mat)[2]) %>%
    layer_dense(units = 30, activation = 'relu') %>%
    layer_dense(units = 1)

model %>% summary()

model %>% compile(
    optimizer = 'adam',
    loss = 'sparse_categorical_crossentropy',
    metrics = c('accuracy')
)

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
        if (epoch %% 80 == 0) cat("\n")
        cat(".")
    }
)

epochs <- 10

# Fit the model and store training stats
history <- model %>% fit(
    train_mat, train_y,
    epochs = epochs,
    validation_split = 0.2,
    verbose = 0,
    callbacks = list(print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
    coord_cartesian(ylim = c(0, 5))

score <- model %>% evaluate(test_mat, test_y)

cat('Test loss:', score$loss, "\n")
cat('Test accuracy:', score$acc, "\n")

predictions <- model %>% predict(test_mat)
class_pred <- model %>% predict_classes(test_mat)
class_pred[1:20]
