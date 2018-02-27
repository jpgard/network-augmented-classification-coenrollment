# # neural net ===================================================================================================================================
source("preproc_utils.R")
source("modeling_utils.R")
# load everything
for (x in list.files(pattern = ".*\\.Rdata")){
    load(x)
}
N_LAYERS = 3 # number of layers for neuralnet; later this will be parameter passed to function
TRAIN_THRESH = 3.0
library(neuralnet)
train_nn = make_nn_matrix(probe_preds)
ensemble_nn = neuralnet(
    paste(paste(train_nn[["y_names"]], collapse='+'),
          '~',
          paste(train_nn[["x_names"]], collapse='+')),
    train_nn[["mx"]],
    hidden = rep(length(train_nn[["x_names"]]), N_LAYERS),
    linear.output=FALSE,
    lifesign='full',
    lifesign.step=100,
    threshold = TRAIN_THRESH
    )
# get result on training data
ensemble_nn_train_preds = predict_nn(ensemble_nn, X)
table(Predicted = ensemble_nn_train_preds, Expected = probe_preds$CRSE_GRD_OFFCL_CD)
ensemble_nn_acc = sum(ensemble_nn_train_preds == as.numeric(probe_preds$CRSE_GRD_OFFCL_CD) - 1)/length(ensemble_nn_train_preds)
ensemble_nn_acc
save(ensemble_nn, file="ensemble_nn.Rdata")

# # predict in neural net ensemble model
X_test = as.matrix(subset(test_preds, select = -c(CRSE_GRD_OFFCL_CD)))
Y_test = as.numeric(test_preds$CRSE_GRD_OFFCL_CD) - 1

test_nn = make_nn_matrix(test_preds)
ensemble_nn_test_preds = predict_nn(ensemble_nn, X_test)
ensemble_nn_acc = sum(ensemble_nn_test_preds == Y_test)/length(ensemble_nn_test_preds)
ensemble_nn_acc
save(ensemble_nn_test_preds, file = "ensemble_nn_test_preds.Rdata")

test_results = data.frame(model = "acc_ensemble_neuralnet_test", acc = ensemble_nn_acc)
test_results
write.csv(test_results, file = "test_results_neuralnet.csv", row.names = FALSE)
mm_list = list()
mm_list[["nn_ensemble"]] = fetch_multiclass_metrics(labs = Y_test, preds=ensemble_nn_test_preds, valid_labs = c(0,1,2,3))
mm = bind_rows(mm_list, .id = "model")
mm
write.csv(mm, file = "multiclass_metrics_neuralnet.csv", row.names = FALSE)

