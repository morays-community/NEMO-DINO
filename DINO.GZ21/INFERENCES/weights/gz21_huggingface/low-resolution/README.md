Commit: `87648f076afc5144baf8f7408077a1f723e037ea`

Command:

```
mlflow run . --experiment-name raehik -e train --env-manager=local \
-P exp_id=835131858780593246 -P run_id=135f3833ee794e50b872bea33f6cf993 \
-P learning_rate=0/5e-4/15/5e-5/30/5e-6 -P n_epochs=200 -P weight_decay=0.00 -P train_split=0.8 \
-P test_split=0.85 -P model_module_name=models.models1 -P model_cls_name=FullyCNN -P batchsize=4 \
-P transformation_cls_name=SoftPlusTransform -P submodel=transform3 \
-P loss_cls_name=HeteroskedasticGaussianLossV2
```

## Forcing data
Commit: `87648f076afc5144baf8f7408077a1f723e037ea`

Command:

```
mlflow run . --experiment-name raehik --env-manager=local \
-P lat_min=-25 -P lat_max=25 -P long_min=-280 -P long_max=80 \
-P factor=4 -P chunk_size=1 -P CO2=1 -P global=0 -P ntimes=100
```
