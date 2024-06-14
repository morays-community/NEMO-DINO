using HDF5, JLD2, Flux
include("fcnn.jl")

weights = h5open("../weights/gz21_huggingface/low-resolution/files/model_weights.h5", "r")

keys_w =["0.weight","0.bias","2.weight","2.bias","4.weight","4.bias","6.weight","6.bias",
 "8.weight","8.bias","10.weight","10.bias","12.weight","12.bias", "14.weight","14.bias"]

model=get_model()

p = Flux.params(model);
for (i, k) in enumerate(keys_w)
   p[i] .= read(weights[k])
end

jldsave("../weights/gz21_huggingface/low-resolution/files/model_weights.jld2";  model_state=Flux.state(model))
