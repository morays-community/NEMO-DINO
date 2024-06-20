using Flux, JLD2, Statistics
include("fcnn.jl")

model=get_model("DINO.GZ21/INFERENCES/julia/model_weights.jld2")

input = ones(Float32, 128, 128, 2, 10);


out = model(input);
out = activation(out);
print(mean(out, dims=(1,2,4)))

u_scale, v_scale, Su_scale, Sv_scale = 10, 10, 1e-7, 1e-7

