using Flux: Conv, relu, Chain
using Flux.Optimise: softplus

function activation(x, precision_indices=range(3,4), min_value=0.0015)
    x[:, :, precision_indices, :] .= softplus.(x[:, :, precision_indices, :]) .+ min_value
    return x 
end

function get_model(weight_path=nothing) 
    # Define the network structure
    model = Chain(
        Conv((5, 5), 2 => 128, pad = (2, 2)), relu,
        Conv((5, 5), 128 => 64, pad = (2, 2)), relu,
        Conv((3, 3), 64 => 32, pad = (1, 1)), relu,
        Conv((3, 3), 32 => 32, pad = (1, 1)), relu,
        Conv((3, 3), 32 => 32, pad = (1, 1)), relu,
        Conv((3, 3), 32 => 32, pad = (1, 1)), relu,
        Conv((3, 3), 32 => 32, pad = (1, 1)), relu,
        Conv((3, 3), 32 => 4, pad = (1, 1)))
    if !isnothing(weight_path) 
        @info "Loading model : ", weight_path
        model_state = JLD2.load(weight_path, "model_state");
        Flux.loadmodel!(model, model_state);
    end
    return model
end




