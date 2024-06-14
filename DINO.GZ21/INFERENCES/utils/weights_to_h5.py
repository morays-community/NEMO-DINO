import torch 
import h5py

def model_weights_to_h5(weights_path='../weights/gz21_huggingface/low-resolution/files/') :     
    model_weights = torch.load(weights_path +'trained_model.pth', map_location='cpu')
    beta = model_weights['final_transformation.min_value'].numpy()
    weight_dict = {k:v.numpy() for k,v in model_weights.items()}
    weight_dict['final_act.index'] = [2,3]
    weight_dict['final_act.min_value'] = beta

    hf = h5py.File(weights_path + 'model_weights.h5', 'w')

    for k,v in weight_dict.items() : 
        hf.create_dataset(k, data=v)
    hf.close()


if __name__=='__main__' : 
    model_weights_to_h5()


