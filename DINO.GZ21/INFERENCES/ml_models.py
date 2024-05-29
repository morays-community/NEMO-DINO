import numpy as np
import sys
import torch
import pickle
import sys

from gz21_ocean_momentum.models.models1 import FullyCNN
import gz21_ocean_momentum.models.transforms as transforms
import gz21_ocean_momentum.train.losses as loss_funcs


# ============================= #
# -- User Defined Parameters --
# ============================= #

# use GPUs if available
if torch.cuda.is_available():
    print("CUDA Available")
    device = torch.device('cuda')
else:
    print('CUDA Not Available')
    device = torch.device('cpu')


#       Utils 
# -----------------
def Is_None(*inputs):
    """ Test presence of at least one None in inputs """
    return any(item is None for item in inputs)

#       Main Model Routines
# ------------------------------
@torch.no_grad()
def momentum_cnn(u, v, mask_u, mask_v):
    """ Take as input u and v fields and return subgrid forcing fields using GZ (2021)  """
    if Is_None([u, v]):
        return None
    else:
        alpha = 0.0000001
        for z in range(u.shape[2]):
            normu, normv = np.std(u[:,:,z]), np.std(v[:,:,z])
            normu = 1.0 if normu == 0.0 else normu
            normv = 1.0 if normv == 0.0 else normv
            u_slice, v_slice = u[:,:,z] / normu, v[:,:,z] / normv
            u_slice, v_slice = torch.tensor(u_slice.astype(np.float32)), torch.tensor(v_slice.astype(np.float32))
            inputs = torch.stack([u_slice, v_slice])[None]
            net = model_loading()
            r = net(inputs) # u, v -> s_x, s_y, std_x, std_y
            Su_mu, Sv_mu, Su_std, Sv_std = r[0, 0], r[0, 1], r[0, 2], r[0, 3]
            fu = ( Su_mu + Su_std*torch.randn_like(Su_std) ).numpy()
            fv = ( Sv_mu + Su_std*torch.randn_like(Sv_std) ).numpy()
            u[:,:,z] = fu * alpha
            v[:,:,z] = fv * alpha
        return u*mask_u , v*mask_v
    
@torch.no_grad()
def model_loading(weights_path='weights/gz21_huggingface/low-resolution/files/', device='cpu') : 
    net = FullyCNN(padding='same')
    model_weights = torch.load('trained_model.pth', map_location=device)
    #net.final_transformation = pickle.load(open(weights_path+'transformation', 'rb')) 
    transformation = transforms.SoftPlusTransform()
    transformation.indices = [2, 3] # Careful if model change
    net.final_transformation = transformation
    net.load_state_dict(model_weights)
    net.eval() 
    return net 

if __name__ == '__main__' : 
    u = np.random.rand(120, 100, 3).astype('float32')
    v = np.random.rand(120, 100, 3).astype('float32')
    mask_u = np.ones((120, 100, 3)).astype('float32')
    mask_v = np.ones((120, 100, 3)).astype('float32')
    n_u, n_v = momentum_cnn(u, v, mask_u, mask_v)
    print(f'Returned n_u : {n_u.shape} n_v : {n_v.shape}')
    print(f'Test successful')
