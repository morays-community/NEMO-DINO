import numpy as np
import sys
import torch
import pickle
import json
import sys
sys.path.append('gz21_ocean_momentum/src/')

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


# From https://zenodo.org/records/7663062
u_scale=1/0.10278768092393875 # ~ 10
v_scale=1/0.07726840674877167 # ~ 12
world_radius_in_meters=6.371e6
angle_to_meters=world_radius_in_meters*2*np.pi/360 
Su_scale=0.004745704121887684/angle_to_meters # ~4e-8
Sv_scale=0.004386111628264189/angle_to_meters # ~4e-8

#       Main Model Routines
# ------------------------------
@torch.no_grad()
def momentum_cnn(u, v, mask_u, mask_v):
    """ Take as input u and v fields and return corrected fields using GZ (2021)  """
    if Is_None([u, v]):
        return None
    else:
        for z in range(u.shape[2]):
            u_slice, v_slice = u[:,:,z]*u_scale, v[:,:,z]*v_scale
            u_slice, v_slice = torch.tensor(u_slice.astype(np.float32)), torch.tensor(v_slice.astype(np.float32))
            inputs = torch.stack([u_slice, v_slice])[None]
            net = model_loading()
            r = net(inputs) # u, v -> s_x, s_y, std_x, std_y
            Su_mu, Sv_mu, Su_std, Sv_std = r[0, 0], r[0, 1], r[0, 2], r[0, 3]
            u_c = Su_scale * ( u_slice + Su_mu + np.sqrt(1/Su_std)*torch.randn_like(Su_std) ).numpy()
            v_c = Sv_scale * ( v_slice + Sv_mu + np.sqrt(1/Sv_std)*torch.randn_like(Sv_std) ).numpy()
            u[:,:,z] = u_c
            v[:,:,z] = v_c
            
            
        return u*mask_u , v*mask_v
    
@torch.no_grad()
def model_loading(weights_path='weights/gz21_huggingface/low-resolution/files/', device='cpu') : 
    net = FullyCNN(padding='same')
    model_weights = torch.load(weights_path +'trained_model.pth', map_location=device)
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



