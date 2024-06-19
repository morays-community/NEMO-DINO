import numpy as np
import torch

from model_gz21_eophis import FullyCNN
import transforms as transforms


#       Utils 
# -----------------
def Is_None(*inputs):
    """ Test presence of at least one None in inputs """
    return any(item is None for item in inputs)


#       Main Model Routines
# ------------------------------
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


# From https://github.com/chzhangudel/Forpy_CNN_GZ21/blob/smartsim/testNN.py
u_scale= 10
v_scale= 10
Su_scale= 1e-7
Sv_scale= 1e-7

#       Main Model Routines
# ------------------------------

@torch.no_grad()
def model_loading(weights_path='../weights/gz21_huggingface/low-resolution/files/trained_model.pth', device='cpu') : 
    net = FullyCNN(padding='same')
    print(f'Loading model from {weights_path}')
    model_weights = torch.load(weights_path, map_location=device)
    transformation = transforms.SoftPlusTransform()
    transformation.indices = [2, 3] # Careful if model change
    net.final_transformation = transformation
    net.load_state_dict(model_weights)
    net.eval()
    return net

net = model_loading()

@torch.no_grad()
def momentum_cnn(u, v, mask_u, mask_v):
    """ Take as input u and v fields and return corrected fields using GZ (2021)  """
    if Is_None([u, v]):
        return None
    else:
        global net
        for z in range(u.shape[2]):
            u_slice, v_slice = u[:,:,z]*u_scale, v[:,:,z]*v_scale
            u_slice, v_slice = torch.tensor(u_slice.astype(np.float32)), torch.tensor(v_slice.astype(np.float32))
            inputs = torch.stack([u_slice, v_slice])[None]
            r = net(inputs) # u, v -> s_x, s_y, std_x, std_y
            Su_mu, Sv_mu, Su_p, Sv_p = r[0, 0], r[0, 1], r[0, 2], r[0, 3]
            u_c = Su_scale * ( Su_mu + np.sqrt(1/Su_p)*torch.randn_like(Su_p) ).numpy()
            v_c = Sv_scale * ( Sv_mu + np.sqrt(1/Sv_p)*torch.randn_like(Sv_p) ).numpy()
            u[:,:,z] = u_c
            v[:,:,z] = v_c   
        return u*mask_u , v*mask_v
    
#einops.rearrange([u, v], 'l i j k -> k  l i j')

if __name__ == '__main__' : 
    u = np.random.rand(120, 100, 3).astype('float32')
    v = np.random.rand(120, 100, 3).astype('float32')
    mask_u = np.ones((120, 100, 3)).astype('float32')
    mask_v = np.ones((120, 100, 3)).astype('float32')
    n_u, n_v = momentum_cnn(u, v, mask_u, mask_v)
    print(f'Returned n_u : {n_u.shape} n_v : {n_v.shape}')
    print(f'Test successful')



