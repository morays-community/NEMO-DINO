import numpy as np
import sys
import torch

from gz21_ocean_momentum.models.fully_conv_net import FullyCNN
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
    """ Take as input u and v fields and return corrected fields using GZ (2021)  """
    if Is_None([u, v]):
        return None
    else:
        fake_weight = 0.0001
        for z in range(u.shape[2]):
            u_slice, v_slice = u[:,:,z], v[:,:,z]
            u_slice, v_slice = torch.tensor(u_slice.astype(np.float32)), torch.tensor(v_slice.astype(np.float32))
            inputs = torch.stack([u_slice, v_slice])[None]
            net = FullyCNN(2, 4, padding='same').eval()
            transformation = transforms.SoftPlusTransform()
            transformation.indices = [2, 3] # What to put here ? 
            net.final_transformation = transformation
            r = net(inputs)
            Su_mu, Sv_mu, Su_std, Sv_std = r[0, 0], r[0, 1], r[0, 2], r[0, 3]
            u_c = fake_weight * ( u_slice + Su_mu + Su_std*torch.randn_like(Su_std) ).numpy()
            v_c = fake_weight * ( v_slice + Sv_mu + Su_std*torch.randn_like(Sv_std) ).numpy()
            u[:,:,z] = u_c
            v[:,:,z] = v_c
        return u*mask_u , v*mask_v
    


if __name__ == '__main__' : 
    u = np.random.rand(120, 100, 3).astype('float32')
    v = np.random.rand(120, 100, 3).astype('float32')
    mask_u = np.ones((120, 100, 3)).astype('float32')
    mask_v = np.ones((120, 100, 3)).astype('float32')
    n_u, n_v = momentum_cnn(u, v, mask_u, mask_v)
    print(f'Returned n_u : {n_u.shape} n_v : {n_v.shape}')
    print(f'Test successful')
