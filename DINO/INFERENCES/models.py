import numpy as np
import sys
import torch


sys.path.append('./gz21_ocean_momentum/src/')

from gz21_ocean_momentum.models.fully_conv_net import FullyCNN
import gz21_ocean_momentum.models.transforms as transforms
import gz21_ocean_momentum.train.losses as loss_funcs
#import submeso_ml.systems.regression_system as regression_system
#import submeso_ml.models.fcnn as fcnn
#import submeso_ml.data.dataset as dataset


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
def momentum_cnn(u, v):
    """ Take as input u and v fields and return corrected fields using GZ (2021)  """
    if Is_None([u, v]):
        return None
    else:
        u, v = torch.tensor(u), torch.tensor(v)
        inputs = torch.stack([u, v])[None]
        net = FullyCNN(2, 4, padding='same').eval()
        transformation = transforms.SoftPlusTransform()
        transformation.indices = [2, 3] # What to put here ? 
        net.final_transformation = transformation
        r = net(inputs)
        Su_mu, Sv_mu, Su_std, Sv_std = r[0, 0], r[0, 1], r[0, 2], r[0, 3]
        u_c = Su_mu + Su_std*torch.randn_like(Su_std)
        v_c = Sv_mu + Su_std*torch.randn_like(Sv_std)
        return (u+u_c).numpy(), (v+v_c).numpy()
    


if __name__ == '__main__' : 
    u = np.random.rand(120, 100).astype('float32')
    v = np.random.rand(120, 100).astype('float32')
    n_u, n_v = momentum_cnn(u, v)
    print(f'Returned n_u : {n_u.shape} n_v : {n_v.shape}')
