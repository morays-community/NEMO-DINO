import numpy as np
import torch, einops

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
def momentum_cnn_old(u, v, mask_u, mask_v, sampling=True):
    """ Take as input u and v fields and return corrected fields using GZ (2021)  """
    u = np.copy(u) # Avoid inplace modification
    v = np.copy(v)
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
            u[:,:,z] = Su_scale * ( Su_mu + np.sqrt(1/Su_p)*torch.randn_like(Su_p)*sampling ).numpy()
            v[:,:,z] = Sv_scale * ( Sv_mu + np.sqrt(1/Sv_p)*torch.randn_like(Sv_p)*sampling ).numpy()
        return u*mask_u , v*mask_v
    
@torch.no_grad()
def momentum_cnn(u, v, mask_u, mask_v, sampling=True):
    """ Take as input u and v fields and return corrected fields using GZ (2021)

    Param :
        u (i, j, k)
        v (i, j, k)
        mask_u (i j k)
        mask_v (i j k)  
        sampling (bool) : to add random noise or not
    Out : 
        Su (i j k)
        Sv (i j k)
    """
    if Is_None([u, v]):
        return None
    else:
        global net
        inp = einops.rearrange([torch.tensor(u*u_scale),
                                torch.tensor(v*v_scale)], 'c i j k -> k c i j')
        r = net(inp)
        Su_mu, Sv_mu, Su_p, Sv_p = r[:, 0], r[:, 1], r[:, 2], r[:, 3] # k i j
        u = Su_scale * ( Su_mu + np.sqrt(1/Su_p)*torch.randn_like(Su_p)*sampling)
        v = Sv_scale * ( Sv_mu + np.sqrt(1/Sv_p)*torch.randn_like(Sv_p)*sampling)
        u = einops.rearrange(u, 'k i j -> i j k').numpy()
        v = einops.rearrange(v, 'k i j -> i j k').numpy()
        return u*mask_u , v*mask_v
    
#einops.rearrange([u, v], 'l i j k -> k  l i j')

if __name__ == '__main__' : 


    #u = np.random.rand(120, 100, 3).astype('float32') *100
    #v = np.random.rand(120, 100, 3).astype('float32') *100

    
    b, c, i, j = 1, 2, 10, 20
    def function_mat_python(b, c, i, j) : 
        return b*0.7 + c*0.1 + i*0.827 + j*0.193

    def create_mat_python(b, c, i, j) : 
        inp = torch.zeros((b,c, i ,j))
        for bi in range(0,b):
            for ci in range(0,c) :
                for ii in range(0,i):
                    for ji in range(0,j) :
                        inp[bi, ci, ii, ji] = function_mat_python(bi,ci,ii,ji)
        return inp

    inp = create_mat_python(b,c,i,j)
    
    u = inp[:,0].permute(1,2,0).numpy()
    v = inp[:,1].permute(1,2,0).numpy()
    
    mask_u = np.ones_like(u).astype('float32')
    mask_v = np.ones_like(v).astype('float32')
    n_u, n_v = momentum_cnn_old(u, v, mask_u, mask_v, sampling=False)

    n_u_new, n_v_new = momentum_cnn(u, v, mask_u, mask_v, sampling=False)
    print(f'Returned n_u : {n_u.shape} n_v : {n_v.shape}')
    

    print('Max diff n_u', np.max(np.abs(n_u - n_u_new)),'- max diff n_v', np.max(np.abs(n_v - n_v_new)))

    print(f'Test successful')