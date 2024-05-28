import os
import argparse
import numpy as np
import xarray as xr
import cmocean

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.colors as colors


def make_plot(data,lon,lat,infos,output):
    # args
    title, cmap, norm, tfs = infos
    data = tfs(data)
    # figure
    plt.figure(figsize=(12, 8))
    ax = plt.axes()
    # color map
    pcm = ax.pcolormesh(lon, lat, data, cmap=cmap, norm=norm)
    cbar = plt.colorbar(pcm, ax=ax, orientation='vertical', pad=0.05, shrink=0.5)
    plt.title(title)
    # write fig
    plt.savefig(output, bbox_inches='tight')
    plt.close()


def main(filepath, var_name, infos):

    print(f'Plotting {var_name}')

    # read files
    ds = xr.open_dataset(filepath)
    lon = ds.nav_lon.values
    lat = ds.nav_lat.values

    # get fields
    var_val = getattr(ds,var_name).values[-1]

    # plot
    plotpath = 'NEVERWORLD.GZ21_' + var_name + '.png'
    make_plot(var_val,lon,lat,infos,plotpath)



if __name__=="__main__":

    # usurf
    infos = [ 'u (m/s)' , cmocean.cm.balance , colors.Normalize(vmin=-0.10, vmax=0.10), lambda x: x ]
    main( filepath='NEVERWORLD_gridUsurf.nc' , var_name='sozocrtx' , infos=infos )

    # vsurf
    infos = [ 'v (m/s)' , cmocean.cm.balance , colors.Normalize(vmin=-0.10, vmax=0.10), lambda x: x ]
    main( filepath='NEVERWORLD_gridVsurf.nc' , var_name='somecrty' , infos=infos )

    # uf
    infos = [ 'uf (m/s)' , cmocean.cm.balance , colors.Normalize(vmin=-0.000001, vmax=0.000001), lambda x: x ]
    main( filepath='NEVERWORLD_gridUsurf.nc' , var_name='soext_uf' , infos=infos )

    # vf
    infos = [ 'vf (m/s)' , cmocean.cm.balance , colors.Normalize(vmin=-0.000001, vmax=0.000001), lambda x: x ]
    main( filepath='NEVERWORLD_gridVsurf.nc' , var_name='soext_vf' , infos=infos )
