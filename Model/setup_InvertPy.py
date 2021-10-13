# -*- coding: utf-8 -*-
"""
Created on Wed Oct  6 11:15:29 2021

@author: jaf54iq
"""

    #pip install git #not used
    # pip install numpy
    # pip install scipy
    # pip install ephem
    # pip install sklearn
    # pip install matplotlib

pip install git+https://github.com/Foztarz/InvertPy

#alternately, as suggested in https://github.com/Foztarz/InvertPy/blob/version-1.0-alpha/README.md
# in powershell
        # mkdir ~/src
        # cd ~/src
        # git clone https://github.com/InsectRobotics/InvertPy.git
        # cd InvertPy
        # conda env create -f environment.yml
        # conda install .
        
        #almost all worked
#(EyeModel) PS C:\Users\jaf54iq> mkdir ~/src


#     Directory: C:\Users\jaf54iq


# Mode                 LastWriteTime         Length Name
# ----                 -------------         ------ ----
# d-----        12/10/2021     16:15                src


# (EyeModel) PS C:\Users\jaf54iq> cd ~/src
# (EyeModel) PS C:\Users\jaf54iq\src> git clone https://github.com/InsectRobotics/InvertPy.git
# Cloning into 'InvertPy'...
# remote: Enumerating objects: 598, done.
# remote: Counting objects: 100% (598/598), done.
# remote: Compressing objects: 100% (340/340), done.
# Receiving objects:  96% (575/598), reused 445 (delta 214), pack-reused 0
# Receiving objects: 100% (598/598), 174.69 KiB | 2.03 MiB/s, done.
# Resolving deltas: 100% (352/352), done.
# (EyeModel) PS C:\Users\jaf54iq\src> cd InvertPy
# (EyeModel) PS C:\Users\jaf54iq\src\InvertPy> conda env create -f environment.yml
# Collecting package metadata (repodata.json): done
# Solving environment: done

# Downloading and Extracting Packages
# tzdata-2021c         | 120 KB    | ############################################################################ | 100%
# openssl-3.0.0        | 10.0 MB   | ############################################################################ | 100%
# joblib-1.1.0         | 210 KB    | ############################################################################ | 100%
# python_abi-3.9       | 4 KB      | ############################################################################ | 100%
# wheel-0.37.0         | 31 KB     | ############################################################################ | 100%
# sqlite-3.36.0        | 1.2 MB    | ############################################################################ | 100%
# vs2015_runtime-14.29 | 1.3 MB    | ############################################################################ | 100%
# liblapack-3.9.0      | 4.5 MB    | ############################################################################ | 100%
# msys2-conda-epoch-20 | 3 KB      | ############################################################################ | 100%
# intel-openmp-2021.3. | 3.0 MB    | ############################################################################ | 100%
# m2w64-gcc-libs-core- | 214 KB    | ############################################################################ | 100%
# mkl-2021.3.0         | 179.7 MB  | ############################################################################ | 100%
# tbb-2021.3.0         | 143 KB    | ############################################################################ | 100%
# ucrt-10.0.20348.0    | 1.2 MB    | ############################################################################ | 100%
# scikit-learn-1.0     | 6.9 MB    | ############################################################################ | 100%
# libcblas-3.9.0       | 4.5 MB    | ############################################################################ | 100%
# pip-21.3             | 1.2 MB    | ############################################################################ | 100%
# threadpoolctl-3.0.0  | 17 KB     | ############################################################################ | 100%
# vc-14.2              | 13 KB     | ############################################################################ | 100%
# m2w64-gcc-libgfortra | 342 KB    | ############################################################################ | 100%
# setuptools-58.2.0    | 1014 KB   | ############################################################################ | 100%
# numpy-1.21.2         | 5.6 MB    | ############################################################################ | 100%
# m2w64-libwinpthread- | 31 KB     | ############################################################################ | 100%
# python-3.9.7         | 20.1 MB   | ############################################################################ | 100%
# m2w64-gmp-6.1.0      | 726 KB    | ############################################################################ | 100%
# m2w64-gcc-libs-5.3.0 | 520 KB    | ############################################################################ | 100%
# scipy-1.7.1          | 24.6 MB   | ############################################################################ | 100%
# ca-certificates-2021 | 176 KB    | ############################################################################ | 100%
# libblas-3.9.0        | 4.5 MB    | ############################################################################ | 100%
# Preparing transaction: done
# Verifying transaction: done
# Executing transaction: done
# #
# # To activate this environment, use
# #
# #     $ conda activate invertpy
# #
# # To deactivate an active environment, use
# #
# #     $ conda deactivate

# (EyeModel) PS C:\Users\jaf54iq\src\InvertPy> conda install .
# Collecting package metadata (current_repodata.json): done
# Solving environment: failed with initial frozen solve. Retrying with flexible solve.
# Collecting package metadata (repodata.json): done
# Solving environment: failed with initial frozen solve. Retrying with flexible solve.

# PackagesNotFoundError: The following packages are not available from current channels:

#   - .

# Current channels:

#   - https://repo.anaconda.com/pkgs/main/win-64
#   - https://repo.anaconda.com/pkgs/main/noarch
#   - https://repo.anaconda.com/pkgs/r/win-64
#   - https://repo.anaconda.com/pkgs/r/noarch
#   - https://repo.anaconda.com/pkgs/msys2/win-64
#   - https://repo.anaconda.com/pkgs/msys2/noarch

# To search for alternate channels that may provide the conda package you're
# looking for, navigate to

#     https://anaconda.org

# and use the search bar at the top of the page.


(EyeModel) PS C:\Users\jaf54iq\src\InvertPy>
