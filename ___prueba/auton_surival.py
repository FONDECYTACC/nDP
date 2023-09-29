# -*- coding: utf-8 -*-
"""
Created on Wed Sep 27 19:57:45 2023

@author: andre
"""

#https://www.cs.cmu.edu/~chiragn/papers/iccai_22_abstract.pdf
#https://youtu.be/ZerjjsJj_dE
#https://www.cs.cmu.edu/~chiragn/papers/auton_survival.pdf
#https://nbviewer.org/github/autonlab/auton-survival/blob/master/examples/Phenotyping%20Censored%20Time-to-Events.ipynb
#https://github.com/autonlab/auton-survival/blob/master/examples/Phenotyping%20Censored%20Time-to-Events.ipynb
pip install auton_survival 
pip install git+https://github.com/autonlab/auton-survival
#ERROR: Ignored the following versions that require a different python version: 0.27.3 Requires-Python >=3.7, !=3.11.*
#ERROR: Could not find a version that satisfies the requirement torch<2.0,>=1.13 (from auton-survival) (from versions: 2.0.0, 2.0.1)
#ERROR: No matching distribution found for torch<2.0,>=1.13

pip install torch==1.13.1+cpu torchvision==0.14.1+cpu torchaudio==0.13.1 --extra-index-url https://download.pytorch.org/whl/cpu
conda install pytorch==1.13.1 torchvision==0.14.1 torchaudio==0.13.1 pytorch-cuda=11.6 -c pytorch -c nvidia

import auton_survival

conda install auton_survival