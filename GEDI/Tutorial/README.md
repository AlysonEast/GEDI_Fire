# Getting Started with GEDI L1B, L2A, and L2B Data in Python Tutorial Series
---
# Objective:
The Getting Started with GEDI Tutorial Series aims to teach GEDI users how to get started with GEDI L1-L2 files in Python. The tutorials consist of a three-part series of Jupyter Notebooks split by product. The tutorials can be completed sequentially or individually for a specific product. The series explains how to open, visualize, subset, and export GEDI L1-L2 datasets through a use case example centered on observing Redwood forests inside of Redwood National Park in northern California.
## Tutorials:  
 - The [Getting Started with GEDI L1B Data in Python Jupyter Notebook](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/GEDI_L1B_Tutorial.ipynb) shows how to use Python to open GEDI L1B files, visualize the full orbit of GEDI points (shots), subset to a region of interest, visualize GEDI full waveforms, and export subsets of GEDI science dataset (SDS) layers as GeoJSON files that can be loaded into GIS and/or Remote Sensing software programs.  
 - The [Getting Started with GEDI L2A Data in Python Jupyter Notebook](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/GEDI_L2A_Tutorial.ipynb) shows how to use Python to open GEDI L2A files, visualize the full orbit of GEDI points (shots), subset to a region of interest, visualize GEDI canopy height, and export subsets of GEDI science dataset (SDS) layers as GeoJSON files that can be loaded into GIS and/or Remote Sensing software programs.  
 - The [Getting Started with GEDI L2B Data in Python Jupyter Notebook](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/GEDI_L2B_Tutorial.ipynb) shows how to use Python to open GEDI L2B files, visualize the full orbit of GEDI points (shots), subset to a region of interest, visualize GEDI canopy height and vertical profile metrics, and export subsets of GEDI science dataset (SDS) layers as GeoJSON files that can be loaded into GIS and/or Remote Sensing software programs.     

## Products Used:
**1. [GEDI01_B.001](https://doi.org/10.5067/GEDI/GEDI01_B.001)**   
**2. [GEDI02_A.001](https://doi.org/10.5067/GEDI/GEDI02_A.001)**   
**3. [GEDI02_B.001](https://doi.org/10.5067/GEDI/GEDI02_B.001)**     

---
# Prerequisites:
*Disclaimer: These tutorials have been tested on Windows and MacOS using the specifications identified below.*  
+ #### Python Version 3.7  
  + `h5py`  
  + `shapely`  
  + `geopandas`  
  + `pandas`  
  + `geoviews`  
  + `holoviews`    
---
# Procedures:
## Getting Started:
#### 1. These tutorials use the GEDI L1B, L2A, and L2B observations from June 19, 2019 (orbit 02932). Use the links below to download the files directly from the LP DAAC Data Pool:   
 - https://e4ftl01.cr.usgs.gov/GEDI/GEDI01_B.001/2019.06.19/GEDI01_B_2019170155833_O02932_T02267_02_003_01.h5   (7.87 GB)  
 - https://e4ftl01.cr.usgs.gov/GEDI/GEDI02_A.001/2019.06.19/GEDI02_A_2019170155833_O02932_T02267_02_001_01.h5   (5.93 GB)    
 - https://e4ftl01.cr.usgs.gov/GEDI/GEDI02_B.001/2019.06.19/GEDI02_B_2019170155833_O02932_T02267_02_001_01.h5   (1.33 GB)  
 - Ancillary Files Needed:  
    - [RedwoodNP.geojson](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/RedwoodNP.geojson)  (for all tutorials)
    - [waveform.csv](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/waveform.csv)  (for L2A tutorial only)  

 Note that you only need to download the GEDI file corresponding to the GEDI Product for each tutorial (ex: if you only plan to execute GEDI L2B tutorial, only download the L2B file and RedwoodNP.geojson file).  
#### 2.	Copy/clone/download the [GEDI Tutorial repo](https://git.earthdata.nasa.gov/rest/api/latest/projects/LPDUR/repos/gedi-tutorials/archive?format=zip), or the desired tutorial from the LP DAAC Data User Resources Repository:   
 -  [Getting Started with GEDI L1B Data in Python Jupyter Notebook](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/GEDI_L1B_Tutorial.ipynb)   
 -  [Getting Started with GEDI L2A Data in Python Jupyter Notebook](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/GEDI_L2A_Tutorial.ipynb)   
 -  [Getting Started with GEDI L2B Data in Python Jupyter Notebook](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/GEDI_L2B_Tutorial.ipynb)   
## Python Environment Setup
> #### 1. It is recommended to use [Conda](https://conda.io/docs/), an environment manager, to set up a compatible Python environment. Download Conda for your OS [here](https://www.anaconda.com/download/). Once you have Conda installed, Follow the instructions below to successfully setup a Python environment on Windows, MacOS, or Linux.
> #### 2. Setup  
> - Using your preferred command line interface (command prompt, terminal, cmder, etc.) type the following to successfully create a compatible python environment:
>   - `conda create -n geditutorial -c conda-forge --yes python=3.7 h5py shapely geopandas pandas geoviews holoviews`   
>   - `conda activate geditutorial`  
>   - `jupyter notebook`  

> If you do not have jupyter notebook installed, you may need to run:  
 > - `conda install jupyter notebook`  

  TIP: Having trouble activating your environment, or loading specific packages once you have activated your environment? Try the following:
  > Type: 'conda update conda'    

If you prefer to not install Conda, the same setup and dependencies can be achieved by using another package manager such as pip and the [requirements.txt file](https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-tutorials/browse/requirements.txt) listed above.  
[Additional information](https://conda.io/docs/user-guide/tasks/manage-environments.html) on setting up and managing Conda environments.  
#### Still having trouble getting a compatible Python environment set up? Contact [LP DAAC User Services](https://lpdaac.usgs.gov/lpdaac-contact-us/).    
---
# Contact Information:
#### Author: Cole Krehbiel¹   
**Contact:** LPDAAC@usgs.gov  
**Voice:** +1-866-573-3222  
**Organization:** Land Processes Distributed Active Archive Center (LP DAAC)  
**Website:** https://lpdaac.usgs.gov/  
**Date last modified:** 05-11-2020  

¹KBR, Inc., contractor to the U.S. Geological Survey, Earth Resources Observation and Science (EROS) Center,  
 Sioux Falls, South Dakota, USA. Work performed under USGS contract G15PD00467 for LP DAAC².  
²LP DAAC Work performed under NASA contract NNG14HH33I.
