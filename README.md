This repository describes the workflow used to analyze morphological changes in the small carpenter bee over the past 118 years. Methods are described in the manuscript "Opposing pressures of climate and land‐use change on a native bee" (Kelemen and Rehan 2021).


**Overview**

Morphological data was loaded and formatted in **morphologicaldata.R**

Climate variables was extracted from raster files in ArcGIS Pro using in **climate_variables.py**
Land-use data was extracted from raster files in ArcGIS Pro using in **landuse_buffers.py**

Land-use data was added to the morphological data in **Land.Use.Combining.R**, which calls the function **Combining.temperature.data.R** and requires **CombiningRawLandUseFiles.R** to be run ahead of time.

Descriptive statistics of the morphological data was calculated in **Descriptive Stats.R**
Analysis associated with Figure 2, testing if land-use and temperature changed through time is calculate in **LandUseandTempThroughTime.R.**

Liner mixed models test if body size (head width) of the small carpenter bee has change through time or with environmental factors is contained in **Analyzing.Morphological.Data.RAC.LME.R**. These models control for spatial correlation using the function calculate_rac in **calulate_rac.R**. The code for the subsequent Figures 3 and 4 based on these analyses are in **Analyzing.Morphological.Data.RAC.Figure3.4.R.** The effects of both temperature and land-use are plotted together in **Figure5_CumulativePlot.R**

Additional analyses were performed sex-specific subsets of the data in **Sex_specific_Analyses.R** along with changes in intertegular width and wing length in **IntertegularWidth_RightWings.R**
