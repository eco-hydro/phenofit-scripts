
# phenofit-examples

A state-of-the-art **remote sensing vegetation phenology** extraction
package: `phenofit` V3.0


**Table 1**. Illustration of `phenofit` examples.

| Path                         | Description                         | INPUT               |
| --------------------------   | ----------------------------------- | ------------------- |
| `multiGS_MODIS_Henan`        | Spatial Application: Henan Province                      | MOD13A2 EVI, 1km    |
| `multiGS_Sentinel2_KongYing` | Spatial Application: KongYing (a small village in Henan) | Sentinel-2 EVI, 10m |
| `paper-phenofit`             | site-scale: CA-NS6, the location of one flux site | MOD13A1 EVI, 500m |
| `PhenoCam`                   | site-scale: PhenoCam bartlett2009                 | `gcc` |

## Spatial Application: Preparing Input Data on the GEE

- `MOD13A2 EVI`: <https://code.earthengine.google.com/350aed10705fd98056a4b8c355a1a825>
- `Sentinel-2 EVI`: <https://code.earthengine.google.com/c9d04866f70a19c4e1cf8588af7aedc5>, `multiGS_Sentinel2_KongYing\GEE_download_sentinel2_kong.js`

# **References**

> \[1\] Kong, D., Zhang, Y., Wang, D., Chen, J., & Gu, X. (2020).
> Photoperiod Explains the Asynchronization Between Vegetation Carbon
> Phenology and Vegetation Greenness Phenology. *Journal of Geophysical
> Research: Biogeosciences*, 125(8), e2020JG005636.
> <https://doi.org/10.1029/2020JG005636>
>
> \[2\] Kong, D., Zhang, Y., Gu, X., & Wang, D. (2019). A robust method
> for reconstructing global MODIS EVI time series on the Google Earth
> Engine. *ISPRS Journal of Photogrammetry and Remote Sensing*, 155,
> 13–24.
>
> \[3\] Kong, D., (2020). R package: A state-of-the-art Vegetation
> Phenology extraction package, `phenofit` version 0.2.6,
> <https://doi.org/10.5281/zenodo.3605560>
>
> \[4\] Zhang, Q., Kong, D., Shi, P., Singh, V.P., Sun, P., 2018.
> Vegetation phenology on the Qinghai-Tibetan Plateau and its response
> to climate change (1982–2013). Agric. For. Meteorol. 248, 408–417.
> <https://doi.org/10.1016/j.agrformet.2017.10.026>

# Acknowledgements

Keep in mind that this repository is released under a GPL2 license,
which permits commercial use but requires that the source code (of
derivatives) is always open even if hosted as a web service.
