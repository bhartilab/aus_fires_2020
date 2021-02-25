# aus_fires_2020
code to accompany "Quantifying the impacts of Australian bushfires on native forests and grey-headed flying foxes" by Baranowski, Faust, Eby and Bharti

Data for this manuscript come from open source data:
1. Fire occurrence data (VIIRS Near Real-Time FIRMS Data): 
    https://earthdata.nasa.gov/earth-observation-data/near-real-time/firms
        2021-09-01 to 2020-01-06 downloaded on 06 January 2020
        2020-01-07 to 2020-01-15 downloaded on 15 January 2020
        2012-09-01 to 2013-03-01 downloaded 15 January 2020
        2020-01-16 to 2020-03-01 downloaded 06 March 2020

2. Fire intensity data from New South Wales for 2019-2020 fires (no version associated with files):
    https://data.nsw.gov.au/data/dataset/fire-extent-and-severity-mapping-fesm 
    downloaded on 14 December 2020
    
3. Global Forest Cover Change version 1.6:
      https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.6.html

4. National Vegetation Information System (NVIS) version 5.1:
    http://environment.gov.au/fed/catalog/search/resource/downloadData.page?uuid=%7B991C36C0-3FEA-4469-8C30-BB56CC2C7772%7D

5. Flying Fox Monitoring Program data: 
      https://www.data.qld.gov.au/dataset/flying-fox-monitoring-program
      downloaded on 10 December 2019
      
6. Flying Fox habitat spatial data
      downloaded on 

The above data was processed and visualized using a combination of ArcGIS (fire severity and NVIS was analyzed in version 2.6, all other data was analyzed in  version 2.5) and R version 3.7. All required input data are provided in the data folder. R code to process spatial products and analyze data are in the following scripts: 

1. ghff_2012_2013_fire_response.R: mixed effect models to evaluate factors affecting grey-headed flying fox (GHFF) response to fires
2. burned_habitat_comparisons.R: GAMs (generalised additive models) to explain variation in burned foraging habitat between previous focal and anomalous fire seasons
3. fire_and_forest_vizualizations.R: plots vizualizing main text figures 
