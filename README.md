# CGRC_prescribed_disturbance
data summary and analysis for prescribed burning and grazing at ACR's Cypress Grove Preserve


Vegetation sampling generally follows the Point Blue Rangeland Monitoring Network protocol.
See here for the field protocol: http://pointblue.org/wp-content/uploads/2018/06/RMN_Handbook_v2.pdf
But note that Cypress Grove veg points have 3 lines instead of 2

And here for some code to process those data: https://github.com/pointblue/RMN.functions/tree/master
The CGRC_prescribed_disturbance repo has code modified for our purposes from a previous version of RMN.functions 

The main functions here are separated into 4 files:  
* bird_functions.R  
* soil_functions.R  
* veg_functons.R  
* general_functions.R - this file contains tools that may be used on veg, birds, or soil.

Other code files call these functions for various purposes:
* summarize_veg.R provides basic visualizations of the plant data
* summarize_birds.R provides basic visualizations of the bird data  
* plant_treatment_response.RMD summarizes veg data on treated and untreated plots in a BACI framework


