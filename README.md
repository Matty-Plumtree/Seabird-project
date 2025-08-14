# Seabird-project
Analysis code for MSc thesis 'Do seabirds habituate to displacement impacts from offshore windfarms'— Contains all R scripts, data processing workflows, statistical models (GLMMs), and visualisation code used to investigate habituation patterns in seabirds at the Kincardine Offshore Wind Farm (2022–2023).

Details of the follow scripts:

- buffer (This visualises and initialises the 15km buffer around the OWF)
- Depth (this loads and attributes a depth to each segments along the transect, this is then used in the GLMM as an environmental predictor)
- Distance to coast (this gives each seabird a distance to the nearest coastal point and then adds it to the dataframe)
- GLMM (this is the main model used to access seabird habituation to the OWF)
- GRID (this grids the segments in 200 x 200 grids allowing for a random intercept)
- Guillemot (this was the first seabird in which i performed data analysis on so just explatory code)
- initialise (this is what i went to second in order to initialise my glmm, my work computer kept closing so instead of going to seperate scripts i created this)
- MRSea (this holds my mrsea modelling framework)
- Plotting sea depth (this was for my exploration of how sea depth affects sea bird distributions)
- predicted plots (This was the plots i used in the results using the predict function to visualise the slope estimates)
- Seabird (this is where i intialised data sets in order to get them into R)
- sitting vs flying (this was also for exploration to see if sitting or flying made a significant different to distribution changes)
- slope estimates (this is where i calculated slope estimates in order to test for habituation)
