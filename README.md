# FinnPRIO Explorer Adapted

FinnPRIO Explorer Adapted presents results from FinnPRIO assessments conducted for Sweden made with the FinnPRIO model (Heikkila et al. 2016). It is a modified version of the original app FinnPRIO-Explorer developed in the Risk Assessment Unit of the Finnish Food Authority. (Marinova-Todorova et al. 2022). 

The current Shiny application and model modification was conducted by the Swedish Risk Assessment Unit, at the Swedish University of Agricultural Sciences.

**FinnPRIO model**

FinnPRIO is a model for ranking non-native plant pests based on the risk that they pose to plant health (Heikkila et al. 2016). It is composed of five sections: likelihood of entry, likelihood of establishment and spread, magnitude of impacts, preventability, and controllability. The score describing the likelihood of invasion is a product of entry and establishment scores. The score describing the manageability of invasion is the minimum of prevantability and controllability scores.

FinnPRIO consists of multiple-choice questions with different answer options yielding a different number of points. For each question, the most likely answer option and the plausible minimum and maximum options are selected based on the available scientific evidence. The selected answer options are used to define a PERT probability distribution and the total section scores are obtained with Monte Carlo simulation. The resulting probability distributions of the section scores describe the uncertainty of the assessment. Summary statistics of the score distributions can be explored in the tab 'Plot pests on a graph'

**FinnPRIO-Explorer Adapted vs. FinnPRIO-Explorer**

FinnPRIO-Explorer Adapted introduces additional functionality: risk scores are shown directly in the interface and a ranking based on risk is included. Users can also select to display all scores not only as median values but also as mean values. The uncertainty is displayed by showing the 5th percentile and the 95th percentile.

**FinnPRIO assessments for Sweden**

The results presented in this app are based on all FinnPRIO assessments done for Sweden and the calculations were done using the FinnPRIO-Assessor app.

The probability distributions of the scores were simulated with 50 000 iterations. The likelihood of entry is assessed taking into account the current management measures.
