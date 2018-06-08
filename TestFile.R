# set to wd where package files are

# Compile latest package and re-documents
Current <- as.package('attritR')
load_all(Current)
document(Current)
# load in data set
data("SimulatedAttrition")

# run calculateWeights function

Weights <- calculateWeights(modelData = SimulatedAttrition[,1:3], 
                            instrumentData = SimulatedAttrition[ , 4])
