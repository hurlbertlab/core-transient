# Install raw datasets using the EcoData Retriever
#
# The EcoData Retriever will need to be installed on your system.
# See: http://ecodataretriever.org

datasets = c("BBS", "MCDB", "PortalMammals")

for (dataset in datasets) {
    system(paste("retriever install sqlite", dataset, "-f ./data/coretrans_data.sqlite"))
}
