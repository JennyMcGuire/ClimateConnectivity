# ClimateConnectivity
Code for "Achieving climate connectivity in a fragmented landscape"

This code demonstrates how to perform a climate connectivity analysis. It is intended for demonstrative purposes only. However, it can be implemented if you have created the following data:

1) cores- a list of land patches or polygons with unique numbers (these represent the origins and destinations to be analyzed)
2) MAT- the current climate variable of interest for each core
3) Neighbors- a list of neighboring cores (can be attained from ArcGIS; necessary for all analyses)
4) AllUSCorridors- a list of cores connected by corridors & any statistics associated that you desire: "Origin", "Dest", "Euc_Dist", "CW_Dist", "LCP_Length", etc.
5) USClip- a list of cores within an area of interest on which you wish to perform statistics if this differes from your full dataset (for use if you buffer your data- recommended)
6) E_W_Cores- identification for any divisions on which you wish to perform statistical analyses (in this case, eastern & western U.S.)
7) CoreAreas- the area of each core; to use for statistics related to area rather than count
8) BonusData- future climate variable of interest for each core
