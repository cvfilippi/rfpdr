# rfpdr

This repository contains datasets files and scripts generated for **RFPDR: a random forest approach for plant disease resistance protein prediction** (Simón, Borsani & Filippi, 2022 | PeerJ | doi: [10.7717/peerj.11683](https://peerj.com/articles/11683/)).

The R scripts require minimum custom settings (i.e.; install needed packages, set the working directory, etc). 
To build the model, running order is: (1) featExt.r (performs feature extraction), (2) featSel.r (performs feature selection), (3) training.r (performs model training), (4) metrics.r (for  metrics estimation). Other trained approaches: SVM (svm.r) and logistic regression (glm.r).

For predictions, use predictions.r, with rfpdr.RDS to load the model. 

For any request, please contact dsimon@fcien.edu.uy and cfilippi@fagro.edu.uy.
