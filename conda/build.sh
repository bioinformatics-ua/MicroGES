#! /bin/bash

mkdir -p "$PREFIX/bin"

cp src/batchRemoval_ComBat.R $PREFIX/bin/
cp src/batchRemoval_fsva.R $PREFIX/bin/
cp src/featureSelection_limma.R $PREFIX/bin/
cp src/linearSvm.R $PREFIX/bin/
cp src/neuralNetwork.R $PREFIX/bin/
cp src/plot_mds.R $PREFIX/bin/
cp src/preprocessing_affymetrix.R $PREFIX/bin/
cp src/preprocessing_illumina.R $PREFIX/bin/
cp src/randomForest.R $PREFIX/bin/