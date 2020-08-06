# MethodologyMicro
This repository contains the code used in the usecases presented in the paper "Methodology to identify gene expression signature by merging microarray datasets" by Olga Fajarda, Sara Duarte-Pereira, Raquel M. Silva and José Luís Oliveira.

# Folder Additional Files
This folder contains the additional files of the paper.

# Folder data
This folder contains the data obtained after the pre-processing.

### Subfolder HF
This subfolder contains the data of the usecase heart failure (HF):
- hf_before_rem_batch: expressions obtained after the pre-processing;
- hf_id: identification of the sequences through the GB accession number;
- hf_platform: identification of the platform used to obtain the expressions;
- hf_samples: identification of the samples;
- hf_type: class of every sample (D: diseased or Z: control).

### Subfolder ASD
This subfolder contains the data of the usecase autism spectrum disorder (ASD):
- asd_before_rem_batch: expressions obtained after the pre-processing;
- asd_id: identification of the sequences through the GB accession number;
- asd_platform: identification of the platform used to obtain the expressions;
- asd_samples: identification of the samples;
- asd_type: class of every sample (D: diseased or Z: control).

# Folder script
This folder contains the scripts used:
- batchRemoval_ComBat: remove the batch effect using ComBat;
- batchRemoval_ratioa: remove the batch effect using ratioa;
- featureSelection_limma: feature selection using limma package;
- plot_mds_after: plot MDS using the expression after removing the batch effect;
- plot_mds_before: plot MDS using the expression before removing the batch effect;
- preprocessing_oligo: Affymetrix data pre-processing;
- preprocessing_limma: Illumina data pre-processing
- randomForest: Random Forest algorithm;
- linearSvm: Linear SVM algorithm;
- neuralNetwork: Neural Network algorithm.

