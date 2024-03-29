# SPCAncestry
A Python package for supervised global population ancestry inference from SNP data using stacking

## Installation
SPCAncestry is available through [PyPI](https://pypi.org/project/spcancestry/0.1.0/) and installable using the following command:
```bash
pip3 install spcancestry==0.1.0
```

## Usage
SPCAncestry assumes the user has SNP training data, in either PLINK, VCF, or Hail MatrixTable, and reliable global
population ancestry labels for all the samples in the training data. We compute PCs using the training data, then these PCs
(Xs) together with the population labels (Y) are used to train the model. The test SNP data
is projected onto the PCs computed using the training data, and then we use the trained model to infer population ancestry.
Below are the steps on how you can use the HGDP1KG data, provided with this package, for ancestry inference.

1. Read the training and test datasets
```python
import spcancestry

path = '/path/to/spcancestry/test_data/hgdp1kg'
inref_mt = spcancestry.Read(file=f'{path}/hgdp1kg_truth.bed', qc=False).as_matrixtable()
input_mt = spcancestry.Read(file=f'{path}/hgdp1kg_unknown.fam', qc=False).as_matrixtable()
```

2. Intersect the two datasets, compute PCs using training data, and project test data onto training PC space
```python
scores_df, colnames = spcancestry.PCProject(ref_mt=inref_mt, data_mt=input_mt,
                                            ref_info=f'{path}hgdp_1kg_truth_labels.txt').run_pca_projection()
```

3. Infer global population ancestry using spcancestry stacking
```python
spcancestry_infered = spcancestry.infer_ancestry(scores_df, colnames)
```

## Performance
Below is a figure, stratified by true population label, showing how SPCAncestry performs at correctly
classifying samples to true population ancestry vs. the gnomAD RF. We use a probability
threshold of >= 0.9 to classify a sample as belonging to a population. As shown in the figure, stacking outperforms (fewer blue circles)
RF (many red triangles) at correctly classifying individuals to their respective population ancestry.

![performance comparison with gnomAD RF](paper/fig1.png)

## Copyright and License
SPCAncestry is generously distributed under the [MIT License](https://github.com/LindoNkambule/spcancestry/blob/main/LICENSE)