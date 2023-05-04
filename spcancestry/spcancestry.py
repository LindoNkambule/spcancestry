import os
import random
import sys

import hail as hl
import pandas as pd

from typing import List, Tuple
from sklearn.ensemble import GradientBoostingClassifier, RandomForestClassifier, StackingClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier


class Read:
    def __init__(self, file: str = None, ref='GRCh38', qc: bool = True, **kwargs):
        self.file = file
        self.qc = qc
        self.ref = ref
        self.filters = kwargs.get('filters')  # QC filters

    def mt(self) -> hl.MatrixTable:
        in_mt = hl.read_matrix_table(self.file)
        return in_mt

    def plink(self) -> hl.MatrixTable:
        filename = os.path.splitext(self.file)[0]  # filename without ext
        in_mt = hl.import_plink(bed=f'{filename}.bed',
                                bim=f'{filename}.bim',
                                fam=f'{filename}.fam',
                                reference_genome=self.ref)

        return in_mt

    def vcf(self) -> hl.MatrixTable:
        in_mt = hl.import_vcf(self.file, reference_genome=self.ref)

        return in_mt

    def as_matrixtable(self) -> hl.MatrixTable:
        ext = os.path.splitext(self.file)[1]  # get file ext

        if ext == '.mt':
            matrixtable = Read.mt(self)
        elif ext == '.bed' or ext == '.bim' or ext == '.fam':
            matrixtable = Read.plink(self)
        elif ext == '.vcf' or ext == '.bgz':
            matrixtable = Read.vcf(self)
        else:
            sys.exit('Unrecognized input file format. SPCAncestry only takes PLINK, Hail, and VCF format as input')

        if self.qc:
            matrixtable = Read.qc_mt(self, matrixtable)

        return matrixtable

    def qc_mt(self, in_matrixtable: hl.MatrixTable) -> hl.MatrixTable:
        qc_mt = in_matrixtable

        print(f'\nThe following filters are going to be used to QC the reference data:\n {self.filters}')

        print(f'\nInitial number of SNPs before filtering: {qc_mt.count_rows()}')
        # 1. MAF
        qc_mt = hl.variant_qc(qc_mt)
        qc_mt = qc_mt.annotate_rows(maf=hl.min(qc_mt.variant_qc.AF))
        qc_mt = qc_mt.filter_rows(qc_mt.maf > self.filters['maf'])

        # 2. HWE
        qc_mt = qc_mt.filter_rows(qc_mt.variant_qc.p_value_hwe > self.filters['hwe'])

        # 3. Variant call rate
        qc_mt = qc_mt.filter_rows(qc_mt.variant_qc.call_rate >= self.filters['cr'])

        # 4. Strand ambiguity
        qc_mt = qc_mt.filter_rows(~hl.is_strand_ambiguous(qc_mt.alleles[0], qc_mt.alleles[1]))

        # 5. MHC and chr8 inversions
        intervals = ['6:25M-35M', '8:7M-13M'] if self.ref == 'GRCh37' else ['chr6:25M-35M', 'chr8:7M-13M']
        qc_mt = hl.filter_intervals(qc_mt, [hl.parse_locus_interval(x, reference_genome=self.ref) for x in intervals],
                                    keep=False)

        # 6. LD

        print(f'\nNumber of SNPs after filtering: {qc_mt.count_rows()}')

        return qc_mt


class PCProject:
    def __init__(self, ref_mt: hl.MatrixTable = None, data_mt: hl.MatrixTable = None, pcs: int = 20,
                 **kwargs):
        self.ref = ref_mt
        self.data = data_mt
        self.pcs = pcs
        # self.loadings = kwargs.get('loadings')  # pre-computed reference loadings
        self.ref_info = kwargs.get('ref_info')

    def intersect(self):
        """
        Intersects reference panel with input data
        :return: intersected reference and input data MatrixTables
        """

        # filter data to sites in ref & array data
        data_in_ref = self.data.filter_rows(hl.is_defined(self.ref.rows()[self.data.row_key]))

        # filter ref to data sites
        ref_in_data = self.ref.filter_rows(hl.is_defined(self.data.rows()[self.ref.row_key]))

        assert ref_in_data.count_rows() == data_in_ref.count_rows()

        return ref_in_data, data_in_ref

    def pca(self,
            reference_mt: hl.MatrixTable = None):
        """
        Run PCA on a dataset
        :param reference_mt: dataset to run PCA on
        :return:
        """

        pca_evals, pca_scores, pca_loadings = hl.hwe_normalized_pca(reference_mt.GT, k=self.pcs, compute_loadings=True)
        pca_mt = reference_mt.annotate_rows(pca_af=hl.agg.mean(reference_mt.GT.n_alt_alleles()) / 2)
        pca_loadings = pca_loadings.annotate(pca_af=pca_mt.rows()[pca_loadings.key].pca_af)

        pca_scores = pca_scores.transmute(**{f'PC{i}': pca_scores.scores[i - 1] for i in range(1, self.pcs+1)})
        pca_scores = pca_scores.key_by('s')  # make sure we key by s so we can annotate

        return pca_scores, pca_loadings

    def run_pca_projection(self) -> Tuple[pd.DataFrame, List[str]]:

        print('Intersecting')
        ref, data = self.intersect()
        print(ref.count())
        print(data.count())

        print('Running reference PCA')
        ref_scores, ref_loadings = self.pca(ref)

        # annotate ref info with SuperPop information
        ref_info = hl.import_table(self.ref_info, key='Sample')
        ref_annotated = ref_scores.annotate(SuperPop=ref_info[ref_scores.s].SuperPop)
        ref_df = ref_annotated.to_pandas()

        data_scores = hl.experimental.pc_project(
            self.data.GT,
            ref_loadings.loadings,
            ref_loadings.pca_af,
        )
        data_scores = data_scores.transmute(**{f'PC{i}': data_scores.scores[i - 1] for i in range(1, self.pcs+1)})
        data_df = data_scores.to_pandas()

        pca_scores_df = pd.concat([ref_df, data_df], sort=False)

        pc_colnames = [f'PC{i+1}' for i in range(self.pcs)]

        return pca_scores_df, pc_colnames


def infer_ancestry(
        pca_scores: pd.DataFrame,
        pc_cols: List[str],
        known_col: str = 'SuperPop',
        seed: int = 42,
        prop_train: float = 0.8,
        n_estimators: int = 100,
        min_prob: float = 0.9,
        output_col: str = 'pop',
        missing_label: str = 'oth',
        write_output: bool = False) -> pd.DataFrame:

    # randomly split training (ref) data for fitting and (internal) validation
    train_data = pca_scores.loc[~pca_scores[known_col].isnull()]
    N = len(train_data)
    random.seed(seed)
    train_subsample_ridx = random.sample(list(range(0, N)), int(N * prop_train))
    train_fit = train_data.iloc[train_subsample_ridx]
    fit_samples = [x for x in train_fit['s']]
    evaluate_fit = train_data.loc[~train_data['s'].isin(fit_samples)]

    training_set_known_labels = train_fit[known_col].values  # reference POP labels (Y)
    training_set_pcs = train_fit[pc_cols].values  # reference PC scores (X)
    evaluation_set_pcs = evaluate_fit[pc_cols].values  # PC scores to be used to infer unknown POP (internal validation)

    # base models
    level0 = [('gnb', GaussianNB()),
              ('knn', KNeighborsClassifier()),
              ('rf', RandomForestClassifier(n_estimators=n_estimators, random_state=seed)),
              ('gb', GradientBoostingClassifier(n_estimators=n_estimators, learning_rate=0.05, random_state=seed))]
    level1 = LogisticRegression() # meta model

    # Train ensemble
    clf = StackingClassifier(estimators=level0, final_estimator=level1, cv=5)
    clf.fit(training_set_pcs, training_set_known_labels)

    # Evaluate ensemble (internal validation)
    predictions = clf.predict(evaluation_set_pcs)
    error_rate = 1 - sum(evaluate_fit[known_col] == predictions) / float(
        len(predictions)
    )
    print(f'Estimated error rate for the meta model is {error_rate}')

    # infer ancestry
    unknown_data = pca_scores.loc[pca_scores[known_col].isnull()]

    # for classifying unlabeled data
    # following line throws a SettingWithCopyWarning
    unknown_data.loc[:, output_col] = clf.predict(unknown_data[pc_cols].values)
    probs = clf.predict_proba(unknown_data[pc_cols].values)
    probs = pd.DataFrame(probs, columns=[f'prob_{p}' for p in clf.classes_])
    df = pd.concat([unknown_data.reset_index(drop=True), probs.reset_index(drop=True)], axis=1)
    probs['max'] = probs.max(axis=1)
    df.loc[probs['max'] < min_prob, output_col] = missing_label

    # remove PC columns before exporting df to file
    cols_to_select = ['s', 'pop'] + [f'prob_{i}' for i in list(train_data[known_col].unique())]
    df = df[cols_to_select]
    if write_output:
        df.to_csv('spcancestry_predicted_pops.txt', sep='\t', index=False)

    return df
