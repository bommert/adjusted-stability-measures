Analysis based on artificial feature sets:
Please run the files in the following order:
1. artificial_experiments.R
2. artificial_evaluation.R

Analysis based on real feature sets:
Please run the files in the following order:
1. datasets.R 
2. real_experiments.R (preferably on a high performance compute cluster)
3. real_evaluation.R
The files extractSelectedFeatures.R, extractSelectedFeaturesInternal.R and resampling.R are helper files. They are sourced when running real_experiments.R.