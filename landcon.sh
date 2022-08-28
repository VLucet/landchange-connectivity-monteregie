#!/bin/bash

#-------------------------------------------------------------------------------
## 0.0 Main thesis utility
#-------------------------------------------------------------------------------

# -- for HPC env
#SBATCH --account=def-gonzalez
#SBATCH --time=2:30:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --mem=16G
#SBATCH --mail-user=valentin.lucet@gmail.com
#SBATCH --mail-type=ALL
#export LD_LIBRARY_PATH=~/projects/def-gonzalez/lowlands/SyncroSim/
#module load nixpkgs/16.09 gcc/7.3.0 r/3.6.0 gdal/2.1.3 python/3.6 mono julia/1.2.0
#setrpaths.sh --path ~/projects/def-gonzalez/lowlands/SyncroSim/ --add_origin
# -- for HPC env

# Variables for R
export R_CROP='FALSE'
export R_AGGR='TRUE'
export R_AGGR_FACT='3'
export R_PART='0.7'
export R_BUFF='TRUE'
export R_METHOD='rf'
export R_RATIO='2'
export R_METHOD_STSIM='rf'
export R_SAMPLING_METHOD='bal'
export OMP_NUM_THREADS='16' # six on thinkpad, 16 on cluster
# Variables for STSIM
export STSIM_ITER='10'
export STSIM_TS_START='0'
export STSIM_TS_END='11'
export STSIM_STEP_SAVE='1'
export STSIM_STEP_COMPUTE='10'
# Variables for CS
export CS_CORES='12' # 2 on thinkpad, 12 on cluster
# Variabled for figure making script
export REPRO_FIGS_ONLY='TRUE'

echo ""
echo "LAND USE CHANGE AND CON MODEL - 2020"
echo "Valentin Lucet - Thesis McGill University"
echo ""

print_vars(){
  echo ""
  echo "Variables for R: R_crop = $R_CROP, R_AGGR = $R_AGGR"
  echo "                 R_AGGR_FACT = $R_AGGR_FACT, R_PART = $R_PART"
  echo "                 R_BUFF = $R_BUFF, R_METHOD = $R_METHOD"
  echo "                 R_N_TREES = $R_N_TREES, R_RATIO = $R_RATIO"
  echo ""
  echo "Variables for ST-SIM: STSIM_ITER = $STSIM_ITER"
  echo "                      STSIM_TS_START = $STSIM_TS_START"
  echo "                      STSIM_TS_END = $STSIM_TS_END"
  echo "                      STSIM_STEP_SAVE = $STSIM_STEP_SAVE"
  echo "                      STSIM_STEP_COMPUTE = $STSIM_STEP_COMPUTE"
  echo ""
  echo "Variables for CS: CS_CORES = $CS_CORES"
  echo ""
  echo "Running main script using $OMP_NUM_THREADS cores"
  echo ""
}

print_main_usage(){
  echo ""
  echo "Usage: landcon.sh [-a run all] [-p prep] [-m model no prep] [-f fit & predict]"
  echo "                  [-s stsim] [-r reclassify] [-c circuitscape] [-d post process] [-g make figures]"
  echo ""
  echo "Subcommands: prep, fitpred, stsim, cs, figs (use -h to see usage on each subcommand)"
  echo ""
}

# STEP FUNCTIONS

prep_raw_data(){
  print_vars
  ## Process raw data
  echo "Prep raw data"
  Rscript scripts/1.1_prep_land_use_and_mun.R
  Rscript scripts/1.2_prep_census_data.R
  Rscript scripts/1.3_prep_env_vars.R
  Rscript scripts/1.4_reclass_land_use.R
  Rscript scripts/1.5_prep_landis.R
  # Rscript scripts/1.6_prep_neighbors.R
}

prep_stsim_data(){
  print_vars
  # Prepare data for stsim
  echo "Prep stsim data"
  Rscript scripts/2.1_prep_stsim_stratums.R
  Rscript scripts/2.2_prep_stsim_transition_targets.R
  Rscript scripts/2.3_prep_stsim_transition_size_distribution.R
  Rscript scripts/2.4_prep_stsim_spatial_multipliers.R
  Rscript scripts/2.5_prep_stsim_datasheets.R
  Rscript scripts/2.6_prep_trans_vals_data.R
}

prep_model_data(){
  print_vars
  ## Run the preparation of data
  echo "Prep model data"
  Rscript scripts/3.1_model_prep_data.R
}

fit_predict_model(){
  print_vars
  ## Run stat model, here a gam
  echo "Fit & predict model"
  Rscript scripts/3.2_fit_and_predict.R
}

run_stsim(){
  print_vars
  echo "Run STSIM"
  ## Remove librairies for StSIM
  rm -rf librairies/stsim/*
  ## Run Stsim
  Rscript scripts/4.1_run_stsim.R
  if [ $STSIM_RUN = "TRUE" ]
    then
      Rscript scripts/4.2_validate_stsim_results.R
    else
      echo "STSIM not run as STSIM_RUN is set to $STSIM_RUN"
  fi
}

reclassify(){
  print_vars
  echo "Reclassify"
  ## Remove files we dont need (from last run)
  rm -rf outputs/reclassed/*
  rm -rf outputs/reclassed_with_buffer/*
  touch scripts/0.2_add_buffer_to_all.sh
  rm scripts/0.2_add_buffer_to_all.sh
  ## Run reclassification
  Rscript scripts/5.1_reclassify_into_resistance.R
  ## Add rows using Guillaume's code
  sh scripts/0.2_add_buffer_to_all.sh
}

prep_circuitscape(){
  print_vars
  echo "Prep Circuitscape"
  rm -rf config/ini_circuitscape/all/*.ini
  ## Prepare for circuitscape
  Rscript scripts/6.1_prep_circuitscape_inputs.R
  # sed -i 's/2\.0/2/g' `ls outputs/reclassed_with_buffer/*.asc`
  # sed -i 's/1\.0/1/g' `ls outputs/reclassed_with_buffer/*.asc`
  ## Remove files we dont need
  rm -rf config/ini_circuitscape/all/.DS_Store
  ## add cron job to turn asc into tif
  # crontab ./scripts/cron_job
  # crontab -l
}

run_circuitscape(){
  print_vars
  echo "Run Circuitscape"
  rm -rf outputs/current_density/*
  ## Run circuitscapes
  julia -p $CS_CORES scripts/6.2_run_circuitscape.jl
  # Remove cron job
  # crontab -r
  # Remove out files
  rm -rf outputs/current_density/*.out
  find outputs/current_density/ -type f  ! -name "*.*" -delete
}

post_process(){
  print_vars
  echo "Run post-precessing"
  rm -rf outputs/current_density_sum/*
  ## Run diagnostics and plots
  Rscript scripts/7.1_post_process_results.R
}

run_surf(){
  python3 scripts/8.1_SURF_analysis.py
  Rscript scripts/8.2_process_SURF_output.R
}

make_figures(){
  Rscript -e "rmarkdown::render('docs/msc_thesis_figures.Rmd',
  params = list(REPRO_FIGS_ONLY = as.logical(Sys.getenv('REPRO_FIGS_ONLY',
  unset = TRUE))), output_file='index.html')"
}

### BUNDLE FUNCTIONS

run_all(){
  print_vars
  echo "Running all"
  ## Run all
  prep_raw_data
  prep_stsim_data
  prep_model_data
  fit_predict_model
  run_stsim
  reclassify
  run_circuitscape
  post_process
  run_surf
  make_figures
}

run_model_no_prep(){
  print_vars
  echo "Running all - no prep"
  # Model after prep (old first workflow)
  fit_predict_model
  run_stsim
  reclassify
  run_circuitscape
  post_process
  run_surf
  make_figures
}

run_prep(){
  print_vars
  echo "Running prep ONLY"
  # Just prep
  prep_raw_data
  prep_stsim_data
  prep_model_data
}

### Arguments matching

while getopts ":aepmfsrcdgy" opt; do
  case ${opt} in
    a )
      #export R_METHOD='all'
      export R_N_TREES='500'
      export STSIM_RUN='TRUE'
      run_all
      ;;
    e )
      # Fails the whole pipeline if error happens, but not GA safe
      set -e
      ;;
    p )
      run_prep
      echo $?
      ;;
    m )
      run_model_no_prep
      ;;
    f )
      #export R_METHOD='all'
      export R_N_TREES='500'
      fit_predict_model
      ;;
    s )
      export STSIM_RUN='TRUE'
      run_stsim
      ;;
    r )
      reclassify
      ;;
    c )
      prep_circuitscape
      run_circuitscape
      ;;
    d )
      post_process
      ;;
    g )
      export REPRO_FIGS_ONLY='TRUE'
      make_figures
      ;;
    y )
      run_surf
      ;;
    \? )
      print_main_usage
      echo "" 1>&2
      exit 1
      ;;
  esac
done

shift $((OPTIND-1))

subcommand=$1; shift # remove subcommand from the argument list
case "$subcommand" in

  prep)
    echo "prep - prepare inputs from raw data"
    echo ""

    while getopts ":rsm" opt; do
      case ${opt} in
        r )
          prep_raw_data
          ;;
        s )
          prep_stsim_data
          ;;
        m )
          prep_model_data
          ;;
        \? )
          echo "Usage: prep [-r raw data] [-s stsim data] [-m model data]"
          echo "" 1>&2
          exit 1
          ;;
      esac
    done
    shift $((OPTIND -1))
    ;;

  fitpred)

    if [[ $# -eq 0 ]] ; then
      echo "Usage: fitpred <string nb trees>"
      echo "" 1>&2
      exit 0
    fi

    export R_N_TREES=$1
    fit_predict_model

    shift $((OPTIND -1))
    ;;

  stsim)
    echo "stsim - prepare stsim library and run all scenarios"
    echo ""

    while getopts ":lr" opt; do
      case ${opt} in
        l )
          export STSIM_RUN='FALSE'
          run_stsim
          ;;
        r )
          export STSIM_RUN='TRUE'
          run_stsim
          ;;
        \? )
          echo "Usage: stsim [-l library only] [-r run stsim]"
          echo "" 1>&2
          exit 1
          ;;
      esac
    done

    shift $((OPTIND -1))
    ;;

  cs)
    echo "cs - prepare circuitscape inputs and run julia script"
    echo ""

    while getopts ":pr:a" opt; do
      case ${opt} in
        p )
          prep_circuitscape
          ;;
        r )
          export CS_CORES=$OPTARG
          run_circuitscape
          ;;
        a )
          prep_circuitscape
          run_circuitscape
          ;;
        \? )
          echo "Usage: cs [-p prep] [-r run <cores, numeric>] [-a all]"
          echo "" 1>&2
          exit 1
          ;;
      esac
    done

    shift $((OPTIND -1))
    ;;

  figs)
    echo "figs - reproduce thesis figures"
    echo ""

    if [ $# -eq 0 ]; then
      echo "Reproducing out-of-the-box reproducible figures only"
      echo "" 1>&2
      export REPRO_FIGS_ONLY='TRUE'
      make_figures
    fi

    while getopts ":a" opt; do
      case ${opt} in
        a )
          echo "Reproducing ALL figures"
          export REPRO_FIGS_ONLY='FALSE'
          make_figures
          ;;
        \? )
          echo "Usage: default (no options) to out-of-the-box reproducible figures only"
          echo "       [-a for ALL figures]"
          echo "" 1>&2
          exit 1
          ;;
      esac
    done

    shift $((OPTIND -1))
    ;;

esac
