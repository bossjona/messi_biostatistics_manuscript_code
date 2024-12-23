# Mediation with External Summary Statistic Information (MESSI) Code Repository

## Simulation Code

The simulation code for the congenial simulation settings is housed in the simulations_correct folder, the simulation code for the incongenial simulation settings is housed in the simulations_incorrect_fixed folder, and the simulation code for the random simulation settings is housed in the simulations_incorrect_random folder. The implementations.R file contains required functions to run the simulation code. Here are the steps to re-run the simulations:

(1) Run simulation_code.R to generate the simulation output (note that arrayid in this script is assigned by the Slurm Workload Manager; this value will take 1-2000 for the 2000 simulation iterations).
(2) After the 2000 runs of simulation_code.R, run combine.R to aggregate and clean-up the simulation output.
(3) After combine.R is run, then run summarize_results_numerical_no_pen.R to generate the simulation results.
(4) After all of the simulations are run in all of the subfolders, run the following scripts to generate the figures and tables presenting the simulation results:
    (i) rmse_visualize_n2000_nde_estimation_unpenalized.R
    (ii) rmse_visualize_n2000_nie_estimation_unpenalized.R
    (iii) rmse_visualize_n200_nde_estimation_unpenalized.R
    (iv) rmse_visualize_n200_nie_estimation_unpenalized.R
    (v) sim_results_n2000_nde_correct.R
    (vi) sim_results_n2000_nie_correct.R
    (vii) sim_results_n200_nde_correct.R
    (viii) sim_results_n200_nie_correct.R

## Data Example Code

