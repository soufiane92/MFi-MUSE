#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --job-name=sequential_100_part
#SBATCH --partition=upvd 
#SBATCH --time=0:20:00
#SBATCH --output=sequential_100_part.%j.out

rm TEST_PARALLELISATION* pos-0* && mpirun -np 4 ../../debug-build/mfixsolver -f mfix-carre-100part_4procs.dat 
