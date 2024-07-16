#!/bin/ksh
######################
## JEANZAY IDRIS ##
######################
#SBATCH --job-name=DINO.GZ21
#SBATCH --output=DINO.GZ21.out
#SBATCH --error=DINO.GZ21.err
#SBATCH --ntasks=2
#SBATCH --gres=gpu:1         # Number of GPU per node
#SBATCH --hint=nomultithread # One MPI process per physical core (no hyperthreading)
#SBATCH --time=00:05:00
#SBATCH --account=cli@v100   # GPU partition
#SBATCH --partition=gpu_p13

# Load Environnment
source ~/.bash_profile

# Move to execution directory
cd ${SLURM_SUBMIT_DIR}
set -x
pwd

# job information 
cat << EOF
------------------------------------------------------------------
Job submit on $SLURM_SUBMIT_HOST by $SLURM_JOB_USER
JobID=$SLURM_JOBID Running_Node=$SLURM_NODELIST 
Node=$SLURM_JOB_NUM_NODES Task=$SLURM_NTASKS
------------------------------------------------------------------
EOF

# Begin of section with executable commands
set -e
ls -l

# run eophis in preproduction mode to generate namcouple
touch namcouple
rm namcouple*
python3 ./main.py --exec preprod

# save eophis preproduction logs
mv eophis.out eophis_preprod.out
mv eophis.err eophis_preprod.err

# check if preproduction did well generate namcouple
namcouple=namcouple
if [ ! -e ${namcouple} ]; then
        echo "namcouple can not be found, preproduction failed"
        exit 1
else
        echo "preproduction successful"
fi

# run coupled NEMO-Python
mpirun  -np 1 ./nemo : -np 1 python3 ./main.py --exec prod
