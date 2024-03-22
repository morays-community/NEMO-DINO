#!/bin/ksh
######################
## JEANZAY IDRIS ##
######################
#SBATCH --job-name=MyJobTest# Job Name 
#SBATCH --output=Script_Output_MyJobTest.000002
#SBATCH --error=Script_Output_MyJobTest.000002
#SBATCH --nodes=9
# standard output # error output
#SBATCH --exclusive
#SBATCH --ntasks=73 # Number of MPI tasks -- 72 NEMO + 1 Python
#SBATCH --hint=nomultithread # 1 processus MPI par par physical core (no hyperthreading)
#SBATCH --time=00:30:00 #
#SBATCH --account=cli@cpu
#SBATCH --qos=qos_cpu-dev # Queue test
# echo des commandes lancées

# Python Environnment
source /gpfswork/rech/cli/udp79td/local_libs/oasis3-mct_5.0/BLD/python/init.sh
source /gpfswork/rech/cli/udp79td/local_libs/oasis3-mct_5.0/BLD/python/init.csh
source /linkhome/rech/genleg01/udp79td/.bash_profile
#conda activate morays

#cd /gpfswork/rech/gzi/ufk69pe/nemo_4.2.0/tests/MY_DINO/EXP00/
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

#cp $bindir/nemo.exe                     nemo
#cp $WORK/trunk/bin/xios_server.exe xios.x
#cp /gpfswork/rech/cli/rote001/DEV/xios_trunk_2430/bin/xios_server.exe xios.x

# eophis preproduction
rm namcouple*
python3 ./main.py --exec preprod
namcouple=namcouple
if [ ! -e ${namcouple} ]; then
        echo "namcouple can not be found, preproduction failed"
        exit 1
else
        echo "preproduction successful"
fi
mv eophis.out eophis_preprod.out
mv eophis.err eophis_preprod.err

# exécution du code
#mpirun  -np 32 ./nemo : -np 1 ./xios.x : -np 1 python3 ./main.py --exec prod
mpirun  -np 32 ./nemo : -np 1 python3 ./main.py --exec prod
