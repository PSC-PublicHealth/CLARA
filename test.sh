#!/bin/bash
#PBS -l ncpus=8
# ncpus must be a multiple of 16 on Blacklight
#PBS -l walltime=60:00:00
#PBS -j oe
##PBS -q batch
# set TEST to the Behaviorspace experiment to be run
# set PBS -N to the same, just for clarity
#PBS -N test
TEST="test"

set -x

MODEL_FILE="CLARA.nlogo"
THREADS=$PBS_NUM_PPN
NETLOGO=/data/fs/packages/netlogo-5.0.4

# get your JAVA
#source /usr/share/modules/init/bash
module load java
which java

cd $PBS_O_WORKDIR
date

mkdir -p $TEST

java -server -Xmx65536M \
    -cp .:$NETLOGO/NetLogo.jar \
    org.nlogo.headless.Main \
    --threads $THREADS \
    --model $MODEL_FILE \
    --experiment $TEST \
    --spreadsheet $TEST/$TEST.csv \
> $TEST/$TEST.out 2> $TEST/$TEST.err

date
