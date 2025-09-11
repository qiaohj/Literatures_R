N=100
COMMAND="Rscript ENM.read.paper.r"

for i in $(seq 1 $N); do
    ${COMMAND} &
done
