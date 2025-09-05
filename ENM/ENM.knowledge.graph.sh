N=50
COMMAND="Rscript ENM.knowledge.graph.r"

for i in $(seq 1 $N); do
    ${COMMAND} &
done
