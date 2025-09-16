N=100
COMMAND="Rscript BIOGEOGRAPHY.extract.items.r"

for i in $(seq 1 $N); do
${COMMAND} &
  done
