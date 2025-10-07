N=30
COMMAND="Rscript merge.extraced.item.r"

for i in $(seq 1 $N); do
${COMMAND} &
  done
