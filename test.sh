#!/bin/bash

BASE_PORT=5000
NUM_PORTS=10
CMD="./main"
i=0
while [ ${i} -lt ${NUM_PORTS} ]; do
  CMD="${CMD} 0.0.0.0:$((${BASE_PORT} + ${i}))"
  i=$((${i} + 1))
done
CMD="${CMD} 127.0.0.1:10000"
echo "${CMD}"
$CMD
