#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$( cd "$(dirname "$0")" ; pwd -P )"
MAX_SECONDS=20

main(){
    kubectl apply -f "$SCRIPT_DIR/test-pod.yaml"
    start_time="$(date +%s)"
    while true; do
        phase="$(get-pod-phase in-cluster-example)"
        consumed_seconds="$(seconds-since $start_time)"

        if [[ "$phase" == "Succeeded" ]]; then
            echo "------------------------------"
            echo "Test passed!"
            echo "------------------------------"
            echo
            echo "------------------------------"
            echo "Logs from test:"
            echo "------------------------------"
            kubectl logs in-cluster-example
            exit 0
        elif [[ "$phase" == "Failed" ]]; then
            echo "------------------------------"
            echo "Test failed!"
            echo "------------------------------"
            print-failure in-cluster-example
            exit 1
        elif (( consumed_seconds > MAX_SECONDS )); then
            echo "------------------------------"
            echo "Test timed out after $MAX_SECONDS seconds!"
            echo "------------------------------"
            print-failure in-cluster-example
            exit 2
        else
            echo "Test still running, pod phase = $phase"
            sleep 0.5
        fi
    done
}

get-pod-phase() {
    kubectl get pod $1 -o 'jsonpath={.status.phase}'
}

print-failure() {
    echo
    echo "------------------------------"
    echo "Pod Description:"
    echo "------------------------------"
    kubectl describe pod $1
    echo
    echo "------------------------------"
    echo "Logs from test:"
    echo "------------------------------"
    kubectl logs $1
}

# Takes epoch time
seconds-since() {
    local start=$1
    local end=$(date +%s)
    echo $(( end - start))
}

main
