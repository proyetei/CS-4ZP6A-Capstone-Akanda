#!/bin/bash

JSON_STRING=$(jq -n '{
  testcases: [{
    name: "testcase_1",
    description: "test case for appending to a list",
    languages: [
      { name: "Idris", tests: [] },
      { name: "Agda", tests: [] },
      { name: "Coq", tests: [] }, 
      { name: "Lean", tests: [] }
    ]
  }]
}')

add_test() {
  local language_name=$1
  local size=$2
  local data=$3
  test=$(jq -n "{size: $size, $data}")
  JSON_STRING=$(echo "$JSON_STRING" | jq '.testcases[0].languages[] |= if .name == "'"$language_name"'" then .tests += ['"$test"'] else . end')
}

cd translators
touch output.txt
allLanguages=("Coq" "Agda" "Idris" "Lean")
file_name="LetExample"
commands=("coqc ./out/$file_name.v" "agda --compile ./out/$file_name.agda" "idris2 --check ./out/$file_name.idr" "lean ./out/$file_name.lean")

for i in {1..5}
do
  rm -rf ./out
  mkdir out
  size=$(($i * 20))
  printf '1\n%s\n' "$size" | ./main
  for j in ${!allLanguages[@]}; do
    IFS=' ' read -r -a command <<< "${commands[$j]}"
    pa_data=$(/usr/bin/time --quiet --format='real_time: %e, user_time: %U, system_time: %S, memory: %M' "${command[@]}" 2>&1 1>output.txt)
    echo $pa_data
    add_test "${allLanguages[$j]}" $size "$pa_data"
  done

done
rm output.txt
echo $JSON_STRING | jq '.' > data.json
