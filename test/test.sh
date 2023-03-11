#!/bin/bash
cd ..
make re
program="./glados"

input_and_expected_output=(
    'define x 5'
    '5 + 6|11'
    '5 - 6|-1'
    '5 * 5|25'
    'div 5 5|1'
    'mod 6 4|2'
    'eq? 5 6|#f'
    'eq? 5 5|#t'
    'if (eq? 5 5) 1 2|1'
    'if (eq? 5 6) 1 2|2'
    'define (add a b) (+ a b)'
    'define (test a b) (+ a (add a b))'
    '(add 2 3)|5'
    '(test 2 3)|7'
    'define (equal a b) (if (eq? a b) 1 0)'
    'define (test a b) (+ a (+ a b))'
)
test_results=()

echo -e "\033[33m###################################\033[0m" > ./test/output_test.txt
echo "" >> ./test/output_test.txt
echo "" >> ./test/output_test.txt


for input_and_output in "${input_and_expected_output[@]}"; do

    if [[ $input_and_output == *"|"* ]]; then
        input=$(echo "$input_and_output" | awk -F '|' '{print $1}')
        expected_output=$(echo "$input_and_output" | awk -F '|' '{print $2}')
        output=$($program <<< "$input")

        if [ "$output" == "$expected_output" ]; then
            echo -e "\e[34mEntrée : $input\e[0m" >> ./test/output_test.txt
            echo "Sortie attendue : $expected_output" >> ./test/output_test.txt
            echo "Sortie obtenue : $output" >> ./test/output_test.txt
            echo -e "\e[32mTest : OK\e[0m" >> ./test/output_test.txt
            test_results+=("1")
        else
            echo -e "\e[34mEntrée : $input\e[0m" >> ./test/output_test.txt
            echo "Sortie attendue : $expected_output" >> ./test/output_test.txt
            echo "Sortie obtenue : $output" >> ./test/output_test.txt
            echo -e "\e[31mTest : \e[0m\e[31mERREUR\e[0m" >> ./test/output_test.txt
            test_results+=("2")
        fi

    else
        echo -e "\e[34mEntrée : $input_and_output\e[0m" >> ./test/output_test.txt
        echo -e "\e[33mAucune sortie attendue pour cette entrée\e[0m" >> ./test/output_test.txt
    fi

    echo "" >> ./test/output_test.txt

done

echo "" >> ./test/output_test.txt
echo -e "\033[33m###################################\033[0m" >> ./test/output_test.txt
cat ./test/output_test.txt

#if [[ " ${test_results[@]} " =~ "2" ]]; then
#    exit 0
#fi
