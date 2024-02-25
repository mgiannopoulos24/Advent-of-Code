#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// This function checks if a character is a digit.
int is_digit(int c) {
    return (c >= '0' && c <= '9');
}

int main() {
    FILE *f = fopen("input_level_one.txt", "r");
    if (f == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char *str = malloc(60 * sizeof(char));
    if (str == NULL) {
        perror("Memory allocation failed");
        fclose(f);
        return EXIT_FAILURE;
    }
    
    char nums[9][8] = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
    int sum = 0;

    while (fgets(str, 60, f)) {
        int first_digit = -1, last_digit = -1, str_len = strlen(str);
        
        for (int i = 0; i < str_len; ++i) {
            if (is_digit(str[i])) {
                int digit = str[i] - '0';
                if (first_digit == -1) first_digit = digit;
                else last_digit = digit;
            } else {
                for (int j = 0; j < 9; ++j) {
                    if (strncmp(str + i, nums[j], strlen(nums[j])) == 0) {
                        int n = j + 1;
                        if (first_digit == -1) first_digit = n;
                        else last_digit = n;
                        break; // Found a match, no need to check other numbers.
                    }
                }
            }
        }
        
        sum += (first_digit == -1 ? 0 : first_digit) * 10;
        sum += (last_digit == -1 ? first_digit : last_digit); // Use first_digit if last_digit is -1.
    }
    
    printf("Calibration value: %d\n", sum);

    // Clean up
    fclose(f);
    free(str);

    return 0;
}


// Taken from https://github.com/StavrosGous/AdventOfCode/blob/main/2023/day1/b.c
// I couldnt figure out how to do it.