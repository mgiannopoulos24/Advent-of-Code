#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

// Function to translate word to digit
int word_to_digit(char *word) {
    if (strcmp(word, "one") == 0) return 1;
    if (strcmp(word, "two") == 0) return 2;
    if (strcmp(word, "three") == 0) return 3;
    if (strcmp(word, "four") == 0) return 4;
    if (strcmp(word, "five") == 0) return 5;
    if (strcmp(word, "six") == 0) return 6;
    if (strcmp(word, "seven") == 0) return 7;
    if (strcmp(word, "eight") == 0) return 8;
    if (strcmp(word, "nine") == 0) return 9;
    return -1; // Not a recognized word
}

int main() {
    FILE *file = fopen("input_level_one.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char str[1024]; // Increase buffer size to handle longer lines
    while (fscanf(file, "%1023s", str) != EOF) { // Ensure we don't exceed buffer size
        int front_digit = -1, back_digit = -1;
        int n = strlen(str);
        int word_start = -1;

        // Check for the first valid digit or word
        for (int i = 0; i < n; ++i) {
            if (isdigit(str[i]) && front_digit == -1) {
                front_digit = str[i] - '0'; // Found a digit
            } else if (isalpha(str[i])) {
                if (word_start == -1) word_start = i; // Mark start of a word
            } else {
                // Non-alphabetic and non-digit character, check if we just passed a word
                if (word_start != -1) {
                    char word[11] = {0}; // Buffer for the word, assuming max word length is 10
                    strncpy(word, str + word_start, i - word_start);
                    word[i - word_start] = '\0';
                    int num = word_to_digit(word);
                    if (num != -1) {
                        front_digit = num; // Convert word to digit
                        break; // Stop after finding the first valid number
                    }
                    word_start = -1; // Reset word start for the next word
                }
            }
        }

        // If a word was being processed at the end of the string
        if (word_start != -1 && front_digit == -1) {
            char word[11] = {0}; // Buffer for the word
            strncpy(word, str + word_start, n - word_start);
            word[n - word_start] = '\0';
            int num = word_to_digit(word);
            if (num != -1) {
                front_digit = num;
            }
        }

        // Reset for back number search
        word_start = -1;

        // Similar logic for finding the back digit/word, iterating from the end
        for (int i = n - 1; i >= 0; --i) {
            if (isdigit(str[i]) && back_digit == -1) {
                back_digit = str[i] - '0'; // Found a digit
            } else if (isalpha(str[i])) {
                if (word_start == -1) word_start = i; // Mark start (end, since we're going backwards) of a word
            } else {
                // Non-alphabetic and non-digit character, check if we just passed a word
                if (word_start != -1) {
                    char word[11] = {0}; // Buffer for the word, backwards
                    for (int j = word_start, k = 0; j >= i + 1; --j, ++k) {
                        word[k] = str[j];
                    }
                    word[word_start - i] = '\0';
                    int num = word_to_digit(word);
                    if (num != -1) {
                        back_digit = num; // Convert word to digit
                        break; // Stop after finding the last valid number
                    }
                    word_start = -1; // Reset word start for the next word
                }
            }
        }

        // If a word was being processed at the start of the string (for back number)
        if (word_start != -1 && back_digit == -1) {
            char word[11] = {0}; // Buffer for the word, backwards
            for (int j = word_start, k = 0; j >= 0; --j, ++k) {
                word[k] = str[j];
            }
            word[word_start + 1] = '\0';
            int num = word_to_digit(word);
            if (num != -1) {
                back_digit = num;
            }
        }

        if (front_digit != -1 && back_digit != -1) {
            int combined_number = front_digit * 10 + back_digit;
            printf("String: %s, Combined number: %d\n", str, combined_number);
        }
    }

    fclose(file);
    return 0;
}
