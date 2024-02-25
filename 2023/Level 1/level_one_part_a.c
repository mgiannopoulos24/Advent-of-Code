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
    return -1;  // Not a recognized word
}

int main() {
    // Open the file for reading
    FILE *file = fopen("input_level_one.txt", "r");

    // Check if the file was opened successfully
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    int sum = 0;
    char str[100];  // Adjust the size based on your input size

    // Read strings from the file until the end is reached
    while (fscanf(file, "%s", str) != EOF) {
        int length = strlen(str);
        int i = 0;

        // Find the front and back digits
        int front_digit = -1;
        int back_digit = -1;

        // Find the front digit
        while (i < length && !isdigit(str[i]) && !isalpha(str[i])) {
            i++;
        }

        if (isdigit(str[i])) {
            front_digit = str[i] - '0';
        } else if (isalpha(str[i])) {
            // If it's a word, translate it to a digit
            front_digit = word_to_digit(str + i);
        }

        // Find the back digit
        i = length - 1;
        while (i >= 0 && !isdigit(str[i]) && !isalpha(str[i])) {
            i--;
        }

        if (isdigit(str[i])) {
            back_digit = str[i] - '0';
        } else if (isalpha(str[i])) {
            // If it's a word, translate it to a digit
            back_digit = word_to_digit(str + i);
        }

        // Combine front_digit and back_digit into a two-digit number and add to sum
        if (front_digit >= 0 && back_digit >= 0) {
            int combined_number = front_digit * 10 + back_digit;
            sum += combined_number;
            printf("String: %s, Combined number: %d\n", str, combined_number);
        }
    }

    // Close the file
    fclose(file);

    // Print the final sum
    printf("Calibration value is: %d\n", sum);

    return 0;
}
