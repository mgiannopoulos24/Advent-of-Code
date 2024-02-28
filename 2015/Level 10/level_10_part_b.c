#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to generate the next look-and-say sequence
char* generate_next_sequence(const char* current_sequence) {
    // Allocate memory for the new sequence (maximum length is at most 2 times the length of the current sequence)
    char* next_sequence = (char*)malloc(2 * strlen(current_sequence) + 1);
    int i = 0; // Index for the next sequence
    int j = 0; // Index for the current run of digits
    int count = 1; // Counter for the current run of digits

    // Iterate through the current sequence
    while (current_sequence[j] != '\0') {
        // If the current digit is different from the next one, add the count and digit to the next sequence
        if (current_sequence[j] != current_sequence[j + 1] || current_sequence[j + 1] == '\0') {
            // Convert the count to a character and add it to the next sequence
            next_sequence[i++] = count + '0';
            // Add the current digit to the next sequence
            next_sequence[i++] = current_sequence[j];
            // Reset the count for the next run of digits
            count = 1;
        } else {
            // Increment the count for the current run of digits
            count++;
        }
        // Move to the next digit in the current sequence
        j++;
    }
    // Add null terminator to the end of the next sequence
    next_sequence[i] = '\0';
    return next_sequence;
}

int main() {
    const char* initial_sequence = "1113222113";
    char* current_sequence = strdup(initial_sequence); // Duplicate the initial sequence

    // Iterate 50 times to generate the sequence
    for (int i = 0; i < 50; i++) {
        char* next_sequence = generate_next_sequence(current_sequence);
        free(current_sequence); // Free memory of the current sequence
        current_sequence = next_sequence; // Update current sequence with the next one
    }

    // Calculate the length of the final sequence
    int length = strlen(current_sequence);
    printf("Length of the result: %d\n", length);

    // Free memory allocated for the final sequence
    free(current_sequence);

    return 0;
}
