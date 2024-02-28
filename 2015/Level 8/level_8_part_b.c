#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to calculate the difference in encoding
void calculate_difference_in_encoding(const char* file_path) {
    FILE *file = fopen(file_path, "r");
    if (!file) {
        printf("File could not be opened.\n");
        return;
    }

    char line[1024]; // Assuming a line will not exceed 1024 characters
    int total_code_chars = 0;
    int total_encoded_chars = 0;
    
    while (fgets(line, sizeof(line), file)) {
        // Remove newline character if present
        line[strcspn(line, "\n")] = 0;
        
        int line_len = strlen(line);
        total_code_chars += line_len;
        
        // Calculate encoded length
        int encoded_line_len = 2; // Start with 2 quotes for the encoded string
        for (int i = 0; i < line_len; i++) {
            if (line[i] == '\\' || line[i] == '"') {
                encoded_line_len += 2; // Prepend a backslash to backslashes and double quotes
            } else {
                encoded_line_len += 1;
            }
        }
        
        total_encoded_chars += encoded_line_len;
    }
    
    fclose(file);
    
    int difference = total_encoded_chars - total_code_chars;
    printf("Total characters of code: %d\n", total_code_chars);
    printf("Total encoded characters: %d\n", total_encoded_chars);
    printf("Difference: %d\n", difference);
}

int main() {
    const char *file_path = "input_level_8.txt"; // Make sure this path is correct
    calculate_difference_in_encoding(file_path);
    return 0;
}
