#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int isdigit(int c) {
    return (c >= '0' && c <= '9') ? 1 : 0;
}

int main(int argc, char *argv[]) {
    FILE *f = fopen("input_level_one.txt", "r");
    if (f == NULL) {
        printf("Error opening file\n");
        return 1;
    }
    
    char *str = malloc(60 * sizeof(char));
    int sum = 0, first_digit, last_digit, combined_digit;
    
    while (fgets(str, 60, f)) {
        // Remove newline character from str, if present
        str[strcspn(str, "\n")] = 0;

        first_digit = -1;
        last_digit = -1;
        for (int i = 0; str[i] != '\0'; i++) {
            if (isdigit(str[i])) {
                if (first_digit == -1) {
                    first_digit = str[i] - '0';
                }
                last_digit = str[i] - '0';
            }
        }
        
        combined_digit = (first_digit != -1) ? first_digit * 10 : 0;
        combined_digit += (last_digit != -1) ? last_digit : first_digit;
        sum += combined_digit;
        
        // This will now print on one line
        printf("String: %s Combined digit: %d\n", str, combined_digit);
    }
    
    printf("Calibration value is: %d\n", sum);
    fclose(f);
    free(str);
    return 0;
}
