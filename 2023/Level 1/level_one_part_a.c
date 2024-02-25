#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int isdigit(int c) {
    return (c >= '0' && c <= '9') ? 1 : 0;
}

int main(int argc, char *argv[]) {
    FILE * f = fopen("./input.txt", "r");
    char *str = malloc (60 * sizeof(char));
    int sum = 0, first_digit, last_digit;
    while (str = fgets(str, 60, f)) {
        first_digit = -1;
        last_digit = -1;
        for (int i=0; i < strlen(str); i++) {
            if (isdigit(str[i])) {
                if (first_digit == -1) {
                    first_digit = str[i] - '0';
                    continue;
                }
                last_digit = str[i] - '0';
            }
        }   
        sum += first_digit * 10;
        if (last_digit == -1) {
            sum += first_digit;
        }
        else {
            sum += last_digit;
        }
    
    }
    printf("%d", sum);
    fclose(f);
    return 0;
}