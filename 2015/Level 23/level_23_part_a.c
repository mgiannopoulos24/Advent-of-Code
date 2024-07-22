#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INSTRUCTIONS 100
#define MAX_LENGTH 20

int run_program(char instructions[][MAX_LENGTH], int num_instructions) {
    int registers[2] = {0, 0}; // registers[0] is 'a', registers[1] is 'b'
    int ip = 0;

    while (ip < num_instructions) {
        char instruction[MAX_LENGTH];
        strcpy(instruction, instructions[ip]);
        char *token = strtok(instruction, " ");

        if (strcmp(token, "hlf") == 0) {
            token = strtok(NULL, " ");
            registers[token[0] - 'a'] /= 2;
            ip += 1;
        } else if (strcmp(token, "tpl") == 0) {
            token = strtok(NULL, " ");
            registers[token[0] - 'a'] *= 3;
            ip += 1;
        } else if (strcmp(token, "inc") == 0) {
            token = strtok(NULL, " ");
            registers[token[0] - 'a'] += 1;
            ip += 1;
        } else if (strcmp(token, "jmp") == 0) {
            token = strtok(NULL, " ");
            ip += atoi(token);
        } else if (strcmp(token, "jie") == 0) {
            token = strtok(NULL, ", ");
            int reg = token[0] - 'a';
            token = strtok(NULL, " ");
            if (registers[reg] % 2 == 0) {
                ip += atoi(token);
            } else {
                ip += 1;
            }
        } else if (strcmp(token, "jio") == 0) {
            token = strtok(NULL, ", ");
            int reg = token[0] - 'a';
            token = strtok(NULL, " ");
            if (registers[reg] == 1) {
                ip += atoi(token);
            } else {
                ip += 1;
            }
        }
    }

    return registers[1];
}

int main() {
    FILE *file = fopen("input_level_23.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char instructions[MAX_INSTRUCTIONS][MAX_LENGTH];
    int num_instructions = 0;

    while (fgets(instructions[num_instructions], MAX_LENGTH, file) != NULL) {
        // Remove newline character if present
        instructions[num_instructions][strcspn(instructions[num_instructions], "\n")] = '\0';
        num_instructions++;
    }

    fclose(file);

    int result = run_program(instructions, num_instructions);
    printf("Value in register b: %d\n", result);

    return 0;
}
