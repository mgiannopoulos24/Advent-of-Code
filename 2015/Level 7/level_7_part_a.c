#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h> 

#define MAX_WIRES 1024
#define MAX_INSTRUCTIONS 2048

typedef struct {
    char name[5];
    uint16_t value;
    int resolved;
} Wire;

typedef struct {
    char operation[10];
    char input1[5];
    char input2[5];
    char output[5];
} Instruction;

Wire wires[MAX_WIRES];
Instruction instructions[MAX_INSTRUCTIONS];
int wireCount = 0, instructionCount = 0;

int findWire(char* name) {
    for (int i = 0; i < wireCount; i++) {
        if (strcmp(wires[i].name, name) == 0) return i;
    }
    strcpy(wires[wireCount].name, name);
    wires[wireCount].resolved = 0;
    return wireCount++;
}

void addInstruction(char* operation, char* input1, char* input2, char* output) {
    strcpy(instructions[instructionCount].operation, operation);
    strcpy(instructions[instructionCount].input1, input1);
    strcpy(instructions[instructionCount].input2, input2);
    strcpy(instructions[instructionCount].output, output);
    instructionCount++;
}

void loadInstructions(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }
    
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        char operation[10] = "";
        char input1[5] = "", input2[5] = "", output[5] = "";
        if (sscanf(line, "%s AND %s -> %s", input1, input2, output) == 3) {
            addInstruction("AND", input1, input2, output);
        } else if (sscanf(line, "%s OR %s -> %s", input1, input2, output) == 3) {
            addInstruction("OR", input1, input2, output);
        } else if (sscanf(line, "%s LSHIFT %s -> %s", input1, input2, output) == 3) {
            addInstruction("LSHIFT", input1, input2, output);
        } else if (sscanf(line, "%s RSHIFT %s -> %s", input1, input2, output) == 3) {
            addInstruction("RSHIFT", input1, input2, output);
        } else if (sscanf(line, "NOT %s -> %s", input1, output) == 2) {
            addInstruction("NOT", input1, "", output);
        } else if (sscanf(line, "%s -> %s", input1, output) == 2) {
            addInstruction("ASSIGN", input1, "", output);
        }
        findWire(output); // Ensure output wire is registered
    }
    fclose(file);
}

void evaluateInstructions() {
    int progress = 1;
    while (progress) {
        progress = 0;
        for (int i = 0; i < instructionCount; i++) {
            Instruction* inst = &instructions[i];
            Wire* outWire = &wires[findWire(inst->output)];
            if (outWire->resolved) continue;

            int idx1 = findWire(inst->input1);
            int idx2 = findWire(inst->input2);
            Wire* inWire1 = &wires[idx1];
            Wire* inWire2 = &wires[idx2];

            uint16_t val1 = inWire1->resolved ? inWire1->value : 0;
            uint16_t val2 = inWire2->resolved ? inWire2->value : 0;
            if (strcmp(inst->input1, "") != 0 && !inWire1->resolved && !isdigit(inst->input1[0])) continue;
            if (strcmp(inst->input2, "") != 0 && !inWire2->resolved && !isdigit(inst->input2[0])) continue;
            if (isdigit(inst->input1[0])) val1 = (uint16_t)atoi(inst->input1);
            if (isdigit(inst->input2[0])) val2 = (uint16_t)atoi(inst->input2);

            if (strcmp(inst->operation, "AND") == 0) {
                outWire->value = val1 & val2;
            } else if (strcmp(inst->operation, "OR") == 0) {
                outWire->value = val1 | val2;
            } else if (strcmp(inst->operation, "LSHIFT") == 0) {
                outWire->value = val1 << val2;
            } else if (strcmp(inst->operation, "RSHIFT") == 0) {
                outWire->value = val1 >> val2;
            } else if (strcmp(inst->operation, "NOT") == 0) {
                outWire->value = ~val1 & 0xFFFF;
            } else if (strcmp(inst->operation, "ASSIGN") == 0) {
                outWire->value = val1;
            }
            outWire->resolved = 1;
            progress = 1;
        }
    }
}

int main() {
    loadInstructions("input_level_7.txt");
    evaluateInstructions();

    int aIdx = findWire("a");
    if (wires[aIdx].resolved) {
        printf("Signal provided to wire a: %u\n", wires[aIdx].value);
    } else {
        printf("Wire a could not be resolved.\n");
    }

    return 0;
}
