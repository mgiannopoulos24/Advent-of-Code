#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_PACKAGES 100

// Function to calculate the product of elements in an array
long long calculateQuantumEntanglement(int *packages, int size) {
    long long product = 1;
    for (int i = 0; i < size; i++) {
        product *= packages[i];
    }
    return product;
}

// Function to find the ideal configuration
void findIdealConfiguration(int *packages, int numPackages, int targetWeight) {
    int minPackages = numPackages;
    long long minQuantumEntanglement = LLONG_MAX;
    int bestGroup[MAX_PACKAGES];
    int group[MAX_PACKAGES];

    // Iterate through all possible combinations
    for (int i = 0; i < (1 << numPackages); i++) {
        int groupWeight = 0;
        int groupSize = 0;
        for (int j = 0; j < numPackages; j++) {
            if (i & (1 << j)) {
                groupWeight += packages[j];
                group[groupSize++] = packages[j];
            }
        }
        if (groupWeight == targetWeight) {
            long long quantumEntanglement = calculateQuantumEntanglement(group, groupSize);
            if (groupSize < minPackages || (groupSize == minPackages && quantumEntanglement < minQuantumEntanglement)) {
                minPackages = groupSize;
                minQuantumEntanglement = quantumEntanglement;
                memcpy(bestGroup, group, groupSize * sizeof(int));
            }
        }
    }

    printf("Ideal configuration with smallest quantum entanglement:\n");
    for (int i = 0; i < minPackages; i++) {
        printf("%d ", bestGroup[i]);
    }
    printf("\nQuantum Entanglement: %lld\n", minQuantumEntanglement);
}

int main() {
    FILE *file = fopen("input_level_24.txt", "r");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    int packages[MAX_PACKAGES];
    int numPackages = 0;
    int totalWeight = 0;

    while (fscanf(file, "%d", &packages[numPackages]) != EOF) {
        totalWeight += packages[numPackages++];
    }
    fclose(file);

    if (totalWeight % 4 != 0) {
        printf("Packages cannot be evenly divided into four groups.\n");
        return 1;
    }

    int targetWeight = totalWeight / 4;
    findIdealConfiguration(packages, numPackages, targetWeight);

    return 0;
}
