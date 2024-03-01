#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INGREDIENTS 10 // Adjust based on the possible maximum number of ingredients

typedef struct {
    int capacity;
    int durability;
    int flavor;
    int texture;
} Ingredient;

int max(int a, int b) {
    return a > b ? a : b;
}

int calculateScore(Ingredient ingredients[], int amounts[], int numIngredients) {
    int totalCapacity = 0;
    int totalDurability = 0;
    int totalFlavor = 0;
    int totalTexture = 0;

    for (int i = 0; i < numIngredients; i++) {
        totalCapacity += ingredients[i].capacity * amounts[i];
        totalDurability += ingredients[i].durability * amounts[i];
        totalFlavor += ingredients[i].flavor * amounts[i];
        totalTexture += ingredients[i].texture * amounts[i];
    }

    totalCapacity = max(0, totalCapacity);
    totalDurability = max(0, totalDurability);
    totalFlavor = max(0, totalFlavor);
    totalTexture = max(0, totalTexture);

    return totalCapacity * totalDurability * totalFlavor * totalTexture;
}

int main() {
    FILE *file = fopen("input_level_15.txt", "r");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    Ingredient ingredients[MAX_INGREDIENTS];
    int numIngredients = 0;
    char line[200];
    while (fgets(line, sizeof(line), file) && numIngredients < MAX_INGREDIENTS) {
        sscanf(line, "%*s capacity %d, durability %d, flavor %d, texture %d, %*s %*d",
               &ingredients[numIngredients].capacity, &ingredients[numIngredients].durability,
               &ingredients[numIngredients].flavor, &ingredients[numIngredients].texture);
        numIngredients++;
    }
    fclose(file);

    int maxScore = 0;
    for (int i = 0; i <= 100; i++) {
        for (int j = 0; j <= 100 - i; j++) {
            for (int k = 0; k <= 100 - i - j; k++) {
                int l = 100 - i - j - k;
                int amounts[] = {i, j, k, l};
                int score = calculateScore(ingredients, amounts, numIngredients);
                maxScore = max(maxScore, score);
            }
        }
    }

    printf("The maximum score for the cookie is: %d\n", maxScore);
    return 0;
}
