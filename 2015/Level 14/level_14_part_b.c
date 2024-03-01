#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char name[100];
    int speed;
    int flyTime;
    int restTime;
    int points;  // Added to track points for each reindeer
} Reindeer;

// This function now returns the distance at each second
void simulateRace(Reindeer *reindeer, int totalTime, int *distances) {
    int cycleTime = reindeer->flyTime + reindeer->restTime;
    for (int i = 0; i < totalTime; i++) {
        int cyclePosition = i % cycleTime;
        if (cyclePosition < reindeer->flyTime) {
            distances[i] = (i > 0 ? distances[i - 1] : 0) + reindeer->speed;
        } else {
            distances[i] = distances[i - 1];  // Resting, so distance doesn't change
        }
    }
}

// New function to calculate the points for each reindeer
void calculatePoints(Reindeer reindeers[], int numReindeers, int totalTime) {
    int distances[numReindeers][totalTime];
    for (int i = 0; i < numReindeers; i++) {
        simulateRace(&reindeers[i], totalTime, distances[i]);
        reindeers[i].points = 0;  // Initialize points to 0
    }

    for (int i = 0; i < totalTime; i++) {
        int maxDistance = 0;
        for (int j = 0; j < numReindeers; j++) {
            if (distances[j][i] > maxDistance) {
                maxDistance = distances[j][i];
            }
        }
        for (int j = 0; j < numReindeers; j++) {
            if (distances[j][i] == maxDistance) {
                reindeers[j].points++;  // Award point to the leading reindeer(s)
            }
        }
    }
}

int findWinningPoints(Reindeer reindeers[], int numReindeers) {
    int maxPoints = 0;
    for (int i = 0; i < numReindeers; i++) {
        if (reindeers[i].points > maxPoints) {
            maxPoints = reindeers[i].points;
        }
    }
    return maxPoints;
}

int main() {
    FILE *file = fopen("input_level_14.txt", "r");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    Reindeer reindeers[10]; // Adjust size as necessary
    int numReindeers = 0;
    while (fscanf(file, "%s can fly %d km/s for %d seconds, but then must rest for %d seconds.",
                  reindeers[numReindeers].name, &reindeers[numReindeers].speed,
                  &reindeers[numReindeers].flyTime, &reindeers[numReindeers].restTime) != EOF) {
        numReindeers++;
    }
    fclose(file);

    int totalTime = 2503; // Total race time
    calculatePoints(reindeers, numReindeers, totalTime);
    int winningPoints = findWinningPoints(reindeers, numReindeers);
    printf("The winning reindeer has %d points.\n", winningPoints);

    return 0;
}
