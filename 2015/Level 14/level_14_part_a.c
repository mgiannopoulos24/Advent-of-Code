#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char name[100];
    int speed;
    int flyTime;
    int restTime;
} Reindeer;

int simulateRace(int speed, int flyTime, int restTime, int totalTime) {
    int cycleTime = flyTime + restTime;
    int fullCycles = totalTime / cycleTime;
    int remainingTime = totalTime % cycleTime;
    int distance = fullCycles * speed * flyTime;
    distance += (remainingTime < flyTime ? remainingTime : flyTime) * speed;
    return distance;
}

int findWinningDistance(Reindeer reindeers[], int numReindeers, int totalTime) {
    int maxDistance = 0;
    for (int i = 0; i < numReindeers; i++) {
        int distance = simulateRace(reindeers[i].speed, reindeers[i].flyTime, reindeers[i].restTime, totalTime);
        if (distance > maxDistance) {
            maxDistance = distance;
        }
    }
    return maxDistance;
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
    int winningDistance = findWinningDistance(reindeers, numReindeers, totalTime);
    printf("The winning reindeer traveled %d km.\n", winningDistance);

    return 0;
}
