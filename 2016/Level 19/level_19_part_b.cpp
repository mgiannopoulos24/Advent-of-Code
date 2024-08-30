#include <iostream>
#include <cmath>
#include <algorithm>

// Function to find the highest power of 3 less than or equal to n
int highestPowerOf3(int n) {
    int p = 1;
    while (p <= n) {
        p *= 3;
    }
    return p / 3;
}

// Function to determine which Elf gets all the presents
int josephusProblem(int n) {
    int hp3 = highestPowerOf3(n);
    return n - hp3 + std::max(n - 2 * hp3, 0);
}

int main() {
    int numberOfElves = 3018458; // Change this to the number of elves in your input
    int winningElf = josephusProblem(numberOfElves);
    std::cout << "The Elf that gets all the presents is: " << winningElf << std::endl;
    return 0;
}
