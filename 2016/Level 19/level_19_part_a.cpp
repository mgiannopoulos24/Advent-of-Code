#include <iostream>
#include <cmath>

// Function to find the highest power of 2 less than or equal to n
int highestPowerOf2(int n) {
    int p = 1;
    while (p <= n) {
        p *= 2;
    }
    return p / 2;
}

// Function to determine which Elf gets all the presents
int josephusProblem(int n) {
    int hp = highestPowerOf2(n);
    int l = n - hp;
    return 2 * l + 1;
}

int main() {
    int numberOfElves = 3018458; // Change this to the number of elves in your input
    int winningElf = josephusProblem(numberOfElves);
    std::cout << "The Elf that gets all the presents is: " << winningElf << std::endl;
    return 0;
}
