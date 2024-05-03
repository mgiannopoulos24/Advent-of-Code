#include <stdio.h>
#include <math.h>

int find_house_number(int puzzle_input) {
    int house_number = 1;
    while (1) {
        int total_presents = 0;
        int elf_visits = 0;
        int limit = (int)sqrt(house_number);
        for (int elf = 1; elf <= limit; elf++) {
            if (house_number % elf == 0) {
                if (house_number / elf <= 50) {
                    total_presents += elf * 11;
                }
                if (elf != house_number / elf && house_number / (house_number / elf) <= 50) {
                    total_presents += (house_number / elf) * 11;
                }
            }
            elf_visits++;
        }
        if (total_presents >= puzzle_input) {
            return house_number;
        }
        house_number++;
    }
}

int main() {
    int puzzle_input = 36000000;
    int lowest_house_number = find_house_number(puzzle_input);
    printf("The lowest house number to get at least as many presents as the puzzle input is: %d\n", lowest_house_number);
    return 0;
}
