#include <stdio.h>
#include <stdbool.h>

#define NUM_WEAPONS 5
#define NUM_ARMORS 6
#define NUM_RINGS 7

typedef struct {
    char name[20];
    int cost;
    int damage;
    int armor;
} Item;

Item weapons[NUM_WEAPONS] = {
    {"Dagger", 8, 4, 0},
    {"Shortsword", 10, 5, 0},
    {"Warhammer", 25, 6, 0},
    {"Longsword", 40, 7, 0},
    {"Greataxe", 74, 8, 0}
};

Item armors[NUM_ARMORS] = {
    {"None", 0, 0, 0}, // No armor
    {"Leather", 13, 0, 1},
    {"Chainmail", 31, 0, 2},
    {"Splintmail", 53, 0, 3},
    {"Bandedmail", 75, 0, 4},
    {"Platemail", 102, 0, 5}
};

Item rings[NUM_RINGS] = {
    {"None", 0, 0, 0}, // No ring
    {"Damage +1", 25, 1, 0},
    {"Damage +2", 50, 2, 0},
    {"Damage +3", 100, 3, 0},
    {"Defense +1", 20, 0, 1},
    {"Defense +2", 40, 0, 2},
    {"Defense +3", 80, 0, 3}
};

int boss_hit_points = 109;
int boss_damage = 8;
int boss_armor = 2;

int player_hit_points = 100;

bool player_loses(int player_damage, int player_armor) {
    int player_hit = player_hit_points;
    int boss_hit = boss_hit_points;

    while (true) {
        // Player's attack
        int damage_dealt_by_player = player_damage - boss_armor < 1 ? 1 : player_damage - boss_armor;
        boss_hit -= damage_dealt_by_player;
        if (boss_hit <= 0) return false;

        // Boss's attack
        int damage_dealt_by_boss = boss_damage - player_armor < 1 ? 1 : boss_damage - player_armor;
        player_hit -= damage_dealt_by_boss;
        if (player_hit <= 0) return true;
    }
}

int main() {
    int max_gold_spent = 0;

    for (int w = 0; w < NUM_WEAPONS; w++) {
        for (int a = 0; a < NUM_ARMORS; a++) {
            for (int r1 = 0; r1 < NUM_RINGS; r1++) {
                for (int r2 = 0; r2 < NUM_RINGS; r2++) {
                    if (r1 == r2 && r1 != 0) continue; // Skip duplicate rings

                    int total_cost = weapons[w].cost + armors[a].cost + rings[r1].cost + rings[r2].cost;
                    int total_damage = weapons[w].damage + rings[r1].damage + rings[r2].damage;
                    int total_armor = armors[a].armor + rings[r1].armor + rings[r2].armor;

                    if (player_loses(total_damage, total_armor) && total_cost > max_gold_spent) {
                        max_gold_spent = total_cost;
                    }
                }
            }
        }
    }

    printf("The most amount of gold you can spend and still lose the fight is: %d\n", max_gold_spent);

    return 0;
}
