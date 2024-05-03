#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define MAX_SPELLS 5

// Structure to represent a spell
typedef struct Spell {
    char name[20];
    int cost;
    int damage;
    int heal;
    int armor;
    int poison;
    int recharge;
    int effectTimer;
} Spell;

// Function to simulate the player's turn
void playerTurn(int *playerHP, int *playerMana, int *bossHP, int *manaSpent, Spell spells[]) {
    int i;
    for (i = 0; i < MAX_SPELLS; i++) {
        if (spells[i].cost <= *playerMana && spells[i].effectTimer == 0) {
            switch (i) {
                case 0: // Magic Missile
                    *bossHP -= spells[i].damage;
                    break;
                case 1: // Drain
                    *bossHP -= spells[i].damage;
                    *playerHP += spells[i].heal;
                    break;
                case 2: // Shield
                    *playerMana -= spells[i].cost;
                    spells[i].effectTimer = spells[i].armor;
                    break;
                case 3: // Poison
                    spells[i].effectTimer = spells[i].poison;
                    break;
                case 4: // Recharge
                    spells[i].effectTimer = spells[i].recharge;
                    break;
            }
            *playerMana -= spells[i].cost;
            *manaSpent += spells[i].cost;
            break;
        }
    }
}

// Function to simulate the boss's turn
void bossTurn(int *playerHP, int *playerMana, int *bossHP, Spell spells[]) {
    int bossDamage = 10;
    int playerArmor = 0;
    for (int i = 0; i < MAX_SPELLS; i++) {
        if (spells[i].effectTimer > 0) {
            switch (i) {
                case 2: // Shield
                    playerArmor = spells[i].armor;
                    break;
                case 3: // Poison
                    *bossHP -= spells[i].damage;
                    break;
                case 4: // Recharge
                    *playerMana += spells[i].recharge;
                    break;
            }
            spells[i].effectTimer--;
        }
    }
    int damage = (bossDamage - playerArmor > 0) ? (bossDamage - playerArmor) : 1;
    *playerHP -= damage;
}

// Function to check if the game is over
bool gameOver(int playerHP, int playerMana, int bossHP) {
    return (playerHP <= 0 || playerMana < 0 || bossHP <= 0);
}

int main() {
    int playerHP = 50;
    int playerMana = 500;
    int bossHP = 71;
    int manaSpent = 0;

    // Initialize spells
    Spell spells[MAX_SPELLS] = {
        {"Magic Missile", 53, 4, 0, 0, 0, 0, 0},
        {"Drain", 73, 2, 2, 0, 0, 0, 0},
        {"Shield", 113, 0, 0, 7, 0, 0, 0},
        {"Poison", 173, 0, 0, 0, 3, 0, 0},
        {"Recharge", 229, 0, 0, 0, 0, 101, 0}
    };

    int leastManaSpent = -1;

    // Simulate the game
    while (!gameOver(playerHP, playerMana, bossHP)) {
        playerTurn(&playerHP, &playerMana, &bossHP, &manaSpent, spells);
        if (gameOver(playerHP, playerMana, bossHP)) break;
        bossTurn(&playerHP, &playerMana, &bossHP, spells);
    }

    // Output the least amount of mana spent to win the fight
    printf("Least amount of mana spent to win the fight: %d\n", manaSpent);

    return 0;
}
